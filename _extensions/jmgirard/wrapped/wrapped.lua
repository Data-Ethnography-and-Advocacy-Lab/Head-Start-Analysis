-- wrapped: wrap body text around figures marked .wrap-left or .wrap-right.
-- A #tbl- table float wraps the same way in all four formats and keeps its
-- normal layout when its table is raw LaTeX in LaTeX (D-021, D-022, D-024);
-- a raw Word XML table wraps in Word (D-025). "Figure" below covers a table
-- float there.
--
-- Runs at pre-quarto, after Quarto creates FloatRefTarget nodes. HTML output,
-- revealjs included, floats each wrapped figure inside a .wrap-group div
-- that also holds the paragraphs after it. Two passes first find the
-- figures that keep a normal layout: those in a context that cannot wrap,
-- such as a list or the margin, and those with no paragraph after them.
-- The float pass also keeps the figures of a code cell that makes more than
-- one figure normal (D-010).
-- LaTeX output puts each wrapped figure in the wrappedfigure environment of
-- wrapped.tex, which calls wrapfig, and a table float, its table rewritten
-- as a tabular, in wrappedtable, and writes \wrappedstop after the
-- paragraphs of its group. Typst output puts each wrapped figure
-- and the paragraphs of its group in a wrap-content call from the bundled
-- wrap-it package. Word output groups each wrapped figure with its
-- paragraphs as in HTML, and marks the group with the frame's side, width,
-- and gap in twips; wrapped-docx.lua turns the group into a Word frame at
-- post-render, or for a table float into a floating table. A LaTeX document with classoption: twocolumn and every other
-- format keep a normal figure. In LaTeX, wrapped.tex also sets a normal
-- figure where it finds columns while LaTeX runs. A figure with no wrap
-- class is left as it is in every format.

local SIDES = { ["wrap-left"] = "left", ["wrap-right"] = "right" }

local DEFAULTS = {
  width = "40%",
  gap = "1em",
  breakpoint = "576px",
}

-- The document's values, read from the `wrapped:` metadata in the first pass.
local options = {
  width = DEFAULTS.width,
  gap = DEFAULTS.gap,
  breakpoint = DEFAULTS.breakpoint,
}

local is_html = quarto.doc.is_format("html:js")
local is_revealjs = quarto.doc.is_format("revealjs")
local is_dashboard = quarto.doc.is_format("dashboard")
-- An HTML page with Quarto's page layout. revealjs slides have no margin
-- column, no margin captions, and no page-wide columns.
local has_page_layout = is_html and not is_revealjs
-- PDF through LaTeX. Beamer is a separate format and keeps a normal figure.
-- The first pass sets is_latex to false for a two-column document.
local is_latex = FORMAT == "latex"
-- PDF through Typst.
local is_typst = FORMAT == "typst"
-- Word.
local is_docx = FORMAT == "docx"
local two_column = false
-- Set in the first pass: meta_fig_column_set when the document sets
-- fig-column, and meta_column_set when it sets fig-column or column.
local meta_fig_column_set, meta_column_set = false, false
-- found_wrap: a figure, table float, image, div, link, span, heading,
-- code, or table carries a wrap class, so a format that cannot wrap prints
-- its one warning. placed: a figure, table float, or image wraps, so the
-- document needs the stylesheet, the LaTeX environment, or wrap-it.
local found_wrap = false
local placed = false

-- True in a format that wraps, which prints a warning for each figure that
-- keeps its normal layout, or one for a code cell whose figures keep it.
local function wraps()
  return is_html or is_latex or is_typst or is_docx
end

-- CSS units that a width, gap, or breakpoint may use, in lowercase: the
-- length units of CSS Values and Units Level 4 (section 6).
local UNITS = {
  px = true, pt = true, pc = true, ["in"] = true, cm = true, mm = true, q = true,
  em = true, rem = true, ex = true, rex = true, cap = true, rcap = true,
  ch = true, rch = true, ic = true, ric = true, lh = true, rlh = true,
  vw = true, vh = true, vi = true, vb = true, vmin = true, vmax = true,
  svw = true, svh = true, svi = true, svb = true, svmin = true, svmax = true,
  lvw = true, lvh = true, lvi = true, lvb = true, lvmin = true, lvmax = true,
  dvw = true, dvh = true, dvi = true, dvb = true, dvmin = true, dvmax = true,
  cqw = true, cqh = true, cqi = true, cqb = true, cqmin = true, cqmax = true,
}

-- CSS functions whose result is a length, read by calc_value.
local FUNCTIONS = { calc = true, min = true, max = true, clamp = true }

-- Pixels for each absolute CSS unit. Every other unit, and a percent, has a
-- base that depends on the page, which the filter does not know but which
-- is above zero.
local PX = { px = 1, pt = 4 / 3, pc = 16, ["in"] = 96, cm = 96 / 2.54, mm = 96 / 25.4, q = 96 / 101.6 }

-- What the filter knows of an expression's result is a bound: nil when
-- nothing is known, or a table with `lo` and `hi`, each a sum (or nil when
-- that side has no known limit), `exact` when the result is the sum
-- itself, and `bad` when the result is not finite. A sum is a table of
-- coefficients by unit, where "" holds the pixels of the absolute units
-- and "%" the percent. A value with var() has no bound.

-- The sum `a` + `k` * `b`, or nil when either is nil.
local function sum_add(a, b, k)
  if a == nil or b == nil then return nil end
  local r = {}
  for u, c in pairs(a) do r[u] = c end
  for u, c in pairs(b) do r[u] = (r[u] or 0) + k * c end
  return r
end

-- True when a sum is zero or below for every base above zero.
local function sum_not_positive(f)
  if f == nil then return false end
  for _, c in pairs(f) do
    if not (c <= 0) then return false end
  end
  return true
end

-- True when a sum is below zero for every base above zero: each
-- coefficient is zero or below, and one is below zero.
local function sum_negative(f)
  if not sum_not_positive(f) then return false end
  for _, c in pairs(f) do
    if c < 0 then return true end
  end
  return false
end

-- The unit and coefficient of a sum with one unit or none, or nil.
local function sum_term(f)
  local unit, coef = "", 0
  for u, c in pairs(f) do
    if c ~= 0 then
      if coef ~= 0 then return nil end
      unit, coef = u, c
    end
  end
  return unit, coef
end

local function exact_bound(f)
  return { lo = f, hi = f, exact = true }
end

-- The bound of `a` + `k` * `b`, for `k` of 1 or -1.
local function bound_add(a, b, k)
  if a == nil or b == nil then return nil end
  if a.bad or b.bad then return { bad = true } end
  local blo, bhi = b.lo, b.hi
  if k < 0 then blo, bhi = bhi, blo end
  return { lo = sum_add(a.lo, blo, k), hi = sum_add(a.hi, bhi, k), exact = a.exact and b.exact }
end

-- The bound of `a` times the number `k`.
local function bound_scale(a, k)
  if a == nil then return nil end
  if a.bad or k ~= k or k == math.huge or k == -math.huge then return { bad = true } end
  local lo, hi = a.lo, a.hi
  if k == 0 then lo, hi = {}, {} end
  if k < 0 then lo, hi = hi, lo end
  return { lo = sum_add({}, lo, k), hi = sum_add({}, hi, k), exact = a.exact }
end

-- The number of an exact bound with no unit, or nil.
local function bound_number(a)
  if a == nil or a.bad or not a.exact then return nil end
  local unit, coef = sum_term(a.lo)
  if unit ~= "" then return nil end
  return coef
end

-- The bound of min() (`pick` of -1) or max() (`pick` of 1) over a list of
-- bounds, where false is a term with none. With exact terms in one unit, the result is exact. Otherwise any
-- term's upper limit bounds a min() from above, and any term's lower limit
-- bounds a max() from below. A max() is zero or below only when every term
-- is. A term that is not finite leaves the result unknown, unless every
-- term is not finite.
local function bound_pick(list, pick)
  local bad = 0
  for _, a in ipairs(list) do
    if not a then return nil end
    if a.bad then bad = bad + 1 end
  end
  if bad == #list then return { bad = true } end
  if bad > 0 then return nil end
  local one, unit, best = true, nil, nil
  for _, a in ipairs(list) do
    local u, c
    if a.exact then u, c = sum_term(a.lo) end
    if c == nil or (c ~= 0 and unit ~= nil and u ~= unit) then
      one = false
      break
    end
    if c ~= 0 then unit = u end
    if best == nil or c * pick > best * pick then best = c end
  end
  if one then return exact_bound({ [unit or ""] = best }) end
  local lo, hi, all_low = nil, nil, true
  for _, a in ipairs(list) do
    if not sum_not_positive(a.hi) then all_low = false end
    if pick < 0 and a.hi and (hi == nil or sum_not_positive(a.hi)) then hi = a.hi end
    if pick > 0 and lo == nil then lo = a.lo end
  end
  if pick > 0 and all_low then hi = {} end
  return { lo = lo, hi = hi }
end

-- The type of the math expression in `s` from position `i`: "number",
-- "length" (a length or a percent), or "any" (a var() term, whose type is
-- not known). Returns the type, the position after the expression, and its
-- bound, or nil when the text is not such an expression. The rules are calc()'s in
-- CSS Values and Units Level 3 (section 8.1), with min(), max(), and
-- clamp() from Level 4 (section 10.2). A + or - between terms needs
-- whitespace on both sides, the terms of a sum share a type, a product
-- has a number factor, and a divisor is a number. min() and max() take
-- one or more terms, and clamp() takes three. With `no_percent`, for a
-- media query, a percent is not a length and a var() term is not read,
-- because a media query takes neither.
local calc_sum

-- Skips whitespace. Returns the position after it and true when there was
-- some.
local function skip(s, i)
  local j = s:match("^%s*()", i)
  return j, j > i
end

-- The common type of two terms of a sum or two arguments, or nil.
local function same_type(a, b)
  if a == "any" then return b end
  if b == "any" or a == b then return a end
  return nil
end

-- A function's argument list from position `i`, after its "(": `count`
-- terms, or one or more when `count` is nil. Returns their common type, the
-- position after the ")", and the list of their bounds, with false for a
-- term that has none.
local function calc_args(s, i, no_percent, count)
  local t, n, bounds = "any", 0, {}
  repeat
    local ti, bi
    ti, i, bi = calc_sum(s, skip(s, i), no_percent)
    if not ti then return nil end
    t, n = same_type(t, ti), n + 1
    if not t then return nil end
    bounds[n] = bi or false
    i = skip(s, i)
    local more = s:sub(i, i) == ","
    if more then i = i + 1 end
  until not more
  if s:sub(i, i) ~= ")" or (count and n ~= count) then return nil end
  return t, i + 1, bounds
end

-- One value: a number, a length, a percent, a sum in parentheses, a math
-- function, or a var() term.
local function calc_value(s, i, no_percent)
  local num, unit, j = s:match("^([+-]?%d*%.?%d+)(%a*%%?)()", i)
  if num then
    -- A float, so a large number does not wrap around as an integer.
    local n = tonumber(num) + 0.0
    if unit == "" then return "number", j, exact_bound({ [""] = n }) end
    if unit == "%" then
      if no_percent then return nil end
      return "length", j, exact_bound({ ["%"] = n })
    end
    local u = unit:lower()
    if not UNITS[u] then return nil end
    if PX[u] then return "length", j, exact_bound({ [""] = n * PX[u] }) end
    return "length", j, exact_bound({ [u] = n })
  end
  if s:sub(i, i) == "(" then
    local t, b
    t, i, b = calc_sum(s, skip(s, i + 1), no_percent)
    if not t then return nil end
    i = skip(s, i)
    if s:sub(i, i) ~= ")" then return nil end
    return t, i + 1, b
  end
  local name, k = s:match("^(%a+)%(()", i)
  if not name then return nil end
  name = name:lower()
  if name == "var" then
    if no_percent then return nil end
    k = s:match("^%s*%-%-[%w_-]+()", k)
    if not k then return nil end
    k = skip(s, k)
    if s:sub(k, k) == ")" then return "any", k + 1 end
    if s:sub(k, k) ~= "," then return nil end
    local t
    t, k = calc_args(s, k + 1, no_percent, 1)
    return t and "any", k
  end
  if not FUNCTIONS[name] then return nil end
  local count = name == "calc" and 1 or name == "clamp" and 3 or nil
  local t, list
  t, k, list = calc_args(s, k, no_percent, count)
  if not t then return nil end
  local b
  if name == "calc" then
    b = list[1] or nil
  elseif name == "clamp" then
    b = bound_pick({ list[1], bound_pick({ list[2], list[3] }, -1) or false }, 1)
  else
    b = bound_pick(list, name == "min" and -1 or 1)
  end
  return t, k, b
end

-- A product: values joined by * or /.
local function calc_product(s, i, no_percent)
  local t, b
  t, i, b = calc_value(s, i, no_percent)
  if not t then return nil end
  while true do
    local j = skip(s, i)
    local op = s:sub(j, j)
    if op ~= "*" and op ~= "/" then return t, i, b end
    local u, c
    u, i, c = calc_value(s, skip(s, j + 1), no_percent)
    if not u then return nil end
    if op == "/" then
      if u == "length" then return nil end
      local k = u == "number" and bound_number(c)
      if c and c.bad then
        -- A value divided by a number that is not finite is zero or NaN,
        -- so the bound leaves the result unknown.
        b = nil
      elseif k == 0 then
        b = b and { bad = true }
      else
        b = k and bound_scale(b, 1 / k) or nil
      end
    elseif t == "number" then
      local k = bound_number(b)
      if b and b.bad then
        b = c and { bad = true }
      else
        b = k and bound_scale(c, k) or nil
      end
      t = u
    elseif u == "length" and t == "length" then
      return nil
    else
      local k = u == "number" and bound_number(c)
      if b and c and c.bad then
        b = { bad = true }
      else
        b = k and bound_scale(b, k) or nil
      end
      if t == "any" and u == "length" then t = "length" end
    end
  end
end

-- A sum: products joined by + or -.
calc_sum = function(s, i, no_percent)
  local t, b
  t, i, b = calc_product(s, i, no_percent)
  if not t then return nil end
  while true do
    local j, before = skip(s, i)
    local op = s:sub(j, j)
    if (op ~= "+" and op ~= "-") or not before then return t, i, b end
    local k, after = skip(s, j + 1)
    if not after then return t, i, b end
    local u, c
    u, i, c = calc_product(s, k, no_percent)
    if not u then return nil end
    t = same_type(t, u)
    if not t then return nil end
    b = bound_add(b, c, op == "+" and 1 or -1)
  end
end

-- True for a calc(), min(), max(), or clamp() value whose result is a
-- length or a percent. Also returns the value's bound.
local function calc_length(s, no_percent)
  local t, i, b = calc_value(s, 1, no_percent)
  return t ~= nil and t ~= "number" and i == #s + 1, b
end

-- Returns the wrap class in a class list, or nil.
local function wrap_class(classes)
  for _, c in ipairs(classes or {}) do
    if SIDES[c] then return c end
  end
  return nil
end

-- Removes every wrap class from a class list, in place.
local function strip_wrap(classes)
  for i = #classes, 1, -1 do
    if SIDES[classes[i]] then classes:remove(i) end
  end
end

-- Returns the CSS length for a value, or nil when the value is not a
-- number, a percent, a number with a CSS unit, or a calc(), min(), max(),
-- or clamp() expression that calc_length reads as a length. A unitless
-- number is a pixel length, as in Pandoc. With `no_percent`, a percent is
-- not a length.
local function css_length(value, no_percent)
  local s = pandoc.utils.stringify(value)
  local fn = s:match("^(%a+)%(")
  if fn and FUNCTIONS[fn:lower()] then
    if calc_length(s, no_percent) then return s end
    return nil
  end
  local num, unit = s:match("^(%d*%.?%d+)(%a*%%?)$")
  if not num then return nil end
  if unit == "" then return num .. "px" end
  if unit == "%" then
    if no_percent then return nil end
    return s
  end
  if UNITS[unit:lower()] then return s end
  return nil
end

-- True for a CSS length from css_length whose number is zero, such as 0%
-- or 0in. A function value is not read here; never_positive reads it.
local function is_zero(length)
  local num = length:match("^(%d*%.?%d+)%a*%%?$")
  return num ~= nil and tonumber(num) == 0
end

-- True for a function value from css_length whose result is zero or below,
-- or not finite, at every page and font size: CSS sets a negative width or
-- a NaN to zero and an infinite one to the largest width. A value with
-- var() is never read so.
local function never_positive(length)
  if not length:match("^%a+%(") then return false end
  local _, b = calc_length(length)
  return b ~= nil and (b.bad or sum_not_positive(b.hi))
end

-- True for a function value from css_length whose result is below zero, or
-- not finite, at every page and font size. A negative gap pulls the text
-- over the figure. A value with var() is never read so, and a result of
-- zero is not below zero.
local function always_negative(length)
  if not length:match("^%a+%(") then return false end
  local _, b = calc_length(length)
  return b ~= nil and (b.bad or sum_negative(b.hi))
end

-- Returns the CSS length for a value, or `fallback` with a warning when the
-- value is not a length. `what` and `where` name the value in the warning.
-- A width of zero is not a width, because a box with no width leaves the
-- figure nothing to fill. A gap or a breakpoint of zero is a length, but a
-- gap below zero is not a gap. A breakpoint passes `no_percent`, because a
-- media query takes no percent.
local function checked_length(value, fallback, what, where, no_percent)
  if value == nil then return fallback end
  local length = css_length(value, no_percent)
  if length and what == "width" and is_zero(length) then
    quarto.log.warning(string.format(
      'wrapped: width "%s" on %s is zero, so it is %s.',
      pandoc.utils.stringify(value), where, fallback))
    return fallback
  end
  if length and what == "width" and never_positive(length) then
    quarto.log.warning(string.format(
      'wrapped: width "%s" on %s is zero, negative, or not finite, so it is %s.',
      pandoc.utils.stringify(value), where, fallback))
    return fallback
  end
  if length and (what == "wrap-gap" or what == "gap") and always_negative(length) then
    quarto.log.warning(string.format(
      'wrapped: %s "%s" on %s is negative or not finite, so it is %s.',
      what, pandoc.utils.stringify(value), where, fallback))
    return fallback
  end
  if length then return length end
  quarto.log.warning(string.format(
    'wrapped: %s "%s" on %s is not %s, so it is %s.',
    what, pandoc.utils.stringify(value), where,
    no_percent and "a length" or "a length or a percent", fallback))
  return fallback
end

-- Absolute lengths that LaTeX reads as written. TeX also reads em and ex.
local TEX_UNITS = { pt = true, pc = true, ["in"] = true, cm = true, mm = true, em = true, ex = true }

-- A number as TeX reads it: decimal, never with an exponent as %g writes
-- it, and with no trailing zeros.
local function tex_number(n)
  local s = string.format("%.6f", n):gsub("0+$", ""):gsub("%.$", "")
  return s
end

-- Returns the LaTeX length for a CSS length from css_length, or nil when
-- LaTeX has no such length. A percent is that fraction of \linewidth, and a
-- px length is 0.75 pt for each px, as in CSS.
local function tex_length(length)
  local num, unit = length:match("^(%d*%.?%d+)(%a*%%?)$")
  if not num then return nil end
  if unit == "%" then return tex_number(tonumber(num) / 100) .. "\\linewidth" end
  unit = unit:lower()
  if unit == "px" then return tex_number((tonumber(num) + 0.0) * 0.75) .. "pt" end
  if TEX_UNITS[unit] then return num .. unit end
  return nil
end

-- Returns the LaTeX length for a value, or the LaTeX length of `fallback`
-- with a warning when the value is not a length that LaTeX can use.
-- `fallback` is a CSS length that tex_length converts.
local function checked_tex_length(value, fallback, what, where)
  local length = checked_length(value, fallback, what, where)
  local tex = tex_length(length)
  if tex then return tex end
  quarto.log.warning(string.format(
    'wrapped: %s "%s" on %s is not a length that LaTeX can use, so it is %s.',
    what, length, where, fallback))
  return tex_length(fallback)
end

-- Lengths that Typst reads as written: absolute units and em.
local TYPST_UNITS = { pt = true, mm = true, cm = true, ["in"] = true, em = true }

-- Returns the Typst length for a CSS length from css_length, or nil when
-- Typst has no such length. A percent stays a percent. A px length is 0.75
-- pt for each px, as in CSS, and a pc length is 12 pt. Typst reads units
-- only in lowercase.
local function typst_length(length)
  local num, unit = length:match("^(%d*%.?%d+)(%a*%%?)$")
  if not num then return nil end
  if unit == "%" then return num .. "%" end
  unit = unit:lower()
  -- A float, so a large number does not wrap around as an integer.
  if unit == "px" then return tex_number((tonumber(num) + 0.0) * 0.75) .. "pt" end
  if unit == "pc" then return tex_number((tonumber(num) + 0.0) * 12) .. "pt" end
  if TYPST_UNITS[unit] then return num .. unit end
  return nil
end

-- Returns the Typst length for a value, or the Typst length of `fallback`
-- with a warning when the value is not a length that Typst can use.
local function checked_typst_length(value, fallback, what, where)
  local length = checked_length(value, fallback, what, where)
  local typ = typst_length(length)
  if typ then return typ end
  quarto.log.warning(string.format(
    'wrapped: %s "%s" on %s is not a length that Typst can use, so it is %s.',
    what, length, where, fallback))
  return typst_length(fallback)
end

-- True for a Typst percent of 100% or more. A box that wide leaves wrap-it
-- no room for the text. At 100% the text runs one word to a line outside
-- the text area, and above 100% part of it drops from the page.
local function typst_too_wide(length)
  local n = length:match("^(%d*%.?%d+)%%$")
  return n ~= nil and tonumber(n) >= 100
end

-- True for a Typst width and gap that are both percents with a sum of 100%
-- or more, which leaves wrap-it no room for the text beside the figure.
local function typst_fills(width, gap)
  local w = width:match("^(%d*%.?%d+)%%$")
  local g = gap:match("^(%d*%.?%d+)%%$")
  return w ~= nil and g ~= nil and (tonumber(w) + 0.0) + (tonumber(g) + 0.0) >= 100
end

-- The opening of a wrap-content call, which takes the figure box as its
-- first argument. The width sets the box's grid column, where a percent is
-- a share of the text width. Set on the box itself, a percent would be a
-- share of the box's own measured column. wrap-it's grid follows the text
-- direction, so the helpers in TYPST_HEADER turn the physical side into
-- the side wrap-it reads, in context. The filter cannot know the text
-- width, so the call runs inside `layout`, and wrapped-width there turns a
-- width that with the gap is as wide as the text or wider into the
-- document default, or into the built-in default when the document default
-- with the gap is that wide too. Typst has no way to print a warning for
-- it, so the filter warns only where it can see the sum: a figure's own
-- percent width with a percent gap. A figure with no width of its own was
-- checked with the metadata. The closing `]))` is written by the grouping
-- pass.
local function typst_open(class, width, gap, where)
  local side = SIDES[class]
  local w = checked_typst_length(width, options.width, "width", where)
  local g = checked_typst_length(gap, options.gap, "wrap-gap", where)
  local fallback = options.width
  if typst_fills(typst_length(fallback), g) then fallback = DEFAULTS.width end
  if typst_too_wide(w) then
    quarto.log.warning(string.format(
      'wrapped: width "%s" on %s leaves no room for text in Typst, so it is %s.',
      w, where, fallback))
    w = typst_length(fallback)
  elseif width ~= nil and typst_fills(w, g) then
    quarto.log.warning(string.format(
      'wrapped: width "%s" and wrap-gap "%s" on %s leave no room for text in Typst, so the width is %s.',
      w, g, where, fallback))
    w = typst_length(fallback)
  end
  return string.format(
    "#layout(wrapped-size => wrap-content(align: top + wrapped-side(%s), columns: wrapped-columns(%s, wrapped-width(wrapped-size, %s, %s, %s, %s)), column-gutter: %s, box(width: 100%%)[",
    side, side, g, w, typst_length(options.width), typst_length(DEFAULTS.width), g)
end

-- The Typst header: the wrap-it import, the side helpers, and the width
-- helpers. Typst takes a text's direction from `dir`, or from its language
-- when `dir` is auto, and these are the languages that Typst sets right to
-- left. wrapped-abs turns a percent into that share of the text width,
-- which `size` from `layout` gives. wrapped-width returns the first width
-- that with the gap is narrower than the text, or the last when none is.
-- On a page of auto width the text width is infinite, so it returns the
-- first.
local TYPST_HEADER = [[
#import "@preview/wrap-it:0.1.1": wrap-content
#let wrapped-rtl() = text.dir == rtl or (text.dir == auto and text.lang in ("ar", "dv", "fa", "he", "ks", "pa", "ps", "sd", "ug", "ur", "yi"))
#let wrapped-side(side) = if wrapped-rtl() { if side == right { left } else { right } } else { side }
#let wrapped-columns(side, width) = if wrapped-side(side) == right { (1fr, width) } else { (width, 1fr) }
#let wrapped-abs(size, x) = if type(x) == ratio { x * size.width } else { x.to-absolute() }
#let wrapped-width(size, gap, ..widths) = { if size.width.pt() == float.inf { return widths.pos().first() }; let fits = widths.pos().filter(w => wrapped-abs(size, w) + wrapped-abs(size, gap) < size.width); if fits.len() > 0 { fits.first() } else { widths.pos().last() } }]]

-- Twips (1/20 pt) for each unit of a length that Word can use. A px length
-- is 1/96 in, as in CSS, and an em is 12 pt.
local TWIPS = { ["in"] = 1440, cm = 1440 / 2.54, mm = 144 / 2.54, pt = 20, pc = 240, px = 15, em = 240 }

-- Word's text width in twips with no page size or margins in the reference
-- doc: Word shows a Letter page with 1 in margins, 6.5 in of text.
local DOCX_TEXT_WIDTH = 9360
local docx_text_width = DOCX_TEXT_WIDTH

-- The reference doc's body w:sectPr sets columns, so Word keeps normal
-- figures and table floats, as LaTeX does in a two-column document.
local docx_columns = false
-- The reference doc that cannot be read, for a warning when a figure wraps.
local docx_unread = nil

-- Reads the reference doc that the render uses. Returns its text width:
-- the page width less the left and right margins and the gutter in the
-- w:sectPr of its body. With no reference doc, or a body w:sectPr with no
-- w:pgSz or no w:pgMar, it is DOCX_TEXT_WIDTH. Also returns true when that
-- w:sectPr sets more than one column. A reference doc that cannot be read
-- is noted in docx_unread.
local function read_docx_text_width()
  local path = PANDOC_WRITER_OPTIONS.reference_doc
  if not path then return DOCX_TEXT_WIDTH, false end
  local ok, xml = pcall(function()
    local f = assert(io.open(path, "rb"))
    local bytes = f:read("a")
    f:close()
    for _, entry in ipairs(pandoc.zip.Archive(bytes).entries) do
      if entry.path == "word/document.xml" then return entry:contents() end
    end
  end)
  if not (ok and xml) then
    docx_unread = path
    return DOCX_TEXT_WIDTH, false
  end
  -- The body's own w:sectPr is the last one in the file. It can be empty
  -- and self-closing, and then it sets nothing.
  local last = nil
  local from = 1
  while true do
    local s = xml:find("<w:sectPr[%s>/]", from)
    if not s then break end
    last = s
    from = s + 1
  end
  local tag = last and xml:match("^<w:sectPr[^>]*>", last)
  if not tag or tag:match("/>$") then return DOCX_TEXT_WIDTH, false end
  local sect = xml:match("^<w:sectPr[^>]*>.-</w:sectPr>", last)
  if not sect then return DOCX_TEXT_WIDTH, false end
  local cols = tonumber(sect:match('<w:cols[^>]-w:num="(%d+)"') or "1")
  local page = sect:match('<w:pgSz[^>]-w:w="(%d+)"')
  local margins = sect:match("<w:pgMar[^>]*>")
  if not (page and margins) then return DOCX_TEXT_WIDTH, cols > 1 end
  local left = margins:match('w:left="(%d+)"') or "0"
  local right = margins:match('w:right="(%d+)"') or "0"
  local gutter = margins:match('w:gutter="(%d+)"') or "0"
  return tonumber(page) - tonumber(left) - tonumber(right) - tonumber(gutter), cols > 1
end

-- Returns the twips for a CSS length from css_length, or nil when Word has
-- no such length, and "wide" when the length is the text width or more,
-- which leaves no room for text. A percent above 0% and below 100% is that
-- share of the text width. With `positive` (a width), a length of 0 twips
-- is not a length, because a frame with no width takes the width of its
-- content.
local function docx_measure(length, positive)
  local num, unit = length:match("^(%d*%.?%d+)(%a*%%?)$")
  if not num then return nil end
  -- A float, so a large number does not wrap around as an integer.
  local n = tonumber(num) + 0.0
  local twips
  if unit == "%" then
    if n >= 100 then return nil, "wide" end
    if n <= 0 then return nil end
    twips = n / 100 * docx_text_width
  else
    local per = TWIPS[unit:lower()]
    if not per then return nil end
    twips = n * per
  end
  twips = math.floor(twips + 0.5)
  if twips >= docx_text_width then return nil, "wide" end
  if positive and twips <= 0 then return nil end
  return twips
end

local function docx_twips(length, positive)
  return (docx_measure(length, positive))
end

-- Returns the twips for a value, or the twips of `fallback` with a warning
-- when the value is not a length that Word can use. Also returns true when
-- the value gave way to `fallback`. A width as wide as the text or wider
-- with `gap`, the gap that a figure with its own gap ends up with, names
-- that gap in its warning.
local function checked_docx_twips(value, fallback, what, where, positive, gap)
  local length = checked_length(value, fallback, what, where)
  local twips, why = docx_measure(length, positive)
  if twips then return twips, value ~= nil and length ~= css_length(value) end
  if why == "wide" and gap then
    quarto.log.warning(string.format(
      'wrapped: %s "%s" and wrap-gap "%s" on %s leave no room for text in Word, so the width is %s.',
      what, length, gap, where, fallback))
  else
    quarto.log.warning(string.format(
      'wrapped: %s "%s" on %s %s, so it is %s.', what, length, where,
      why == "wide" and "leaves no room for text in Word" or "is not a length that Word can use",
      fallback))
  end
  return docx_twips(fallback, positive), true
end

-- A frame's side, width, and gap in twips, as "right 3744 240", and its
-- width. A figure that sets its own width or a gap that Word can use, and
-- whose width plus gap is as wide as the text or wider, takes the first of
-- the document default and 40% that with the gap is narrower than the
-- text, or 40% when neither is, with a warning. Typst picks its width in
-- the same order. A width that Word cannot use takes that same width. Each
-- value is rounded to twips before the sum. A figure with neither was
-- checked with the metadata, which can leave 40% plus the gap still as
-- wide as the text.
local function docx_frame(class, width, gap, where)
  local g, g_fell = checked_docx_twips(gap, options.gap, "wrap-gap", where)
  local own_gap = gap ~= nil and not g_fell
  local g_length = own_gap and css_length(gap) or options.gap
  local fallback = DEFAULTS.width
  if docx_twips(options.width, true) + g < docx_text_width then fallback = options.width end
  local asked = width or options.width
  local w, fell = checked_docx_twips(asked, fallback, "width", where, true, own_gap and g_length)
  if not fell and (width ~= nil or own_gap) and w + g >= docx_text_width then
    quarto.log.warning(string.format(
      'wrapped: width "%s" and wrap-gap "%s" on %s leave no room for text in Word, so the width is %s.',
      css_length(asked), g_length, where, fallback))
    w = docx_twips(fallback, true)
  end
  return string.format("%s %d %d", SIDES[class], w, g), w
end

-- Sets each image in a list of blocks to the frame's width, so the image
-- fills the frame. With `by_column`, for a table float's box, an image in a
-- table cell takes at most its cell's share of the box, less the 108-twip
-- cell margins on each side that Pandoc's Table style sets. The shares are
-- the ones wrapped-docx.lua's fit_table gives the columns: each column's
-- relative width, or an equal share for a column with none. An image with a
-- smaller width of its own keeps it, so an icon stays small.
local function column_shares(tbl)
  local shares, sum = {}, 0
  for i, spec in ipairs(tbl.colspecs) do
    local w = spec[2]
    shares[i] = (type(w) == "number" and w > 0) and w or 1
    sum = sum + shares[i]
  end
  for i = 1, #shares do shares[i] = shares[i] / sum end
  return shares
end

-- The shares of a LaTeX table float's box for a table whose columns have
-- no widths: each column's share is in proportion to the length of the
-- longest text in a cell of that column that spans no other column, and a
-- column with no such text counts as one character. A table with a width
-- on any column takes column_shares, as in Word. Pandoc leaves the cells
-- that a row span covers out of the rows below it, so the walk skips the
-- columns that a span from a row above still covers.
local function text_shares(tbl)
  for _, spec in ipairs(tbl.colspecs) do
    local w = spec[2]
    if type(w) == "number" and w > 0 then return column_shares(tbl) end
  end
  local shares, sum = {}, 0
  local function weigh(rows)
    local covered = {}
    for _, row in ipairs(rows) do
      local col = 1
      for _, cell in ipairs(row.cells) do
        while (covered[col] or 0) > 0 do col = col + 1 end
        if cell.col_span == 1 then
          local n = utf8.len(pandoc.utils.stringify(cell.contents)) or 1
          shares[col] = math.max(shares[col] or 1, n)
        end
        for c = col, col + cell.col_span - 1 do covered[c] = cell.row_span end
        col = col + cell.col_span
      end
      for c, n in pairs(covered) do covered[c] = math.max(n - 1, 0) end
    end
  end
  weigh(tbl.head.rows)
  for _, body in ipairs(tbl.bodies) do
    weigh(body.head)
    weigh(body.body)
  end
  weigh(tbl.foot.rows)
  for i = 1, #tbl.colspecs do
    shares[i] = shares[i] or 1
    sum = sum + shares[i]
  end
  for i = 1, #shares do shares[i] = shares[i] / sum end
  return shares
end

-- The column widths that a tbl-colwidths value of percentages gives a
-- table, as Quarto gives them: each column takes its value, or the last
-- value past the list's end, a list whose sum passes 100 is scaled to 100,
-- and each width is a share of the line. Quarto applies the value after
-- pre-quarto filters, and only to a table with no spans that it did not
-- parse from raw HTML (its quarto-postprocess attribute), so the filter
-- applies it to a table it rewrites. The filter reads the value on the
-- table or its float, where a code cell's option lands. Quarto takes a
-- document's value out of the metadata before filters run, so that value
-- does not reach the filter. Any other value, such as true, auto, or
-- false, gives nil, and the columns take text_shares.
local function table_colwidths(tbl, value)
  if value == nil or tbl.attributes["quarto-postprocess"] then return nil end
  local list = value
  if type(value) == "string" then
    if value:sub(1, 1) ~= "[" then value = "[" .. value .. "]" end
    local ok, decoded = pcall(quarto.json.decode, value)
    list = ok and decoded or nil
  end
  if type(list) ~= "table" or #list == 0 then return nil end
  for _, row in ipairs(tbl.head.rows) do
    for _, cell in ipairs(row.cells) do
      if cell.col_span ~= 1 or cell.row_span ~= 1 then return nil end
    end
  end
  for _, body in ipairs(tbl.bodies) do
    for _, rows in ipairs({ body.head, body.body }) do
      for _, row in ipairs(rows) do
        for _, cell in ipairs(row.cells) do
          if cell.col_span ~= 1 or cell.row_span ~= 1 then return nil end
        end
      end
    end
  end
  local widths, total = {}, 0
  for i = 1, #tbl.colspecs do
    local w = tonumber(list[math.min(i, #list)])
    if not w then return nil end
    widths[i] = w + 0.0
    total = total + widths[i]
  end
  -- Quarto rounds a scaled value to one decimal, then a share to two.
  local function round(x, places)
    local m = 10 ^ places
    return math.floor(x * m + 0.5) / m
  end
  for i = 1, #widths do
    if total > 100 then widths[i] = round(widths[i] / total * 100, 1) end
    widths[i] = round(widths[i] / 100, 2)
  end
  return widths
end

-- Writes a Pandoc table as a raw tabular for a wrappedtable, since wrapfig's
-- wraptable cannot hold the longtable that Pandoc writes (D-024). Each
-- column takes its share of the box (text_shares), so Pandoc writes p{}
-- columns that fit it. Pandoc's longtable holds a first head, a head that
-- repeats on later pages, and a foot before the body. The tabular keeps the
-- first head and sets the foot after the body. Pandoc puts a longtable with
-- no caption in a group that keeps the table counter, which a tabular does
-- not need.
--
-- The table is written before Quarto resolves shortcodes, cross-references,
-- and citations, and in a document of its own, whose template the main one
-- does not see. So each cell is written as a token, and the result is the
-- raw pieces of the written tabular with the cells' own content between
-- them: a one-paragraph cell's inlines, or another cell's blocks in a
-- minipage, as Pandoc sets a list in a cell. Quarto then processes that
-- content, and the main writer writes it, with the packages it needs. The
-- blank lines that Pandoc writes between blocks fall inside the minipage.
-- `float_colwidths` is the float's tbl-colwidths attribute.
local ALIGN_TEX = { AlignRight = "\\raggedleft", AlignCenter = "\\centering" }

local function latex_tabular(tbl, float_colwidths)
  local widths = table_colwidths(tbl, tbl.attributes["tbl-colwidths"] or float_colwidths)
  local shares = widths or text_shares(tbl)
  for i, spec in ipairs(tbl.colspecs) do tbl.colspecs[i] = { spec[1], shares[i] } end
  local held, n = {}, 0
  local function hold(rows)
    local covered = {}
    for _, row in ipairs(rows) do
      local col = 1
      for _, cell in ipairs(row.cells) do
        while (covered[col] or 0) > 0 do col = col + 1 end
        local c = cell.contents
        if #c > 0 then
          n = n + 1
          if #c == 1 and (c[1].t == "Plain" or c[1].t == "Para") then
            held[n] = { inlines = c[1].content }
          else
            local align = cell.alignment
            if align == "AlignDefault" then align = tbl.colspecs[col] and tbl.colspecs[col][1] end
            held[n] = { blocks = c, align = ALIGN_TEX[align] or "\\raggedright" }
          end
          cell.contents = pandoc.Blocks({ pandoc.Plain({ pandoc.Str("WRAPPEDCELL" .. n .. "Z") }) })
        end
        for k = col, col + cell.col_span - 1 do covered[k] = cell.row_span end
        col = col + cell.col_span
      end
      for k, left in pairs(covered) do covered[k] = math.max(left - 1, 0) end
    end
  end
  hold(tbl.head.rows)
  for _, body in ipairs(tbl.bodies) do
    hold(body.head)
    hold(body.body)
  end
  hold(tbl.foot.rows)
  local tex = pandoc.write(pandoc.Pandoc({ tbl }), "latex")
  tex = tex:gsub("^{\\def\\LTcaptype{none}[^\n]*\n(.*)\n}%s*$", "%1")
  local spec, rows = tex:match("\\begin{longtable}%[[^%]]*%](%b{})\n?(.*)\\end{longtable}")
  local function split(text, mark)
    local at = text:find(mark, 1, true)
    if not at then return "", text end
    return text:sub(1, at - 1), text:sub(at + #mark):gsub("^\n", "")
  end
  local head, body, foot
  if rows:find("\\endfirsthead", 1, true) then
    head = split(rows, "\\endfirsthead")
    local _, rest = split(rows, "\\endhead")
    rows = rest
  else
    head, rows = split(rows, "\\endhead")
  end
  foot, body = split(rows, "\\endlastfoot")
  local _, last = split(foot, "\\endfoot")
  -- Pandoc's template loads multirow only for a table it writes itself.
  if tex:find("\\multirow", 1, true) then quarto.doc.use_latex_package("multirow") end
  local out = "\\begin{tabular}" .. spec .. "\n" .. head .. body .. last .. "\\end{tabular}"
  local blocks, inlines, pos = pandoc.Blocks({}), pandoc.Inlines({}), 1
  for s, i, e in out:gmatch("()WRAPPEDCELL(%d+)Z()") do
    local cell = held[tonumber(i)]
    inlines:insert(pandoc.RawInline("latex", out:sub(pos, s - 1)))
    if cell.inlines then
      inlines:extend(cell.inlines)
    else
      inlines:insert(pandoc.RawInline("latex", "\\begin{minipage}[t]{\\linewidth}" .. cell.align))
      blocks:insert(pandoc.Plain(inlines))
      blocks:extend(cell.blocks)
      inlines = pandoc.Inlines({ pandoc.RawInline("latex", "\\end{minipage}") })
    end
    pos = e
  end
  inlines:insert(pandoc.RawInline("latex", out:sub(pos)))
  blocks:insert(pandoc.Plain(inlines))
  return blocks
end

local function docx_size(blocks, twips, by_column)
  local function size(img, w)
    img.attributes.width = tex_number(w / 1440) .. "in"
    return img
  end
  local function size_cells(tbl)
    local shares = column_shares(tbl)
    local function rows(list)
      for _, row in ipairs(list) do
        local col = 1
        for _, cell in ipairs(row.cells) do
          local share = 0
          for c = col, col + cell.col_span - 1 do share = share + (shares[c] or 0) end
          local cell_w = twips * share
          local max_w = math.max(cell_w - 216, cell_w / 2)
          cell.contents = cell.contents:walk({
            Image = function(img)
              local own = img.attributes.width and docx_twips(img.attributes.width, true)
              return size(img, own and math.min(own, max_w) or max_w)
            end,
          })
          col = col + cell.col_span
        end
      end
    end
    rows(tbl.head.rows)
    for _, body in ipairs(tbl.bodies) do
      rows(body.head)
      rows(body.body)
    end
    rows(tbl.foot.rows)
    return tbl
  end
  return blocks:walk({
    traverse = "topdown",
    Table = function(tbl)
      if not by_column then return nil end
      return size_cells(tbl), false
    end,
    Image = function(img) return size(img, twips) end,
  })
end

-- The custom properties for a wrapped box, followed by any style the user
-- set on it.
local function style_for(width, gap, where, existing)
  local style = string.format("--wrap-width: %s; --wrap-gap: %s;",
    checked_length(width, options.width, "width", where),
    checked_length(gap, options.gap, "wrap-gap", where))
  if existing and existing ~= "" then style = style .. " " .. existing end
  return style
end

-- True for a Div that stands for a Quarto float in a plain walk.
local function is_float_div(div)
  return div.attributes.__quarto_custom_type == "FloatRefTarget"
end

-- Reads the first wrap class, width, wrap-gap, and wrap-lines of the images
-- in a list of blocks, and changes nothing. The images of a subfigure
-- belong to the subfigure, not to its panel, so the walk skips nested
-- floats.
local function image_wrap(blocks)
  local class, width, gap, lines
  blocks:walk({
    traverse = "topdown",
    Div = function(d)
      if is_float_div(d) then return nil, false end
    end,
    Image = function(img)
      class = class or wrap_class(img.classes)
      width = width or img.attributes.width
      gap = gap or img.attributes["wrap-gap"]
      lines = lines or img.attributes["wrap-lines"]
    end,
  })
  return class, width, gap, lines
end

-- Removes the wrap class, wrap-gap, and wrap-lines from an image, and
-- keeps its width.
local function drop_wrap(img)
  strip_wrap(img.classes)
  img.attributes["wrap-gap"] = nil
  img.attributes["wrap-lines"] = nil
  return img
end

-- Cleans an image in a wrapped figure, so the image fills the figure box.
-- HTML removes the width. LaTeX sets the width to the box width. An image
-- with a height keeps it and loses its width, so Pandoc fits the image in
-- the box width and the height and keeps its shape. Typst sets the width to
-- the box width and removes the height, because Typst does not fit an image
-- with a height in the box: an image wider than the box is cut off at its
-- edges. Word removes the height in the same way, and docx_size sets the
-- width, to the frame width for a figure. In a table float, an image takes
-- the width of the box or of its table column (docx_size), so the warning
-- says so. Elsewhere the image keeps its width.
local function clean_image(img, in_table)
  drop_wrap(img)
  if is_html then
    img.attributes.width = nil
  elseif is_latex then
    img.attributes.width = (img.attributes.height == nil) and "100%" or nil
  elseif is_typst or is_docx then
    if img.attributes.height then
      local why = in_table and "in a wrapped table, so the image keeps its shape, no wider than the box or its column"
        or "in a wrapped figure, so the image fills the box width"
      quarto.log.warning(string.format(
        "wrapped: %s does not use the height of the image %s %s.",
        is_typst and "Typst" or "Word", pandoc.utils.stringify(img.src), why))
    end
    if is_typst then img.attributes.width = "100%" end
    img.attributes.height = nil
  end
  return img
end

local function clean_images(blocks, in_table)
  return blocks:walk({ Image = function(img) return clean_image(img, in_table) end })
end

-- Returns the line count of a wrap-lines value, or nil with a warning when
-- it is not a whole number above 0.
local function checked_lines(value, where)
  if value == nil then return nil end
  local s = pandoc.utils.stringify(value)
  if s:match("^%d+$") and tonumber(s) > 0 then return s end
  quarto.log.warning(string.format(
    'wrapped: wrap-lines "%s" on %s is not a whole number above 0, so it is not used.', s, where))
  return nil
end

-- The options of a wrappedfigure environment, such as
-- side=right,width=0.4\linewidth,gap=1em,lines=12.
local function tex_options(class, width, gap, lines, where)
  local opts = {
    "side=" .. SIDES[class],
    "width=" .. checked_tex_length(width, options.width, "width", where),
    "gap=" .. checked_tex_length(gap, options.gap, "wrap-gap", where),
  }
  local n = checked_lines(lines, where)
  if n then table.insert(opts, "lines=" .. n) end
  return table.concat(opts, ",")
end

-- Counts the floats (figures and tables) inside a code cell. Inside a user
-- filter, a float in a cell is a Div that Quarto marks as a FloatRefTarget,
-- and a plain walk does not call the FloatRefTarget handler.
local function count_floats(div)
  local n = 0
  div:walk({
    Div = function(d)
      if d.attributes.__quarto_custom_type == "FloatRefTarget" then n = n + 1 end
    end,
  })
  return n
end

-- Counts the floats of a code cell that sit in no other float. A cell with
-- `fig-subcap` makes one float that holds a subfloat for each plot, and a
-- cell that draws two plots without it makes two floats.
local function count_top_floats(div)
  local n = 0
  div.content:walk({
    traverse = "topdown",
    Div = function(d)
      if d.attributes.__quarto_custom_type ~= "FloatRefTarget" then return nil end
      n = n + 1
      return nil, false
    end,
  })
  return n
end

-- Custom ids of floats whose next block is not a paragraph that joins a
-- group, from the next-block pass.
local no_text = {}

-- Why a float keeps its normal layout, as "is in a list", by the float's
-- custom id. The float pass gives each such float with a wrap class a
-- normal layout and a warning.
local context_of_float = {}

-- The contexts that a code cell's fig-column-* or tbl-column-* class sets,
-- by custom id, kept apart from context_of_float because Quarto places
-- only that type of float by the class: { context, scope = "fig" or
-- "tbl" }. The float pass takes one for a float of its type only, and the
-- walk goes on inside the cell, so a subfigure panel there is still
-- recorded. A context that the cell's other classes set holds for the
-- other type, in context_of_float.
local scoped_context = {}

-- Drops the column classes that Quarto scopes to the other float type:
-- fig-column-* places only figures and tbl-column-* only tables.
local function own_type_classes(classes, is_table)
  local other = is_table and "^fig%-column%-" or "^tbl%-column%-"
  return classes:filter(function(c) return not c:match(other) end)
end

-- Returns the context that a margin class names, or nil. A code cell with
-- `fig-column: margin` carries the fig-column-margin class, and one with
-- `tbl-column: margin` the tbl-column-margin class. revealjs slides have no
-- margin column, so the margin classes name no place there, but revealjs
-- sets an aside at the foot of the slide by default.
local function margin_context(classes)
  if not is_revealjs and (classes:includes("column-margin") or classes:includes("fig-column-margin")
      or classes:includes("tbl-column-margin")) then
    return "is in the margin"
  end
  if classes:includes("aside") then return "is in an aside" end
  return nil
end

-- Quarto's page-wide column classes. An HTML page sets each one wider than
-- the body text, where a wrapped figure does not float.
local PAGE_WIDE = { ["column-screen-inset-shaded"] = true }
for _, name in ipairs({ "column-body-outset", "column-page-inset", "column-page",
    "column-screen-inset", "column-screen" }) do
  PAGE_WIDE[name], PAGE_WIDE[name .. "-left"], PAGE_WIDE[name .. "-right"] = true, true, true
end

-- Returns the context that a page-wide column class names, or nil. A code
-- cell with `column: page` carries the column-page class, and one with
-- `fig-column: page` or `tbl-column: page` the fig-column-page or
-- tbl-column-page class. LaTeX makes a figure
-- page-wide only for the names that start with column-page or
-- column-screen and do not end in -left, as Quarto's isStarEnv does. Typst
-- sets a div or a code cell wider than the text for each name, in a
-- wideblock or a column function, but a float's own class only for the
-- column-page and column-screen names without -inset (Quarto 1.10.19). Word
-- keeps the figure in the body.
local function page_context(classes, own)
  for _, class in ipairs(classes) do
    local name = class:gsub("^fig%-", ""):gsub("^tbl%-", "")
    local starred = name:match("^column%-page") or name:match("^column%-screen")
    if PAGE_WIDE[name] and (has_page_layout
        or is_typst and not (own and (not starred or name:match("%-inset")))
        or is_latex and not name:match("%-left$") and starred) then
      return "is page-wide"
    end
  end
  return nil
end

-- Returns the context that a margin or page-wide column class names, or nil.
-- `own` is true for a float's own classes.
local function class_context(classes, own)
  return margin_context(classes) or page_context(classes, own)
end

-- The number of dashboard `.card` divs around the block that find_contexts
-- is at.
local card_depth = 0

-- Returns the context that a block sets for the blocks inside it, or nil.
-- A code cell with `column: margin` carries the column-margin class. A
-- context that only a fig-column-* or tbl-column-* class sets comes with
-- its scope, "fig" or "tbl", as a second value (scoped_context).
local function block_context(block)
  if block.t == "BulletList" or block.t == "OrderedList" or block.t == "DefinitionList" then
    return "is in a list"
  end
  -- wrapfig does not wrap in a block quote, which LaTeX sets as a list.
  if block.t == "BlockQuote" then return is_latex and "is in a block quote" or nil end
  -- A Word frame in a table cell rendered with no text beside it in a
  -- LibreOffice render of the docx, and a wrappedfigure in a LaTeX
  -- longtable breaks the build.
  if block.t == "Table" then return (is_latex or is_docx) and "is in a table" or nil end
  if block.t ~= "Div" then return nil end
  local kind = block.attributes.__quarto_custom_type
  if kind == "Callout" then return "is in a callout" end
  if kind == "Tabset" or block.classes:includes("panel-tabset") then return "is in a tabset" end
  if block.classes:includes("columns") or block.classes:includes("column") then
    return "is in a column layout"
  end
  -- A div or a code cell with a layout attribute lays out its content in a
  -- grid of panels.
  local attrs = block.attributes
  if attrs["layout-ncol"] or attrs["layout-nrow"] or attrs.layout then
    return "is in a panel layout"
  end
  -- In LaTeX and Typst, the document's fig-column or column decides where
  -- a code cell's figure goes, over the cell's own column (Quarto 1.10.19).
  if (is_latex or is_typst) and meta_column_set and block.classes:includes("cell") then
    return nil
  end
  -- Quarto's dashboard filter, which runs after this one, makes a code cell
  -- outside a `.card` div a card of its own, so the paragraphs after the
  -- cell are not in its card. It leaves a Jupyter markdown cell's content
  -- in place.
  if is_dashboard and card_depth == 0 and block.classes:includes("cell")
      and not block.classes:includes("markdown") then
    return "is outside a card in a dashboard"
  end
  -- The context reads all the classes, margin before page-wide; then the
  -- classes that give it decide its scope. A scoped context comes with the
  -- context of the other classes, if any, as a third value: it holds for
  -- the other float type, as on a cell with `column: page` and
  -- `tbl-column: margin`.
  local context = class_context(block.classes)
  if not context then return nil end
  local unscoped = block.classes:filter(function(c)
    return not (c:match("^fig%-column%-") or c:match("^tbl%-column%-"))
  end)
  local plain = class_context(unscoped)
  if plain == context then return context end
  for _, scope in ipairs({ "fig", "tbl" }) do
    local scoped = block.classes:filter(function(c) return c:match("^" .. scope .. "%-column%-") end)
    if class_context(scoped) == context then return context, scope, plain end
  end
  return context
end

-- A context warning, printed in a format that wraps. Other formats print
-- the one format warning instead.
local function context_warning(subject, context)
  if wraps() then
    quarto.log.warning(string.format("wrapped: %s %s, so it does not wrap.", subject, context))
  end
end

-- Records the floats in a list of blocks, subfloats included, by custom id.
-- `scope` is the float type the context holds for, or nil (scoped_context).
local function record_floats(blocks, context, scope)
  blocks:walk({
    Div = function(d)
      local id = d.attributes.__quarto_custom_id
      if not (is_float_div(d) and id) then return end
      if scope then
        scoped_context[id] = scoped_context[id] or { context = context, scope = scope }
      else
        context_of_float[id] = context_of_float[id] or context
      end
    end,
  })
end

-- Records each float in a context's blocks, and gives each wrapped figure
-- or image there a normal layout with a warning. A float warns later, in
-- the float pass, where its own id is known. A scoped context also records
-- `plain`, the context of the cell's other classes, for every float (see
-- block_context); a captioned image with no id or an image, which is no
-- table, takes `plain` under a context scoped to tables.
local function mark_context(blocks, context, scope, plain)
  local own = scope == "tbl" and plain or context
  return blocks:walk({
    traverse = "topdown",
    Div = function(div)
      if not is_float_div(div) then return nil end
      record_floats(pandoc.Blocks({ div }), context, scope)
      if scope and plain then record_floats(pandoc.Blocks({ div }), plain) end
      return nil, false
    end,
    Figure = function(fig)
      if not own then return nil end
      if not (wrap_class(fig.classes) or image_wrap(fig.content)) then return nil end
      found_wrap = true
      strip_wrap(fig.classes)
      fig.content = fig.content:walk({ Image = drop_wrap })
      context_warning(fig.identifier ~= "" and ("the figure #" .. fig.identifier)
        or "a figure with no id", own)
      return fig, false
    end,
    Image = function(img)
      if not own or not wrap_class(img.classes) then return nil end
      found_wrap = true
      context_warning("the image " .. pandoc.utils.stringify(img.src), own)
      return drop_wrap(img)
    end,
  })
end

-- Finds the lists, callouts, column layouts, panel layouts, tabsets,
-- asides, margin blocks, page-wide columns, subfigure panels, and the block
-- quotes and tables of the formats that cannot wrap there, outermost first, and marks
-- what is inside each one. A float that holds other floats is a subfigure
-- panel. A captioned image with no id, or an image, with its own margin
-- class is in the margin too, except on revealjs slides. Quarto makes only
-- floats and divs page-wide, so a float's own margin or page-wide class is
-- read in the float pass. Inside a context the walk stops, except in a code
-- cell whose context is scoped to one float type (block_context), where it
-- goes on to record the subfigure panels.
local function find_contexts(doc)
  local filter
  local function mark(block)
    -- A card that is also a context, such as one with layout-ncol, is
    -- marked as that context below.
    if is_dashboard and block.t == "Div" and block.classes:includes("card")
        and not block_context(block) then
      card_depth = card_depth + 1
      block.content = block.content:walk(filter)
      card_depth = card_depth - 1
      return block, false
    end
    if block.t == "Div" and is_float_div(block) then
      local inner = pandoc.Blocks({})
      block.content:walk({
        Div = function(d) if is_float_div(d) then inner:insert(d) end end,
      })
      local id = block.attributes.__quarto_custom_id
      if #inner > 0 and id then
        context_of_float[id] = context_of_float[id] or "is a subfigure panel"
        record_floats(inner, "is in a subfigure panel")
      end
      return nil, false
    end
    local context, scope, plain = block_context(block)
    if not context then return nil end
    if block.t == "Div" then
      block.content = mark_context(block.content, context, scope, plain)
      -- A scoped context holds for one float type only, so the walk goes
      -- on inside the cell and records a subfigure panel there too.
      if scope then return block end
    elseif block.t == "BulletList" or block.t == "OrderedList" then
      for i, item in ipairs(block.content) do
        block.content[i] = mark_context(item, context)
      end
    else
      -- A definition list's items hold a term and a list of definitions,
      -- and a table's cells sit in its head, bodies, and foot, so the walk
      -- marks the whole block.
      block = mark_context(pandoc.Blocks({ block }), context)[1]
    end
    return block, false
  end
  local function own_margin(fig)
    local context = margin_context(fig.classes)
    fig.content:walk({
      Image = function(img) context = context or margin_context(img.classes) end,
    })
    if not context then return nil end
    return mark_context(pandoc.Blocks({ fig }), context), false
  end
  filter = {
    traverse = "topdown",
    Div = mark,
    BulletList = mark,
    OrderedList = mark,
    DefinitionList = mark,
    BlockQuote = mark,
    Table = mark,
    Figure = own_margin,
    Image = function(img)
      local context = margin_context(img.classes)
      if not (context and wrap_class(img.classes)) then return nil end
      found_wrap = true
      context_warning("the image " .. pandoc.utils.stringify(img.src), context)
      return drop_wrap(img)
    end,
  }
  doc.blocks = doc.blocks:walk(filter)
  return doc
end

-- True when `classoption`, a string or a list, turns on twocolumn: an
-- option `twocolumn`, or KOMA's `twocolumn=true`, `on`, or `yes`.
local TWO_COLUMN = { [""] = true, ["true"] = true, on = true, yes = true }

local function names_two_column(classoption)
  if classoption == nil then return false end
  local items = pandoc.utils.type(classoption) == "List" and classoption or { classoption }
  for _, item in ipairs(items) do
    for option in pandoc.utils.stringify(item):gmatch("[^,]+") do
      local value = option:lower():match("^%s*twocolumn%s*=?%s*(%a*)%s*$")
      if value and TWO_COLUMN[value] then return true end
    end
  end
  return false
end

-- In LaTeX, Typst, and Word, a metadata width or gap that the format cannot
-- use, such as 50vw, gives way to the default with a warning. `convert` is
-- tex_length, typst_length, or docx_twips, and `name` names the format.
local function format_default(key, what, convert, name)
  if convert(options[key]) then return end
  quarto.log.warning(string.format(
    'wrapped: %s "%s" in the wrapped metadata is not a length that %s can use, so it is %s.',
    what, options[key], name, DEFAULTS[key]))
  options[key] = DEFAULTS[key]
end

-- The text of a metadata value, or nil. Pandoc reads metadata as Markdown,
-- and its smart punctuation turns the `--` of `var(--w)` into an en dash,
-- which no CSS length holds, so the en dash turns back into `--`.
local function meta_text(value)
  if value == nil then return nil end
  return (pandoc.utils.stringify(value):gsub("\u{2013}", "--"))
end

-- The document's fig-cap-location, tbl-cap-location, and cap-location,
-- read in the first pass.
local meta_fig_cap_location = nil
local meta_tbl_cap_location = nil
local meta_cap_location = nil

-- The document's metadata, read in the first pass, for the stop rule's
-- meta shortcodes.
local document_meta = {}

-- The contexts that the document's fig-column, or else its column, gives a
-- code cell's figure float (cell) and any other figure float (float); each
-- is nil when the value names neither the margin nor a page-wide column
-- there. Quarto 1.10.19 applies the value to a code cell's float even when
-- the cell sets fig-column: body, and not to a captioned image with no id,
-- which is no float. A table float takes the document's tbl-column, or
-- else its column, in the same way, and not its fig-column
-- (meta_table_column_context).
local meta_column_context = {}
local meta_table_column_context = {}
-- Set in the first pass when the document sets tbl-column.
local meta_tbl_column_set = false

-- The text of a fig-column or column metadata value, or nil. As Quarto's
-- columnOption does, it reads the first item of a list and treats an empty
-- value as unset.
local function meta_column(value)
  if pandoc.utils.type(value) == "List" then value = value[1] end
  local text = meta_text(value)
  if text ~= "" then return text end
  return nil
end

-- Whether a class sends a caption to the margin. Quarto reads both
-- margin-caption and caption-margin on a float, but moves the caption of a
-- captioned image with no id only for margin-caption (Quarto 1.10.19).
local function margin_caption_class(classes, is_float)
  return classes:includes("margin-caption") or (is_float and classes:includes("caption-margin"))
end

-- Whether a float's caption goes to the margin of an HTML page, as
-- Quarto 1.10.19 decides it. A margin caption class on the float or its
-- image sends it there. Otherwise the float's own cap-location decides
-- when it is set. Without one, the caption goes to the margin when the
-- float's fig-cap-location, the document's fig-cap-location, or the
-- document's cap-location is margin, and a value other than margin in one
-- of them overrides none of the others. Quarto has already copied a code
-- cell's caption location, and its caption-margin class, to the cell's
-- figure as cap-location. A table float reads tbl-cap-location, its own
-- and the document's, in place of fig-cap-location; Quarto copies it from
-- a code cell to its table float under that name.
local function caption_in_margin(float)
  local by_class = margin_caption_class(float.classes, true)
  float.content:walk({
    Image = function(img) by_class = by_class or margin_caption_class(img.classes, true) end,
  })
  if by_class then return true end
  local own = float.attributes["cap-location"]
  if own then return own == "margin" end
  local key, meta_value = "fig-cap-location", meta_fig_cap_location
  if float.type == "Table" then key, meta_value = "tbl-cap-location", meta_tbl_cap_location end
  return float.attributes[key] == "margin" or meta_value == "margin" or meta_cap_location == "margin"
end

-- Returns the context that a float's column gives it, or nil. In LaTeX and
-- Typst, Quarto 1.10.19 reads a figure float's own fig-column-* class, the
-- document's fig-column, its own column-* or aside class, and the
-- document's column, and the first one set decides. A table float reads
-- tbl-column in place of fig-column. A code cell's own column is on the
-- cell, not its float, and find_contexts reads it when the document sets
-- neither key. HTML and Word read a margin or page-wide class of the
-- float's own, else the document's value; a cell's classes that Quarto
-- copies to the float include a scoped class of the other float type,
-- which places nothing here (own_type_classes). The second value is true
-- when the document's value decides, which float_target reads after a
-- layout.
local function column_context(float, cell)
  local is_table = float.type == "Table"
  local context = is_table and meta_table_column_context or meta_column_context
  local document = cell and context.cell or context.float
  if not (is_latex or is_typst) then
    local own = class_context(own_type_classes(float.classes, is_table), true)
    if own then return own, false end
    return document, true
  end
  local prefix = is_table and "^tbl%-column%-" or "^fig%-column%-"
  local scoped = float.classes:filter(function(c) return c:match(prefix) end)
  if #scoped > 0 then return class_context(scoped, true), false end
  if (is_table and meta_tbl_column_set) or (not is_table and meta_fig_column_set) then
    return document, true
  end
  local own = float.classes:filter(function(c) return c:match("^column%-") or c == "aside" end)
  if #own > 0 then return class_context(own, true), false end
  return document, true
end

local function read_meta(meta)
  document_meta = meta
  meta_fig_cap_location = meta_text(meta["fig-cap-location"])
  meta_tbl_cap_location = meta_text(meta["tbl-cap-location"])
  meta_cap_location = meta_text(meta["cap-location"])
  if is_docx then
    docx_text_width, docx_columns = read_docx_text_width()
    if docx_columns then is_docx = false end
  end
  if is_latex and names_two_column(meta.classoption) then
    is_latex = false
    two_column = true
  end
  local fig_column = meta_column(meta["fig-column"])
  local column = fig_column or meta_column(meta.column)
  meta_fig_column_set, meta_column_set = fig_column ~= nil, column ~= nil
  if column then
    local classes = pandoc.List({ "column-" .. column })
    meta_column_context = { float = class_context(classes, true), cell = class_context(classes) }
  end
  local tbl_column = meta_column(meta["tbl-column"])
  local table_column = tbl_column or meta_column(meta.column)
  meta_tbl_column_set = tbl_column ~= nil
  if table_column then
    local classes = pandoc.List({ "column-" .. table_column })
    meta_table_column_context = { float = class_context(classes, true), cell = class_context(classes) }
  end
  local m = meta.wrapped
  if m == nil then return end
  if pandoc.utils.type(m) ~= "table" then
    quarto.log.warning(
      "wrapped: the wrapped metadata is not a map of width, gap, and breakpoint, so it is ignored.")
    return
  end
  -- Any other key does nothing, so each one warns, in byte order of the key
  -- (D-027).
  -- An attribute that only this filter reads opens with "wrap-", but a key
  -- in this map does not, so `wrap-gap` names the key to write.
  local unknown = {}
  for key in pairs(m) do
    if key ~= "width" and key ~= "gap" and key ~= "breakpoint" then unknown[#unknown + 1] = tostring(key) end
  end
  table.sort(unknown)
  for _, key in ipairs(unknown) do
    quarto.log.warning(string.format(
      "wrapped: the key %s in the wrapped metadata is not width, gap, or breakpoint, so it is ignored.%s",
      key, key == "wrap-gap" and " Write gap for the space between a figure and the text." or ""))
  end
  options.width = checked_length(meta_text(m.width), DEFAULTS.width, "width", "the wrapped metadata")
  options.gap = checked_length(meta_text(m.gap), DEFAULTS.gap, "gap", "the wrapped metadata")
  options.breakpoint = checked_length(meta_text(m.breakpoint), DEFAULTS.breakpoint, "breakpoint",
    "the wrapped metadata", true)
  if is_latex then
    format_default("width", "width", tex_length, "LaTeX")
    format_default("gap", "gap", tex_length, "LaTeX")
  elseif is_typst then
    format_default("width", "width", typst_length, "Typst")
    format_default("gap", "gap", typst_length, "Typst")
    if typst_too_wide(typst_length(options.width)) then
      quarto.log.warning(string.format(
        'wrapped: width "%s" in the wrapped metadata leaves no room for text in Typst, so it is %s.',
        options.width, DEFAULTS.width))
      options.width = DEFAULTS.width
    elseif m.width ~= nil and typst_fills(typst_length(options.width), typst_length(options.gap)) then
      quarto.log.warning(string.format(
        'wrapped: width "%s" and gap "%s" in the wrapped metadata leave no room for text in Typst, so the width is %s.',
        options.width, options.gap, DEFAULTS.width))
      options.width = DEFAULTS.width
    end
  elseif is_docx then
    local asked = m.width ~= nil and css_length(meta_text(m.width))
    format_default("width", "width", function(l) return docx_twips(l, true) end, "Word")
    format_default("gap", "gap", docx_twips, "Word")
    -- A metadata width that with the gap is as wide as the text or wider
    -- leaves no room for text, so the document default becomes 40%, as in
    -- Typst. A width that already gave way, with its own warning, is not
    -- checked again. Each figure with its own width or gap is checked again
    -- in docx_frame.
    if asked and asked == options.width
        and docx_twips(options.width, true) + docx_twips(options.gap) >= docx_text_width then
      quarto.log.warning(string.format(
        'wrapped: width "%s" and gap "%s" in the wrapped metadata leave no room for text in Word, so the width is %s.',
        options.width, options.gap, DEFAULTS.width))
      options.width = DEFAULTS.width
    end
  end
end

-- Custom ids of wrapped floats outside a code cell, and the anchor tokens of
-- cells whose figure wrapped, for the grouping pass.
local wrapped_ids = {}
local wrapped_cells = {}
local next_token = 0

-- In Typst, the opening of each wrapped float's wrap-content call, by the
-- float's custom id, cell floats included.
local typst_opens = {}

-- In Word, each wrapped float's frame from docx_frame, by the float's
-- custom id, cell floats included.
local docx_frames = {}

-- The code cell that the float pass is in, with its wrap class, if any,
-- carried to the floats inside that cell. `remaining` counts the cell's
-- floats, figures and tables, so a cell with no float carries nothing.
-- A cell with no wrap class still anchors its group, since a table float
-- inside it can take a wrap class from its caption block. `several` is
-- true when the cell makes more than one float that sits in no other
-- float, and `warned` is set once the cell's one warning is printed.
local carry = nil

local function cell_div(div)
  if not div.classes:includes("cell") then return nil end
  -- Each cell ends any carry left from an earlier cell.
  carry = nil
  local c = wrap_class(div.classes)
  if c then strip_wrap(div.classes) end
  local n = count_floats(div)
  if c and n == 0 and wraps() then
    quarto.log.warning(
      "wrapped: a code cell with a wrap class makes no figure with a fig- label, so nothing wraps.")
  end
  if n > 0 then
    next_token = next_token + 1
    local token = tostring(next_token)
    -- Quarto turns a cell's caption-margin class into the cap-location of
    -- its figure, but not its margin-caption class, so that one is carried.
    -- knitr puts a cell's `width` and `wrap-gap` options on the cell div. A
    -- table float gets them too, and so does the parent float of a cell's
    -- subfigure panels, which keeps its normal layout. A cell's one figure
    -- float gets neither, so that figure reads its gap from here.
    carry = { class = c, remaining = n, token = token, several = count_top_floats(div) > 1,
      margin_caption = div.classes:includes("margin-caption"),
      gap = div.attributes["wrap-gap"], width = div.attributes.width }
    if wraps() then div.attributes["data-wrap-anchor"] = token end
  end
  return div
end

-- Reads the attribute block at the end of a caption, as in
-- `A table {.wrap-left}`, the way Quarto reads it after this filter runs
-- (parseTableCaption in Quarto's filters/main.lua). Returns the block's
-- attributes, the inlines before it, and whether its text opens with "{#",
-- without which Quarto drops the id; nil when the caption has no block.
local function caption_block(content)
  local inlines = pandoc.Inlines({ table.unpack(content) })
  if #inlines > 0 and inlines[#inlines].t == "Space" then inlines:remove(#inlines) end
  local final = inlines[#inlines]
  if not final or final.t ~= "Str" or not final.text:match("}%s*$") then return nil end
  local first
  for i, inline in ipairs(inlines) do
    if inline.t == "Str" and inline.text:sub(1, 1) == "{" then
      first = i
      break
    end
  end
  if not first then return nil end
  local block = pandoc.utils.stringify(pandoc.Inlines({ table.unpack(inlines, first) }))
  block = block:match("^%s*(.-)%s*$"):gsub("“", "'"):gsub("”", "'")
  local header = pandoc.read("## " .. block).blocks[1]
  if not header or not header.attr then return nil end
  return header.attr, pandoc.Inlines({ table.unpack(inlines, 1, first - 1) }), block:sub(1, 2) == "{#"
end

-- Quarto adds the classes of the block at a table float's caption end to
-- the float and its table after this filter runs (table_classes in
-- Quarto's filters/main.lua), so a wrap class there would float a table
-- that keeps its normal layout in HTML. This writes the block again
-- without its wrap classes, or drops it when nothing is left, and returns
-- the first wrap class it held, which float_target places.
local function drop_caption_wrap(float)
  local caption = float.caption_long
  if not caption or not caption.content then return nil end
  local attr, before, keep_id = caption_block(caption.content)
  local class = attr and wrap_class(attr.classes)
  if not class then return nil end
  strip_wrap(attr.classes)
  -- The text before the block ends in the space before it, which Quarto
  -- removes only when the block is still there to be read.
  while #before > 0 and (before[#before].t == "Space" or before[#before].t == "SoftBreak") do
    before:remove(#before)
  end
  local parts = {}
  if keep_id and attr.identifier ~= "" then parts[#parts + 1] = "#" .. attr.identifier end
  for _, c in ipairs(attr.classes) do parts[#parts + 1] = "." .. c end
  for k, v in pairs(attr.attributes) do
    local quoted = v:gsub("\\", "\\\\"):gsub('"', '\\"')
    parts[#parts + 1] = string.format('%s="%s"', k, quoted)
  end
  if #parts > 0 then
    if #before > 0 then before:insert(pandoc.Space()) end
    before:insert(pandoc.Str("{" .. table.concat(parts, " ") .. "}"))
  end
  caption.content = before
  return class
end

-- True when a float's content holds any raw LaTeX block, as a table from
-- gt, kableExtra, tinytable, or knitr::kable(format = "latex") does. The
-- filter cannot rewrite such a table for wraptable (D-024).
local function raw_latex_table(content)
  if pandoc.utils.type(content) == "Block" then content = pandoc.Blocks({ content }) end
  local found = false
  content:walk({
    RawBlock = function(raw)
      if raw.format == "latex" or raw.format == "tex" then found = true end
    end,
  })
  return found
end

-- Gives a float that does not wrap its normal layout: its images lose
-- their wrap classes, and the float loses its wrap options.
local function keep_normal(float)
  float.content = float.content:walk({ Image = drop_wrap })
  float.attributes["wrap-gap"] = nil
  float.attributes["wrap-lines"] = nil
  return float
end

local function float_target(float)
  -- Every float of a cell uses up the carry, tables included, so the carry
  -- never reaches a float after the cell. `carried` is set only when the
  -- cell has a wrap class, and `cell` then, or for a table float in any
  -- cell, whose class can come from its caption block. A figure in a cell
  -- with no wrap class is read as a float of its own, as before tables
  -- wrapped.
  local is_table = float.type == "Table"
  local carried, token, cell
  if carry and carry.remaining > 0 then
    carry.remaining = carry.remaining - 1
    carried, token = carry.class, carry.token
    if carried or is_table then cell = carry end
  end
  local img_class, img_width, img_gap, img_lines = image_wrap(float.content)
  local caption_class = is_table and drop_caption_wrap(float) or nil
  local class = wrap_class(float.classes) or carried or img_class or caption_class
  if not class then return nil end
  if float.type == "Figure" or is_table then found_wrap = true end
  strip_wrap(float.classes)
  -- Figures and tables wrap, and other floats keep their normal layout.
  -- Quarto copies a cell's classes onto its float, so the wrap class goes.
  -- A float in a context keeps its normal layout, and so does an HTML
  -- float whose caption goes to the margin: CSS places the caption in the
  -- margin, outside the float's box, and no text sits beside the float.
  -- A figure's own column class, or the document's fig-column or column,
  -- can put the float in the margin or a page-wide column (column_context).
  local node = float.__quarto_custom_node
  local custom_id = node and node.attributes and node.attributes.__quarto_custom_id
  local layout = float.attributes["layout-ncol"] or float.attributes["layout-nrow"]
    or float.attributes.layout
  local column, by_document = column_context(float, cell)
  -- A context that a code cell's fig-column-* or tbl-column-* class set
  -- holds only for floats of that type, and there it comes before any
  -- other, as find_contexts reads the cell before what is inside it
  -- (scoped_context).
  local recorded = context_of_float[custom_id or ""]
  local scoped = scoped_context[custom_id or ""]
  if scoped and scoped.scope == (is_table and "tbl" or "fig") then recorded = scoped.context end
  -- Quarto moves a knitr table cell's column class, as from `column: page`,
  -- from the cell to the output div inside the table float, where
  -- find_contexts does not look. Quarto 1.10.19's Typst output keeps that
  -- column with a document tbl-column or column of `body` set too. A
  -- float's content can be one block, which a walk from that block does
  -- not visit, so the walk starts from a list.
  local inner
  if is_table then
    local content = float.content
    if pandoc.utils.type(content) == "Block" then content = pandoc.Blocks({ content }) end
    content:walk({
      Div = function(d) inner = inner or class_context(own_type_classes(d.classes, true)) end,
    })
  end
  local context = (float.type ~= "Figure" and not is_table and "is not a figure")
    or recorded
    or (not by_document and column)
    or inner
    or (layout and "is a subfigure panel")
    or (by_document and column)
    or (has_page_layout and (caption_in_margin(float) or (cell and cell.margin_caption))
      and "has its caption in the margin")
  -- A cell that makes more than one figure without `fig-subcap` keeps all
  -- its figures normal, with one warning for the cell at its first figure
  -- (D-010). The warning gives the figure's context as the reason when it
  -- has one. A cell in no context with no paragraph after it prints no
  -- warning. The cell's count holds every float, tables included, but the
  -- rule keeps only its figures normal: a table float in such a cell wraps
  -- or warns on its own. Quarto numbers such figures
  -- with the cell's label and `-1`, `-2`, and so on.
  local several = carried and cell.several and float.type == "Figure"
  -- In LaTeX, a table float that holds any raw LaTeX block, as a table
  -- from gt, kableExtra, or tinytable does, keeps its normal layout: the
  -- filter cannot rewrite such a table as a tabular for wraptable (D-024).
  -- It warns before the next-block rule. In Word, a raw openxml table, as
  -- gt and flextable write it, wraps: wrapped-docx.lua fits it to the box
  -- (D-025).
  if is_table and is_latex and not context and raw_latex_table(float.content) then
    keep_normal(float)
    quarto.log.warning(string.format(
      "wrapped: the table #%s holds raw LaTeX, as a table from gt, kableExtra, or tinytable does, so it does not wrap in LaTeX.",
      float.identifier))
    return float
  end
  -- A figure with no paragraph after it keeps a normal figure with no
  -- warning (the next-block rule).
  if context or no_text[custom_id or ""] or several then
    keep_normal(float)
    if several and (context or not no_text[custom_id or ""]) then
      if not cell.warned then
        cell.warned = true
        local label = float.identifier:gsub("%-%d+$", "")
        context_warning("the code cell #" .. label, context or "makes more than one figure")
      end
    elseif context then
      local noun = ({ Figure = "figure", Table = "table" })[float.type] or "float"
      context_warning(string.format("the %s #%s", noun, float.identifier), context)
    end
    return float
  end
  float.content = clean_images(float.content, is_table)
  if not wraps() then return float end
  -- knitr writes the plot's pixel size as a unitless image width. That is
  -- not a box width, so a cell figure takes only a width with a unit.
  if carried and img_width and tostring(img_width):match("^%d+%.?%d*$") then
    img_width = nil
  end
  local width = float.attributes.width or img_width
  local gap = float.attributes["wrap-gap"] or (cell and cell.gap) or img_gap
  local lines = float.attributes["wrap-lines"] or img_lines
  -- A figure cell's width comes from `out-width`, which knitr gives the
  -- image, so the cell's `width` option does nothing for a figure.
  if float.type == "Figure" and cell and cell.width and not cell.width_warned then
    cell.width_warned = true
    quarto.log.warning(string.format(
      "wrapped: the code cell of the figure #%s sets width, which a figure cell does not read. Set the figure's width with out-width.",
      float.identifier))
  end
  float.attributes.width = nil
  float.attributes["wrap-gap"] = nil
  float.attributes["wrap-lines"] = nil
  placed = true
  if is_latex then
    -- Quarto writes \begin{<fig-env>}[<fig-pos>] for the float, and for a
    -- table float, once its table is a tabular and not a longtable,
    -- \begin{<fig-env>}[<tbl-pos>]. A float's content can be one block,
    -- which a walk from that block does not visit.
    float.attributes["fig-env"] = is_table and "wrappedtable" or "wrappedfigure"
    float.attributes[is_table and "tbl-pos" or "fig-pos"] =
      tex_options(class, width, gap, lines, "#" .. float.identifier)
    if is_table then
      local content = float.content
      if pandoc.utils.type(content) == "Block" then content = pandoc.Blocks({ content }) end
      local colwidths = float.attributes["tbl-colwidths"]
      float.content = content:walk({ Table = function(tbl) return latex_tabular(tbl, colwidths) end })
    end
  elseif is_typst then
    if custom_id then
      typst_opens[custom_id] = typst_open(class, width, gap, "#" .. float.identifier)
    end
  elseif is_docx then
    -- A table float's box is a floating table that wrapped-docx.lua makes
    -- at post-render (D-022). An image outside its table fills the box, and
    -- one in the table takes at most its cell's share (docx_size). A float's
    -- content can be one block, which a walk from that block does not
    -- visit.
    local frame, w = docx_frame(class, width, gap, "#" .. float.identifier)
    if is_table and pandoc.utils.type(float.content) == "Block" then
      float.content = docx_size(pandoc.Blocks({ float.content }), w, true)[1]
    else
      float.content = docx_size(float.content, w, is_table)
    end
    if custom_id then docx_frames[custom_id] = frame end
  else
    float.classes:insert(class)
    float.attributes.style = style_for(width, gap, "#" .. float.identifier, float.attributes.style)
  end
  if cell then
    wrapped_cells[token] = true
  else
    -- The grouping pass sees this float as a plain Div that Quarto marks
    -- with the same custom id, so it can find the float by that id.
    if custom_id then wrapped_ids[custom_id] = true end
  end
  return float
end

-- A captioned image with no #fig- id is a Pandoc Figure, not a float.
-- Quarto drops a Figure's own classes and style in HTML, so the filter puts
-- the Figure inside a .wrapped-image box. Returning false stops the
-- traversal from visiting the Figure again inside the box.
local function figure_block(fig)
  local img_class, img_width, img_gap, img_lines = image_wrap(fig.content)
  local class = wrap_class(fig.classes) or img_class
  if not class then return nil end
  found_wrap = true
  strip_wrap(fig.classes)
  local where = fig.identifier ~= "" and ("#" .. fig.identifier) or "a figure with no id"
  -- As for a float, a caption in the margin of an HTML page leaves no text
  -- beside the figure. The image may carry the margin-caption class instead
  -- of the figure. Quarto 1.10.19 reads no caption location on the figure
  -- or its image, only the document's fig-cap-location, else its
  -- cap-location.
  if has_page_layout then
    local by_class = margin_caption_class(fig.classes, false)
    fig.content:walk({
      Image = function(img) by_class = by_class or margin_caption_class(img.classes, false) end,
    })
    if by_class or (meta_fig_cap_location or meta_cap_location) == "margin" then
      fig.content = fig.content:walk({ Image = drop_wrap })
      context_warning(where == "a figure with no id" and where or ("the figure " .. where),
        "has its caption in the margin")
      return fig, false
    end
  end
  fig.content = clean_images(fig.content)
  if not wraps() then return fig end
  local width = fig.attributes.width or img_width
  local gap = fig.attributes["wrap-gap"] or img_gap
  local lines = fig.attributes["wrap-lines"] or img_lines
  fig.attributes.width = nil
  fig.attributes["wrap-gap"] = nil
  fig.attributes["wrap-lines"] = nil
  placed = true
  if is_latex then
    fig.attributes["fig-env"] = "wrappedfigure"
    fig.attributes["fig-pos"] = tex_options(class, width, gap, lines, where)
    return fig, false
  end
  local attrs
  if is_typst then
    attrs = { ["data-wrap-open"] = typst_open(class, width, gap, where) }
  elseif is_docx then
    local frame, w = docx_frame(class, width, gap, where)
    fig.content = docx_size(fig.content, w)
    attrs = { ["data-wrap-frame"] = frame }
  else
    attrs = { style = style_for(width, gap, where, nil) }
  end
  local box = pandoc.Div({ fig }, pandoc.Attr("", { "wrapped-image", class }, attrs))
  return box, false
end

-- The one inline in a list of inlines, spaces aside, or nil.
local function only_inline(inlines)
  local found
  for _, inline in ipairs(inlines) do
    if inline.t ~= "Space" and inline.t ~= "SoftBreak" then
      if found then return nil end
      found = inline
    end
  end
  return found
end

-- A paragraph that holds only an image with a wrap class, and no caption,
-- or only a link that holds only such an image. Returns the image and the
-- link, if any. Quarto makes a linked image with a caption a figure.
local function lone_wrapped_image(block)
  if block.t ~= "Para" then return nil end
  local inline = only_inline(block.content)
  local link
  if inline and inline.t == "Link" then
    link = inline
    inline = only_inline(link.content)
  end
  if inline and inline.t == "Image" and wrap_class(inline.classes) then return inline, link end
  return nil
end

-- The inline that a wrap box holds: the image, or the link with the image
-- as its content.
local function placed_image(img, link)
  if not link then return img end
  link.content = pandoc.Inlines({ img })
  return link
end

local function wrapped_image_div(img, link)
  local class = wrap_class(img.classes)
  strip_wrap(img.classes)
  local width = img.attributes.width
  local gap = img.attributes["wrap-gap"]
  img.attributes.width = nil
  img.attributes["wrap-gap"] = nil
  img.attributes["wrap-lines"] = nil
  return pandoc.Div({ pandoc.Plain({ placed_image(img, link) }) }, pandoc.Attr("", { "wrapped-image", class },
    { style = style_for(width, gap, pandoc.utils.stringify(img.src), nil) }))
end

-- In Typst, an image with no caption goes in a .wrapped-image box that the
-- grouping pass opens into a wrap-content call. As in LaTeX, an image with
-- fig-alt and no description takes fig-alt as its description.
local function wrapped_image_typst(img, link)
  local class = wrap_class(img.classes)
  local open = typst_open(class, img.attributes.width, img.attributes["wrap-gap"],
    pandoc.utils.stringify(img.src))
  clean_image(img)
  local alt = img.attributes["fig-alt"]
  if alt and #img.caption == 0 then
    img.caption = pandoc.Inlines(alt)
  end
  return pandoc.Div({ pandoc.Plain({ placed_image(img, link) }) }, pandoc.Attr("", { "wrapped-image", class },
    { ["data-wrap-open"] = open }))
end

-- In Word, an image with no caption goes in a .wrapped-image box that holds
-- its frame. The image keeps its fig-alt, which wrapped-docx.lua sets as
-- its alt text when it frames the image.
local function wrapped_image_docx(img, link)
  local class = wrap_class(img.classes)
  local frame, w = docx_frame(class, img.attributes.width, img.attributes["wrap-gap"],
    pandoc.utils.stringify(img.src))
  clean_image(img)
  return pandoc.Div(docx_size(pandoc.Blocks({ pandoc.Plain({ placed_image(img, link) }) }), w),
    pandoc.Attr("", { "wrapped-image", class }, { ["data-wrap-frame"] = frame }))
end

-- In LaTeX, an image with no caption goes in a raw wrappedfigure. The image
-- stays a Pandoc image, so the LaTeX writer keeps its alt text. Quarto
-- writes fig-alt as alt text only for a figure, so an image with fig-alt
-- and no description takes fig-alt as its description. The caption=false
-- option sets the image in place, not as a float, where the text runs in
-- columns.
local function wrapped_image_tex(img, link)
  local class = wrap_class(img.classes)
  local opts = tex_options(class, img.attributes.width, img.attributes["wrap-gap"],
    img.attributes["wrap-lines"], pandoc.utils.stringify(img.src)) .. ",caption=false"
  clean_image(img)
  local alt = img.attributes["fig-alt"]
  if alt and #img.caption == 0 then
    img.caption = pandoc.Inlines(alt)
  end
  return pandoc.List({
    pandoc.RawBlock("latex", "\\begin{wrappedfigure}[" .. opts .. "]\n\\centering"),
    pandoc.Plain({ placed_image(img, link) }),
    pandoc.RawBlock("latex", "\\end{wrappedfigure}"),
  })
end

-- True when raw HTML is one comment and nothing else.
local function is_comment(text)
  local body = text:match("^%s*<!%-%-(.*)%-%->%s*$")
  return body ~= nil and not body:find("-->", 1, true)
end

-- Shortcode names, and the key of each meta shortcode, by custom id, from
-- the shortcode pass.
local shortcode_names = {}
local meta_keys = {}

local function find_shortcode(shortcode)
  local id = shortcode.__quarto_custom_node.attributes.__quarto_custom_id
  shortcode_names[id] = shortcode.name
  if shortcode.name == "meta" then
    for _, param in ipairs(shortcode.params) do
      -- A key made by a nested shortcode is not known here, so it joins.
      if param.type == "shortcode" then break end
      if param.type == "param" then
        -- As Quarto's meta shortcode, strip one pair of quotes around a key.
        local key = param.value
        if key:sub(1, 1) == '"' and key:sub(-1) == '"' then
          key = key:sub(2, -2)
        elseif key:sub(1, 1) == "'" and key:sub(-1) == "'" then
          key = key:sub(2, -2)
        end
        meta_keys[id] = key
        break
      end
    end
  end
end

-- Quarto's shortcodes that usually make text, an image, or paragraphs, which
-- join a group when the paragraph shows nothing else. A lone meta shortcode
-- whose value holds a block other than a paragraph, such as a list, stops
-- (block_meta).
local JOINING_SHORTCODES = {
  meta = true, var = true, env = true, kbd = true, lipsum = true,
  placeholder = true, version = true, brand = true,
}

-- The metadata value of a meta shortcode's key, read as Quarto's
-- parseOption reads it: dots split the key into keys, `\.` is a dot in a
-- key, and a number after the first key indexes a list.
local function meta_value(key)
  key = key:gsub("\\%.", "\1")
  local value = document_meta
  local first = true
  for part in key:gmatch("[^.]+") do
    if type(value) ~= "table" then return nil end
    part = part:gsub("\1", "."):gsub("\\(.)", "%1")
    value = value[not first and tonumber(part) or part]
    first = false
    if value == nil then return nil end
  end
  return value
end

-- True when a meta shortcode that is its paragraph's one inline makes a
-- block other than a paragraph. Quarto renders the shortcode there as the
-- value's blocks, and a one-item list as its item.
local function block_meta(id)
  local value = meta_keys[id] and meta_value(meta_keys[id])
  while pandoc.utils.type(value) == "List" and #value == 1 do value = value[1] end
  if pandoc.utils.type(value) ~= "Blocks" then return false end
  for _, block in ipairs(value) do
    if block.t ~= "Para" and block.t ~= "Plain" then return true end
  end
  return false
end

-- Inlines that show nothing beside a shortcode: spaces, line breaks in the
-- source, and HTML comments.
local function shows_nothing(el)
  return el.t == "Space" or el.t == "SoftBreak"
    or (el.t == "RawInline" and el.format:match("html") ~= nil and is_comment(el.text))
end

-- The one shortcode in `inlines` when the rest shows nothing, else nil. With
-- `in_span`, a span whose own content is such a shortcode counts as it.
local function lone_shortcode(inlines, in_span)
  local found
  for _, el in ipairs(inlines) do
    if not shows_nothing(el) then
      local shortcode = el.t == "Span" and (el.attributes.__quarto_custom_type == "Shortcode" and el
        or in_span and lone_shortcode(el.content, false))
      if not shortcode or found then return nil end
      found = shortcode
    end
  end
  return found
end

-- True for a paragraph that shows only one shortcode, which Quarto has not
-- yet rendered, with nothing else but spaces, comments, and at most one span
-- around it, unless the shortcode is one that joins. Such a shortcode can
-- make a block, such as a page break or a video. A meta shortcode stops only
-- when it is itself the paragraph's one inline, where Quarto renders its
-- value as blocks; in a span or beside anything else Quarto renders the
-- value as text.
local function is_block_shortcode(para)
  local shortcode = lone_shortcode(para.content, true)
  if not shortcode then return false end
  local id = shortcode.attributes.__quarto_custom_id
  if shortcode_names[id] == "meta" then
    local only = #para.content == 1 and para.content[1]
    return only and only.t == "Span" and only.attributes.__quarto_custom_type == "Shortcode"
      and block_meta(id) or false
  end
  return not JOINING_SHORTCODES[shortcode_names[id]]
end

-- A block that a group takes in: a paragraph that does not start a wrap of
-- its own and is not a block shortcode, or a raw HTML comment, which shows
-- nothing.
local function joins_group(block)
  if block.t == "Para" then return lone_wrapped_image(block) == nil and not is_block_shortcode(block) end
  return block.t == "RawBlock" and block.format:match("html") ~= nil
    and is_comment(block.text)
end

-- True when the first block after blocks[i] that is not a comment is a
-- paragraph that joins a group.
local function text_follows(blocks, i)
  for j = i + 1, #blocks do
    if not joins_group(blocks[j]) then return false end
    if blocks[j].t == "Para" then return true end
  end
  return false
end

-- The next-block rule. A wrapped figure whose next block is not a
-- paragraph that joins a group, such as a heading or a lone block
-- shortcode, has no text to wrap, so it keeps a normal figure with no
-- warning. A float or a code cell has its floats recorded for the float
-- pass. A captioned image with no id, or an image with no caption, loses
-- its wrap class here. Runs before the float pass, on the blocks as written.
local function no_text_blocks(blocks)
  for i, b in ipairs(blocks) do
    if not text_follows(blocks, i) then
      if b.t == "Div" and (is_float_div(b) or b.classes:includes("cell")) then
        pandoc.Blocks({ b }):walk({
          Div = function(d)
            local id = d.attributes.__quarto_custom_id
            if is_float_div(d) and id then no_text[id] = true end
          end,
        })
      elseif b.t == "Figure" and (wrap_class(b.classes) or image_wrap(b.content)) then
        found_wrap = true
        strip_wrap(b.classes)
        b.content = b.content:walk({ Image = drop_wrap })
        blocks[i] = b
      elseif lone_wrapped_image(b) then
        found_wrap = true
        blocks[i] = b:walk({ Image = drop_wrap })
      end
    end
  end
  return blocks
end

-- Applies the next-block rule to every list of blocks except those inside
-- a code cell or a float, where a figure is the last block by design.
local function find_no_text(doc)
  return doc:walk({
    traverse = "topdown",
    Blocks = no_text_blocks,
    Div = function(div)
      if is_float_div(div) or div.classes:includes("cell") then return nil, false end
    end,
  })
end

-- In Typst, a group's blocks as one wrap-content call: the figure in the
-- box of the first argument, and the paragraphs in the second, as raw
-- blocks around them (spike S3). A .wrapped-image box gives up its
-- content. A code cell keeps its code and other output above the call,
-- and gives its wrapped floats to the box.
local function typst_group(members)
  local first = members[1]
  local before, fixed = pandoc.Blocks({}), pandoc.Blocks({})
  local open
  if first.classes:includes("wrapped-image") then
    open = first.attributes["data-wrap-open"]
    fixed:extend(first.content)
  elseif first.classes:includes("cell") then
    first.content = first.content:walk({
      Div = function(d)
        local o = typst_opens[d.attributes.__quarto_custom_id or ""]
        if not (is_float_div(d) and o) then return nil end
        open = open or o
        fixed:insert(d)
        return {}
      end,
    })
    before:insert(first)
  else
    open = typst_opens[first.attributes.__quarto_custom_id]
    fixed:insert(first)
  end
  -- A .wrapped-image div that the filter did not make has no opening, so
  -- its group keeps its blocks as they are.
  if not open then return members end
  local out = before
  out:insert(pandoc.RawBlock("typst", open))
  out:extend(fixed)
  out:insert(pandoc.RawBlock("typst", "], ["))
  for i = 2, #members do out:insert(members[i]) end
  out:insert(pandoc.RawBlock("typst", "]))"))
  return out
end

-- In Word, a group's .wrap-group div carries its figure's or table's frame
-- (for a table, the floating table's side, width, and gap), which
-- wrapped-docx.lua reads at post-render. A .wrapped-image box gives its
-- frame to the group, and a code cell or a float gives the frame of its
-- wrapped float.
local function docx_group(members)
  local first = members[1]
  local frame = first.attributes["data-wrap-frame"]
  first.attributes["data-wrap-frame"] = nil
  if not frame then
    pandoc.Blocks({ first }):walk({
      Div = function(d)
        frame = frame or docx_frames[d.attributes.__quarto_custom_id or ""]
      end,
    })
  end
  -- A .wrapped-image div that the filter did not make has no frame, so its
  -- group keeps its blocks as they are.
  if not frame then return members end
  return pandoc.List({ pandoc.Div(members, pandoc.Attr("", { "wrap-group" }, { ["data-wrap-frame"] = frame })) })
end

-- Groups each wrapped figure or table float with the paragraphs after it.
-- The group stops
-- at the first block that does not join it: a block that is not a
-- paragraph, or a paragraph with a lone block shortcode (the stop rule,
-- `joins_group`). The group's
-- CSS contains its float, so the stop block starts below the figure. In
-- Typst the group is a wrap-content call instead, and in LaTeX the group's
-- blocks are followed by \wrappedstop.
local function group_blocks(blocks)
  local out = pandoc.List()
  local i = 1
  while i <= #blocks do
    local b = blocks[i]
    local members
    local img, link = lone_wrapped_image(b)
    if img then
      found_wrap = true
      if is_html then
        placed = true
        members = pandoc.List({ wrapped_image_div(img, link) })
      elseif is_typst then
        placed = true
        members = pandoc.List({ wrapped_image_typst(img, link) })
      elseif is_docx then
        placed = true
        members = pandoc.List({ wrapped_image_docx(img, link) })
      elseif is_latex then
        placed = true
        members = wrapped_image_tex(img, link)
      else
        strip_wrap(img.classes)
      end
    elseif not is_latex and b.t == "Div" and b.classes:includes("wrapped-image") then
      -- In LaTeX the filter makes no .wrapped-image box.
      members = pandoc.List({ b })
    elseif is_latex and b.t == "Figure" and b.attributes["fig-env"] == "wrappedfigure" then
      -- In LaTeX, a captioned image with no id stays a Figure.
      members = pandoc.List({ b })
    elseif b.t == "Div" and b.attributes["data-wrap-anchor"] then
      -- A cell starts a group only if one of its figure or table floats
      -- wrapped.
      local token = b.attributes["data-wrap-anchor"]
      b.attributes["data-wrap-anchor"] = nil
      if wrapped_cells[token] then members = pandoc.List({ b }) end
    elseif b.t == "Div" and wrapped_ids[b.attributes.__quarto_custom_id or ""] then
      members = pandoc.List({ b })
    end
    if members then
      i = i + 1
      while i <= #blocks and joins_group(blocks[i]) do
        members:insert(blocks[i])
        i = i + 1
      end
      if is_typst then
        out:extend(typst_group(members))
      elseif is_docx then
        out:extend(docx_group(members))
      elseif is_latex then
        -- wrapfig wraps the paragraphs that follow the figure, so the group
        -- needs no div. \wrappedstop ends the wrap at the stop block.
        out:extend(members)
        out:insert(pandoc.RawBlock("latex", "\\wrappedstop"))
      else
        out:insert(pandoc.Div(members, pandoc.Attr("", { "wrap-group" })))
      end
    else
      out:insert(b)
      i = i + 1
    end
  end
  return out
end

-- An image whose wrap class is still there after the grouping pass sits
-- where the filter cannot place a box, such as beside other content in its
-- paragraph or in its link. It keeps a normal layout, with a warning in
-- a format that wraps. Other formats print the one format warning instead.
local function unplaced_image(img)
  if not wrap_class(img.classes) then return nil end
  found_wrap = true
  strip_wrap(img.classes)
  if wraps() then
    quarto.log.warning(string.format(
      "wrapped: the image %s is not a figure or the only content of its paragraph, so it does not wrap.",
      pandoc.utils.stringify(img.src)))
  end
  return img
end

-- A div with a wrap class that is not a figure, such as `::: {.wrap-right}`,
-- keeps a normal layout, with a warning in a format that wraps. The
-- filter's own .wrapped-image boxes carry a wrap class on purpose.
local function unplaced_div(div)
  if not wrap_class(div.classes) or div.classes:includes("wrapped-image") then return nil end
  found_wrap = true
  strip_wrap(div.classes)
  if wraps() then
    local where = div.identifier ~= "" and ("#" .. div.identifier) or "with no id"
    quarto.log.warning(string.format(
      "wrapped: the div %s is not a figure, so it does not wrap.", where))
  end
  return div
end

-- A link with a wrap class, such as `[![](logo.png)](url){.wrap-left}`,
-- wraps nothing, with a warning in a format that wraps. An image inside it
-- wraps only by its own class. The link keeps its class, which no style
-- reads.
local function unplaced_link(link)
  if not wrap_class(link.classes) then return nil end
  found_wrap = true
  if wraps() then
    quarto.log.warning(string.format(
      "wrapped: the link to %s is not a figure, so it does not wrap. A wrap class goes on an image or a figure.",
      link.target))
  end
  return nil
end

-- Names an element in a warning: `#<id>` when it has one, else the first
-- line of its text in quotes, cut after 40 characters, else "with no text".
local function warning_name(id, text)
  if id ~= nil and id ~= "" then return "#" .. id end
  text = text:match("^[^\n]*"):match("^%s*(.-)%s*$")
  if text == "" then return "with no text" end
  local count = utf8.len(text)
  if count and count > 40 then
    return '"' .. text:sub(1, utf8.offset(text, 41) - 1) .. '..."'
  end
  return '"' .. text .. '"'
end

-- A span, heading, inline code, code block, or table with a wrap class
-- wraps nothing, with a warning in a format that wraps. It keeps its class,
-- which no style reads.
local function warn_unplaced(kind, classes, id, text)
  if not wrap_class(classes) then return end
  found_wrap = true
  if wraps() then
    quarto.log.warning(string.format(
      "wrapped: the %s %s is not a figure, so it does not wrap. A wrap class goes on an image or a figure.",
      kind, warning_name(id, text)))
  end
end

local function unplaced_span(span)
  warn_unplaced("span", span.classes, span.identifier, pandoc.utils.stringify(span))
end

local function unplaced_header(header)
  warn_unplaced("heading", header.classes, header.identifier, pandoc.utils.stringify(header))
end

local function unplaced_code(code)
  warn_unplaced("inline code", code.classes, code.identifier, code.text)
end

local function unplaced_code_block(block)
  warn_unplaced("code block", block.classes, block.identifier, block.text)
end

-- A table's classes come from an attribute block at the end of its caption,
-- as in `: A table {.wrap-left}` (caption_block). A table with a tbl- id is
-- a float, which the float pass places or warns for.
local function unplaced_table(tbl)
  local long = tbl.caption.long
  local last = long and long[#long]
  if not last or not last.content then return nil end
  local attr, before = caption_block(last.content)
  if not attr then return nil end
  warn_unplaced("table", attr.classes, nil, pandoc.utils.stringify(before))
  return nil
end

local function add_html_assets()
  -- No version here: `_extension.yml` holds the one version marker.
  quarto.doc.add_html_dependency({
    name = "wrapped",
    stylesheets = { "wrapped.css" },
  })
  -- A custom property cannot set a media query, so the breakpoint rule is
  -- written here from the document's value.
  quarto.doc.include_text("in-header", string.format([[
<style>
@media (max-width: %s) {
  .quarto-float.wrap-left, .quarto-float.wrap-right,
  .wrapped-image.wrap-left, .wrapped-image.wrap-right {
    float: none; width: auto; margin: 1em 0;
  }
}
</style>]], options.breakpoint))
end

local function finish(doc)
  if is_html then
    if placed then add_html_assets() end
  elseif is_latex then
    if placed then
      quarto.doc.use_latex_package("wrapfig")
      quarto.doc.include_file("in-header", "wrapped.tex")
    end
  elseif is_typst then
    -- Quarto loads the package from the extension's typst/packages folder.
    if placed then
      quarto.doc.include_text("in-header", TYPST_HEADER)
    end
  elseif is_docx then
    -- wrapped-docx.lua builds the frames at post-render and needs no header.
    if docx_unread and found_wrap then
      quarto.log.warning(string.format(
        "wrapped: the reference doc %s cannot be read, so a percent width in Word is a share of 6.5 in.",
        docx_unread))
    end
  elseif docx_columns and found_wrap then
    quarto.log.warning(
      "wrapped: Word does not wrap figures in a document with columns, so wrapped figures render as normal figures.")
  elseif two_column and found_wrap then
    quarto.log.warning(
      "wrapped: LaTeX does not wrap figures in a two-column document, so wrapped figures render as normal figures.")
  elseif found_wrap then
    quarto.log.warning(string.format(
      "wrapped: %s output does not wrap figures yet, so wrapped figures render as normal figures.",
      FORMAT))
  end
  return nil
end

return {
  { Meta = read_meta },
  { Shortcode = find_shortcode },
  { Pandoc = find_contexts },
  { Pandoc = find_no_text },
  {
    traverse = "topdown",
    Div = cell_div,
    FloatRefTarget = float_target,
    Figure = figure_block,
  },
  { Blocks = group_blocks },
  {
    Image = unplaced_image,
    Div = unplaced_div,
    Link = unplaced_link,
    Span = unplaced_span,
    Header = unplaced_header,
    Code = unplaced_code,
    CodeBlock = unplaced_code_block,
    Table = unplaced_table,
  },
  { Pandoc = finish },
}
