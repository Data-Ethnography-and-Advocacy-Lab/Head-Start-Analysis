-- wrapped, Word part: runs at post-render, after Quarto renders each
-- figure. In docx, the pre-quarto pass in wrapped.lua puts each wrapped
-- figure and the paragraphs after it in a .wrap-group div, whose
-- data-wrap-frame holds the frame's side, width, and gap in twips, as
-- "right 3744 240". This pass turns a figure into a Word frame. It
-- removes Quarto's one-cell figure table and gives the image paragraph and
-- each caption paragraph the same w:framePr, so Word shows them as one
-- frame with the group's paragraphs beside it. Pandoc writes every image
-- inline and assigns its relationship id, so the image stays a Pandoc
-- image, and a framed image takes its fig-alt text as its alt text (a
-- float's fig-alt goes on the frame's first image only, unless that image
-- has its own fig-alt). A table float's one-cell table becomes a floating
-- table in place of a frame (floating_table), and a raw openxml table in
-- it, as gt and flextable write one, is fitted to the box (fit_raw). A
-- clearing break after the group's last paragraph ends the wrap
-- (the stop rule). Other formats are left as they are.

if FORMAT ~= "docx" then return {} end

local CLEAR = '<w:r><w:br w:type="textWrapping" w:clear="all"/></w:r>'

-- The w:pPr that holds the frame, with a paragraph style first when one is
-- given, in schema order.
local function frame_pr(frame, style)
  return string.format(
    '<w:pPr>%s<w:framePr w:w="%s" w:wrap="around" w:hSpace="%s" w:vAnchor="text" w:hAnchor="margin" w:xAlign="%s"/></w:pPr>',
    style and string.format('<w:pStyle w:val="%s"/>', style) or "",
    frame.width, frame.gap, frame.side)
end

-- True when a paragraph holds an image at its top level or in a link there.
local function has_image(block)
  for _, inline in ipairs(block.content) do
    if inline.t == "Image" then return true end
    if inline.t == "Link" then
      for _, el in ipairs(inline.content) do
        if el.t == "Image" then return true end
      end
    end
  end
  return false
end

-- Pandoc writes an image's description as its Word alt text. Quarto keeps a
-- figure's fig-alt on its image, as `alt` on the image of a float and as
-- `fig-alt` on a captioned image with no #fig- id, but leaves the
-- description empty or the caption. So a framed image takes its own
-- fig-alt in place of its description. Quarto puts a float's `alt` on each
-- of its images, so only the frame's first image (`first`) with no fig-alt
-- of its own takes it, and a later image keeps its own fig-alt or
-- description; the alt text of a div float is only in `alt`. The text goes in as one Str, which keeps
-- its repeated spaces. An empty fig-alt gives empty alt text, as in HTML.
local function alt_image(img, first)
  local alt = img.attributes["fig-alt"]
  if alt == nil and first then alt = img.attributes.alt end
  if alt ~= nil then
    img.caption = alt == "" and pandoc.Inlines({}) or pandoc.Inlines({ pandoc.Str(alt) })
  end
  return img
end

-- Puts the frame on a paragraph. Pandoc writes the paragraph's own w:pPr
-- first, and a caption from Quarto starts with a raw w:pPr of its own, so
-- the frame follows that one. Word reads the frame from either position,
-- and its style from the last w:pPr that sets one. An image paragraph takes
-- `image_style`, the style it had in the figure, because out of the figure
-- Pandoc gives it the body text style. `state.seen` is set at the frame's
-- first image that holds Quarto's `alt`, so no later image takes the
-- float's alt. An image in a caption, such as an icon, holds no `alt` and
-- does not count, though a top caption comes before the images.
local function frame_para(block, frame, image_style, state)
  local first = block.content[1]
  local at = 1
  if first and first.t == "RawInline" and first.format == "openxml"
    and first.text:match("^%s*<w:pPr>") then
    at = 2
  end
  local style = has_image(block) and image_style or nil
  block.content:insert(at, pandoc.RawInline("openxml", frame_pr(frame, style)))
  return block:walk({
    Image = function(img)
      local is_first = false
      if img.attributes.alt ~= nil and not state.seen then
        is_first = true
        state.seen = true
      end
      return alt_image(img, is_first)
    end,
  })
end

-- A Figure (a captioned image with no #fig- id) has no place for a raw
-- w:framePr: with a raw inline beside its image, Pandoc writes the Figure as
-- a one-cell table, and a frame in a table cell is not beside the body
-- text. So the Figure becomes its image paragraphs and a caption paragraph
-- for each caption block in the ImageCaption style, as Pandoc styles it, in
-- a div that keeps its id.
local CAPTION_PR = '<w:pPr><w:pStyle w:val="ImageCaption"/></w:pPr>'

local function figure_paras(fig)
  local blocks = pandoc.Blocks({})
  blocks:extend(fig.content)
  for _, b in ipairs(fig.caption.long) do
    if b.t == "Para" or b.t == "Plain" then
      local caption = pandoc.Inlines({ pandoc.RawInline("openxml", CAPTION_PR) })
      caption:extend(b.content)
      blocks:insert(pandoc.Para(caption))
    else
      blocks:insert(b)
    end
  end
  return pandoc.Div(blocks, pandoc.Attr(fig.identifier))
end

-- Blocks that Word writes as something other than a paragraph that a raw
-- w:pPr can join, so a figure that holds one cannot become one frame.
local UNFRAMEABLE = {
  CodeBlock = true, Table = true, HorizontalRule = true, LineBlock = true,
  RawBlock = true, Header = true,
}

local function frameable(blocks)
  local ok = true
  blocks:walk({
    Block = function(b) if UNFRAMEABLE[b.t] then ok = false end end,
  })
  return ok
end

-- Frames every paragraph in a list of blocks, at any depth, the paragraphs
-- of a Pandoc Figure (its image and its caption) and of a list included.
-- One `state` table serves the whole frame.
local function frame_blocks(blocks, frame, image_style, state)
  return blocks:walk({
    traverse = "topdown",
    Figure = function(fig)
      local div = figure_paras(fig)
      div.content = frame_blocks(div.content, frame, "CaptionedFigure", state)
      return div, false
    end,
    Para = function(p) return frame_para(p, frame, image_style, state), false end,
    Plain = function(p) return frame_para(p, frame, image_style, state), false end,
  })
end

-- Returns the div inside a float table, Quarto's one-cell table that holds
-- a div with the float's id, with "fig" or "tbl", or nil for any other
-- table.
local function float_div(tbl)
  if #tbl.bodies ~= 1 or #tbl.bodies[1].body ~= 1 then return nil end
  local cells = tbl.bodies[1].body[1].cells
  if #cells ~= 1 or #cells[1].contents ~= 1 then return nil end
  local div = cells[1].contents[1]
  if div.t ~= "Div" then return nil end
  local kind = div.identifier:match("^(fig)%-") or div.identifier:match("^(tbl)%-")
  if kind then return div, kind end
  return nil
end

-- A table float's box is a floating table (D-022): Word wraps text around
-- a table with a w:tblpPr, but a frame holds paragraphs only. Its one cell
-- holds the float's div, with the caption, the table, and the bookmark
-- around them. Pandoc writes raw openxml as it is, so a raw block opens the
-- floating table before the div and another closes it after. The gap goes
-- on the side toward the text. The cell has no margins, so its content is
-- as wide as the box, as in a frame. A cell must end with a paragraph, so
-- an empty one follows a table at the end. Word joins two tables with
-- nothing between them, and Pandoc cannot see the raw table to put its
-- paragraph there, so a 1 pt paragraph comes before the floating table. It
-- keeps with the next paragraph, so a heading before it, which keeps with
-- it, moves to the next page with the table (seen in LibreOffice).

-- Gives each column of a table in the box its relative width, or an equal
-- share for a column with none, so the table fills the box. wrapped.lua's
-- column_shares sizes cell images by the same shares; change both together.
local function fit_table(tbl)
  local shares, sum = {}, 0
  for i, spec in ipairs(tbl.colspecs) do
    local w = spec[2]
    shares[i] = (type(w) == "number" and w > 0) and w or 1
    sum = sum + shares[i]
  end
  for i, spec in ipairs(tbl.colspecs) do
    tbl.colspecs[i] = { spec[1], shares[i] / sum }
  end
  return tbl
end

-- The w:tblPr children that come before w:tblW, in schema order.
local BEFORE_TBLW = {
  "tblStyle", "tblpPr", "tblOverlap", "bidiVisual", "tblStyleRowBandSize",
  "tblStyleColBandSize",
}

-- The end of the element `name` (self-closing or with a closing tag, with
-- or without attributes) in `pr`, or nil when `pr` has none.
local function element_end(pr, name)
  local s, e, after = pr:find("<w:" .. name .. "([%s/>])")
  if not s then return nil end
  if after ~= ">" then e = pr:find(">", e, true) end
  if pr:sub(e - 1, e) == "/>" then return e end
  local _, close = pr:find("</w:" .. name .. ">", e + 1, true)
  return close
end

-- Fits a raw openxml table, as gt and flextable write one (D-025), to the
-- box: its w:tblW becomes the box width, in schema order, in a w:tblPr
-- added when it has none. A w:tblGrid, as flextable's fixed layout writes,
-- scales to the box in its own proportions, each column the difference of
-- its rounded edges, so the sum is the box width; each dxa w:tcW of the
-- table's own cells then scales by the same ratio. A table with no grid,
-- as gt writes, keeps its w:tcW widths. Only the block's first table is
-- fitted, and a table nested in its cells keeps its widths.
local function fit_raw(raw, width)
  if raw.format ~= "openxml" then return nil end
  local text = raw.text
  local start, open_end = text:find("<w:tbl>", 1, true)
  local s2, e2 = text:find("<w:tbl%s[^>]*>")
  if s2 and (not start or s2 < start) then start, open_end = s2, e2 end
  if not start then return nil end
  local tblw = string.format('<w:tblW w:type="dxa" w:w="%d"/>', width)
  -- The table's own w:tblPr and w:tblGrid come before its first row, the
  -- w:tblPr after any range markup, such as a bookmark.
  local function first_row()
    return text:find("<w:tr[%s>]", open_end + 1) or #text + 1
  end
  local empty_s, empty_e = text:find("<w:tblPr%s*/>", open_end + 1)
  if empty_s and empty_s < first_row() then
    text = text:sub(1, empty_s - 1) .. "<w:tblPr></w:tblPr>" .. text:sub(empty_e + 1)
  end
  local pr_s, pr_e = text:find("<w:tblPr%s*>", open_end + 1)
  if pr_s and pr_s < first_row() then
    local close_s = text:find("</w:tblPr>", pr_e + 1, true)
    local pr = text:sub(pr_e + 1, close_s - 1)
    pr = pr:gsub("<w:tblW[^>]*/>", ""):gsub("<w:tblW[^>]*>.-</w:tblW>", "")
    local at = 0
    for _, name in ipairs(BEFORE_TBLW) do
      local e = element_end(pr, name)
      if e and e > at then at = e end
    end
    pr = pr:sub(1, at) .. tblw .. pr:sub(at + 1)
    text = text:sub(1, pr_e) .. pr .. text:sub(close_s)
  else
    -- A new w:tblPr goes after any range markup, before the grid or the
    -- first row.
    local at = math.min(text:find("<w:tblGrid[%s>]", open_end + 1) or #text + 1, first_row())
    text = text:sub(1, at - 1) .. "<w:tblPr>" .. tblw .. "</w:tblPr>" .. text:sub(at)
  end
  local grid_s, grid_e = text:find("<w:tblGrid%s*>.-</w:tblGrid>", open_end + 1)
  if grid_s and grid_s > first_row() then grid_s = nil end
  if not grid_s then
    raw.text = text
    return raw
  end
  local grid = text:sub(grid_s, grid_e)
  local sum = 0
  for w in grid:gmatch('<w:gridCol[^>]-w:w="(%d+)"') do sum = sum + tonumber(w) end
  if sum == 0 then
    raw.text = text
    return raw
  end
  local ratio = width / sum
  local cum, edge = 0, 0
  grid = grid:gsub('(<w:gridCol[^>]-w:w=")(%d+)(")', function(a, w, b)
    cum = cum + tonumber(w)
    local next_edge = math.floor(cum * ratio + 0.5)
    local col = next_edge - edge
    edge = next_edge
    return a .. string.format("%d", col) .. b
  end)
  -- The twip w:tcW of the table's own cells, at depth 1, up to its end. A
  -- w:tcW with no w:type is in twips.
  local rest = text:sub(grid_e + 1)
  local depth, done = 1, false
  rest = rest:gsub("<[^>]*>", function(tag)
    if done then return nil end
    if tag:match("^<w:tbl[%s>]") then
      depth = depth + 1
    elseif tag == "</w:tbl>" then
      depth = depth - 1
      if depth == 0 then done = true end
    elseif depth == 1 and tag:match("^<w:tcW[%s/>]")
      and (tag:match('w:type="dxa"') or not tag:match("w:type=")) then
      return (tag:gsub('w:w="(%d+)"', function(w)
        return string.format('w:w="%d"', math.floor(tonumber(w) * ratio + 0.5))
      end))
    end
    return nil
  end)
  raw.text = text:sub(1, grid_s - 1) .. grid .. rest
  return raw
end

local function floating_table(div, frame)
  div = div:walk({
    Table = fit_table,
    RawBlock = function(raw) return fit_raw(raw, tonumber(frame.width)) end,
  })
  local left = frame.side == "left" and 0 or frame.gap
  local right = frame.side == "left" and frame.gap or 0
  local open = string.format(
    '<w:p><w:pPr><w:keepNext /><w:spacing w:before="0" w:after="0" w:line="20" w:lineRule="exact" /></w:pPr></w:p><w:tbl><w:tblPr><w:tblStyle w:val="Table" /><w:tblpPr w:leftFromText="%s" w:rightFromText="%s" w:vertAnchor="text" w:horzAnchor="margin" w:tblpXSpec="%s" /><w:tblW w:type="dxa" w:w="%s" /><w:tblLayout w:type="fixed" /><w:tblCellMar><w:left w:w="0" w:type="dxa" /><w:right w:w="0" w:type="dxa" /></w:tblCellMar><w:tblLook w:firstRow="0" w:lastRow="0" w:firstColumn="0" w:lastColumn="0" w:noHBand="0" w:noVBand="0" w:val="0000" /></w:tblPr><w:tblGrid><w:gridCol w:w="%s" /></w:tblGrid><w:tr><w:tc><w:tcPr><w:tcW w:type="dxa" w:w="%s" /></w:tcPr>',
    left, right, frame.side, frame.width, frame.width, frame.width)
  -- The cell's last block, looking into a div that ends the cell and past
  -- an empty one, as Pandoc writes a div's blocks with no element of its
  -- own. The closing paragraph goes in unless that block is one Pandoc
  -- writes as a paragraph; it drops an empty one. A block quote or list
  -- can end in a table, so it takes the paragraph too.
  local function last_block(blocks)
    for i = #blocks, 1, -1 do
      local b = blocks[i]
      if b.t ~= "Div" then return b end
      local inner = last_block(b.content)
      if inner then return inner end
    end
    return nil
  end
  local last = last_block(div.content)
  local paragraph = last and (
    ((last.t == "Para" or last.t == "Plain") and #last.content > 0)
    or last.t == "Header" or last.t == "CodeBlock" or last.t == "HorizontalRule")
  local needs = not paragraph
  local close = (needs and "<w:p />" or "") .. "</w:tc></w:tr></w:tbl>"
  return pandoc.Blocks({
    pandoc.RawBlock("openxml", open), div, pandoc.RawBlock("openxml", close),
  })
end

-- Frames the group's figure, which is a float table, a .wrapped-image box,
-- or a code cell that holds a float table. A float table's div keeps the
-- float's id, so Pandoc writes its bookmark around the frame or inside the
-- floating table, and the @fig- links still reach a frame. Quarto
-- writes a figure table's image paragraph in the Compact style. Returns
-- the figure's name and nil when it framed the figure, or the name and the
-- reason when it did not.
local function frame_first(blocks, frame)
  local first = blocks[1]
  local function framed(div, kind)
    local name = div.identifier ~= "" and ("#" .. div.identifier) or "a wrapped figure"
    if kind == "tbl" then return pandoc.Div(floating_table(div, frame)), name end
    if not frameable(div.content) then
      return nil, name, "holds a block that Word cannot put in a frame"
    end
    return pandoc.Div(frame_blocks(div.content, frame, "Compact", {}), pandoc.Attr(div.identifier)), name
  end
  if first.t == "Table" then
    local div, kind = float_div(first)
    if not div then return "a wrapped figure", "is not in a form that the filter knows" end
    local new, name, why = framed(div, kind)
    if why then return name, why end
    blocks[1] = new
    return name
  end
  if first.t ~= "Div" then return "a wrapped figure", "is not in a form that the filter knows" end
  if first.classes:includes("wrapped-image") then
    if not frameable(first.content) then
      return "a wrapped image", "holds a block that Word cannot put in a frame"
    end
    first.content = frame_blocks(first.content, frame, nil, {})
    return "a wrapped image"
  end
  local name, why = "a wrapped figure", "is not in a form that the filter knows"
  local done = false
  first.content = first.content:walk({
    traverse = "topdown",
    Table = function(tbl)
      if done then return nil end
      local div, kind = float_div(tbl)
      if not div then return nil end
      done = true
      local new, n, w = framed(div, kind)
      name, why = n, w
      if w then return nil, false end
      return new, false
    end,
  })
  return name, why
end

local function wrap_group(div)
  local data = div.attributes["data-wrap-frame"]
  if not (div.classes:includes("wrap-group") and data) then return nil end
  local side, width, gap = data:match("^(%a+) (%d+) (%d+)$")
  local blocks = div.content
  if not side then return blocks end
  local name, why = frame_first(blocks, { side = side, width = width, gap = gap })
  if why then
    quarto.log.warning(string.format(
      "wrapped: %s %s, so it does not wrap in Word.", name, why))
    return blocks
  end
  for i = #blocks, 2, -1 do
    if blocks[i].t == "Para" then
      blocks[i].content:insert(pandoc.RawInline("openxml", CLEAR))
      break
    end
  end
  return blocks
end

return {
  { Div = wrap_group },
}
