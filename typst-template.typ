#let report(
  title: none,
  date: none,
  content,
) = {
  set page(
    paper: "us-letter",
    margin: (top: 1in, bottom: 0.75in, x: 0.75in),
    header: {
      rect(
        width: 100%,
        height: 0.75in,
        outset: (x: 15%),
        fill: rgb("#0063a6"),
        pad(top: 16pt, block([
          #grid(
            columns: (75%, 25%),
          )
        ])),
      )
    },
    footer: {
      rect(
        width: 100%,
        height: 0.75in,
        outset: (x: 15%),
        fill: rgb("#0063a6"),
        pad(top: 16pt, block([
          #grid(
            columns: (75%, 25%),
            align(left)[
              #text(
                title,
                font: "Roboto",
                fill: white,
                weight: "black",
              )
            ],
            align(right)[
              #text(
                date,
                font: "Roboto",
                fill: white,
                weight: "black",
              )
            ],
          )
        ])),
      )
    },
  )
  set text(
    lang: "en",
    region: "US",
    font: "Lato",
    size: 10pt,
  )
  show heading: it => {
    let sizes = (
      "1": 16pt, // Heading level 1
      "2": 14pt, // Heading level 2
    )
    let level = str(it.level)
    let size = sizes.at(level)
    let formatted_heading = if level == "1" { upper(it) } else { it }
    let alignment = left

    set text(
      size: size,
      fill: rgb("#0063a6"),
      font: "Roboto",
      weight: "black",
    )
    align(alignment)[#formatted_heading]
  }

  //image("logo.png", width: 4in)

  grid(
    columns: (80%, 15%, 5%),
    align(left)[
      #text(
        title,
        font: "Roboto",
        size: 20pt,
        fill: rgb("#0063a6"),
        weight: "black",
      )
    ],
    align(right)[
      //#image("flag.svg")
    ],
    align(right)[],
  )

  content
}

#let headline-box(stat-text: "", summary-text: "") = {
  let top_box = box(
    radius: 4pt,
    width: 2in,
    height: 0.7in,
    fill: rgb("#F8F8F8"),
    stroke: (bottom: 1pt + rgb("#0063a6")),
    inset: 2pt,
    align(center + horizon)[
      #text(fill: rgb("#0063a6"), weight: "bold", size: 10pt)[#stat-text] \
      #text(fill: black, weight: "bold", size: 6pt)[#summary-text]
    ],
  )

  top_box
}