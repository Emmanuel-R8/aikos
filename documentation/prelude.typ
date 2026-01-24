// Codeblock shim: Typst has no built-in "codeblock"; render as a styled block.
// Use: #codeblock(lang: "mermaid", [content]) or #codeblock("mermaid", [content])
#let codeblock(lang: "text", body) = block(fill: luma(248), inset: 8pt, width: 100%, stroke: 0.5pt + luma(220))[#set text(font: "DejaVu Sans Mono", size: 9pt); #body]
