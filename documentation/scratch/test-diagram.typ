#set page(margin: 2cm)

#figure(
  caption: [Test Diagram],
  diagram(
    node-stroke: 0.5pt,
    spacing: 1.5em,
    
    a = rect([Node A]),
    b = rect([Node B]),
    
    connect(a, b),
  )
)
