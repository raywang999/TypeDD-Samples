import ex_10_3

area : (shape : Shape) -> Double
area shape with (shapeView shape)
  area (triangle base height) | (STriangle base height) = 0.5 * base * height
  area (rectangle length height) | (SRectangle length height) = length * height
  area (circle radius) | (SCircle radius) = pi * radius * radius
