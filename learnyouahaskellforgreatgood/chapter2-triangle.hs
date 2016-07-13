triangles = [ (a, b, c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
rightTriangles = [ (a, b, c) | c <- [1..10], b <- [1..10], a <- [1..10], a^2 + b^2 == c^2 ]
rightTriangles' = [ (a, b, c) | c <- [1..10], b <- [1..10], a <- [1..10], a^2 + b^2 == c^2, a+b+c == 24 ]
