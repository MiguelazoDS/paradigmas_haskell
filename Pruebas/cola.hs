data Cola a = Cola [a] deriving (Show)

encolar :: a -> Cola a -> Cola a
encolar x (Cola xs) = Cola (xs ++ [x])

desencolar :: Cola a -> (a, Cola a)
desencolar (Cola []) = error "Cola vacía"
desencolar (Cola xs) = (head xs, Cola $ tail xs)

esVacia :: Cola a -> Bool
esVacia (Cola xs)
              |null xs = True
              |otherwise = False
