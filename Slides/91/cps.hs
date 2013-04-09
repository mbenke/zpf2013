add :: Int -> Int -> Int
add x y = x + y

add_cps :: Int -> Int -> (Int -> r ) -> r
add_cps x y k =  k (x+y)

type Cont r a = (a -> r) -> r
add_cps' :: Int -> Int -> Cont r Int
add_cps' = add_cps

square x = x*x

square_cps :: Int -> (Int -> r) -> r
square_cps x k = k (square x)

pythagoras_cps :: Int -> Int -> (Int -> r) -> r
pythagoras_cps x y k =
 square_cps x $ \x_squared ->
 square_cps y $ \y_squared ->
 add_cps x_squared y_squared $ \sum_of_squares ->
 k sum_of_squares