import Control.Monad.Cont

add_cont :: Int -> Int -> Cont r Int
add_cont x y = return (x+y)

square_cont :: Int -> Cont r Int
square_cont x = return (x*x)

pythagoras_cont :: Int -> Int -> Cont r Int
pythagoras_cont x y =
    do x_squared <- square_cont x
       y_squared <- square_cont y
       sum_of_squares <- add_cont x_squared y_squared
       return sum_of_squares
       