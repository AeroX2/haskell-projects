module RPN (rpn) where

main :: IO ()
main = do
        input <- getLine
        print $ rpn input []

rpn :: String -> [Float] -> Float
rpn input stack 
            | null input = head stack
            | hi == "+" = rpn (unwords ti) (x+y:st)
            | hi == "-" = rpn (unwords ti) (x-y:st)
            | hi == "*" = rpn (unwords ti) (x*y:st)
            | hi == "/" = rpn (unwords ti) (x/y:st)
            | otherwise = rpn (unwords ti) (number:stack)
            where
                (x:y:st) = stack
                (hi:ti) = words input
                number = read hi
