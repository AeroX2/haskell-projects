module RPN (rpn) where

-- import Debug.Trace

main :: IO ()
main = do
        input <- getLine
        print $ rpn input []

rpn :: String -> [Float] -> Float
rpn input stack -- | trace ("rpn " ++ show (stack) ++ " " ++ show input) False = undefined
            | null input = head stack
            | hi == "+" = rpn (unwords ti) (x+y:st)
            | hi == "-" = rpn (unwords ti) (y-x:st)
            | hi == "*" = rpn (unwords ti) (x*y:st)
            | hi == "/" = rpn (unwords ti) (y/x:st)
            | otherwise = rpn (unwords ti) (number:stack)
            where
                (x:y:st) = stack
                (hi:ti) = words input
                number = read hi
