import Data.Maybe
import Data.List.Split
import Text.Read
import qualified Data.Map as Map

--import Debug.Trace
import RPN

main :: IO ()
main = do
        input <- getLine
        putStrLn $ infix' input [] []

operators :: Map.Map String Int
operators = Map.fromList [
                         ("+",0),
                         ("-",0),
                         ("*",1),
                         ("/",1),
                         ("^",2)
                         ]

infix' :: String -> [String] -> [String] -> String
infix' input istack ostack -- | trace ("infix " ++ show (reverse ostack) ++ " " ++ show istack ++ " " ++ show input) False = undefined
        | null input         = show $ rpn (reverse (unwords (istack++ostack))) []  -- rpn (unwords $ reverse $ ostack++istack) []
        | is_number token    = infix' rest istack (token:ostack)
        | in_operators token = infix' rest (token:pop_istack (\z -> precedence z >= precedence token)) (popr_istack (\z -> precedence z >= precedence token) ++ ostack)
        | token == "("       = infix' rest (token:istack) ostack
        | token == ")"       = infix' rest (tail (pop_istack (/="("))) (popr_istack (/="(") ++ ostack)
        | otherwise          = infix' rest istack ostack
        where
            (token:xs) = split (dropInitBlank . dropFinalBlank . dropInnerBlanks . oneOf $ "()"++concat (Map.keys operators)) input
            rest = concat xs
            is_number x = isJust (readMaybe x :: Maybe Int)
            in_operators x = Map.member x operators 
            precedence x = if in_operators x then operators Map.! x else (-1)
            pop_istack f = dropWhile f istack
            popr_istack f = takeWhile f istack
