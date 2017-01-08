module UnOptimizer (unoptimize) where

unoptimize :: Int -> String -> String
unoptimize i = replace '\n' ('\n' : mconcat (replicate i "nop\n"))

replace :: (Eq a) => a -> [a] -> [a] -> [a]
replace from to [] = []
replace from to (h:t) =
    if h == from then to ++ replace from to t
    else h : replace from to t
