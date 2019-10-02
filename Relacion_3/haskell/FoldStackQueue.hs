module FoldStackQueue where
import qualified DataStructures.Stack.LinearStack as S

foldrStack :: (a -> b -> b) -> b -> S.Stack a -> b
foldrStack f z s
    | S.isEmpty s = z
    | otherwise = S.top s 'f' foldrStack f z (S.pop s)