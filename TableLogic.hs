module TableLogic where

import           TermType
import           Utils

-- търси в таблицата стойност по подаден ключ
searchTable :: String -> Table -> Result TermType
searchTable key table =
    let filterResult =  map snd (filter (\(k,v) -> k == key) table)
    in if length filterResult == 1 then Ok (head filterResult) else Err "Key not found or at least two same keys found"

-- като map, но над речник и само върху стойностите му
changeValues :: (TermType -> TermType) -> Table -> Table
changeValues changeFunction = map (\(k,v) -> (k, changeFunction v))

-- добавя несъществуваща двойка или променя стойността в речника, ако той(речника) вече има подадения ключ
insertPair :: String -> TermType -> Table -> Table
insertPair nk nv []           = [(nk, nv)]
insertPair nk nv (curr@(k,_):rest) = if nk == k then (nk, nv) : rest else curr : insertPair nk nv rest
