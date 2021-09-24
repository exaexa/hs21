module Main where

import Data.Char (isAlpha)

react :: String -> IO ()
react [] = putStrLn "Na vstupu nic nebylo!!!"
react (a : _)
  | isAlpha a =
    putStrLn $
      "První znak je "
        ++ if a `elem` "klmno"
          then "podezřele blízko polovině abecedy"
          else "úplně obyčejné písmeno"
  | otherwise = putStrLn "První znak nebylo písmeno!"

main :: IO ()
main = do
  prvniRadek <- getLine
  putStrLn $ replicate 78 '*' --dekorace kolem výstupu nemají moc smysl...
  react prvniRadek
  putStrLn $ take 78 $ cycle "---8<---" --ale program aspoň nevypadá tak chudě

{- Jak zatím chápat IO:
 -
 - `getLine` má typ `IO String`, což je popis nějaké IO akce která vyrobí jeden
 - String.
 -
 - "Bind" pomocí syntaxe <- vyrobí akci, která z původní akce vyextrahuje
 - výsledek a dá ho k dispozici dalším akcím; z typu původní akce tj. efektivně
 - odstraní "IO". V našem případě:
 -
 - getLine :: IO String
 - prvniRadek :: String
 -
 - Akce v bloku uvedeném `do` se pospojují tak, aby se vyhodnotily ve správném
 - pořadí postupně shora dolů. Funkce `react` vždycky vrací jen jednu takovou
 - akci, takže `do` nepotřebuje.
 -
 - Detailnější popis toho co se vlastně děje proběhne na druhé přednášce.
 -}
