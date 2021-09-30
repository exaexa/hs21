import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

{- Svet je reprezentovany pomerne primitivne integerem -}
initialWorld = 5

{- Funkce na nakresleni sveta (integeru) do obrazku (gloss Picture) -}
drawWorld n =
  Color black $
  Pictures $
  flip map [1 .. n] $ \i ->
    Translate (100 * fromInteger i - 550) 0 $ ThickCircle 50 20

{- Funkce ktera meni svet (n) v zavislosti na eventu co prisel (v tomto pripade sipkach) -}
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) n = max 0 $ n - 1
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) n = min 10 $ n + 1
handleEvent _ n = n

{- Funkce ktera meni svet "pravidelne" pri uplynuti nejakeho casu (parametr je
 - casovy rozdil mezi snimky); tady nepotrebujeme updatovat nic -}
updateWorld _ = id

{- Do glossoveho `play` se vyplni funkce na managovani sveta, zbytek zaridi knihovna. -}
main = play FullScreen white 25 initialWorld drawWorld handleEvent updateWorld
