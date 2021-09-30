module Main where

import Brick
import qualified Brick.Types as T
import Brick.Util (bg, fg, on)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center)
import Control.Monad (void)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes.Color as Color

{- 
 - ScreenState je nas jednoduchy datovy typ ve kterem ukladame stav aplikace.
 - Pouzity "record syntax" je trochu slozitejsi nez defaultni definice dat, ale
 - jinak ekvivalentni. Rozhodne je podobnejsi C-style strukturam. Jmena polozek
 - se "ven" dostanou jako acesory, tj. obycejne funkce s typem napr.:
 - curPos :: ScreenState -> Int
 -}
data ScreenState = ScreenState
  { curPos :: Int
  , curHighlighted :: Bool
  }

{- Tohle vyrobi pocatecni stav aplikace -}
initState = ScreenState {curPos = 0, curHighlighted = False}

{- Tahle funkce zkonvertuje stav na seznam Brickovych vrstev widgetu, ktere se
 - pak vykresli. Tady mame jen jednu vrstvu. -}
renderApp s = [center (title <=> mainLayer s)]

{- Operator <=> kombinuje widgety vertikalne (nad sebe)
 - operator <+> je kombinuje vedle sebe horizontalne -}
mainLayer s = foldr1 (<+>) $ map (letter s) [0 .. 10]

title = border $ str "Brick demo app!"

cursorHl = attrName "cursorHl"

letter s i
  | curPos s == i =
    (if curHighlighted s
       then withAttr cursorHl
       else id) $
    str "X"
  | otherwise = str "-"

{- 
 - Event-handling. Funkce v zavislosti na prichozim eventu meni stav aplikace.
 - Ve skutecnosti bychom meli "vracet" normalni monadu, ale Brick situaci
 - vyrazne zjednodusuje 2ma pomocnymi funkcemi:
 -
 - halt newstate   -- zastavi aplikaci s finalnim stavem
 - continue newstate    -- pokracuje ve vykonavani aplikace s modifikovanym stavem
 -
 - Pouzivame "record-update" syntax, zhruba takhle:
 - staryZaznam {zmenenePolicko=novaHodnota}
 -}
handleEvent s (T.VtyEvent (V.EvKey k [])) =
  case k of
    V.KEsc -> halt s
    V.KLeft -> continue $ s {curPos = max 0 (curPos s - 1)}
    V.KRight -> continue $ s {curPos = min 10 (curPos s + 1)}
    V.KChar ' ' -> continue $ s {curHighlighted = not (curHighlighted s)}
    _ -> continue s --ignorujeme vsechny ostatni klavesy
handleEvent s _ = continue s --a jeste navic ignorujeme vsechny ostatni eventy

{- Tohle z definovanych funkci slepi Brickovou aplikaci -}
app :: App ScreenState e ()
app =
  App
    { appDraw = renderApp
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap =
        const $ attrMap V.defAttr [(cursorHl, Color.black `on` Color.cyan)]
    }

{- funkce `main` proste jen spusti aplikaci na pocatecnim stavu, a vysledek
 - (finalni stav) zahodi pomoci `void`, aby nevadil. -}
main :: IO ()
main = void $ defaultMain app initState
