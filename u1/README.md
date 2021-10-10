# Úkol 1 -- Malování

Naprogramujte miniaturní bitmapový grafický editor s několika jednoduchými funkcemi. Samozřejmě neimplementujte celou alternativu Photoshopu; zadání si výrazně zjednodušíme:

- Bitmapa má předem daný (malý) rozměr -- pro účely DÚ si vyberte jakoukoliv velikost, stačí 32×32. (Můžete si představit, že vyrábíte editor ikon pro Windows 3.11.)
- Bitmapa obsahuje jen 3 barvy -- "prázdnou" (tj. "průhlednou" nebo "bílou" jako papír), "světlou" a "tmavou" (odstín obou si vyberte libovolně, např. světle a tmavě modrou).
- Bitmapu nejde nijak uložit a při restartu programu se celá smaže, čímž reflektujete pomíjivou podstatu ikonového umění.

## Úkol

Editor naprogramujte v Haskellu pomocí knihoven na výrobu uživatelských rozhraní, např. Gloss nebo Brick (viz níže).

Celý program by měl jít ovládat klávesnicí, konkrétně šipkami které nad bitmapou posouvají kurzor, a několika dalšími klávesami:
  - mezerník smaže barvu na pixelu pod kurzorem (tj. nastaví "prázdnou" barvu)
  - `x` nastaví aktuální pixel na světlou barvu
  - `z` nastaví aktuální pixel na tmavou barvu

K základním vlastnostem doimplementujte ještě alespoň 3 z následujících možností:

- kreslení vyplněného obdélníku (s libovolným uživatelským rozhraním, např. jeden roh obdélníku se vybere pomocí `r` (jako Rectangle), a druhý pomocí `x` nebo `z` podle toho, jakou má obdélník mít barvu)
- kreslení rovné čáry pomocí [nějakého algoritmu na kreslení bitmapových čar](https://en.wikipedia.org/wiki/Line_drawing_algorithm) -- ideálně Bresenhamova, protože ten v Haskellu jde velmi pěkně vyjádřit pomocí `scanl` (rozhraní podobně jako u obdélníku, jen pomocí jiné klávesy, třeba `l`)
- kreslení vyplněného kruhu (pomocí kulatého `o`)
  - Bonus: kreslení elipsy
- inverze pixelů (v obdélníku vybraném pomocí `i` se tmavé pixely změní na světlé a světlé na tmavé)
- copy&paste (vybraný obdélník se zkopíruje pomocí `c` a vloží na jiné místo pomocí `v`)
- efekt `eroze` (všechny pixely které hraničí s průhlednou barvou zmizí) a `dilatace` (všechny průhledné pixely které hraničí s nějakou barvou dostanou barvu podle nejčastější barvy sousedů), [podobně jako v GIMPu](https://docs.gimp.org/2.8/en/filters-generic.html)

Požadavky:
- V průběhu kreslení složitějších tvarů by mělo být jasně vidět kde je první označený bod (aby si ho uživatel nemusel pamatovat). V ideálním případě uživatel předem uvidí, jak velký obdélník nebo kruh bude ještě před "potvrzením".
- Zkuste nějakým rozumným způsobem pěkně a rozšiřitelně pokrýt všechny možné chyby, kterých se na vstupu může dopustit uživatel, např. pokud začne kreslit obdélník a místo `x` nebo `z` ho zakončí klávesou pro kreslení čáry.
  - Těžší bonus: Alternativně navrhněte nějaký systém ovládání, ve kterém jsou všechny kombinace akcí dobře definované, a ideálně je připravený na složitější úkoly vyžadující celou sekvenci vstupů, jako např. kreslení Beziérových křivek. (K programu pak ale prosím přidejte komentář s návodem na ovládání, případně nějakou alternativu `vimtutor`u).

## Návod

Použijte nějakou Haskellovou knihovnu na výrobu jednoduchých grafických aplikací, doporučuju jednu z následujících:

- [Gloss](https://hackage.haskell.org/package/gloss-1.13.1.2/docs/Graphics-Gloss.html) -- dostanete jednoduché grafické okno do kterého můžete kreslit vektorovou grafiku, knihovna navíc podporuje jednoduché vyrábění podobných aplikací (většinou skoro her) pomocí funkce [`play`](https://hackage.haskell.org/package/gloss-1.13.1.2/docs/Graphics-Gloss.html#v:play).
- [Brick](https://github.com/jtdaugherty/brick/) -- knihovna účelem podobná Glossu, ale místo grafického okna pro vektorovou grafiku dostanete terminálové okno pro kreslení ASCII grafiky.

Grafickou reprezentaci bitmapy, kurzoru a vybraného regionu nepřehánějte -- v případě Glossu stačí obarvené čtverce a trojůhelníková šipka místo kurzoru, v případě Bricku stačí ASCII znaky (např. `.` je prázdno, `#` světlá barva a `o` je tmavá barva).

Pro oba případy můžete využít jednoduché připravené aplikace, které "už něco dělají":

- [gloss.hs](./gloss.hs) -- potřebuje knihovnu `gloss`, tu si můžete nainstalovat pomocí `cabal install gloss`, zdrojový kód "mimo projekt" skompilujete pomocí `ghc -package gloss gloss.hs -o gloss`
- [brick.hs](./brick.hs) -- potřebuje knihovny `brick` a `vty`, po jejich nainstalování kompilujte pomocí `ghc -package brick -package vty brick.hs -o brick`

### Datové struktury

Celou bitmapu můžete reprezentovat jako seznam seznamů, ale mnohem lepší je použít arraye (přecejen jsou rychlejší...). Nízkoúrovňové funkcionální arraye z `Data.Array` jsou poněkud moc generické (tj. pro začátečníky nepoužitelné), proto je výrazně lepší je používat obalené v [`Data.Vector`](https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector.html). Vektor je v tomto případě víceméně ekvivalent konstantní C-čkové arraye.

Vektory jsou "konstantní" a copy-on-write, tj. změna jednoho prvku někde uprostřed je docela drahá. Místo toho můžete zkusit jednu z následujících možností:

- Vektor můžete nejdřív vyrobit jako seznam (to je díky lazy vyhodnocování většinou "zadarmo"), a na ten pak zavolat `fromList`.
- Použít [`Data.Vector.Mutable`](https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector-Mutable.html) (to je víceméně obyčejná C-čková zapisovatelná array). Čtení a úpravy mutovatelného vektoru jsou ale operace, u kterých záleží na pořadí provádění, takže je nutné je "spojovat" opatrně, stejným mechanismem jako IO akce. [Příklad s bubblesortem](https://www.ksi.mff.cuni.cz/~kratochvil/haskell/source/MVectorBubbleSort.hs). Mutovatelné vektory fungují i bez `IO` -- stačí jim monáda která umí zaručit lokální ordering paměťových operací, například `STM` ("software transactional memory") dostupná nejjednodušším způsobem pomocí funkce [`runST`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Monad-ST.html#v:runST).

## Odevzdání a hodnocení

Z programu vyrobte cabalový balík pojmenovaný `u1-vaseprijmeni`, ten zabalte pomocí `cabal sdist` a nahrajte do odpovídající kolonky do SISu.

Snažte se o hezký, přehledný, čitelný a _krátký_ kód. Tj.: pokud možno neřešte všechny problémy "ručně" rekurzí, ale používejte rozumné vyšší funkce (`map`, `zip`, `foldr`, ...). Opakovaný kód vytrhněte do vlastní funkce a nahraďte nějakou zkratkou (ve funkcionálním programování jde vytrhnout a zkrátit prakticky cokoliv).

Odchylky od specifikace jsou v rozumné míře OK.
