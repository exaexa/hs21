
# Úkol 0: statistika ASCII písmen

Program přečte celý standardní vstup (`stdin`) a vypíše kolik procent písmen jsou samohlásky (`aeiouy`).

Pokud se nechcete zabývat tím jak v Haskellu fungují floatová čísla, procenta můžete seříznout na celá čísla. Počítejte jen s ASCII znaky, můžete předpokládat, že unikód na vstupu nebude, a zároveň že vstup nebude nijak obrovský.

Příklad vstupu:
```
aaasssddd
    ---neconeco...
```

Odpovídající výstup:
```
41%
```

## Návod

Protože je to první úkol, přidávám seznam funkcí ze standardní knihovny, které asi budete chtít použít. Všechny funkce si můžete dohledat na [hoogle](https://hoogle.haskell.org/); většinou se vyplatí i prohledat mírné okolí.

- [`elem`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:elem)
- [`isAlpha`](https://hackage.haskell.org/package/base/docs/Data-Char.html#v:isAlpha) z knihovny `Data.Char` (funguje prakticky stejně jako Cčkové `isalpha` z `<ctype.h>`)
- [`getContent`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:getContents) na načtení celého vstupu (je to IO akce která vyprodukuje `String`), případně [`interact`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:interact)
- [`putStrLn`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:putStrLn), [`show`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:show) nebo [`print`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:print) pro vypsání a/nebo naformátování výstupu
- [`fromIntegral`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:fromIntegral) pro zkonvertování integerovitého typu na cokoliv, hodí se např. pokud chcete počítat pomocí floatů. Na zpětnou konverzi se může hodit nějaká funkce [odsud](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:truncate).

### Jak vyrobit "projekt"?

Haskellový ekosystém standardizoval poměrně užitečné balíčky managované programem `cabal`. Ten se používá následovně:

#### 1. Inicializace

Vyrobte si adresář na projekt, a v něm spusťte:
```
cabal init --interactive
```
Cabal se zeptá na nějaké detaily o projektu (jestli to je knihovna nebo spustitelný program, licenci, název, ...) a vyrobí minimální adresářovou strukturu. První otázka se většinou ptá na to, jestli se mají prostě všude použít defaulty, tam nejspíš chcete odpovědět "n", ale defaulty jsou jinak docela dobrá volba.

Projekt na domácí úkol pojmenujte `u0-prijmeni`, tj. já bych ho pojmenoval `u0-kratochvil`.

Všechny informace o projektu jsou zapsané v souboru `jmenoprojektu.cabal`. Interaktivní systém se vás mimo jiné zeptá i na to, jestli v souboru chcete nechat užitečné komentáře, což se může dost hodit:
```
Add informative comments to each field in the cabal file (y/n)? [default: n] y
```

#### 2. Programování

Pokud se všechno povedlo, meli byste mít k dispozici defaultní "poloprázdný" soubor `Main.hs`. Ten můžete normálně editovat. Projekt můžete kdykoliv nechat sestavit a spustit pomocí `cabal run`. (Defaultní `Main.hs` většinou výpíše něco jako `Hello Haskell!`).

Další příkazy:
- Projekt můžete případně nechat "jen sestavit" pomocí `cabal build`
- Pokud potřebujete nestandardní knihovny, můžete si je nainstalovat pomocí `cabal install jmenoknihovny`. Například na spuštění demonstračního programu ze slajdů (kreslícího kolečka) potřebujete `cabal install gloss`. Názvy balíků tradičně najdete na Hoogle. Balík nezapomeňte připsat do závislostí projektu v souboru `.cabal`.
- Pokud si chcete interaktivně vyzkoušet nějakou část programu nebo funkci, použijte `cabal repl` -- to spustí GHCi se správným prostředím (tj. dostupnými balíky z projektu). Pomocí `:l` můžete načíst libovolný soubor z projektu; podobně funguje i `import`.

Do `Main.hs` teď můžete vyplnit řešení a otestovat funkčnost.

#### 3. Odevzdávání

Hotový projekt můžete nechat automaticky zabalit do odevzdatelné podoby jednoduše pomocí:
```
cabal sdist
```
To by mělo vyrobit archiv `u0-prijmeni-verze.tar.gz`, ten nahrajte do SISu do odpovídající kolonky ve studijních mezivýsledcích.

**Demonstrační balík je k dispozici [tady](u0-kratochvil-0.1.0.0.tar.gz) a rozbalený [tady](u0-kratochvil-0.1.0.0/).** (Demonstrační program ale řeší trochu jednodušší úkol než DÚ0.)

Kromě toho je vhodné si program předem nechat trochu zkontrolovat automatickými prostředky. Například je vhodné si zapnout všechna kompilační varování tím že do souboru `.cabal` do sekce s `executable` přidáte kompilační flagy:
```
executable u0-prijmeni

  ...
  ghc-options: -Wall
  ...
```

Projekt můžete s novými `ghc-options` nechat přestavět celý tím, že provedete `cabal clean` (což je víceméně tosamé jako `make clean`).

Formát balíku si můžete nechat zkontrolovat pomocí `cabal check`, ale varování jsou určena spíš do situace kdy balík chcete publikovat, většinu není pro potřeby DÚ potřeba řešit.

Zdrojový kód si můžete nechat "zkontrolovat" na běžné nedostatky pomocí linteru, nejběžnější je asi `hlint`. Ten nainstalujete pomocí `cabal install hlint`. Instalace chvíli trvá (odhadem max. cca 10 minut). Hlint se nainstaluje do adresáře, ve kterém jsou všechny spustitelné programy nainstalované cabalem, na Unixech to bývá `~/.cabal/bin` (ten si můžete pro větší pohodlí přidat do `PATH`). Zdrojový soubor pak můžete nechat zkontrolovat pomocí `~/.cabal/bin/hlint Main.hs` (případně jen `hlint Main.hs`, pokud tam máte správně nastavenou cestu).

Zdrojový kód je zároveň vhodné si nechat trochu automaticky zformátovat. "Standardní" program `hindent` je bohužel příliš "cutting-edge", experimentální a popkulturní, proto je pro nové uživatele většinou naprosto nepoužitelný. Místo toho můžete použít např. [ormolu](https://github.com/tweag/ormolu), který nainstalujete a spustíte podobně jako hlint: `cabal install ormolu`, následně `ormolu --mode inplace Module.hs`.
