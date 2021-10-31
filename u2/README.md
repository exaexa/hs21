# Úkol 2 -- bencode

[Bencode](https://en.wikipedia.org/wiki/Bencode) je formát výměny dat podobný
JSONu a YAMLu nebo CBORu, kterým jde poměrně jednoduše vyjadřovat stromové
struktury dat vyrobené ze slovníků, arrayí, stringů, apod. Používá se v
několika široce používaných síťových protokolech, z nichž nejznámější je asi
[bittorrent](https://en.wikipedia.org/wiki/BitTorrent).

Hlavní výhoda Bencode oproti JSONu a jiným lidským formátům je, že kódování dat
do Bencode je bijektivní funkce, tj. pro každá data existuje přesně jeden
validní zápis, bencodové stringu je tedy možné bez potíží přímo hashovat a
podepisovat, a ekvivalence dat jde zjistit prostě porovnáním stringů. Binární
data jde navíc uložit přímo do stringů bez "escapování", což zjednodušuje např.
přenos obrázků a v případě přenosu velkého množství dat vede ke značným
úsporám. Nevýhodou je, že výsledek nevypadá úplně tak neskutečně skvěle jako
hezky zformátovaný YAML, ale formát se pořád se dá celkem bez potíží editovat
ručně. (Rozhodně to bolí míň než editovat XML.)

Kódování je poměrně jednoduché:
- celé číslo se zakóduje zapsáním v desítkové soustavě mezi značkami `i` a `e`,
  tj. nula se zakóduje jako `i0e`, 123
- string se zakóduje pomocí zapsání délky desítkovým číslem následovaným
  dvojtečkou a obsahem stringu. Prázdný string se zakóduje jako `0:`, "ahoj" se
  zapíše jako `4:ahoj`, a tento string jde "nestovat" do dalšího stringu jako
  `6:4:ahoj` (obsahuje "4:ahoj")
- seznam hodnot začíná písmenem `l` a končí písmenem `e`, hodnoty se zapíšou za
  sebe bez oddělovače. Např. seznam obsahující seznam čísla a stringu a jedno
  číslo navíc se zapíše jako `lli10e4:ahojei20ee`.
- slovník se zapíše podobně mezi písmena `d` a `e`, liché položky jsou vždy
  klíče a sudé jsou odpovídající hodnoty -- například "5 metrů" by šlo zapsat
  jako `d7:hodnotai5e8:jednotka4:metre` (klíče musí být lexikálně seřazené)

Důležité drobnosti:
- délka stringů se měří v bajtech, pro účely úkolu ale můžete použít i
  unikódovou variantu kde se délka měří v množství unikódových symbolů (které v
  některých kódováních mohou zabírat víc než 1 bajt)
- aby byla reprezentace jednoznačná, uvozovací nuly u jakýchkoliv čísel jsou
  považovány za chybu (kromě případů `i0e` a `0:`)
- podobně, položky ve slovníku musí být unikátní a vždy uložené v
  lexikografickém pořadí
- floating-point hodnoty formát nepodporuje kvůli tradičním potížím s
  jednoznačnou reprezentací

## Část první -- prettyprinting

Protože číst data v dlooooooooooouhém řádku je otrava, vyrobte program
`bencode_pretty` který na standardním vstupu načte bencode, a na standardním
výstupu vypíše pěknou, odřádkovanou a odsazenou variantu. Přesný formát
odsazení si můžete zvolit libovolně.

Například bencode `d7:hodnotai5e8:jednotka4:metre` by program mohl vypsat jako:
```
d
 7:hodnota
  i5e
 8:jednotka
  4:metr
e
```

Formát výstupu si zvolte zcela libovolně, cokoliv přehledného je OK.

Cílem úkolu je vyzkoušet si práci s parsovacími kombinátory a prettyprintingem,
řešení tedy pokud možno strukturujte následovně:
- navrhněte si vhodnou datovou strukturu pro reprezentaci dat. Můžete
  předpokládat, že všechna data v úkolu 2 jsou ASCII, tj. problémy s velikostmi
  kódovaných stringů nemusíte řešit a všechny stringy můžete ukládat jako
  obyčejný `String` (v jiných specializovaných případech byste mohli použít i
  [`Text`](https://hackage.haskell.org/package/text) nebo
  [`ByteString`](https://hackage.haskell.org/package/bytestring))
- na parsování vstupu použijte
  [`megaparsec`](https://hackage.haskell.org/package/megaparsec) nebo
  [`attoparsec`](https://hackage.haskell.org/package/attoparsec) (nebo
  jakýkoliv jiný monádový parser, třeba ručně vyrobený)

*Bonus 1A*:
Formátování vstupu nedělejte ručně, ale použijte nějakou specializovanou
knihovnu na prettyprinting, ideálně
[`pretty`](https://hackage.haskell.org/package/pretty)

## Část druhá -- konverze

Knihovna [`aeson`](https://hackage.haskell.org/package/aeson) dovoluje pro
prakticky jakékoliv haskellové hodnoty poměrně příjemně vyrábět odpovídající
JSONové reprezentace, a hodnoty pak načítat a vypisovat jako JSON.

Pro zvolenou reprezentaci Bencode vyrobte instance typových tříd
[`FromJSON`](https://hackage.haskell.org/package/aeson-2.0.1.0/docs/Data-Aeson.html#t:FromJSON)
a
[`ToJSON`](https://hackage.haskell.org/package/aeson-2.0.1.0/docs/Data-Aeson.html#t:ToJSON)
(které zajišťují konverzi) a následně vyrobte 2 programy `json2bencode` a
`bencode2json` které převádí JSON (nebo Bencode) na standardním vstupu na
Bencode (nebo JSON) na standardním výstupu.

**Bonus 2A**:
JSON vypisujte "pěkně", tj. s odřádkováním a odsazením, ideálně pomocí knihovny
[`aeson-pretty`](https://hackage.haskell.org/package/aeson-pretty).

**Bonus 2B** (`*`):
To samé zprovozněte i pro YAML pomocí balíku
[`yaml`](https://hackage.haskell.org/package/yaml).

**Příklad**: Výše uvedený pětimetrový Bencode by měly programy správně
zkonvertovat na následující JSON a načíst ho zpět:
```
{
  "hodnota": 5,
  "jednotka": "metr"
}
```

## Část třetí -- schémata

Formát Bencode nevyžaduje prakticky žádné konvence o obsahu; to ale v
realistických aplikacích není úplně rozumné. Tradičním požadavkem na formát dat
je uniformita arrayí: všechny položky v každé arrayi by měly být "stejného
typu".

Vyrobte program `bencode_check` který Bencode na vstupu zkontroluje následovně:

- všechny prvky v arrayích jsou stejného "typu", tj. všechny buď čísla, nebo
  stringy, nebo arraye, nebo slovníky
- pokud jsou v arrayi vedle sebe 2 slovníky, průnik jejich obsahu je
  "unifikovaný", tj. odpovídající hodnoty na stejných klíčích jsou také
  stejného typu (ale množiny klíčů nemusí být stejné)
- pokud jsou v arrayi vedle sebe 2 arraye, jejich "obsah" je taky unifikovaný,
  tj. vsechny jejich prvky podléhají stejným podmínkám jako by byly v jedné
  arrayi

(Z pravidel můžete (mimo jiné) odvodit, že všechny "nestované arraye" musí mít
stejnou hloubku.)

Pokud kontrola projde, program UNIXově nezahlásí žádnou chybu a nic nevypíše; v
případě nalezených chyb vypiště suše `chyba`, volitelně vraťte nenulový exit
status (např. aby bylo chybu možné detekovat z shellu).

*Bonus 3A (error messages)*:
Program vypíše chybu i s "lokací" prvního nalezeného problému ve stromě, např.:
```
Problem found at:
  array index 5,
  dictionary key "values",
  array index 3,
  dictionary key "unit"
```

*Bonus 3B* (`**`):
Informaci o očekávaném obsahu stromu shrňte nějakým hmatatelným schématem --
inspirovat se můžete např. Haskellovými algebraickými typy, JSONovými schématy,
nebo Typescriptovými záznamovými typy. Schéma vyjádřete nějakou rozumnou
datovou strukturou, a odvozené schéma při úspěšné kontrole vypište (uživatel ho
pak může porovnat s očekáváním).

*Bonus 3C* (`***`):
Vyrobte program umožňující zkontrolovat kompatibilitu 2 schémat. Pravidlo: data
A vyhovují schématu B pokud je schéma A "podmnožina" schématu B -- v A např.
některé klíče slovníků můžou chybět, a schémata arrayí nemusí být úplná protože
některé arraye v A mohou být prázdné. Pro načítání schémat doporučuju zneužít
typové třídy `FromJSON` a `ToJSON` podobně jako to dělá balík `yaml`, nebo si
vyrobit vlastní třídy `FromBencode` a `ToBencode`.
