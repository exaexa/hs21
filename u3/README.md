
# Domácí úkol 3 -- Paintbrush for Teams

Hlavním úkolem ve třetím domácím úkolu je procvičit si konkurentní programování
a synchronizační primitiva, např.
[Chan](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-Chan.html)
a
[MVar](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-MVar.html).

Run-time Haskellu obsahuje vlastní implementaci pollování, díky tomu není
potřeba řešit tradiční problémy s blokujícím čtením a zápisem do socketu, a
konkurentní programování (tj. vícevláknové pomocí jednoho skutečného vlákna
operačního systému) jde implementovat jednoduše pomocí `forkIO`. "Odlehčená"
vlákna, která `forkIO` vyrábí, váš program prakticky nic nestojí a můžete s
nimi poměrně jednoduše programovat všelijaké síťové aplikace, ve kterých byste
v jiných jazycích buď museli explicitně spouštět víc (drahých) vláken
operačního systému, nebo pollování museli implementovat ručně.

## Předběžnosti

K dispozici máte [jednoduchou implementaci serveru malování](paintserver.hs).
Server umožnuje víc uživatelům přes internet editovat jedinou "kreslící plochu" s jedním obrázkem.
Velikost papíru je pro účely domácího úkolu vždycky přesně 32×32 pixelů.

Komunikační protokol serveru je jednoduchý: Klienti se připojují pomocí
obyčejného TCP spojení na dohodnutém portu, posílají serveru jednořádkové
textové příkazy, a přijímají jednořádkové textové updaty stavu papíru.

Klient serveru může poslat následující jednořádkové příkazy:
- `Transparent x y` serveru říká, aby změnil pixel v x-tém sloupci a y-tém
  řádku (počítáno od nuly) na průhledný
- podobně `Light x y` a `Dark x y` změní pixel na světlý a tmavý
- `Quit`, po kterém server ukončí spojení
- `Poll`, po kterém server explicitně pošle klientovi současný stav papíru.
  (Protože poll je poměrně drahá operace, je vhodné ji neprovádět moc často,
  ideálně jen jednou na začátku spojení.)

Server umí posílat následující tři typy zpráv:

- `Error` klientovi říká, že poslal nezpracovatelnou zprávu
- `Paper [...]` je odpověď na `Poll`. Ve které je zakódovaný celý stav papíru
  jako řetězec délky 1024 znaků (32×32), který obsahuje buď tečky (`.`)
  reprezentující průhledný pixel, kolečka (písmeno `o`) reprezentující tmavé
  pixely, a křížky (`x`) reprezentuj světlé pixely. Papír je zakódovaný
  postupně po řádcích, bez mezer.
  Zpráva `Paper` vám občas může přijít i bez toho, že byste poslali `Poll`
  (server tak mimo jiné může vyřešit příliš masivní updaty.
- `Transparent x y`, podobně `Light` a `Dark` oznamují změnu jednoho pixelu
  některým z uživatelů.

Komunikaci se serverem si můžete vyzkoušet ručně pomocí `telnet`u nebo netcatu
(`nc`), zhruba takhle:

```
 ~ $ telnet localhost 10042
Trying ::1...
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
< Poll
> Paper ................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................
< Dark 0 0
> Dark 0 0
< Light 0 2
> Light 0 2
< Poll
> Paper o...............................................................x...............................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................
< Dark 2 0
> Dark 2 0
< Light 1 2
> Light 1 2
< Light 2 2
> Light 2 2
< Poll
> Paper o.o.............................................................xxx.............................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................
< Quit
Connection closed by foreign host.
 ~ $
```

V příkladu klient nakreslil postmoderní verzi znuděného obličeje do horního
levého rohu hrací plochy a nechal si poslat celý výsledek pomocí `Poll`u.
(Řádky s komunikací jsou pro přehlednost prefixované `<` pro zprávy od klienta
a `>` pro zprávy od serveru.)

Můžete si vyzkoušet i [pomocný testovací program](sendpixel.hs), který resolvuje
adresu serveru, připojí se k ní, pošle dva příkazy (`Dark 0 0` a `Poll`) a na výstup
vypíše výsledek. (Program se odpojuje celkem hrubě zavřením
socketu, ne příkazem `Quit`. Server proto bude pravděpodobně vypisovat
odpovídající stížnosti o neočekávaném odpojení klienta.)

## Úkol

Vezměte svou implementaci kreslení z úkolu 1 a přidejte do ní podporu pro
editování obrázku na serveru.

Doporučený postup implementace je následující:
- Implementujte přijímací a vysílací část jako samostatné "lehké" thready
  (spuštěné pomocí `forkIO`). Ke připojení na zbytek programu použijte vhodné
  komunikační prostředky (reference, kanály, zamykací proměnné), které
  "zobrazovací" části programu dovolí fungovat nezávisle (asynchronně) na
  komunikaci se serverem. Konkrétně:
  - Pokud jste použili Gloss, jakákoliv blokující operace spuštěná v hlavním
    threadu způsobí zamrznutí celého kreslícího cyklu (což asi nechcete).
  - Pokud jste použili Brick, musíte navíc Brickovému frontendu občas říct, že
    se má probudit a překreslit (narozdíl od Glossu to nedělá pořád).
    Konkrétně, Brick občas jen čeká na vstup a nevšíma si ničeho, co by se
    mohlo dít na síti nebo jiných komunikačních kanálech. To se dá vyřešit
    vyrobením vlastního typu eventu, který Bricku můžete poslat např. v
    případě, že ze serveru přijde nějaká nová informace; ideálně podle [tohohle
    tutorialu](https://samtay.github.io/posts/introduction-to-brick) --
    hledejte sekci "Variable speed".
- Kreslicí plochu sami nijak nemodifikujte (ani v reakci na uživatelský vstup) --
  prostě jen zobrazujte verzi, kterou vám poslal server. Jediná věc,
  kterou potřebujete navíc, je "kurzor". Pozici kurzoru serveru nijak
  neoznamujete (obecně je naprosto nezávislá na čemkoliv co si server zrovna
  myslí).
- V případě nějaké uživatelské akce pošlete odpovídající zprávu (nebo zprávy, v
  případě kreslení většího tvaru) vláknu, které zajišťuje komunikaci se
  serverem. Zprávu nechte dojít na server, časem vám přijde odpověď která
  obsahuje nový stav papíru (s potenciálními dalšími updaty od ostatních
  klientů a správně vyřešenou serializací úprav). Váš program by na odpověď
  neměl čekat, a uživateli by měl dovolit provádět další akce -- představte si,
  že server laguje, a máte nakreslit
  [tohle](https://www.redbubble.com/es/i/lamina-fotografica/Mona-Pixel-Pixelated-Mona-Lisa-de-Galiderath/16003491.6Q0TX).
- V programu zachovejte aspoň jednu funkcionalitu pro kreslení složitějších
  tvarů nebo efektů např. obdélníků, kruhů nebo inverze. Serveru tento tvar
  pošlete "rozdrobený" na jednotlivé pixely.
- Různá možná selhání sítě (např. ztrátu spojení) nebo extrémní latenci nijak
  ošetřovat nemusíte.


### Detaily

- Nejspíš budete chtít použít nějakou kombinaci síťování, haskellových
  "lehkých" vláken a konkurentních programovacích primitiv. Pomůcky pro
  komunikaci se sítí najdete v `Network.Socket`, vlákna v `Control.Concurrent`
  a komunikační prostředky v `Control.Concurrent.MVar`, případně v
  `Control.Concurrent.Chan` nebo v nějaké alternativě z balíku `stm`. Je vhodné
  se inspirovat kódem serveru.
- Adresu a port serveru (defaultní je `10042`) přečtěte z příkazového řádku.
  Program by měl podporovat na pozici nezávislé argumenty `-a <ip_adresa>` a
  `-p <port>`, a stěžovat si (nebo se odmítnout spustit) při nedostatečně
  specifikovaných parametrech, nebo výskytu neinterpretovatelných parametrů.

Navíc si vyberte jeden ze dvou následujících volitelných úkolů (můžete
samozřejmě zvládnout i oba dva a následně se těšit z vlastní dokonalosti).

## Volitelná část A

Na naparsování parametrů z příkazové řádky použijte knihovnu
`optparse-applicative`. Pro oba parametry implementujte i "dlouhou" verzi
(např. `--address`), a v případě chybně zadaných parametrů (nebo když si
uživatel řekne o `--help`) vypište nápovědu.

## Volitelná část B

Modifikace lokálního stavu naprogramujte velmi hezky a supermoderně, pomocí monády
`State` a optiky (tj. `Control.Lens` nebo `Lens.Micro.*`). Konkrétně zkuste
příchozí eventy zpracovávat např. takhle:

```hs
handleCursorRight = runState $ do
  cursorX += 1
  cursorX . filtered (>31) .= 31
```

(Kód zvětší hodnotu v cursorX o 1, a výsledek zarovná na 19. Podobně jde napsat
např. `cursorX . filtered (<31) += 1`.)

## Extrémní bonus

Do serveru implementujte podporu zpráv `Nick jmenoUmelce` a `CursorPos x y`,
kterými uživatel oznamuje jak se jmenuje a kde právě edituje, a `CursorAt
jmenoUmelce x y` a `Lost jmenoUmelce`, kterymi server oznamuje, kde který
uživatel právě edituje, případně odpojení uživatele (zmizení jeho kurzoru).

Kurzory všech připojených uživatelů renderujte v GUI.

## Hinty

- Vaše aplikace _jen zobrazuje stav serveru_; logiku mergování uživatelových
  akcí s asynchronními updaty ze serveru implementovat nemusíte (a nechcete), a
  serveru můžete úplně věřit.
- Pokud budete mít problém s pollováním, přidejte jednoduchou funkcionalitu pro
  refresh (třeba pomocí `F5`), která serveru pošle `Poll`.
- Pokud je tohle první síťová aplikace kterou programujete, API (odvozené od
  Berkeleyovských socketů) vás může poněkud šokovat. Většinu síťového kódu
  naštěstí stačí zkopírovat z dodaných příkladů. Hlavní rozdíl oproti serveru
  je ten, že nebudete používat "naslouchací" kombo `bind`-`listen`-`accept`,
  ale použijete jen jedinou funkci
  [`connect`](https://hackage.haskell.org/package/network-3.1.1.0/docs/Network-Socket.html#v:connect)
  která zařídí vytvoření TCP spojení s adresou serveru.
- IP adresu z příkazového řádku **neparsujte ručně**. (Pokud se ji rozhodnete
  parsovat ručně, dodělejte i podporu IPv6!). Místo toho chcete použít
  [`getAddrInfo`](https://hackage.haskell.org/package/network-3.1.1.0/docs/Network-Socket.html#v:getAddrInfo).
- V některých situacích chcete použít stavový výpočet a IO najednou (např. v
  event-handlerech některé akce mění pozice kurzoru, a jiné akce odesílají
  zprávu serveru). V takovém případě se dost hodí transformery.

## Odevzdávání

Program sbalíkujte tradičně cabalem do balíku `vasePrijmeni3`, a archiv s
balíkem nahrajte do odpovídajícího políčka v SISu.
