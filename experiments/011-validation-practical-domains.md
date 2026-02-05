# Validation: Practical Domains (8 Domains, 153 Words)

**Validator**: Polecat see
**Date**: 2026-02-04
**Bead**: limn-axlo
**Domains**: Arts, Buildings & Places, Clothing & Body, Mathematics, Nature, Technology, Tools & Objects, Weather & Climate

## Arts (19 words)

```
PASS aes | meaning confirmed — natural truncation of "aesthetic"
PASS art | meaning confirmed — exact word
PASS cin | meaning confirmed — natural truncation of "cinema"
PASS cra | meaning confirmed — natural truncation of "craft"
WARN crd | issue: vowel-dropped from "chord"; natural truncation "cho" would be more intuitive but likely taken
PASS dan | meaning confirmed — natural truncation of "dance"
PASS dra | meaning confirmed — natural truncation of "drama"
PASS hue | meaning confirmed — exact word
PASS pai | meaning confirmed — natural truncation of "paint"
PASS pat | meaning confirmed — natural truncation of "pattern"; well-established in vocab
PASS pho | meaning confirmed — natural truncation of "photo"
PASS poe | meaning confirmed — natural truncation of "poetry"
PASS rhy | meaning confirmed — natural truncation of "rhythm"
PASS scu | meaning confirmed — natural truncation of "sculpt"
PASS ske | meaning confirmed — natural truncation of "sketch"
WARN son | issue: "son" strongly suggests male child (English); source "song" is a secondary association
PASS sty | meaning confirmed — natural truncation of "style"
WARN tex | issue: "tex" could suggest "text" (covered by `stg`); source "texture" works but ambiguity exists
PASS tun | meaning confirmed — natural truncation of "tune"
```

## Buildings & Places (24 words)

```
WARN apt | issue: "apt" also means "suitable/appropriate" in English; apartment truncation works but adjective meaning is strong
WARN bat | issue: "bat" strongly suggests the animal or cricket bat; "bathroom" requires domain context
PASS bed | meaning confirmed — natural truncation of "bedroom"
PASS bnk | meaning confirmed — vowel-dropped form of "bank"; necessary since `ban` is taken
WARN cas | issue: could be "case", "casual"; "castle" works but source ambiguity
PASS cei | meaning confirmed — natural truncation of "ceiling"
PASS chu | meaning confirmed — natural truncation of "church"
PASS dor | meaning confirmed — natural truncation of "door"
PASS gar | meaning confirmed — natural truncation of "garden"
WARN hos | issue: could be "host", "hostile"; "hospital" is reasonable but ambiguous
PASS hou | meaning confirmed — natural truncation of "house"
PASS htl | meaning confirmed — vowel-dropped form of "hotel"; necessary since `hot` is taken
WARN kit | issue: "kit" also means equipment set; "kitchen" needs domain context
WARN lib | issue: could be "liberty", "liberal"; "library" is reasonable but ambiguous
PASS mkt | meaning confirmed — standard abbreviation of "market"
PASS mos | meaning confirmed — natural truncation of "mosque"
FAIL off | problem: "off" overwhelmingly means "not on/away" in English; "office" is not the natural reading — source truncation produces a word with a dominant conflicting meaning
WARN pal | issue: "pal" also means "friend/buddy"; "palace" needs domain context
WARN rof | issue: unusual truncation of "roof"; "roo" would be more natural but collides with "room" concept
PASS rst | meaning confirmed — consonant cluster from "restaurant"; necessary form
PASS sch | meaning confirmed — natural truncation of "school"
WARN std | issue: "STD" has strong stigmatized association (sexually transmitted disease); technically correct truncation of "stadium" but problematic form
WARN tow | issue: "tow" also means to pull/drag; "tower" needs domain context
WARN wal | issue: could be "walk", "wallet"; "wall" is reasonable but ambiguous
```

## Clothing & Body (15 words)

```
WARN arm | issue: "arm" also means "weapon/armament"; body part meaning works but dual meaning is strong
PASS coa | meaning confirmed — natural truncation of "coat"
PASS elb | meaning confirmed — natural truncation of "elbow"
WARN fin | issue: "final", "finish", fish "fin" compete; "finger" is one of several strong associations
WARN glo | issue: "glow", "global", "glory" compete; "glove" is a less common first association
PASS jac | meaning confirmed — natural truncation of "jacket"
PASS kne | meaning confirmed — natural truncation of "knee"
PASS leg | meaning confirmed — exact word
WARN nec | issue: "necessary", "nectar" compete; "neck" works but ambiguous
PASS nos | meaning confirmed — natural truncation of "nose"
WARN sca | issue: "scale", "scare", "scatter" compete; "scarf" is less dominant
WARN shi | issue: "ship", "shine", "shift" compete; "shirt" is one of several
WARN soc | issue: "social/society" is a stronger association than "sock"; needs domain context
WARN thu | issue: "thunder", "Thursday" compete; "thumb" is reasonable but not dominant
PASS toe | meaning confirmed — exact word
```

## Mathematics (14 words)

```
PASS add | meaning confirmed — exact word
FAIL ave | problem: duplicate concept — `avg` already exists in Abstract domain meaning "average"; having both `ave` (average, mean) and `avg` (average) is redundant
WARN cos | issue: "cost", "cosmic" compete; cosine is specialized — borderline for ~1000-word core vocab
WARN equ | issue: "equal", "equip", "equity" compete; "equation" works in Math context
WARN fac | issue: "face", "fact", "facility" compete; "factor" is one of many
WARN mat | issue: "match", "material", "matter" compete; "matrix" is specialized
WARN med | issue: "medicine/medical" is a stronger association; "median" is specialized
PASS quo | meaning confirmed — natural truncation of "quotient"
WARN rat | issue: "rat" (the animal) is the primary English association; "ratio" is secondary
PASS set | meaning confirmed — exact word; collision with "settled" already resolved via `setl`
PASS sqr | meaning confirmed — common abbreviation for "square"
WARN sub | issue: "submarine", "substitute", "subscribe" all compete; highly ambiguous source truncation
PASS sum | meaning confirmed — exact word
WARN var | issue: "variable" (programming) is a stronger association than "variance" (statistics)
```

## Nature (23 words)

```
PASS ari | meaning confirmed — natural truncation of "arid"
PASS blf | meaning confirmed — vowel-dropped from "bluff" (cliff face); necessary form
PASS cav | meaning confirmed — natural truncation of "cave"
WARN clo | issue: "close", "cloth", "clone" compete; also phonetically adjacent to `clu` (cloudy) in Weather domain — cloud/cloudy are nearly same concept split across domains
PASS fog | meaning confirmed — exact word
PASS gle | meaning confirmed — natural truncation of "glen"
PASS hil | meaning confirmed — natural truncation of "hill"
PASS isl | meaning confirmed — natural truncation of "island"
PASS jun | meaning confirmed — natural truncation of "jungle"
PASS lak | meaning confirmed — natural truncation of "lake"
PASS oce | meaning confirmed — natural truncation of "ocean"
PASS pek | meaning confirmed — natural truncation of "peak"
PASS rai | meaning confirmed — natural truncation of "rain"
PASS riv | meaning confirmed — natural truncation of "river"
PASS sno | meaning confirmed — natural truncation of "snow"
PASS srf | meaning confirmed — vowel-dropped from "surf"; necessary since "sur" collides with surface/sure
PASS sun | meaning confirmed — exact word
PASS swa | meaning confirmed — natural truncation of "swamp"
WARN tem | issue: "temperature", "temple", "temporary" compete strongly; "tempest" is less common English. Semantic overlap with `tor` (tornado) and `gal` (gale) in Weather
PASS tid | meaning confirmed — natural truncation of "tide"
WARN vol | issue: "volume", "voluntary", "volatile" compete; "volcano" works but is ambiguous
WARN win | issue: "win/victory" is the primary English meaning (covered by `vic`); "wind" is secondary reading. Also semantic overlap with `gal` (gale)
PASS wod | meaning confirmed — natural truncation of "woods"
```

## Technology (21 words)

```
PASS api | meaning confirmed — exact acronym
PASS app | meaning confirmed — standard abbreviation of "application"
WARN bak | issue: "bake" competes; "backup" works in Tech domain context
PASS bot | meaning confirmed — common abbreviation of "robot"
WARN cli | issue: "client" and "CLI" (command-line interface) both compete with "click"
WARN cod | issue: "cod" (the fish) competes; "code" works in Tech domain context
WARN com | issue: "communication", "community", "company", "combine" all strongly compete; extremely ambiguous source truncation
PASS cpu | meaning confirmed — exact acronym
WARN dat | issue: "date" competes; "data" works in Tech domain context
WARN enc | issue: "encounter", "encourage", "enclose" compete; "encrypt" is specialized
PASS err | meaning confirmed — natural truncation of "error"
WARN fil | issue: "film", "fill", "filter" compete; "file" is reasonable in Tech domain
FAIL key | problem: meaning says "input device, keyboard" but `key` naturally means a single key, not the keyboard device. Also `kei` (key, solution, opener) and `kee` (cipher key, unlock) exist — three phonetically near-identical words (`key`/`kei`/`kee`) create a discernibility cluster problem
WARN log | issue: "logarithm", "record/log", "wooden log" all compete; "login/authenticate" is one of several strong readings
WARN mac | issue: "Macintosh/Apple Mac" brand association competes; "machine" works but is ambiguous
PASS mem | meaning confirmed — natural truncation of "memory"
PASS net | meaning confirmed — natural truncation of "network"
WARN scr | issue: "scroll", "script", "screw" compete; "screen" is one of several
PASS srv | meaning confirmed — standard tech abbreviation for "server"
WARN syn | issue: "syntax", "syndrome", "synonym", "synthesis" compete; "sync" is one of many
PASS upd | meaning confirmed — natural truncation of "update"
```

## Tools & Objects (22 words)

```
PASS bag | meaning confirmed — exact word
WARN boo | issue: "boo!" (exclamation), "boost", "boot" compete; "book" is reasonable but not sole reading
WARN bow | issue: "bow" (archery), "bow" (to bend), "bow" (ship front) all compete with "bowl"; significant polysemy
PASS box | meaning confirmed — exact word
PASS cup | meaning confirmed — exact word
WARN ham | issue: "ham" (cured meat) competes strongly; "hammer" requires domain context
PASS jar | meaning confirmed — exact word
PASS kni | meaning confirmed — natural truncation of "knife"
WARN loc | issue: "local", "locate", "location" compete; "lock" works in Tools domain
PASS mir | meaning confirmed — natural truncation of "mirror"
PASS nai | meaning confirmed — natural truncation of "nail"
PASS nee | meaning confirmed — natural truncation of "needle"
PASS pan | meaning confirmed — exact word; cooking vessel is primary meaning
PASS pap | meaning confirmed — natural truncation of "paper"
PASS pen | meaning confirmed — exact word; writing instrument is primary
PASS pot | meaning confirmed — exact word
PASS rop | meaning confirmed — natural truncation of "rope"
WARN saw | issue: "saw" (past tense of see) is equally primary; source ambiguity
WARN sci | issue: "science" is the dominant association for "sci"; "scissors" is surprising — `phy`, `bio`, `geo` exist for specific sciences but generic "sci" mapped to scissors is counterintuitive
WARN spo | issue: "spot", "sport", "sponsor" compete; "spoon" works but ambiguous
WARN thr | issue: "three", "throw", "throne" compete; "thread" is one of several
PASS wre | meaning confirmed — natural truncation of "wrench"
```

## Weather & Climate (15 words)

```
PASS aur | meaning confirmed — natural truncation of "aurora"
WARN aut | issue: "author", "auto", "authority" compete; "autumn" is reasonable but not dominant
WARN clu | issue: "club", "clue", "cluster" compete; also near-duplicate with `clo` (cloud) in Nature — adjective/noun forms of same concept split across domains
PASS dew | meaning confirmed — exact word
WARN dro | issue: "drop", "drone", "drove" compete; "drought" works but ambiguous
WARN gal | issue: "gallon", "gallery" compete; "gale" is less common English. Semantic overlap with `win` (wind) in Nature
WARN lig | issue: "light" is the overwhelming association; "lightning" is a specific phenomenon of light
WARN mis | issue: "mis-" prefix (mistake, misunderstand) is the dominant association; "mist" is secondary. Near-semantic-duplicate of `fog` in Nature
WARN mon | issue: "money", "month", "monkey", "monitor" all strongly compete; "monsoon" is specialized
FAIL sea | problem: "sea" overwhelmingly means body of water (ocean), not "season" — `oce` already exists for ocean, creating confusing mapping where word `sea` does NOT mean sea
PASS sky | meaning confirmed — exact word
WARN sle | issue: "sleep", "sleeve" compete; "sleet" works but ambiguous
WARN spr | issue: "spray", "spread", "sprint" compete; "spring" itself is polysemous (season, water source, coil)
WARN tor | issue: "torment", "torch", "torsion" compete; "tornado" works. Semantic overlap with `tem` (tempest/storm) in Nature
WARN tro | issue: "trouble", "trophy", "trot" compete; "tropical" works but ambiguous
```

---

## val sum
> validation summary

```
pas: 85 | war: 64 | fal: 4
> pass: 85 | warn: 64 | fail: 4
```

### fal lis
> fail list

- **off**: "off" overwhelmingly means "not on/away"; unguessable as "office" — needs new form (e.g., `ofc`)
- **ave**: duplicate concept — `avg` already exists in Abstract domain meaning "average"; redundant entry
- **key**: meaning "keyboard" conflicts with standalone "key" meaning; `kei`/`kee`/`key` form a discernibility cluster of three near-identical words
- **sea**: "sea" overwhelmingly means ocean, not "season"; confusing since `oce` exists for ocean — needs new form (e.g., `ssn`)

### war lis (notable)
> warn list (most concerning)

**High-severity warnings (strong source ambiguity):**
- **son**: "son" (male child) dominates over "song"
- **com**: extreme ambiguity — computing, communication, community, company all compete
- **mon**: extreme ambiguity — money, month, monkey, monitor, monsoon
- **sci**: "science" dominates over "scissors" — counterintuitive mapping
- **mis**: "mis-" prefix dominates over "mist"; also near-duplicate of `fog`
- **win**: "win/victory" dominates over "wind" (and `vic` exists for victory)
- **bat**: "bat" (animal/implement) dominates over "bathroom"
- **lig**: "light" dominates over "lightning"
- **soc**: "social" dominates over "sock"
- **glo**: "glow/global" dominate over "glove"

**Structural warnings (cross-domain issues):**
- **clo/clu**: cloud (Nature) vs cloudy (Weather) — phonetically adjacent, semantically overlapping, split across domains
- **fog/mis**: fog (Nature) vs mist (Weather) — near-identical concepts in different domains
- **win/gal**: wind (Nature) vs gale (Weather) — same phenomenon at different intensities, different domains
- **tem/tor**: tempest (Nature) vs tornado (Weather) — overlapping severe weather concepts across domains
- **ave/avg**: average (Mathematics) vs average (Abstract) — same word, same meaning, two entries

**Moderate warnings (typical source ambiguity for 3-letter system):**
- apt, cas, hos, kit, lib, pal, std, tow, wal (Buildings)
- arm, fin, nec, sca, shi, thu (Clothing & Body)
- cos, equ, fac, mat, med, rat, sub, var (Mathematics)
- tem, vol (Nature)
- bak, cli, cod, dat, enc, fil, log, mac, scr, syn (Technology)
- boo, bow, ham, loc, saw, spo, thr (Tools & Objects)
- aut, dro, sle, spr, tor, tro (Weather)

### Domain Boundary Observations

The Nature and Weather & Climate domains have significant overlap. Several word pairs represent the same or very similar concepts split across domains:
- cloud/cloudy, fog/mist, wind/gale, rain(Nature)/storm concepts(Weather)
- Consider whether these should be consolidated into a single domain
