# Parallel Corpora: English-Limn (v1)

> **Purpose:** Training data for v5 model — 200 English-Limn aligned pairs across 4 domains.
> **Format:** Each pair has an English sentence and its Limn translation.
> **Quality bar:** All Limn passes `vocab.sh validate`.
> **Bead:** hq-cv-4b4nw

---

## Domain 1: Scientific Text (50 pairs)

### Biology

**S1.01**
EN: The cell divides into two daughter cells.
LM: `cel cut → cel two new`

**S1.02**
EN: DNA stores the genetic code of every organism.
LM: `dna = cod gen eac org`

**S1.03**
EN: Oxygen enters the blood through the lungs.
LM: `oxy inp blo`

**S1.04**
EN: Photosynthesis converts light into chemical energy.
LM: `lux tra → nrg mol`

**S1.05**
EN: Neurons transmit signals through the nervous system.
LM: `ner sen sig thr ner net`

**S1.06**
EN: Evolution occurs through natural selection over time.
LM: `evo = sel str:tau lon`

**S1.07**
EN: The heart pumps blood throughout the body.
LM: `hrt pus blo thr bod`

**S1.08**
EN: Proteins fold into specific three-dimensional shapes.
LM: `thr bnd → pat sph`

**S1.09**
EN: Mitochondria produce energy for the cell.
LM: `mol cel → nrg cel`

**S1.10**
EN: Bacteria can reproduce by splitting in half.
LM: `org sma dup:cut`

**S1.11**
EN: The immune system identifies and destroys foreign cells.
LM: `bod obs cel oth → att → cel oth dec`

**S1.12**
EN: Genes are passed from parent to offspring.
LM: `gen par → gen new lif`

**S1.13**
EN: Roots absorb water and nutrients from the soil.
LM: `roo tak aqu ter`

**S1.14**
EN: Carbon dioxide is exhaled from the lungs.
LM: `car oxy out bod`

**S1.15**
EN: Enzymes accelerate chemical reactions.
LM: `cat acc mol tra`

**S1.16**
EN: The brain processes sensory information continuously.
LM: `bra prf per dat lon`

**S1.17**
EN: Species adapt to their environments over generations.
LM: `spe cha:gen lon | evo`

**S1.18**
EN: Blood carries oxygen to every cell in the body.
LM: `blo oxy → eac cel bod`

**S1.19**
EN: Viruses replicate by hijacking host cells.
LM: `org sma inv cel → dup:cel`

**S1.20**
EN: Ecosystems depend on the balance between predators and prey.
LM: `org all dep eql bet att ani and ani`

### Physics

**S1.21**
EN: Energy cannot be created or destroyed, only transformed.
LM: `nrg non new | nrg non end | nrg tra onl`

**S1.22**
EN: Light travels at the maximum possible speed.
LM: `lux mov vel max`

**S1.23**
EN: Gravity pulls objects toward the center of mass.
LM: `pul all → nuc den`

**S1.24**
EN: Heat flows from hot regions to cold regions.
LM: `hot flo → col | nev col → hot`

**S1.25**
EN: Sound is a wave that propagates through matter.
LM: `wav mov thr sol or liq or gas`

**S1.26**
EN: Electrons orbit the nucleus of an atom.
LM: `ele orb nuc ato`

**S1.27**
EN: Acceleration is the rate of change of velocity.
LM: `acc = vel cha rat`

**S1.28**
EN: Friction converts kinetic energy into heat.
LM: `mov nrg tra → hot:nrg`

**S1.29**
EN: An electric current creates a magnetic field.
LM: `ele flo → mag ris`

**S1.30**
EN: Waves can constructively or destructively interfere.
LM: `wav joi wav → wav big or wav zer`

**S1.31**
EN: Pressure increases with depth in a fluid.
LM: `pre ris:dep ins liq`

**S1.32**
EN: A prism separates white light into a spectrum.
LM: `lux clr thr sol → spc`

**S1.33**
EN: Nuclear reactions release enormous amounts of energy.
LM: `nuc tra → nrg big^0.9 rel`

**S1.34**
EN: The universe is expanding in all directions.
LM: `all exp all for`

**S1.35**
EN: Superconductors have zero electrical resistance.
LM: `ele flo:res zer`

**S1.36**
EN: Entropy always increases in a closed system.
LM: `dis ris alw:sys clo`

**S1.37**
EN: A pendulum oscillates with a constant period.
LM: `wav cyc:dur sta`

**S1.38**
EN: Magnetism can attract or repel depending on polarity.
LM: `mag = pul ± pus:poi`

**S1.39**
EN: Photons are particles of light with zero mass.
LM: `lux ato:den zer`

**S1.40**
EN: Radioactive elements decay at predictable rates.
LM: `ato dec:rat def`

**S1.41**
EN: Ice melts when heated above the freezing point.
LM: `sol aqu hot → liq aqu:fre abo`

**S1.42**
EN: A vacuum contains no matter at all.
LM: `spa emp:mol zer`

**S1.43**
EN: Electromagnetic waves do not need a medium to travel.
LM: `ele mag wav mov:mol non req`

**S1.44**
EN: Temperature measures the average kinetic energy of particles.
LM: `hot = nrg mov ato eql`

**S1.45**
EN: Refraction occurs when light passes through different media.
LM: `lux bnd:thr mol dif`

**S1.46**
EN: Black holes have gravitational pull so strong that light cannot escape.
LM: `pul str^1.0 | lux non rel`

**S1.47**
EN: Convection transfers heat through fluid motion.
LM: `hot tra thr liq mov`

**S1.48**
EN: An atom consists of protons, neutrons, and electrons.
LM: `ato = nuc and ele`

**S1.49**
EN: Kinetic energy depends on mass and velocity.
LM: `nrg mov dep den and vel`

**S1.50**
EN: Quantum particles exist in superposition until observed.
LM: `ato sma rea her ± yon | obs → poi one`

---

## Domain 2: Emotional Expression (50 pairs)

**E2.01**
EN: I feel overwhelmed by sadness.
LM: `sad pre^0.9 sel`

**E2.02**
EN: Joy comes unexpectedly, like light through clouds.
LM: `joy ris non def | lux thr clo`

**E2.03**
EN: Trust takes years to build and seconds to break.
LM: `tru gro lon | tru brk bre`

**E2.04**
EN: I am afraid of losing the people I love.
LM: `fea los sel lov`

**E2.05**
EN: Anger masks deeper pain.
LM: `ang pro sad dep`

**E2.06**
EN: Loneliness feels heaviest in a crowd.
LM: `sel far:oth nea = sad dep`

**E2.07**
EN: Hope persists even when everything else fails.
LM: `hop sta | all oth fal`

**E2.08**
EN: Guilt is about what you did; shame is about who you are.
LM: `gui = prf fal | sha = sel fal`

**E2.09**
EN: I cannot tell if this feeling is love or fear.
LM: `sel per lov ± fea`

**E2.10**
EN: Healing is not linear — it spirals.
LM: `hel non lin | hel spi`

**E2.11**
EN: I feel both grateful and grieving at the same time.
LM: `grt ± sad | now sam`

**E2.12**
EN: Anxiety tells me the future is dangerous, but it lies.
LM: `anx = fut fea | non rea`

**E2.13**
EN: Vulnerability is the price of genuine connection.
LM: `sel ope = joi rea req`

**E2.14**
EN: I miss who I used to be.
LM: `des sel pas | sel now dif`

**E2.15**
EN: Forgiveness is not for them — it's for me.
LM: `rel ang = sel hel | non oth hel`

**E2.16**
EN: Envy reveals what we secretly desire.
LM: `see oth → des sec sho`

**E2.17**
EN: Calm doesn't mean the storm is over; it means I'm standing in it.
LM: `cal ≠ tem end | cal = sel sta:tem`

**E2.18**
EN: My heart knows things my mind resists.
LM: `hrt kno | bra dis | hrt and bra cnt`

**E2.19**
EN: The hardest conversations are with myself.
LM: `sel dis sel = har^0.9`

**E2.20**
EN: I carry my parents' fears without choosing to.
LM: `par fea → sel fea | non sel cha`

**E2.21**
EN: Nostalgia edits the past with warm light.
LM: `mem:pas tra:lux hot^0.3`

**E2.22**
EN: Depression is not sadness — it's the absence of feeling.
LM: `non sad | non fea | non joy | emp`

**E2.23**
EN: I need to be seen, not fixed.
LM: `req see | non req hel`

**E2.24**
EN: Love grows stronger through difficulty, not despite it.
LM: `lov gro:har | non lov gro\har`

**E2.25**
EN: Peace is not the absence of conflict — it's the ability to sit with it.
LM: `cal ≠ cnt non | cal = sta:cnt`

**E2.26**
EN: I grieve for the future I imagined.
LM: `sad:fut des | fut non rea`

**E2.27**
EN: Empathy means carrying someone else's weight briefly.
LM: `sel per oth pre bre`

**E2.28**
EN: I cannot stop thinking about what I should have said.
LM: `thi cyc:pas | des cha | non pos`

**E2.29**
EN: Excitement and anxiety feel almost the same in the body.
LM: `exc ± anx | bod per sam`

**E2.30**
EN: I am more than my worst moments.
LM: `sel mor sel fal`

**E2.31**
EN: Tenderness requires immense strength.
LM: `sof req str dep`

**E2.32**
EN: I feel small in the face of so much loss.
LM: `sel sma:los big`

**E2.33**
EN: Desire without boundaries becomes obsession.
LM: `des non lmt → des^1.0 | sel los`

**E2.34**
EN: The silence between us says more than words.
LM: `non sen bet | mea mor`

**E2.35**
EN: I am learning to let go of who I thought I should be.
LM: `gro rel sel des | sel rea acc`

**E2.36**
EN: Shame thrives in darkness and dies in connection.
LM: `sha gro:sec | sha dec:joi`

**E2.37**
EN: I am tired in a way that sleep cannot fix.
LM: `wea dep | res non hel`

**E2.38**
EN: Courage is not the absence of fear but action despite it.
LM: `cur ≠ fea non | cur = prf:fea`

**E2.39**
EN: My anxiety is trying to protect me from something that already happened.
LM: `anx pro sel:pas | pas end | anx non kno`

**E2.40**
EN: I do not need to earn love.
LM: `lov non req prf | lov giv`

**E2.41**
EN: The grief never fully leaves; you just learn to carry it.
LM: `sad nev end ful | sel gro:sad`

**E2.42**
EN: I feel alive when I create.
LM: `cre → lif per`

**E2.43**
EN: Sometimes the bravest thing is asking for help.
LM: `cur^0.9 = que hel`

**E2.44**
EN: Jealousy is fear wearing anger's mask.
LM: `fea:ang pro | fea ins`

**E2.45**
EN: I hold both darkness and light within me.
LM: `sel wit nox and lux`

**E2.46**
EN: Growth often feels like breaking.
LM: `gro per brk | non brk rea`

**E2.47**
EN: I deserve rest without having to justify it.
LM: `res non req why`

**E2.48**
EN: Love is choosing someone again and again.
LM: `lov = sel cha oth cyc | non one`

**E2.49**
EN: I am simultaneously falling apart and coming together.
LM: `sel brk ± sel joi | now`

**E2.50**
EN: Acceptance does not mean approval.
LM: `acc ≠ pls | acc = see rea`

---

## Domain 3: Technical Documentation (50 pairs)

**T3.01**
EN: The client sends a request to the server.
LM: `cli sen req → net → svr`

**T3.02**
EN: The server returns a response with status code 200.
LM: `svr sen ans | cod = acc`

**T3.03**
EN: Authentication failed — invalid token.
LM: `log err | tok non tru`

**T3.04**
EN: The database connection timed out.
LM: `dat joi err | dur max`

**T3.05**
EN: Retry the request after a brief delay.
LM: `wai bre → req bac`

**T3.06**
EN: The API accepts JSON input and returns JSON output.
LM: `api inp dat → api out dat`

**T3.07**
EN: Rate limit exceeded — wait before sending more requests.
LM: `req lmt abo | wai → req`

**T3.08**
EN: Data must be validated before processing.
LM: `dat tes req bre prf`

**T3.09**
EN: The cache stores frequently accessed data in memory.
LM: `mem dat req fas`

**T3.10**
EN: Encryption protects data in transit and at rest.
LM: `cod pro dat:mov and dat:res`

**T3.11**
EN: The load balancer distributes traffic across multiple servers.
LM: `req div → svr div | eql`

**T3.12**
EN: A message queue decouples producers from consumers.
LM: `sen dat → que → tak dat | non syn`

**T3.13**
EN: The WebSocket connection remains open for bidirectional communication.
LM: `joi ope sta | dat flo cli ↔ svr`

**T3.14**
EN: Deploy the latest build to the production environment.
LM: `cod new rel → sys`

**T3.15**
EN: All errors are logged with timestamps.
LM: `all err → log:tau`

**T3.16**
EN: The health check endpoint returns system status.
LM: `sys hea que → ans sta`

**T3.17**
EN: Input must be sanitized to prevent injection attacks.
LM: `inp tes | cod rel → dat saf`

**T3.18**
EN: Sessions expire after thirty minutes of inactivity.
LM: `tok dur lmt | non prf → tok dec`

**T3.19**
EN: The circuit breaker prevents cascading failures.
LM: `err cyc → req sto | pro sys`

**T3.20**
EN: Each microservice has a single responsibility.
LM: `eac sys fra = pur one uni`

**T3.21**
EN: The API version must be specified in the request header.
LM: `api new def req`

**T3.22**
EN: Idempotent operations produce the same result when repeated.
LM: `prf one eff = prf two eff | sam`

**T3.23**
EN: The database transaction rolled back due to an error.
LM: `err → all prf bac | dat cha non`

**T3.24**
EN: DNS resolves domain names to IP addresses.
LM: `url tra → net poi`

**T3.25**
EN: The server returned a 404 error — resource not found.
LM: `svr ans err | ref non rea`

**T3.26**
EN: Use HTTPS to encrypt all network traffic.
LM: `net dat cod req | sec`

**T3.27**
EN: The function returns null if the input is invalid.
LM: `inp non tru → out emp`

**T3.28**
EN: Containers isolate applications from the host system.
LM: `cod ins sys | cod cut sys`

**T3.29**
EN: The webhook fires when the event occurs.
LM: `eff → sen dat aut`

**T3.30**
EN: Horizontal scaling adds more server instances.
LM: `sys pre ris → svr mor`

**T3.31**
EN: The primary database replicates to the secondary.
LM: `dat one dup → dat two`

**T3.32**
EN: API keys must be stored as secrets, never in code.
LM: `tok sec | non tok wit cod`

**T3.33**
EN: Pagination limits the number of results per request.
LM: `ans fra lmt eac req`

**T3.34**
EN: The event loop processes callbacks asynchronously.
LM: `cyc prf eac dat:non syn`

**T3.35**
EN: A deadlock occurs when two processes wait for each other.
LM: `prf two wai mut → stk`

**T3.36**
EN: The garbage collector frees unused memory automatically.
LM: `mem non req → mem rel aut`

**T3.37**
EN: Configuration should be loaded from environment variables.
LM: `def dat inp:sys`

**T3.38**
EN: The middleware intercepts requests before they reach the handler.
LM: `req → tes bre → prf`

**T3.39**
EN: Logs should include request ID for tracing.
LM: `log req ref uni`

**T3.40**
EN: The CDN caches static assets closer to users.
LM: `mem dat nea cli → fas`

**T3.41**
EN: A mutex ensures only one thread accesses the resource at a time.
LM: `loc → one thr onl | oth wai`

**T3.42**
EN: The test suite must pass before merging to main.
LM: `tes all acc req bre cod joi`

**T3.43**
EN: Compression reduces the size of data transferred over the network.
LM: `con dat → dat sma → net fas`

**T3.44**
EN: The server validates the request signature to verify authenticity.
LM: `svr tes req sig → tru or err`

**T3.45**
EN: Feature flags allow enabling features without deploying new code.
LM: `if def → prf new | non cod new req`

**T3.46**
EN: A reverse proxy sits between clients and backend servers.
LM: `cli → net bet → svr`

**T3.47**
EN: Batch processing handles large volumes of data efficiently.
LM: `dat big prf:fra → fas`

**T3.48**
EN: The OAuth token grants limited access to protected resources.
LM: `tok giv acc lmt | dat pro`

**T3.49**
EN: Blue-green deployments minimize downtime during releases.
LM: `sys two | sys old and sys new | cha fas`

**T3.50**
EN: The monitoring dashboard displays real-time system metrics.
LM: `sys obs dat sho:now`

---

## Domain 4: Dialogue (50 pairs)

**D4.01**
EN: "What time is it?" "Almost noon."
LM: `que tau | ans tau hig`

**D4.02**
EN: "Can you help me?" "Of course."
LM: `que hel | ans acc`

**D4.03**
EN: "I don't understand." "Let me explain again."
LM: `non und | sho bac`

**D4.04**
EN: "Where are you going?" "Home."
LM: `que whe for | ans her`

**D4.05**
EN: "Is it safe?" "I think so."
LM: `que saf | ans pos`

**D4.06**
EN: "How do you feel?" "Better than yesterday."
LM: `que how sel | ans hea mor:pas`

**D4.07**
EN: "Why did you leave?" "I had to."
LM: `que why rel | ans mus`

**D4.08**
EN: "Do you remember?" "Every detail."
LM: `que mem | ans all`

**D4.09**
EN: "Are we lost?" "Not yet."
LM: `que los | ans non now`

**D4.10**
EN: "Tell me the truth." "You already know."
LM: `req tru | ans sel kno`

**D4.11**
EN: "I'm sorry." "I know."
LM: `sha sho | ans kno`

**D4.12**
EN: "Will you wait for me?" "Always."
LM: `que wai sel | ans alw`

**D4.13**
EN: "What happened?" "Everything changed."
LM: `que wha eff | ans all cha`

**D4.14**
EN: "Is anyone there?" "Just me."
LM: `que any her | ans sel onl`

**D4.15**
EN: "Can I trust you?" "That's your choice."
LM: `que tru pos | ans sel cha`

**D4.16**
EN: "How long will it take?" "Longer than you think."
LM: `que dur | ans dur mor:thi`

**D4.17**
EN: "Are you sure?" "No. But I'm going anyway."
LM: `que crt | ans non | for`

**D4.18**
EN: "What do you want?" "To understand."
LM: `que des | ans und`

**D4.19**
EN: "Don't go." "I have to."
LM: `req non rel | ans mus`

**D4.20**
EN: "Is this the end?" "Or the beginning."
LM: `que end | ans or beg`

**D4.21**
EN: "Who are you?" "I'm still figuring that out."
LM: `que who | ans sel cur sel`

**D4.22**
EN: "Does it hurt?" "Less now."
LM: `que sad | ans sad^0.3 now`

**D4.23**
EN: "What's the plan?" "Survive."
LM: `que how for | ans lif`

**D4.24**
EN: "I missed you." "I was always here."
LM: `des sel:pas | ans alw her`

**D4.25**
EN: "How did you know?" "I didn't. I guessed."
LM: `que how kno | ans non kno | int`

**D4.26**
EN: "Teach me." "Watch carefully."
LM: `req gro | ans obs att`

**D4.27**
EN: "I can't do this alone." "You're not alone."
LM: `non abl sel onl | ans non sel onl`

**D4.28**
EN: "Forgive me." "Not yet. But maybe someday."
LM: `req rel ang | ans non now | pos fut`

**D4.29**
EN: "What matters most?" "What you do next."
LM: `que vit | ans prf nxt`

**D4.30**
EN: "Are we winning?" "Define winning."
LM: `que win | ans def win`

**D4.31**
EN: "Say something." "There are no words."
LM: `req sen | ans emp`

**D4.32**
EN: "I'm scared." "So am I. That's okay."
LM: `fea sel | ans fea sam | acc`

**D4.33**
EN: "How far?" "Almost there."
LM: `que far | ans nea`

**D4.34**
EN: "Promise me." "I promise."
LM: `req tru | ans tru`

**D4.35**
EN: "Was it worth it?" "Ask me tomorrow."
LM: `que val | ans que fut`

**D4.36**
EN: "Look at this." "I see it."
LM: `req see | ans see`

**D4.37**
EN: "I didn't mean to hurt you." "But you did."
LM: `non pur hrt oth | ans hrt rea`

**D4.38**
EN: "What happens now?" "We begin again."
LM: `que nxt | ans beg bac`

**D4.39**
EN: "Everyone left." "I'm still here."
LM: `all rel | ans sel her sta`

**D4.40**
EN: "Is this real?" "As real as anything."
LM: `que rea | ans rea sam all`

**D4.41**
EN: "I changed my mind." "Good."
LM: `sel cha thi | ans acc`

**D4.42**
EN: "How do I start?" "One step."
LM: `que how beg | ans one`

**D4.43**
EN: "I wish things were different." "They can be."
LM: `des dif | ans pos`

**D4.44**
EN: "Don't lie to me." "I never did."
LM: `req non fak | ans nev`

**D4.45**
EN: "It's too late." "It's never too late."
LM: `dur end | ans nev end`

**D4.46**
EN: "I need time." "Take all you need."
LM: `req tau | ans tak all`

**D4.47**
EN: "You were right." "That doesn't matter now."
LM: `oth tru | ans non mea now`

**D4.48**
EN: "What did you learn?" "That I was wrong."
LM: `que gro | ans sel err`

**D4.49**
EN: "Stay." "I will."
LM: `req sta | ans acc`

**D4.50**
EN: "The end." "No. A pause."
LM: `end | ans non | res bre`

---

*fra sig = two hun | dom fou | ful*
*> fragments summed = two hundred | domains four | complete*
