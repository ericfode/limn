# Limn Language Reference

Limn is a constraint-based language where meaning emerges from structure. Each 3-letter word defines a region in semantic space.

## Operators

| Op | Name | Function | Example |
|----|------|----------|---------|
| `@` | Projection | Extract B-component of A | `lov@fea` = fear in love |
| `*` | Interference | Emergent meaning from A+B | `sol*liq` = gel |
| `^` | Gradient | Intensity 0.0-1.0 | `big^0.7` = fairly big |
| `\` | Subtraction | A without B removed | `lov\fea` = secure love |
| `±` | Superposition | Both A and B unresolved | `yes±no` = undecided |
| `:` | Conditional | A given B context | `lov:fth` = love given trust |
| `|` | Scope | Boundary between clauses | `fnd err | fix cod` |
| `→` | Then | Sequential flow | `red fil → und pur` |

Precedence: `^` > `@` > `*` > `\` > `:` > `±`

## Grammar

- Words are 3-letter semantic atoms (CVC pattern)
- Sentences: constraint sequences separated by `|`
- Flow: `A → B` means "A then B"
- Context: `A @ B` means "A in context of B"
- Negation: `nu` = not

## Full Vocabulary (1040 words)

### Abstract
age=agent, all=all/every, alp=alpha/first, alt=alternatively, amb=ambiguous, anc=ancient, and=conjunction, any=any, as=as/like, ate=make/cause, att=attention, avg=average, bad=bad, bea=beautiful, bec=because, bou=boundary, bri=bright/hope, cau=cause, cho=choice, cla=conflicting, cnt=contradicts, col=distant/calm, cot=contingent, ctx=context, cut=decision, dar=dark, dee=deep, def=definition, det=single-meaning, dif=different, div=diverse, doo=door/opportunity, due=due-to, eac=each, eff=effect, els=else, emb=embedding, emp=empty, ena=enable, eps=epsilon, ergo=therefore, eta=efficiency, eve=event, exa=exact, ext=external, fal=failure, few=few, ful=full, goo=good, gra=gradient, gvn=given-that, hig=high, if=condition, in=in, is=is/exists, jux=juxtaposition, lim=boundary, lot=many, low=low, max=maximum, mid=middle, min=minimum, mod=mode, mor=more, mos=most, muc=much, neg=negative, no=no, nor=normal, not=not, nu=not, of=of, on=on, one=one, or=or, out=out, ove=over, per=per, pos=positive, prt=partial, qua=quality, rea=real, sam=same, sca=scale, sig=sigma/sum, sim=simple, sma=small, som=some, tot=total, typ=type, und=under, uni=universal, unk=unknown, yes=yes, zer=zero

### Actions & Verbs
avo=avoid, ben=bend, bld=build, bng=bring, brk=break, clm=climb, ctc=catch, dfc=defend, drk=drink, drp=drop, fht=fight, fix=fix, fnd=find, hol=hold, jum=jump, kik=kick, lft=lift, mak=make, ope=open, pry=pray, psh=push, puk=put, rcv=receive, rel=release, run=run, seh=see, sit=sit, slp=sleep, spk=speak, src=search, stp=stop, sts=start, suc=succeed, swm=swim, tch=touch, thw=throw, tur=turn, twi=twist, wai=wait, wak=walk, wri=write

### Agent/AI
ack=acknowledge, act=action, ada=adapt, agg=aggregate, agt=agent, ale=aleatoric, alg=aligned, alv=alive, anch=anchoring, asm=assume, attn=attention, awt=await, blk=blocked, bnd=bend-recursive, brc=branching, clb=calibrated, clr=clarity, clst=cluster, cmp=complete, cmt=committing, coh=coherent, cont=continuous, conv=converge, crsn=coarsening, crys=crystallizing, cst=constraint, dat=data, dec=decay, dlt=delete, dns=dense, drft=drifting, dst=distant, dup=duplicate, echo=echo, erg=ergodic, esp=epistemic, eva=evaluate, flat=flattened, fnc=function, foc=focus, frk=fork, gnd=grounded, grw=growth, hlu=hallucinate, hyp=hypothesis, inp=input, itr=iterate, log=log, lop=loop, lst=list, mcal=miscalibrated, midf=midflight, mnt=monitor, nod=node, opt=optimize, otp=output, pin=pinned, prc=precise, prx=proximate, qeu=queue, rdc=reduce, red=read, rep=repeat, resp=response, rev=review, rwd=rewind, scl=scale, setl=settled, shf=shift, sig=significant, smp=sample, spa=sparse, spn=spawn, sprs=sparse, stg=string, stk=stuck, stp=stop, syn=sync, tes=test, thk=think, tok=token, traj=trajectory, trg=trigger, ubk=unblocked, und=understand, upd=update, vlu=value, vod=void, vsn=version, wnd=wander

### Mathematics
add=addition, ave=average, cos=cosine, equ=equation, fac=factor, mat=matrix, med=median, quo=quotient, rat=ratio, set=set, sqr=square, sub=subtraction, sum=sum, var=variance

### Technology
api=interface, app=application, bak=backup, bot=agent, cli=click, cod=code, com=computing, cpu=processor, dat=data, enc=encrypt, err=error, fil=file, key=keyboard, log=login, mac=machine, mem=memory, net=network, scr=screen, srv=server, syn=sync, upd=update

### Mind & Cognition
abd=abduct, abl=able, abs=abstract, aim=aim, ang=anger, anx=anxious, asu=assume, aud=hearing, bli=believe, bor=bored, cal=calm, cld=cold, cou=courage, cow=coward, cre=creative, crt=certain, cur=curious, ded=deduce, des=desire, dou=doubt, dre=dream, dub=doubt, evi=evidence, exc=excitement, fea=fear, fee=feeling, foc=focus, gue=guess, hat=hate, hop=hope, hyp=hypothesize, ide=idea, ima=imagine, ind=induce, int=intuition, joy=joy, kno=know, lik=likely, lov=love, pra=practical, prb=probable, pri=pride, pur=purpose, puz=puzzled, rem=remember, sad=sad, sha=shame, thi=think, und=understand, vis=vision, wis=wisdom, won=wonder

### Communication
agr=agree, ans=answer, arg=argue, ask=ask, cle=clear, cri=criticize, dis=disagree, hid=hide, jok=joke, lis=listen, mea=meaning, nam=name, pub=public, que=question, rum=rumor, say=say, sec=secret, sen=sentence, sho=show, tal=talk, tel=tell, tru=true, war=warn, wor=word

### Physical World
aer=air, apr=appear, aqu=water, arv=arrive, ato=atom, big=large, bol=boil, bur=burn, che=chemical, con=contract, cry=crystal, dam=block, den=dense, dim=dim, dry=dry, ele=electron, exp=expand, fab=fabric, flo=flow, fre=freeze, gas=gas, gel=gel, gla=glass, glu=glue, gol=gold, har=hard, hev=heavy, imp=implode, iro=iron, joi=join, lie=recline, lit=light, lux=light, mag=magnet, mel=melt, mix=mix, mol=molecule, mov=move, nar=narrow, neu=neutron, nox=dark, odd=odd, oil=oil, opa=opaque, oxi=oxidize, pla=plasma, por=porous, pow=powder, pre=pressure, pul=pull, pus=push, pyr=fire, rad=radiate, rig=rigid, rot=rot, rub=rubber, sha=sharp, smo=smooth, sof=soft, sol=solid, sti=sticky, ter=earth, thi=thin, vis=viscous, wet=wet, wid=wide

### Social
ann=announce, cer=ceremony, chi=child, cru=cruel, cul=culture, cus=custom, dut=duty, ene=enemy, fai=fair, fam=family, fol=follower, fri=friend, giv=give, gre=greedy, hel=help, hon=honest, hur=hurt, kin=kind, lea=leader, los=loss, loy=loyal, obe=obey, oth=other, par=parent, pro=protect, pun=punish, reb=rebel, rew=reward, rul=rule, sel=self, shr=share, tak=take, tea=team, unf=unfair, vic=victory

### Time & Change
acc=accelerate, aft=after, alw=always, bef=before, beg=begin, bir=birth, bre=brief, cha=change, cyc=cycle, daw=dawn, dea=death, dec=decay, del=delay, dur=duration, dus=dusk, end=end, epo=epoch, era=era, evo=evolution, fut=future, grd=gradual, has=haste, las=last, lon=long, mid=middle, mom=moment, nev=never, now=now, oft=often, onc=once, pas=past, pha=phase, rar=rare, seq=sequence, sta=stable, tra=transform, unt=until, urg=urgent, yet=yet

### Space & Position
abo=above, acr=across, aga=against, alo=along, amo=among, arc=arc, are=area, aro=around, awa=away, bac=backward, beh=behind, bel=below, bes=beside, bet=between, cen=center, cir=circle, cor=core, dow=down, eas=east, edg=edge, far=far, for=forward, fro=front, gap=gap, hei=height, her=here, hig=high, ins=inside, lef=left, len=length, lin=line, low=low, nea=near, nor=north, out=outside, poi=point, rig=right, sid=side, sou=south, sph=sphere, the=there, wes=west

### Living Things
ani=animal, avi=bird, blo=blood, bon=bone, bra=brain, bug=insect, cel=cell, dog=dog, ear=ear, eag=eagle, eye=eye, fis=fish, foo=foot, fox=fox, gen=gene, gra=grass, hai=hair, han=hand, hea=healthy, hed=head, hor=horse, hrt=heart, hum=human, lif=alive, lio=lion, mou=mouth, ner=nerve, old=old, owl=owl, pig=pig, rep=reptile, roo=root, sed=seed, ski=skin, sna=snake, str=strong, tre=tree, wea=weak, wol=wolf, you=young

### Virtue & Ethics
chst=chastity, dil=diligence, eny=envy, frt=fortitude, fth=faith/trust, glt=gluttony, gnr=generosity, grt=gratitude, hbl=humility, hns=honesty, itr=integrity, jus=justice, knd=kindness, lus=lust, mds=modesty, mer=mercy, pie=piety, prd=pride, pru=prudence, slh=sloth, tpr=temperance, wrt=wrath

### Metalinguistic
bab=babel, can=can/able, clp=collapse, gui=guide, hav=have, how=how, idi=idiom, lex=lexicon, met=metaphor, mls=meaningless, mud=mud, rcr=recursive, ref=reference, sem=semantics, slf=self-reference, sup=superposition, whn=when, who=who, whr=where, why=why
