# Limn Ful - Complete Metacircular Limn
# sol pro cns | nu eng | sol Limn

# ============================================================================
# wor (word) - Limn vocabulary as numbers
# ============================================================================

# ope wor (operation words)
JOI = 1   # joi = add
CUT = 2   # cut = subtract
EXP = 3   # exp = multiply
CON = 4   # con = divide

# rel wor (relation words)
SA = 10   # sa = same
MA = 11   # ma = more
MI = 12   # mi = less
AMO = 13  # amo = among

# spe wor (special words)
PRO = 20  # pro = program
VAR = 21  # var = variable
WHE = 22  # whe = where
CNS = 23  # cns = constraint
NU = 24   # nu = negation
PA = 25   # pa = parallel/or
IF = 26   # if = condition
THE = 27  # the = then
OTH = 28  # oth = otherwise
CAU = 29  # cau = cause/call
EFF = 30  # eff = effect
GRO = 31  # gro = group
WHO = 32  # who = size
FST = 33  # fst = first
NXT = 34  # nxt = next/tail
FIN = 35  # fin = final
PAR = 36  # par = part/index
YO = 37   # yo = this (proximal)
AN = 38   # an = that (distal)
TRU = 39  # tru = true
FAL = 40  # fal = false
HOL = 41  # hol = hole

# pip wor (pipe)
PIP = 99  # | = pipe/separator

# ============================================================================
# tok (token) - wor val
# ============================================================================

type Tok:
  Wor { wor }        # Limn word
  One { val }        # number (one)
  Str { val }        # string (wor)

# ============================================================================
# nod (AST node)
# ============================================================================

type Nod:
  Lit { val }
  Var { nom }
  Bin { ope, lef, rig }
  Cns { rel, lef, rig, inv }
  Gro { par }
  Alt { bra }
  Cnd { cnd, the, oth }
  Cal { nom, arg, eff }

# ============================================================================
# env (environment)
# ============================================================================

type Env:
  Hol
  Bin { nom, val, nxt }

def sol_env(nom, env):
  match env:
    case Env/Hol:
      return 0
    case Env/Bin:
      if nom == env.nom:
        return env.val
      else:
        return sol_env(nom, env.nxt)

def joi_env(nom, val, env):
  return Env/Bin { nom: nom, val: val, nxt: env }

# ============================================================================
# eva (evaluate)
# ============================================================================

def eva(nod, env):
  match nod:
    case Nod/Lit:
      return nod.val
    case Nod/Var:
      return sol_env(nod.nom, env)
    case Nod/Bin:
      l = eva(nod.lef, env)
      r = eva(nod.rig, env)
      if nod.ope == JOI:
        return l + r
      else:
        if nod.ope == CUT:
          return l - r
        else:
          if nod.ope == EXP:
            return l * r
          else:
            if nod.ope == CON:
              return l / r
            else:
              return 0
    case Nod/Gro:
      return eva_gro(nod.par, env)
    case _:
      return 0

def eva_gro(par, env):
  match par:
    case List/Nil:
      return List/Nil
    case List/Cons:
      return List/Cons { head: eva(par.head, env), tail: eva_gro(par.tail, env) }

# ============================================================================
# eva_rel (evaluate relation) - cns sa tru/fal
# ============================================================================

def eva_rel(cns, env):
  match cns:
    case Nod/Cns:
      l = eva(cns.lef, env)
      r = eva(cns.rig, env)
      sat = eva_rel_ope(cns.rel, l, r)
      # nu (negation) - inv sa 1
      if cns.inv == 1:
        if sat == 1:
          return 0
        else:
          return 1
      else:
        return sat
    case _:
      return 0

def eva_rel_ope(rel, l, r):
  if rel == SA:
    if l == r:
      return 1
    else:
      return 0
  else:
    if rel == MA:
      if l > r:
        return 1
      else:
        return 0
    else:
      if rel == MI:
        if l < r:
          return 1
        else:
          return 0
      else:
        if rel == AMO:
          return eva_amo(l, r)
        else:
          return 0

# eva_amo (evaluate among) - val amo gro
def eva_amo(val, gro):
  match gro:
    case List/Nil:
      return 0
    case List/Cons:
      if val == gro.head:
        return 1
      else:
        return eva_amo(val, gro.tail)

# ============================================================================
# eva_cnd (evaluate conditional) - if cns the nod oth nod
# ============================================================================

def eva_cnd(nod, env):
  match nod:
    case Nod/Cnd:
      cnd_val = eva_rel(nod.cnd, env)
      if cnd_val == 1:
        return eva(nod.the, env)
      else:
        return eva(nod.oth, env)
    case _:
      return 0

# ============================================================================
# eva_alt (evaluate alternative) - pa nod nod (superposition)
# ============================================================================

def eva_alt(nod, env):
  match nod:
    case Nod/Alt:
      return eva_alt_gro(nod.bra, env)
    case _:
      return 0

def eva_alt_gro(bra, env):
  match bra:
    case List/Nil:
      return 0
    case List/Cons:
      hd = eva(bra.head, env)
      match bra.tail:
        case List/Nil:
          return hd
        case List/Cons:
          tl = eva_alt_gro(bra.tail, env)
          return {hd, tl}

# ============================================================================
# fun (function store) - cau nom arg eff nod
# ============================================================================

type Fun:
  Hol
  Bin { nom, arg, bod, nxt }

def sol_fun(nom, fun):
  match fun:
    case Fun/Hol:
      return Fun/Hol
    case Fun/Bin:
      if nom == fun.nom:
        return fun
      else:
        return sol_fun(nom, fun.nxt)

def joi_fun(nom, arg, bod, fun):
  return Fun/Bin { nom: nom, arg: arg, bod: bod, nxt: fun }

# ============================================================================
# eva_cal (evaluate call) - cau nom arg sa eff
# ============================================================================

def eva_cal(nod, env, fun):
  match nod:
    case Nod/Cal:
      f = sol_fun(nod.nom, fun)
      match f:
        case Fun/Bin:
          # joi arg val env
          arg_val = eva_gro(nod.arg, env)
          env_nxt = joi_arg(f.arg, arg_val, env)
          return eva_ful(f.bod, env_nxt, fun)
        case _:
          return 0
    case _:
      return 0

def joi_arg(nom_gro, val_gro, env):
  match nom_gro:
    case List/Nil:
      return env
    case List/Cons:
      match val_gro:
        case List/Nil:
          return env
        case List/Cons:
          env_nxt = joi_env(nom_gro.head, val_gro.head, env)
          return joi_arg(nom_gro.tail, val_gro.tail, env_nxt)

# ============================================================================
# eva_ful (evaluate full) - nod env fun sa val
# ============================================================================

def eva_ful(nod, env, fun):
  match nod:
    case Nod/Lit:
      return nod.val
    case Nod/Var:
      return sol_env(nod.nom, env)
    case Nod/Bin:
      l = eva_ful(nod.lef, env, fun)
      r = eva_ful(nod.rig, env, fun)
      if nod.ope == JOI:
        return l + r
      else:
        if nod.ope == CUT:
          return l - r
        else:
          if nod.ope == EXP:
            return l * r
          else:
            if nod.ope == CON:
              return l / r
            else:
              return 0
    case Nod/Gro:
      return eva_gro_ful(nod.par, env, fun)
    case Nod/Cnd:
      cnd_val = eva_rel(nod.cnd, env)
      if cnd_val == 1:
        return eva_ful(nod.the, env, fun)
      else:
        return eva_ful(nod.oth, env, fun)
    case Nod/Alt:
      return eva_alt_ful(nod.bra, env, fun)
    case Nod/Cal:
      return eva_cal(nod, env, fun)
    case _:
      return 0

def eva_gro_ful(par, env, fun):
  match par:
    case List/Nil:
      return List/Nil
    case List/Cons:
      return List/Cons { head: eva_ful(par.head, env, fun), tail: eva_gro_ful(par.tail, env, fun) }

def eva_alt_ful(bra, env, fun):
  match bra:
    case List/Nil:
      return 0
    case List/Cons:
      hd = eva_ful(bra.head, env, fun)
      match bra.tail:
        case List/Nil:
          return hd
        case List/Cons:
          tl = eva_alt_ful(bra.tail, env, fun)
          return {hd, tl}

# ============================================================================
# sol_cns (solve constraint) - bidirectional
# ============================================================================

def sol_cns(cns, env):
  match cns:
    case Nod/Cns:
      match cns.lef:
        # bin sa rig (binary operation equals right)
        case Nod/Bin:
          l_l = eva(cns.lef.lef, env)
          l_r = eva(cns.lef.rig, env)
          r = eva(cns.rig, env)

          # sol rig var (forward: a ope b sa ?)
          if r == 0:
            match cns.rig:
              case Nod/Var:
                return sol_cns_fwd(cns.lef.ope, l_l, l_r, cns.rig.nom, env)
              case _:
                return env
          else:
            # sol lef var (backward: ? ope b sa c OR a ope ? sa c)
            if l_l == 0:
              match cns.lef.lef:
                case Nod/Var:
                  return sol_cns_bwd_lef(cns.lef.ope, l_r, r, cns.lef.lef.nom, env)
                case _:
                  return env
            else:
              if l_r == 0:
                match cns.lef.rig:
                  case Nod/Var:
                    return sol_cns_bwd_rig(cns.lef.ope, l_l, r, cns.lef.rig.nom, env)
                  case _:
                    return env
              else:
                return env

        # var sa val
        case Nod/Var:
          r = eva(cns.rig, env)
          if r != 0:
            return joi_env(cns.lef.nom, r, env)
          else:
            return env

        case _:
          return env
    case _:
      return env

# sol_cns_fwd (solve forward: a ope b sa c, sol c)
def sol_cns_fwd(ope, l_l, l_r, nom, env):
  if ope == JOI:
    return joi_env(nom, l_l + l_r, env)
  else:
    if ope == CUT:
      return joi_env(nom, l_l - l_r, env)
    else:
      if ope == EXP:
        return joi_env(nom, l_l * l_r, env)
      else:
        if ope == CON:
          return joi_env(nom, l_l / l_r, env)
        else:
          return env

# sol_cns_bwd_lef (solve backward left: ? ope b sa c, sol ?)
# joi: ? joi b sa c -> ? sa c cut b
# cut: ? cut b sa c -> ? sa c joi b
# exp: ? exp b sa c -> ? sa c con b
# con: ? con b sa c -> ? sa c exp b
def sol_cns_bwd_lef(ope, b, c, nom, env):
  if ope == JOI:
    return joi_env(nom, c - b, env)
  else:
    if ope == CUT:
      return joi_env(nom, c + b, env)
    else:
      if ope == EXP:
        return joi_env(nom, c / b, env)
      else:
        if ope == CON:
          return joi_env(nom, c * b, env)
        else:
          return env

# sol_cns_bwd_rig (solve backward right: a ope ? sa c, sol ?)
# joi: a joi ? sa c -> ? sa c cut a
# cut: a cut ? sa c -> ? sa a cut c
# exp: a exp ? sa c -> ? sa c con a
# con: a con ? sa c -> ? sa a con c
def sol_cns_bwd_rig(ope, a, c, nom, env):
  if ope == JOI:
    return joi_env(nom, c - a, env)
  else:
    if ope == CUT:
      return joi_env(nom, a - c, env)
    else:
      if ope == EXP:
        return joi_env(nom, c / a, env)
      else:
        if ope == CON:
          return joi_env(nom, a / c, env)
        else:
          return env

# ============================================================================
# sol_cns_gro (solve constraint group) - iterative propagation
# ============================================================================

def sol_cns_gro(cns_gro, env):
  match cns_gro:
    case List/Nil:
      return env
    case List/Cons:
      env_nxt = sol_cns(cns_gro.head, env)
      return sol_cns_gro(cns_gro.tail, env_nxt)

# sol_cns_itr (solve constraints iteratively until fixed point)
def sol_cns_itr(cns_gro, env, max_itr):
  if max_itr == 0:
    return env
  else:
    env_nxt = sol_cns_gro(cns_gro, env)
    # sol nxt itr (solve next iteration)
    return sol_cns_itr(cns_gro, env_nxt, max_itr - 1)

# ============================================================================
# pro (program) - nom var cns
# ============================================================================

type Pro:
  Pro { nom, var, cns }

def sol_pro(pro, key, max_itr):
  match pro:
    case Pro/Pro:
      return sol_cns_itr(pro.cns, key, max_itr)

# ============================================================================
# gro ope (group operations)
# ============================================================================

def who(gro):
  match gro:
    case List/Nil:
      return 0
    case List/Cons:
      return 1 + who(gro.tail)

def fst(gro):
  match gro:
    case List/Nil:
      return 0
    case List/Cons:
      return gro.head

def nxt(gro):
  match gro:
    case List/Nil:
      return List/Nil
    case List/Cons:
      return gro.tail

def fin(gro):
  match gro:
    case List/Nil:
      return 0
    case List/Cons:
      match gro.tail:
        case List/Nil:
          return gro.head
        case List/Cons:
          return fin(gro.tail)

def par(i, gro):
  match gro:
    case List/Nil:
      return 0
    case List/Cons:
      if i == 0:
        return gro.head
      else:
        return par(i - 1, gro.tail)

# ============================================================================
# dem (demonstration)
# ============================================================================

def main():
  # ========================================
  # dem 1: ope cns (operation constraint)
  # Limn: a joi b sa c | a sa 3 | b sa 5
  # sol: c sa 8
  # ========================================

  a = Nod/Var { nom: 1 }
  b = Nod/Var { nom: 2 }
  c = Nod/Var { nom: 3 }

  a_joi_b = Nod/Bin { ope: JOI, lef: a, rig: b }
  cns_joi = Nod/Cns { rel: SA, lef: a_joi_b, rig: c, inv: 0 }

  key = joi_env(1, 3, joi_env(2, 5, Env/Hol))
  env = sol_cns(cns_joi, key)
  c_val = sol_env(3, env)

  # ========================================
  # dem 2: rel cns (relation constraint)
  # Limn: 5 ma 3 | 2 mi 7 | nu 5 sa 5
  # ========================================

  five = Nod/Lit { val: 5 }
  three = Nod/Lit { val: 3 }
  two = Nod/Lit { val: 2 }
  seven = Nod/Lit { val: 7 }

  # 5 ma 3 sa tru (5 > 3)
  cns_ma = Nod/Cns { rel: MA, lef: five, rig: three, inv: 0 }
  ma_val = eva_rel(cns_ma, Env/Hol)

  # 2 mi 7 sa tru (2 < 7)
  cns_mi = Nod/Cns { rel: MI, lef: two, rig: seven, inv: 0 }
  mi_val = eva_rel(cns_mi, Env/Hol)

  # nu 5 sa 5 sa fal (not 5 == 5)
  cns_nu = Nod/Cns { rel: SA, lef: five, rig: five, inv: 1 }
  nu_val = eva_rel(cns_nu, Env/Hol)

  # ========================================
  # dem 3: cnd (conditional)
  # Limn: if x ma 0 the x oth 0 | x sa 10
  # sol: 10
  # ========================================

  x = Nod/Var { nom: 10 }
  zero = Nod/Lit { val: 0 }

  # x ma 0
  cns_x_ma_0 = Nod/Cns { rel: MA, lef: x, rig: zero, inv: 0 }

  # if x ma 0 the x oth 0
  cnd_nod = Nod/Cnd { cnd: cns_x_ma_0, the: x, oth: zero }

  key3 = joi_env(10, 10, Env/Hol)
  cnd_val = eva_cnd(cnd_nod, key3)

  # ========================================
  # dem 4: alt (alternative/pa)
  # Limn: pa 1 pa 2 pa 3
  # sol: {1, {2, 3}}
  # ========================================

  one = Nod/Lit { val: 1 }
  two_lit = Nod/Lit { val: 2 }
  three_lit = Nod/Lit { val: 3 }

  alt_nod = Nod/Alt { bra: [one, two_lit, three_lit] }
  alt_val = eva_alt(alt_nod, Env/Hol)

  # ========================================
  # dem 5: cau eff (function call)
  # Limn: cau dub n eff n exp 2
  # Limn: dub 5 sa eff
  # sol: eff sa 10
  # ========================================

  n = Nod/Var { nom: 20 }
  two_n = Nod/Lit { val: 2 }
  n_exp_2 = Nod/Bin { ope: EXP, lef: n, rig: two_n }

  # cau dub n eff n exp 2 (fun nom = 100, arg = [20])
  fun = joi_fun(100, [20], n_exp_2, Fun/Hol)

  # dub 5 (cal nom = 100, arg = [5])
  five_lit = Nod/Lit { val: 5 }
  cal_nod = Nod/Cal { nom: 100, arg: [five_lit], eff: 0 }

  cal_val = eva_cal(cal_nod, Env/Hol, fun)

  # ========================================
  # dem 6: gro ope (group operations)
  # ========================================

  gro = [100, 200, 300, 400]
  siz = who(gro)
  fir = fst(gro)
  las = fin(gro)
  sec = par(1, gro)

  # ========================================
  # dem 7: rec (recursion) - factorial
  # Limn: cau fac n eff if n sa 0 the 1 oth n exp fac n cut 1
  # sol: fac 5 sa 120
  # ========================================

  # fac nod - recursive body
  n_var = Nod/Var { nom: 30 }
  zero_lit = Nod/Lit { val: 0 }
  one_lit = Nod/Lit { val: 1 }

  # n sa 0
  cns_n_sa_0 = Nod/Cns { rel: SA, lef: n_var, rig: zero_lit, inv: 0 }

  # n cut 1
  n_cut_1 = Nod/Bin { ope: CUT, lef: n_var, rig: one_lit }

  # fac (n cut 1) - recursive call
  fac_n_1 = Nod/Cal { nom: 200, arg: [n_cut_1], eff: 0 }

  # n exp fac (n cut 1)
  n_exp_fac = Nod/Bin { ope: EXP, lef: n_var, rig: fac_n_1 }

  # if n sa 0 the 1 oth n exp fac (n cut 1)
  fac_bod = Nod/Cnd { cnd: cns_n_sa_0, the: one_lit, oth: n_exp_fac }

  # cau fac n eff ... (fun nom = 200, arg = [30])
  fun_fac = joi_fun(200, [30], fac_bod, Fun/Hol)

  # fac 5
  five_fac = Nod/Lit { val: 5 }
  cal_fac = Nod/Cal { nom: 200, arg: [five_fac], eff: 0 }

  fac_val = eva_cal(cal_fac, Env/Hol, fun_fac)

  # ========================================
  # dem 8: bwd (bidirectional) - sol lef
  # Limn: x joi 5 sa 12 | sol x
  # sol: x sa 7
  # ========================================

  x_bwd = Nod/Var { nom: 40 }
  five_bwd = Nod/Lit { val: 5 }
  twelve = Nod/Lit { val: 12 }

  x_joi_5 = Nod/Bin { ope: JOI, lef: x_bwd, rig: five_bwd }
  cns_bwd = Nod/Cns { rel: SA, lef: x_joi_5, rig: twelve, inv: 0 }

  key_bwd = Env/Hol
  env_bwd = sol_cns(cns_bwd, key_bwd)
  x_bwd_val = sol_env(40, env_bwd)

  # ========================================
  # dem 9: bwd2 (bidirectional right)
  # Limn: 20 con y sa 4 | sol y
  # sol: y sa 5 (20 / y = 4 -> y = 20 / 4 = 5)
  # ========================================

  twenty = Nod/Lit { val: 20 }
  y_bwd = Nod/Var { nom: 41 }
  four = Nod/Lit { val: 4 }

  twenty_con_y = Nod/Bin { ope: CON, lef: twenty, rig: y_bwd }
  cns_bwd2 = Nod/Cns { rel: SA, lef: twenty_con_y, rig: four, inv: 0 }

  env_bwd2 = sol_cns(cns_bwd2, Env/Hol)
  y_bwd_val = sol_env(41, env_bwd2)

  # ========================================
  # dem 10: itr (iterative propagation)
  # Limn: a joi b sa c | c exp 2 sa d | a sa 3 | b sa 4
  # sol: c sa 7 | d sa 14
  # ========================================

  a_itr = Nod/Var { nom: 50 }
  b_itr = Nod/Var { nom: 51 }
  c_itr = Nod/Var { nom: 52 }
  d_itr = Nod/Var { nom: 53 }
  two_itr = Nod/Lit { val: 2 }

  # a joi b sa c
  a_joi_b_itr = Nod/Bin { ope: JOI, lef: a_itr, rig: b_itr }
  cns_itr_1 = Nod/Cns { rel: SA, lef: a_joi_b_itr, rig: c_itr, inv: 0 }

  # c exp 2 sa d
  c_exp_2 = Nod/Bin { ope: EXP, lef: c_itr, rig: two_itr }
  cns_itr_2 = Nod/Cns { rel: SA, lef: c_exp_2, rig: d_itr, inv: 0 }

  # key: a sa 3 | b sa 4
  key_itr = joi_env(50, 3, joi_env(51, 4, Env/Hol))

  # sol cns gro (solve constraint group with 2 iterations)
  cns_gro_itr = [cns_itr_1, cns_itr_2]
  env_itr = sol_cns_itr(cns_gro_itr, key_itr, 2)

  c_itr_val = sol_env(52, env_itr)
  d_itr_val = sol_env(53, env_itr)

  # ========================================
  # dem 11: amo (among) - val amo gro
  # Limn: 3 amo gro 1 2 3 4 5 | nu 7 amo gro 1 2 3
  # sol: tru | tru
  # ========================================

  three_amo = Nod/Lit { val: 3 }
  seven_amo = Nod/Lit { val: 7 }
  gro_amo = Nod/Gro { par: [Nod/Lit { val: 1 }, Nod/Lit { val: 2 }, Nod/Lit { val: 3 }, Nod/Lit { val: 4 }, Nod/Lit { val: 5 }] }
  gro_amo_2 = Nod/Gro { par: [Nod/Lit { val: 1 }, Nod/Lit { val: 2 }, Nod/Lit { val: 3 }] }

  # 3 amo [1, 2, 3, 4, 5] sa tru
  cns_amo = Nod/Cns { rel: AMO, lef: three_amo, rig: gro_amo, inv: 0 }
  amo_val = eva_rel(cns_amo, Env/Hol)

  # nu 7 amo [1, 2, 3] sa tru (7 is NOT among the group)
  cns_nu_amo = Nod/Cns { rel: AMO, lef: seven_amo, rig: gro_amo_2, inv: 1 }
  nu_amo_val = eva_rel(cns_nu_amo, Env/Hol)

  # ========================================
  # dem 12: key (key-collapsible semantics)
  # Limn pro: a joi b sa c
  # key 1: a sa 10 | b sa 20 -> c sa 30
  # key 2: a sa 5 | b sa 3 -> c sa 8
  # key 3: a sa 100 | b sa 50 -> c sa 150
  # Same constraint, different keys, different solutions
  # ========================================

  a_key = Nod/Var { nom: 60 }
  b_key = Nod/Var { nom: 61 }
  c_key = Nod/Var { nom: 62 }

  a_joi_b_key = Nod/Bin { ope: JOI, lef: a_key, rig: b_key }
  cns_key = Nod/Cns { rel: SA, lef: a_joi_b_key, rig: c_key, inv: 0 }
  cns_gro_key = [cns_key]

  # key 1: a sa 10 | b sa 20
  key_1 = joi_env(60, 10, joi_env(61, 20, Env/Hol))
  env_key_1 = sol_cns_itr(cns_gro_key, key_1, 1)
  c_key_1 = sol_env(62, env_key_1)

  # key 2: a sa 5 | b sa 3
  key_2 = joi_env(60, 5, joi_env(61, 3, Env/Hol))
  env_key_2 = sol_cns_itr(cns_gro_key, key_2, 1)
  c_key_2 = sol_env(62, env_key_2)

  # key 3: a sa 100 | b sa 50
  key_3 = joi_env(60, 100, joi_env(61, 50, Env/Hol))
  env_key_3 = sol_cns_itr(cns_gro_key, key_3, 1)
  c_key_3 = sol_env(62, env_key_3)

  # ========================================
  # dem 13: sup_cns (superposition in constraints)
  # Limn: x exp 2 sa y | x sa pa 2 pa 3 pa 4
  # sol: y sa {4, {9, 16}}
  # HVM superposition finds all valid y values
  # ========================================

  x_sup = {2, 3, 4}
  y_sup = x_sup * x_sup

  # ========================================
  # dem 14: sup_gro (superposition over group)
  # Limn: pa gro 1 2 3 pa gro 4 5 6 | fst gro
  # sol: fst sa {1, 4}
  # ========================================

  gro_sup = {[1, 2, 3], [4, 5, 6]}
  fst_sup = fst(gro_sup)

  # ========================================
  # dem 15: sup (basic superposition demo)
  # Limn: pa 10 pa 20 pa 30 exp 2
  # sol: {20, {40, 60}}
  # ========================================

  sup_val = {10, 20, 30}
  sup_exp = sup_val * 2

  return (c_val, ma_val, mi_val, nu_val, cnd_val, alt_val, cal_val, fac_val, x_bwd_val, y_bwd_val, c_itr_val, d_itr_val, amo_val, nu_amo_val, c_key_1, c_key_2, c_key_3, y_sup, fst_sup, siz, fir, las, sec, sup_exp)
