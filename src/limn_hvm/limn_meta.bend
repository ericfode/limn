# Limn Meta - Metacircular Limn in Bend
# sol cns (all constraints)
# nu eng (no english)

# ============================================================================
# AST wor (word types)
# ============================================================================

type Nod:  # nod = node
  Lit { val }                     # lit = literal (one/flo/wor)
  Var { nom }                     # var = variable, nom = name
  Bin { ope, lef, rig }           # bin = binary op
  Cns { rel, lef, rig, inv }      # cns = constraint, inv = inverted (nu)
  Gro { par }                     # gro = group, par = parts
  Alt { bra }                     # alt = alternative (pa), bra = branches
  Con { cnd, the, oth }           # con = conditional
  Cal { nom, arg, eff }           # cal = call, eff = effect/result

# ============================================================================
# ope (operators) - sol Limn wor
# ============================================================================

type Ope:  # ope = operation
  Joi   # joi = add
  Cut   # cut = subtract
  Exp   # exp = multiply
  Con   # con = divide

type Rel:  # rel = relation
  Sa    # sa = same (equals)
  Ma    # ma = more (greater)
  Mi    # mi = less
  Amo   # amo = among (element of)

# ============================================================================
# env (environment) - nom sa val (name same value)
# ============================================================================

type Env:
  Hol                             # hol = hole (empty)
  Bin { nom, val, nxt }           # bin = binding, nxt = next

# sol nom env (solve name in environment)
def sol_nom(nom, env):
  match env:
    case Env/Hol:
      return 0  # nu sol (not solved)
    case Env/Bin:
      if nom == env.nom:
        return env.val
      else:
        return sol_nom(nom, env.nxt)

# joi nom val env (join name value to environment)
def joi_nom(nom, val, env):
  return Env/Bin { nom: nom, val: val, nxt: env }

# ============================================================================
# eva (evaluate) - nod env sa val
# ============================================================================

def eva(nod, env):
  match nod:
    case Nod/Lit:
      return nod.val

    case Nod/Var:
      return sol_nom(nod.nom, env)

    case Nod/Bin:
      lef_val = eva(nod.lef, env)
      rig_val = eva(nod.rig, env)
      match nod.ope:
        case Ope/Joi:
          return lef_val + rig_val
        case Ope/Cut:
          return lef_val - rig_val
        case Ope/Exp:
          return lef_val * rig_val
        case Ope/Con:
          return lef_val / rig_val

    case Nod/Gro:
      return eva_gro(nod.par, env)

    case Nod/Alt:
      return eva_alt(nod.bra, env)

    case Nod/Con:
      cnd_val = eva_cns(nod.cnd, env)
      if cnd_val != 0:
        return eva(nod.the, env)
      else:
        return eva(nod.oth, env)

    case _:
      return 0

# eva_gro (evaluate group)
def eva_gro(par, env):
  match par:
    case List/Nil:
      return List/Nil
    case List/Cons:
      return List/Cons { head: eva(par.head, env), tail: eva_gro(par.tail, env) }

# eva_alt (evaluate alternative) - fst sol (first solution)
def eva_alt(bra, env):
  match bra:
    case List/Nil:
      return 0
    case List/Cons:
      val = eva(bra.head, env)
      return val  # fst (first)

# ============================================================================
# eva_cns (evaluate constraint) - cns sa tru/fal
# ============================================================================

def eva_cns(cns, env):
  match cns:
    case Nod/Cns:
      lef_val = eva(cns.lef, env)
      rig_val = eva(cns.rig, env)

      sat = 0  # sat = satisfied
      match cns.rel:
        case Rel/Sa:
          if lef_val == rig_val:
            sat = 1
          else:
            sat = 0
        case Rel/Ma:
          if lef_val > rig_val:
            sat = 1
          else:
            sat = 0
        case Rel/Mi:
          if lef_val < rig_val:
            sat = 1
          else:
            sat = 0
        case Rel/Amo:
          sat = amo_sol(lef_val, rig_val)

      # inv = inverted (nu)
      if cns.inv == 1:
        if sat == 1:
          return 0
        else:
          return 1
      else:
        return sat
    case _:
      return 0

# amo_sol (among solve) - val amo gro
def amo_sol(val, gro):
  match gro:
    case List/Nil:
      return 0
    case List/Cons:
      if val == gro.head:
        return 1
      else:
        return amo_sol(val, gro.tail)

# ============================================================================
# sol_cns (solve constraint) - cns env sa env_nxt (new environment)
# ============================================================================

def sol_cns(cns, env):
  match cns:
    case Nod/Cns:
      # bin ope lef rig sa res (binary operation on left)
      match cns.lef:
        case Nod/Bin:
          lef_lef = eva(cns.lef.lef, env)
          lef_rig = eva(cns.lef.rig, env)
          rig_val = eva(cns.rig, env)

          # sol ope (solve operation)
          match cns.lef.ope:
            case Ope/Joi:
              # a joi b sa c, sol c
              if rig_val == 0:
                c_val = lef_lef + lef_rig
                match cns.rig:
                  case Nod/Var:
                    return joi_nom(cns.rig.nom, c_val, env)
                  case _:
                    return env
              else:
                return env
            case _:
              return env

        case Nod/Var:
          # var sa val
          rig_val = eva(cns.rig, env)
          if rig_val != 0:
            return joi_nom(cns.lef.nom, rig_val, env)
          else:
            return env

        case _:
          return env
    case _:
      return env

# ============================================================================
# gro ope (group operations) - Limn par/who/fst/nxt/fin
# ============================================================================

# who gro sa one (size)
def who(gro):
  match gro:
    case List/Nil:
      return 0
    case List/Cons:
      return 1 + who(gro.tail)

# fst gro sa val (first)
def fst(gro):
  match gro:
    case List/Nil:
      return 0
    case List/Cons:
      return gro.head

# nxt gro sa gro (tail/rest)
def nxt(gro):
  match gro:
    case List/Nil:
      return List/Nil
    case List/Cons:
      return gro.tail

# fin gro sa val (final/last)
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

# par i gro sa val (part/index)
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
# pro (program) - nom var cns
# ============================================================================

type Pro:
  Pro { nom, var, cns }  # pro = program

# sol_pro (solve program) - pro key sa env
def sol_pro(pro, key):
  match pro:
    case Pro/Pro:
      # beg env (begin environment)
      env = key

      # sol cns (solve constraints) - one iteration
      env = sol_cns_gro(pro.cns, env)

      return env

# sol_cns_gro (solve constraint group)
def sol_cns_gro(cns_gro, env):
  match cns_gro:
    case List/Nil:
      return env
    case List/Cons:
      env_nxt = sol_cns(cns_gro.head, env)
      return sol_cns_gro(cns_gro.tail, env_nxt)

# ============================================================================
# dem (demonstration) - Limn pro sol
# ============================================================================

def main():
  # dem 1: a joi b sa c
  # var a sa 3, var b sa 5
  # cns a joi b sa c

  # AST nod
  a_var = Nod/Var { nom: 1 }  # a = nom 1
  b_var = Nod/Var { nom: 2 }  # b = nom 2
  c_var = Nod/Var { nom: 3 }  # c = nom 3

  a_joi_b = Nod/Bin { ope: Ope/Joi, lef: a_var, rig: b_var }
  cns_1 = Nod/Cns { rel: Rel/Sa, lef: a_joi_b, rig: c_var, inv: 0 }

  # key: a sa 3, b sa 5
  key = joi_nom(1, 3, joi_nom(2, 5, Env/Hol))

  # sol cns
  env = sol_cns(cns_1, key)

  # c val
  c_val = sol_nom(3, env)

  # dem 2: gro ope
  gro_dem = [10, 20, 30, 40, 50]
  siz = who(gro_dem)     # 5
  fir = fst(gro_dem)     # 10
  las = fin(gro_dem)     # 50
  sec = par(1, gro_dem)  # 20
  res = nxt(gro_dem)     # [20, 30, 40, 50]

  # dem 3: alt (pa) - sup (superposition)
  x = {1, 2, 3}
  x_exp = x * 2  # {2, 4, 6}

  return (c_val, siz, fir, las, sec, x_exp)
