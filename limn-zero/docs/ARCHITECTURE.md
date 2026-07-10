# Architecture

## Strong Path

```text
observation x_t
  -> context encoder E_theta
  -> continuous context z_t
  -> bottleneck Q
  -> persistent state g_t
  -> action-conditioned predictor P_phi(g_t, a_t, target_spec)
  -> predicted target z_hat_(t+k)

target observation x_(t+k)
  -> EMA target encoder E_xi
  -> target z_(t+k)
```

The primary JEPA loss compares `z_hat_(t+k)` with a stop-gradient target `z_(t+k)`.

For recurrent prediction:

```text
g_(t+k) = Q(z_hat_(t+k))
```

All other activations and caches are discarded at the recurrence boundary.

## Components

### Context Encoder

Encodes partial observations. It may be domain-specific, but every experimental variant must receive
equivalent information and comparable capacity.

### Target Encoder

Encodes complete or future targets. Its parameters are updated by exponential moving average unless
the current contract specifies another collapse-resistant JEPA mechanism.

### Predictor

Predicts target embeddings from the declared bottleneck plus explicit conditioning such as action,
horizon, masked region, or counterfactual intervention.

### Bottleneck Variants

- **Continuous:** unrestricted embedding; establishes the performance ceiling.
- **Flat quantized:** fixed-size discrete code without graph structure.
- **Fixed graph:** human-designed factor and relation types.
- **Emergent graph:** learned factorized codebooks and learned relation use under a fixed graph budget.

### Emergent Graph State

The initial design budget is a bounded collection of nodes and typed edges:

```text
node := concept_code + optional role + optional grade
edge := relation_code(source, target) + optional temporal offset
graph := bounded set(nodes, edges)
```

The exact sizes belong in immutable run configuration, not in source-code constants.

## Loss Families

The primary loss is predictive, not generative:

- joint-embedding target prediction,
- multi-horizon action-conditioned prediction,
- masked semantic-factor prediction,
- counterfactual outcome prediction.

Permitted auxiliary losses must be preregistered:

- task outcome,
- sparse channel cost,
- codebook commitment,
- graph well-formedness,
- cross-agent receiver prediction,
- cycle or algebraic consistency.

Avoid forcing uniform token entropy. Monitor collapse and unused codes, but do not reward diversity for
its own sake.

## No-Bypass Requirements

Automated tests must verify:

- the predictor receives no unquantized encoder output in discrete variants,
- recurrent rollouts receive no prior hidden state or KV cache,
- observations are not reintroduced downstream of the bottleneck,
- training-only target information cannot enter the context path,
- evaluation code cannot load sender-private state into a receiver.

## Semantic Target Design

Random low-level masking is insufficient. Targets must demand semantic coverage:

- complete objects or entities,
- roles and relations,
- causal links,
- future event branches,
- counterfactual outcomes,
- factors that distinguish hard negative states.

Attribute audit heads may verify coverage, but probe success alone is not a primary result.

## Human-Readable Surface

Naming is downstream work:

```text
machine code or graph motif -> stable behavioral meaning -> optional human label
```

Do not assign historical Limn words to unstable codes during primary training. A renderer must never
be presented as evidence that the predictive path used the rendered language.
