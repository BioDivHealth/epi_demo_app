<!-- Generated from cursor rule 'epidemiological-model' -->

> Epidemiological model equations and observation logic for the outbreak simulation

# Epidemiological Model Reference

## Branching process (hidden truth)
State per borough *i*, turn *t*:

```
I_{i,t+1} ~ NegBin(mean = mu_{i,t}, size = k)
mu_{i,t} = Rt * sum_j K[j, i] * I_{j,t}     (in R: crossprod(K, I) * Rt)
```

Where:
- **K** = spatial kernel. **Row j = where borough j's onward infections land**; each row sums to 1, so every case causes Rt new cases wherever it is. Built in `load_inputs()` as `within_share * I + (1 - within_share) * kernel_K.rds` (default 0.7; measles 0.8). Any replacement kernel (e.g. gravity) must use the same convention.
- **k** = overdispersion parameter (superspreading)
- **Rt** = effective reproduction number

**Imports**: Small stochastic introductions each turn:
```r
gate <- runif(n) < imports_prob
imports <- ifelse(gate, rpois(n, lambda_import), 0L)
I_next <- I_next + imports
```

Implementation: `step_truth()` in [mod truth engine.R](../../../../R/mod_truth_engine.R)

## Observation model (what students see)
Given allocations and prevalence proxy `p = I / pop`:

```
p_star = Se * p + (1 - Sp) * (1 - p)
X ~ Binomial(n_tests, p_star)
```

- **Se** = sensitivity (true positive rate)
- **Sp** = specificity (true negative rate)
- Results enter delay queues (antigen: short, PCR: longer)
- Only dequeued results are visible
- Prevalence uses the whole borough population. Scenario presets set a susceptible fraction `s0` (vaccination): `mu` is multiplied by the share still susceptible, `s0 - cumulative / pop`, so the effective R starts at `Rt × s0`.
- Symptomatic pool mode: `p = I * care_i / (pop * pool_frac_t)`, where `care_i` is a hidden per-borough care-seeking multiplier and `pool_frac_t` swells during a hidden wave of non-target respiratory illness. Both are drawn each game by `draw_game_biases()`.

Implementation: `observe_tests()` in [mod observation.R](../../../../R/mod_observation.R)

## Interventions
Defined in [interventions.R](../../../../R/interventions.R), values in `teaching_params()`:
- **Social distancing**: Rt × 0.8
- **Targeted quarantine**: imports × 0.5
- **Rapid labs**: PCR delay −1 turn

Each intervention is charged every turn it is on, has no effect for `lag` turns after switching on, then its effect is scaled by `fatigue^(turns_effective - 1)`.

## Engine
One turn of the game is `play_turn()` in [game_engine.R](../../../../R/game_engine.R): tests are drawn, found cases are isolated (and traced), then the truth steps with `I_transmit = (I - isolated) * local_multiplier`. Players and bots only see `player_view()`.

## Health-system strain
- **Hospitals:** admissions = Binomial(infections `hosp_delay` turns ago, `hosp_frac`). Beds are a London-wide pool (`teaching_params()$hospital$beds`), patients stay `stay_turns`. When occupancy exceeds beds, patients admitted without a bed die at `death_if_no_bed`, and reported admissions fall. Bed use is public.
- **Labs:** PCR beyond the testing level's capacity joins a backlog; results come back `floor((backlog + today - 1) / capacity)` turns late. Antigen is capped by kit supply. The backlog is public.
- **Isolation strain:** isolation effectiveness is divided by `1 + positives / half_strength_load` (all positives, real or false).

## Tuning
All balance values (test Se/Sp/cost/delay, budget, starter alert accuracy, symptomatic biases, intervention cost/lag/fatigue) live in [teaching_params.R](../../../../R/teaching_params.R).

## Key constraint
Analytics shown to students must **only** use returned results. Never compute Rt or positivity from hidden states.
