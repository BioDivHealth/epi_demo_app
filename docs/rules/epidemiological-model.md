<!-- Generated from cursor rule 'epidemiological-model' -->

> Epidemiological model equations and observation logic for the outbreak simulation

# Epidemiological Model Reference

## Branching process (hidden truth)
State per borough *i*, turn *t*:

```
I_{i,t+1} ~ NegBin(mean = mu_{i,t}, size = k)
mu_{i,t} = Rt^g * sum_j K[j, i] * I_{j,t}   (in R: crossprod(K, I) * Rt^g)
```

Where:
- **g** = generations per turn = `report_days / gen_days` (`config$gens_per_turn`). One turn is one report (7 days by default) for every disease, so a turn spans g generations and every per-generation multiplier (Rt, susceptible share, local restrictions, intervention effects) is raised to g. Growth per day is unchanged: log(Rt^g) / report_days = log(Rt) / gen_days. g = 1 is the old one-generation step.
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
- **Social distancing**: Rt × 0.5 (fades by `fatigue` each week it runs)
- **Contact tracing**: follows up test positives and reported hospital admissions, up to `capacity` a week; an admission's contacts are discounted by `delay_decay^(hosp_delay × g)`
- **Build better testing**: moves up one testing level (reference lab → hospital labs → rapid antigen tests). No disease starts with antigen; PCR results always come back at the next report.

Each intervention is charged every turn it is on, has no effect for `lag` turns after switching on, then its effect is scaled by `fatigue^(turns_effective - 1)`.

## Engine
One turn of the game is `play_turn()` in [game_engine.R](../../../../R/game_engine.R): tests are drawn, found cases are isolated (and traced), then the truth steps with `I_transmit = (I - isolated) * local_multiplier`. Players and bots only see `player_view()`.

## Hidden start, first patient and calendar
- `new_game()` starts the outbreak from one infected person and runs it unseen until it is noticed: `head_start$admissions` hospital admissions in total (Binomial(infections `hosp_delay` reports earlier, `hosp_frac`)), or the first death. An outbreak that dies out before that is redrawn (it would never have been reported). The first report comes from the area with the most admissions (or the death). The scenario's `seed_cases` is no longer used.
- The first patient is reported from an area chosen in proportion to current infections (a real case, not necessarily where it began). There are no free tests.
- Scoring counts infections and deaths during the response only (`start_infections`, `start_deaths_eventual`).
- One turn is one report, `teaching_params()$report_days` apart (7 = weekly) for every disease. Hospital and death delays are set in days per disease (`hosp_delay_days`, `death_delay_days` in scenarios.R) and rounded to whole reports. Isolation value lost per turn of result delay is `delay_decay^(delay * g)` (delay_decay is per generation). The dashboard's Rt is the per-report growth estimate raised to 1/g, i.e. per generation. Day 0 is `teaching_params()$start_date`; see `turn_day()` / `turn_date()`.

## Health-system strain
- **Hospitals:** admissions = Binomial(infections `hosp_delay` turns ago, `hosp_frac`). Beds are a London-wide pool (`teaching_params()$hospital$beds`), patients stay `stay_turns`. When occupancy exceeds beds, patients admitted without a bed die at `death_if_no_bed`, and reported admissions fall. Bed use is public.
- **Labs:** PCR beyond the testing level's capacity joins a backlog; results come back `floor((backlog + today - 1) / capacity)` turns late. Antigen is capped by kit supply. The backlog is public.
- **Isolation strain:** isolation effectiveness is divided by `1 + positives / half_strength_load` (all positives, real or false).

## Tuning
All balance values (test Se/Sp/cost/delay, budget, starter alert accuracy, symptomatic biases, intervention cost/lag/fatigue) live in [teaching_params.R](../../../../R/teaching_params.R).

## Key constraint
Analytics shown to students must **only** use returned results. Never compute Rt or positivity from hidden states.
