<!-- Generated from cursor rule 'epidemiological-model' -->

> Epidemiological model equations and observation logic for the outbreak simulation

# Epidemiological Model Reference

## Branching process (hidden truth)
State per borough *i*, turn *t*:

```
I_{i,t+1} ~ NegBin(mean = mu_{i,t}, size = k)
mu_{i,t} = (K %*% I_t)[i] * Rt
```

Where:
- **K** = row-normalised spatial kernel (each row sums to 1), from `data/kernel_K.rds`
- Scenario variants may swap to an alternate kernel (e.g., `data/kernel_K_measles.rds`) without changing the equation.
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
- Scenario presets may apply a susceptible fraction by scaling the population `pop` used for prevalence.

Implementation: `observe_tests()` in [mod observation.R](../../../../R/mod_observation.R)

## Interventions
Defined in [interventions.R](../../../../R/interventions.R):
- **Social distancing**: Rt × 0.8
- **Targeted quarantine**: imports × 0.5
- **Rapid labs**: PCR delay −1 turn

## Key constraint
Analytics shown to students must **only** use returned results. Never compute Rt or positivity from hidden states.
