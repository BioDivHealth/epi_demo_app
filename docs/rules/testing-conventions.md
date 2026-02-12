<!-- Generated from cursor rule 'testing-conventions' -->

# Testing Conventions

## Test framework
- Uses `testthat` (edition 3) with `devtools::test()`
- Test files in `tests/testthat/` named `test_*.R`

## Running tests
```r
devtools::test()
# or from terminal:
R -q -e 'devtools::test()'
```

## Test coverage
| File | Purpose |
|------|---------|
| [test adjacency.R](../../../../tests/testthat/test_adjacency.R) | A and K are square; `rowSums(K) == 1`; `length(nb) == 33`; measles kernel checked when present |
| [test branching.R](../../../../tests/testthat/test_branching.R) | `step_truth()` returns non-negative integers; mean growth tracks Rt |
| [test observation.R](../../../../tests/testthat/test_observation.R) | p* behaves at extremes (Se/Sp in {0,1}); delay queues advance correctly |
| [test interventions.R](../../../../tests/testthat/test_interventions.R) | Intervention effects compute correctly |
| [test starter intel.R](../../../../tests/testthat/test_starter_intel.R) | Starter intel allocations are valid |
| [test logging.R](../../../../tests/testthat/test_logging.R) | Turn logging utilities work correctly |
| [test reporting.R](../../../../tests/testthat/test_reporting.R) | Run report generation works |
| [test app.R](../../../../tests/testthat/test_app.R) | App launches; key UI elements exist |

## Test fixtures
- Create fixtures in `tests/fixtures/`, **never** in `data/`
- Use `withr::with_seed()` for reproducible random tests
