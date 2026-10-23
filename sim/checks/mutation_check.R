# Would the invariant tests have caught the borough-order bug? ----------------
#
# Puts the old bug back IN MEMORY ONLY (population table left in alphabetical
# order, as pop_boroughs.rds is stored) and runs the invariant tests. They
# should fail. Nothing on disk is changed.
#
#   Rscript --vanilla sim/checks/mutation_check.R

.libPaths(c("C:/Users/a-davir/AppData/Local/Temp/rl", .libPaths()))
suppressMessages({ library(testthat); library(tibble); library(dplyr) })

env <- new.env()
for (f in list.files("R", full.names = TRUE)) sys.source(f, envir = env)

# the buggy loader: same as load_inputs() but without the re-ordering
correct_loader <- env$load_inputs
env$load_inputs <- function(data_dir = "data") {
  inputs <- correct_loader(data_dir)
  inputs$pop_tbl <- readRDS(file.path(data_dir, "pop_boroughs.rds"))
  inputs
}

r <- as.data.frame(test_file("tests/testthat/test_invariants.R", env = new.env(parent = env), reporter = "silent"))
cat("With the old bug put back:\n")
print(r[, c("test", "failed")])
cat(if (any(r$failed > 0)) "CAUGHT: the invariant tests fail on the old bug.\n" else "MISSED: the tests did not notice the bug.\n")
