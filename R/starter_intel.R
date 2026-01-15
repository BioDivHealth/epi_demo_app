# Starter intel helpers ------------------------------------------------------

starter_intel_allocations <- function(option = "none", truth_state, pop_tbl) {
  n_b <- length(truth_state$incidence)
  base <- tibble::tibble(
    borough_index = seq_len(n_b),
    borough_name = pop_tbl$borough_name,
    n_ag = integer(n_b),
    n_pcr = integer(n_b)
  )

  if (option == "none") {
    return(base)
  }

  if (option == "alert") {
    candidate <- which.max(truth_state$incidence)
    if (truth_state$incidence[candidate] == 0) {
      candidate <- sample.int(n_b, 1L)
    }
    base$n_ag[candidate] <- 200L
    return(base)
  }

  if (option == "sweep") {
    n_targets <- min(4L, n_b)
    positive_idx <- which(truth_state$incidence > 0)
    targets <- if (length(positive_idx) >= n_targets) {
      sample(positive_idx, n_targets)
    } else {
      sample(seq_len(n_b), n_targets)
    }
    base$n_ag[targets] <- 25L
    base$n_pcr[targets] <- 10L
    return(base)
  }

  base
}

starter_intel_label <- function(option) {
  switch(
    option,
    alert = "Starter intel: targeted alert",
    sweep = "Starter intel: wider screening",
    ""
  )
}
