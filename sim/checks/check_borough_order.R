# Do the data files list the boroughs in the same order? -----------------------
#
# The engine indexes populations, kernel K and adjacency A by position, so
# they must all be in the same borough order.
#
#   Rscript --vanilla sim/checks/check_borough_order.R

suppressMessages({ library(sf) })

boros <- readRDS("data/boroughs.rds")
pop_tbl <- readRDS("data/pop_boroughs.rds")
K <- readRDS("data/kernel_K.rds")
A <- readRDS("data/adjacency_matrix.rds")
K_measles <- readRDS("data/kernel_K_measles.rds")

cat("boroughs.rds columns:", paste(names(boros), collapse = ", "), "\n")
cat("pop_boroughs.rds columns:", paste(names(pop_tbl), collapse = ", "), "\n")
cat("K dimnames present:", !is.null(rownames(K)), " A dimnames present:", !is.null(rownames(A)), "\n\n")

order_tbl <- data.frame(
  row = seq_len(nrow(boros)),
  boros_name = boros$borough_name,
  pop_name = pop_tbl$borough_name,
  K_row = if (!is.null(rownames(K))) rownames(K) else NA,
  A_row = if (!is.null(rownames(A))) rownames(A) else NA
)
print(order_tbl)

cat("\nboros vs pop names match by position:", all(order_tbl$boros_name == order_tbl$pop_name), "\n")
if (!is.null(rownames(K))) cat("K rows vs boros match by position:", all(rownames(K) == boros$borough_name | rownames(K) == boros$gss_code), "\n")
if (!is.null(rownames(A))) cat("A rows vs boros match by position:", all(rownames(A) == boros$borough_name | rownames(A) == boros$gss_code), "\n")

# sanity check that does not rely on names: neighbours in A should touch on the map
touches <- sf::st_touches(sf::st_make_valid(boros), sparse = FALSE)
cat("A agrees with which boroughs.rds polygons touch:", mean(A == touches) , "share of cells\n")
