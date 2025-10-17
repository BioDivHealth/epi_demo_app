# prepare_data.R (London boroughs + population + adjacency + kernel)
# Outputs:
#   data/boroughs.rds (sf, WGS84), data/boroughs_planar.rds (sf, EPSG:27700 if available)
#   data/adjacency_nb.rds, data/adjacency_matrix.rds, data/kernel_K.rds
#   data/pop_boroughs.rds (gss_code, borough_name, pop)

# ---- packages ----
req <- c("sf","dplyr","readr","stringr","spdep","tibble","purrr")
new <- setdiff(req, rownames(installed.packages()))
if(length(new)) install.packages(new, quiet = TRUE)
invisible(lapply(req, library, character.only = TRUE))

# ---- helpers ----
here_file <- function(fname){
  cand <- c(file.path("data_raw", fname), fname)
  cand[which(file.exists(cand))[1]] %||% stop("File not found: ", fname, call.=FALSE)
}
`%||%` <- function(x,y) if(is.null(x) || length(x)==0 || is.na(x)) y else x

canon_name <- function(x){
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("&","and") |>
    stringr::str_replace_all("[^a-z0-9]+"," ") |>
    stringr::str_squish()
}

detect_first_col_ci <- function(x, candidates){
  nms <- names(x)
  idx <- match(tolower(candidates), tolower(nms))
  if(any(!is.na(idx))) nms[na.omit(idx)[1]] else NA_character_
}

# ---- input paths ----
gpkg_path <- here_file("London_Boroughs.gpkg")
pop_path  <- here_file("LDN_POP.csv")
message("Using:\n  - ", gpkg_path, "\n  - ", pop_path)

# ---- read polygons ----
boros_raw <- sf::st_read(gpkg_path, quiet = TRUE)

# detect columns (case-insensitive)
name_candidates <- c("name","borough","boro_name","lad_name","lad23nm","lad22nm","lad21nm","lad20nm")
code_candidates <- c("gss_code","lad23cd","lad22cd","lad21cd","lad20cd","lad19cd","ladcd","code")

nm_col <- detect_first_col_ci(boros_raw, name_candidates)
cd_col <- detect_first_col_ci(boros_raw, code_candidates)

# fallback: regex-detect any E09xxxxx column if code missing
if(is.na(cd_col)){
  maybe <- purrr::keep(names(boros_raw), function(col){
    v <- boros_raw[[col]]
    is.character(v) && any(grepl("^E09\\d{5}$", v))
  })
  cd_col <- maybe %||% NA_character_
}

if(is.na(nm_col) || is.na(cd_col)){
  stop("Could not detect borough name/code columns. ",
       "Detected name='", nm_col, "', code='", cd_col,
       "'. Columns present: ", paste(names(boros_raw), collapse=", "))
}

boros <- boros_raw |>
  dplyr::rename(borough_name = !!nm_col, gss_code = !!cd_col) |>
  dplyr::mutate(
    borough_name = as.character(borough_name),
    gss_code     = as.character(gss_code),
    name_key     = canon_name(borough_name)
  )

# planar copy for adjacency (prefer EPSG:27700)
crs_epsg <- tryCatch(sf::st_crs(boros)$epsg, error = function(e) NA_integer_)
boros_planar <- tryCatch({
  if(!is.na(crs_epsg) && crs_epsg == 27700) boros else sf::st_transform(boros, 27700)
}, error = function(e) boros)

# ---- read population CSV ----
# Expected cols include: Code, Name, Geography, All ages (others ignored)
pop_raw <- readr::read_csv(pop_path, show_col_types = FALSE,
                           locale = readr::locale(grouping_mark = ","))

# Keep London Borough rows; some files include extra geographies
pop_boros <- pop_raw |>
  dplyr::filter(Geography == "London Borough" | grepl("^E09\\d{5}$", Code)) |>
  dplyr::transmute(
    gss_code     = as.character(Code),
    borough_name = as.character(Name),
    all_ages     = as.numeric(`All ages`),
    name_key     = canon_name(borough_name)
  )

# ---- join populations (prefer code, fallback name_key) ----
joined <- boros |>
  dplyr::left_join(pop_boros |> dplyr::select(gss_code, all_ages), by = "gss_code") |>
  dplyr::left_join(pop_boros |> dplyr::select(name_key, all_ages_name = all_ages), by = "name_key") |>
  dplyr::mutate(pop = dplyr::coalesce(all_ages, all_ages_name)) |>
  dplyr::select(-all_ages, -all_ages_name)

miss <- joined |> dplyr::filter(is.na(pop)) |> dplyr::pull(borough_name)
if(length(miss)){
  warning("Population missing for: ", paste(miss, collapse=", "),
          "\nCheck name spellings or Code in LDN_POP.csv.")
}

# ---- adjacency & kernel ----
nb <- spdep::poly2nb(sf::st_geometry(boros_planar), queen = TRUE)
A  <- spdep::nb2mat(nb, style = "B", zero.policy = TRUE)  # binary adjacency
row_sums <- rowSums(A)
is_island <- row_sums == 0
if(any(is_island)){
  diag(A)[is_island] <- 1
  row_sums <- rowSums(A)
}
K <- A / row_sums  # row-normalised kernel

# ---- save outputs ----
dir.create("data", showWarnings = FALSE)

boros_wgs84 <- tryCatch(sf::st_transform(joined, 4326), error = function(e) joined)
saveRDS(boros_wgs84, file = file.path("data","boroughs.rds"))
saveRDS(boros_planar, file = file.path("data","boroughs_planar.rds"))
saveRDS(nb,           file = file.path("data","adjacency_nb.rds"))
saveRDS(A,            file = file.path("data","adjacency_matrix.rds"))
saveRDS(K,            file = file.path("data","kernel_K.rds"))

pop_out <- boros_wgs84 |>
  sf::st_drop_geometry() |>
  dplyr::select(gss_code, borough_name, pop) |>
  dplyr::arrange(borough_name)
saveRDS(pop_out, file = file.path("data","pop_boroughs.rds"))

message("Done.\n",
        " • data/boroughs.rds (", nrow(boros_wgs84), " features)\n",
        " • data/adjacency_matrix.rds (", nrow(A), "x", ncol(A), ")\n",
        " • data/kernel_K.rds (row-normalised)\n",
        " • data/pop_boroughs.rds")

invisible(TRUE)
