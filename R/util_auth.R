# Auth utilities ---------------------------------------------------------------

load_credentials <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf(
      "Auth credentials file not found at '%s'. Create it before running the app.",
      path
    ))
  }

  creds <- utils::read.csv(path, stringsAsFactors = FALSE)
  required <- c("user", "password")
  missing <- setdiff(required, names(creds))
  if (length(missing) > 0) {
    stop(sprintf(
      "Auth credentials missing required columns: %s",
      paste(missing, collapse = ", ")
    ))
  }

  if (!"admin" %in% names(creds)) {
    creds$admin <- FALSE
  }
  creds$admin <- as.logical(creds$admin)
  creds
}

get_admin_users <- function(creds) {
  if (is.null(creds) || !("user" %in% names(creds))) {
    return(character())
  }
  admins <- rep(FALSE, nrow(creds))
  if ("admin" %in% names(creds)) {
    admins <- as.logical(creds$admin)
    admins[is.na(admins)] <- FALSE
  }
  creds$user[admins]
}
