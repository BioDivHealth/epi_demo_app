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

is_blank_auth_value <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(TRUE)
  }

  value <- trimws(as.character(value[[1]]))
  is.na(value) || identical(value, "")
}

check_admin_credentials <- function(user, password, creds) {
  if (is_blank_auth_value(user) || is_blank_auth_value(password)) {
    return(list(
      ok = FALSE,
      reason = "missing",
      user = NULL,
      user_info = NULL
    ))
  }

  if (is.null(creds)) {
    return(list(
      ok = FALSE,
      reason = "auth_disabled",
      user = NULL,
      user_info = NULL
    ))
  }

  user <- trimws(as.character(user[[1]]))
  password <- as.character(password[[1]])
  auth <- shinymanager::check_credentials(creds)(user, password)

  if (!isTRUE(auth$result)) {
    reason <- "invalid"
    if (isTRUE(auth$expired)) {
      reason <- "expired"
    } else if (!isTRUE(auth$authorized)) {
      reason <- "unauthorized"
    }

    return(list(
      ok = FALSE,
      reason = reason,
      user = user,
      user_info = auth$user_info
    ))
  }

  admin_flag <- FALSE
  if (!is.null(auth$user_info) && "admin" %in% names(auth$user_info)) {
    admin_flag <- isTRUE(as.logical(auth$user_info$admin[[1]]))
  }

  if (!admin_flag) {
    return(list(
      ok = FALSE,
      reason = "not_admin",
      user = user,
      user_info = auth$user_info
    ))
  }

  list(
    ok = TRUE,
    reason = "ok",
    user = user,
    user_info = auth$user_info
  )
}

admin_auth_failure_message <- function(reason) {
  switch(
    reason,
    missing = "Enter both username and password.",
    expired = "That admin account has expired.",
    not_admin = "That account is not allowed to open admin controls.",
    unauthorized = "That account is not authorized for this app.",
    auth_disabled = "Admin login is disabled for this run.",
    "Incorrect admin username or password."
  )
}
