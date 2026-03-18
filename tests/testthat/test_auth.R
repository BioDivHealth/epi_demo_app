library(testthat)

test_that("check_admin_credentials accepts valid admin credentials", {
  creds <- data.frame(
    user = c("admin", "student"),
    password = c("secret", "classpass"),
    admin = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  result <- check_admin_credentials("admin", "secret", creds)

  expect_true(result$ok)
  expect_identical(result$reason, "ok")
  expect_identical(result$user, "admin")
})

test_that("check_admin_credentials rejects non-admin and missing credentials", {
  creds <- data.frame(
    user = c("admin", "student"),
    password = c("secret", "classpass"),
    admin = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  student_result <- check_admin_credentials("student", "classpass", creds)
  missing_result <- check_admin_credentials("", "", creds)

  expect_false(student_result$ok)
  expect_identical(student_result$reason, "not_admin")
  expect_false(missing_result$ok)
  expect_identical(missing_result$reason, "missing")
})
