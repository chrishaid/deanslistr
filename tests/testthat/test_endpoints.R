context("Test that all endpoints are available")

dla_key <- Sys.getenv("KEY_DLA")

test_that("suspensions endpoint returns data", {
  skip_on_cran()
  x <- deanslist_api(endpoint = 'suspensions',
                     domain = 'dlacademy',
                     key = dla_key)

  expect_s3_class(x, "deanslist_api")
  expect_is(x$content$data, "data.frame")

})


test_that("behavior endpoint returns data", {
  skip_on_cran()
  x <- deanslist_api(endpoint = 'behavior',
                     domain = 'dlacademy',
                     key = dla_key,
                     endpoint_version = 'beta')

  expect_s3_class(x, "deanslist_api")
  expect_is(x$content, "list")

})


test_that("pointbank endpoint returns data", {
  skip_on_cran()
  x <- deanslist_api(endpoint = 'pointbank',
                     domain = 'dlacademy',
                     key = dla_key,
                     endpoint_version = 'beta')

  expect_s3_class(x, "deanslist_api")
  expect_is(x$content, "list")

})

test_that("users endpoint returns data", {
  skip_on_cran()
  x <- deanslist_api(endpoint = 'users',
                     domain = 'dlacademy',
                     key = dla_key,
                     endpoint_version = 'beta')

  expect_s3_class(x, "deanslist_api")
  expect_is(x$content$data, "data.frame")

})


test_that("students beta endpoint returns data", {
  skip_on_cran()
  x <- deanslist_api(endpoint = 'students',
                     domain = 'dlacademy',
                     key = dla_key,
                     endpoint_version = 'beta')

  expect_s3_class(x, "deanslist_api")
  expect_is(x$content$data, "data.frame")

})


test_that("roster-assignments endpoint returns data", {
  skip_on_cran()
  x <- deanslist_api(endpoint = 'roster-assignments',
                     domain = 'dlacademy',
                     key = dla_key,
                     endpoint_version = 'beta')

  expect_s3_class(x, "deanslist_api")
  expect_is(x$content$data, "data.frame")

})


test_that("students (v1) endpoint returns data", {
  skip_on_cran()
  x <- deanslist_api(endpoint = 'students',
                     domain = 'dlacademy',
                     key = dla_key,
                     endpoint_version = 'v1')

  expect_s3_class(x, "deanslist_api")
  expect_is(x$content$data, "data.frame")

})

test_that("referrals endpoint returns data", {
  skip_on_cran()
  x <- deanslist_api(endpoint = 'referrals',
                     domain = 'dlacademy',
                     key = dla_key,
                     endpoint_version = 'v1')

  expect_s3_class(x, "deanslist_api")
  expect_is(x$content$data, "data.frame")

})

test_that("incidents endpoint returns data", {
  skip_on_cran()
  x <- deanslist_api(endpoint = 'incidents',
                     domain = 'dlacademy',
                     key = dla_key,
                     endpoint_version = 'v1')

  expect_s3_class(x, "deanslist_api")
  expect_is(x$content$data, "data.frame")

})

test_that("followups endpoint returns data", {
  skip_on_cran()
  x <- deanslist_api(endpoint = 'followups',
                     domain = 'dlacademy',
                     key = dla_key,
                     endpoint_version = 'v1')

  expect_s3_class(x, "deanslist_api")
  expect_is(x$content$data, "data.frame")

})


test_that("lists endpoints returns data", {
  skip_on_cran()
  x <- deanslist_api(endpoint = 'lists',
                     domain = 'dlacademy',
                     key = dla_key,
                     endpoint_version = 'v1')

  expect_s3_class(x, "deanslist_api")
  expect_is(x$content$data, "data.frame")

  list_id <- x$content$data$ListID
  x <- deanslist_api(endpoint = sprintf('lists/%s', list_id[[1]]),
                     domain = 'dlacademy',
                     key = dla_key,
                     endpoint_version = 'v1')

  expect_s3_class(x, "deanslist_api")
  expect_is(x$content$Sessions, "data.frame")
})

test_that("terms endpoint returns data", {
  skip_on_cran()
  x <- deanslist_api(endpoint = 'terms',
                     domain = 'dlacademy',
                     key = dla_key,
                     endpoint_version = 'v1')

  expect_s3_class(x, "deanslist_api")
  expect_is(x$content$data, "data.frame")

})

test_that("daily-attendance endpoint returns data", {
  skip_on_cran()
  x <- deanslist_api(endpoint = 'daily-attendance',
                     domain = 'dlacademy',
                     key = dla_key,
                     endpoint_version = 'v1')

  expect_s3_class(x, "deanslist_api")
  expect_is(x$content$data, "list")

})

test_that("class-attendance endpoint returns data", {
  skip_on_cran()
  x <- deanslist_api(endpoint = 'class-attendance',
                     domain = 'dlacademy',
                     key = dla_key,
                     endpoint_version = 'v1')

  expect_s3_class(x, "deanslist_api")
  expect_is(x$content$data, "list")

})

test_that("rosters endpoint returns data", {
  skip_on_cran()
  x <- deanslist_api(endpoint = 'rosters',
                     domain = 'dlacademy',
                     key = dla_key,
                     endpoint_version = 'v1')

  expect_s3_class(x, "deanslist_api")
  expect_is(x$content$data, "data.frame")

  roster_id <- x$content$data$RosterID
  x <- deanslist_api(endpoint = sprintf('rosters/%s', roster_id[[2]]),
                     domain = 'dlacademy',
                     key = dla_key,
                     endpoint_version = 'v1')

  expect_s3_class(x, "deanslist_api")
  expect_is(x$content$Students, "data.frame")

})
