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