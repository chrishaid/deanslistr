context("Test error handling")

dla_key <- Sys.getenv("KEY_DLA")

test_that("deans_list_api handles errors", {
  expect_error(deanslist_api(endpoint = 'suspensions',
                     domain = 'dlacademy',
                     key = 'dla_key'),
               regexp = "Unauthorized \\(HTTP 401\\)"
               )

  expect_error(deanslist_api(endpoint = 'suspens',
                             domain = 'dlacademy',
                             key = dla_key),
              )
})