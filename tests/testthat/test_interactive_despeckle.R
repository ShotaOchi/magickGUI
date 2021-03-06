test_that("interactive_despeckle",
{
  skip_on_cran()
  iniv <- 0 # initial value is 0
  names(iniv) <- "times"
  expected <- image_despeckle(img, iniv)
  expect_equal(expected, interactive_despeckle(img))
  expect_equal(iniv, interactive_despeckle(img, return_param = TRUE))
  expect_equal(expected, interactive_despeckle(img, scale = scale1))
})
