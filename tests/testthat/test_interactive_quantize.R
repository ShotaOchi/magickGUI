test_that("interactive_quantize",
{
  skip_on_cran()
  iniv <- 0 # initial value is 0
  names(iniv) <- "max"
  expected <- image_quantize(img, iniv)
  expect_equal(expected, interactive_quantize(img))
  expect_equal(iniv, interactive_quantize(img, return_param = TRUE))
  expect_equal(expected, interactive_quantize(img, scale = scale1))
})
