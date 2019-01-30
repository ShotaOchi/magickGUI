test_that("interactive_threshold",
{
  img <- wizard
  iniv <- "0%" # initial value of interactive_threshold is "0%"
  expected <- image_threshold(img, threshold = iniv)
  expect_equal(expected, interactive_threshold(img))
  expect_equal(iniv, interactive_threshold(img, return_thr = TRUE))
})
