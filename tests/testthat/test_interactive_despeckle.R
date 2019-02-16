test_that("interactive_despeckle",
{
  iniv <- 0 # initial value of interactive_implode is 0
  names(iniv) <- "times"
  expected <- image_despeckle(img, iniv)
  expect_equal(expected, interactive_despeckle(img))
  expect_equal(iniv, interactive_despeckle(img, return_param = TRUE))
})
