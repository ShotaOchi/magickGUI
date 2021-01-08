test_that("interactive_fill",
{
  skip_on_cran()
  iniv <- list(point = "+1+1", fuzz = 0)
  color <- "black"
  expected <- image_fill(img, color, iniv[[1]], iniv[[2]])
  expect_equal(expected, interactive_fill(img, color))
  expect_equal(iniv[[1]], interactive_fill(img, color, return_param = TRUE)[[1]])
  expect_equal(iniv[[2]], interactive_fill(img, color, return_param = TRUE)[[2]])
  expect_equal(expected, interactive_fill(img, color, scale = scale1))
})