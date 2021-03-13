test_that("interactive_shade",
{
  skip_on_cran()
  iniv <- c(0,0)
  names(iniv) <- c("azimuth", "elevation")
  expected <- image_shade(img, iniv[1], iniv[2])
  expect_equal(expected, interactive_shade(img))
  expect_equal(iniv, interactive_shade(img, return_param = TRUE))
  expect_equal(expected, interactive_shade(img, scale = scale1))
  expect_error(interactive_shade(img, range_max_azimuth = -1, range_min_azimuth = 1))
  expect_error(interactive_shade(img, range_max_elevation = -1, range_min_elevation = 1))
})
