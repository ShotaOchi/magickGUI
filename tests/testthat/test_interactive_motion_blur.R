test_that("interactive_motion_blur",
{
  skip_on_cran()
  iniv <- c(0,0,0)
  names(iniv) <- c("radius", "sigma", "angle")
  expected <- image_motion_blur(img)
  expect_equal(expected, interactive_motion_blur(img))
  expect_equal(iniv, interactive_motion_blur(img, return_param = TRUE))
  expect_equal(expected, interactive_motion_blur(img, scale = scale1))
})
