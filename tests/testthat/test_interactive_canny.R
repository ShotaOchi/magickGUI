test_that("interactive_canny",
{
  skip_on_cran()
  iniv <- "0x1+10%+30%" # initial value
  expected <- image_canny(img, iniv)
  expect_equal(expected, interactive_canny(img))
  expect_equal(iniv, interactive_canny(img, return_param = TRUE))
  expect_equal(expected, interactive_canny(img, scale = scale1))
})
