test_that("interactive_composite",
{
  skip_on_cran()
  iniv <- "+0+0" # initial value is "0%"
  expected <- image_composite(img, img2, offset = iniv)
  expect_equal(expected, interactive_composite(img, img2))
  expect_equal(iniv, interactive_composite(img, img2, return_param = TRUE))
})
