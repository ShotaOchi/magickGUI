test_that("interactive_implode",
{
  iniv <- 0 # initial value of interactive_implode is 0
  expected <- image_implode(img, iniv)
  expect_equal(expected, interactive_implode(img))
  expect_equal(iniv, interactive_implode(img, return_param = TRUE))
})
