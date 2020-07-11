test_that("interactive_crop",
{
  iminfo <- image_info(img)
  iniwidth <- as.integer(iminfo["width"] / 2) + 1
  iniheight <- as.integer(iminfo["height"] / 2) + 1
  iniv <- geometry_area(iniwidth, iniheight)
  expected <- image_crop(img, iniv)
  expect_equal(expected, interactive_crop(img))
  expect_equal(iniv, interactive_crop(img, return_param = TRUE))
  expect_equal(expected, interactive_crop(img, scale = scale1))
  expect_error(interactive_crop(img, color = "none"))
})
