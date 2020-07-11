test_that("interactive_annotate",
{
  inix <- 0
  iniy <- 0
  inidegrees <- 0
  inisize <- 10
  iniweight <- 400
  inikerning <- 0
  txt <- "hello"
  iniv <- list(location = geometry_point(inix, iniy), degrees = inidegrees, size = inisize, weight = iniweight, kerning = inikerning)
  expected <- image_annotate(img, txt, location = geometry_point(inix, iniy), degrees = inidegrees, size = inisize, weight = iniweight, kerning = inikerning)
  expect_equal(expected, interactive_annotate(img, txt))
  expect_equal(iniv, interactive_annotate(img, txt, return_param = TRUE))
  expect_equal(expected, interactive_annotate(img, txt, scale = scale1))
})
