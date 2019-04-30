library(testthat)
library(magickGUI)


get_minimum_version <- function()
{
  "6.9.5.4" # minimum version is written in R/misc.R file, too. Rewrite R/misc.R file if minimum version is changed.
}

version_ImageMagick <- magick_config()$version

if (version_ImageMagick >= get_minimum_version())
{
  test_check("magickGUI")
}
