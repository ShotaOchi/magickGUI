#' magickGUI: GUI tools for interactive image processing with 'magick'
#'
#' magickGUI enables us to use the functions of the package 'magick' interactively.
#' @docType package
#' @name magickGUI
NULL

#' @importFrom graphics plot
#' @importFrom magick geometry_area
#' @importFrom magick geometry_point
#' @importFrom magick geometry_size_percent
#' @importFrom magick image_annotate
#' @importFrom magick image_blank
#' @importFrom magick image_blur
#' @importFrom magick image_canny
#' @importFrom magick image_charcoal
#' @importFrom magick image_composite
#' @importFrom magick image_convert
#' @importFrom magick image_crop
#' @importFrom magick image_despeckle
#' @importFrom magick image_emboss
#' @importFrom magick image_fill
#' @importFrom magick image_implode
#' @importFrom magick image_info
#' @importFrom magick image_modulate
#' @importFrom magick image_oilpaint
#' @importFrom magick image_quantize
#' @importFrom magick image_reducenoise
#' @importFrom magick image_scale
#' @importFrom magick image_shade
#' @importFrom magick image_threshold
#' @importFrom magick image_write
#' @importFrom magick magick_config
#' @importFrom tcltk tclvalue
#' @importFrom tcltk tclVar
#' @importFrom tcltk tkbutton
#' @importFrom tcltk tkconfigure
#' @importFrom tcltk tkdestroy
#' @importFrom tcltk tkframe
#' @importFrom tcltk tkimage.create
#' @importFrom tcltk tkinvoke
#' @importFrom tcltk tklabel
#' @importFrom tcltk tkpack
#' @importFrom tcltk tkscale
#' @importFrom tcltk tktoplevel
#' @importFrom tcltk tkwm.state
NULL

wait_time <- function()
{
  wait_start <- proc.time()[3]
  wait_time <- 0.5 # sec
  while (proc.time()[3] - wait_start < wait_time) {}
}

wait_time_long <- function()
{
  wait_start <- proc.time()[3]
  wait_time <- 1.0 # sec
  while (proc.time()[3] - wait_start < wait_time) {}
}

get_minimum_version <- function()
{
  "6.9.5.4" # minimum version is written in test/testthat.R file, too. Rewrite test/testthat.R file if minimum version is changed.
}

.onAttach <- function(lib, pkg){
  version_ImageMagick <- magick_config()$version
  if (version_ImageMagick < get_minimum_version())
  {
    packageStartupMessage(sprintf("The version of ImageMagick is %s.\nThe version of ImageMagick should be greater than or equal to %s.", version_ImageMagick, get_minimum_version()))
  }
}

geometry_canny_magickGUI <- function(radius, sigma, lower, upper)
{
  paste(paste(as.character(radius), as.character(sigma), sep = "x"), geometry_size_percent(lower), geometry_size_percent(upper), sep = "+")
}
