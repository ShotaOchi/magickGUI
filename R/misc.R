#' magickGUI: GUI tools for interactive image processing with 'magick'
#'
#' magickGUI enables us to use the functions of the package 'magick' interactively.
#' @docType package
#' @name magickGUI
NULL

#' @importFrom graphics plot
#' @importFrom magick image_blur
#' @importFrom magick image_charcoal
#' @importFrom magick image_despeckle
#' @importFrom magick image_emboss
#' @importFrom magick image_implode
#' @importFrom magick image_info
#' @importFrom magick image_modulate
#' @importFrom magick image_oilpaint
#' @importFrom magick image_quantize
#' @importFrom magick image_reducenoise
#' @importFrom magick image_threshold
#' @importFrom magick image_write
#' @importFrom tcltk tclvalue
#' @importFrom tcltk tclVar
#' @importFrom tcltk tkbutton
#' @importFrom tcltk tkconfigure
#' @importFrom tcltk tkdestroy
#' @importFrom tcltk tkframe
#' @importFrom tcltk tkimage.create
#' @importFrom tcltk tklabel
#' @importFrom tcltk tkpack
#' @importFrom tcltk tkscale
#' @importFrom tcltk tktoplevel
#' @importFrom tcltk tkwm.state
NULL

wait_time <- function()
{
  wait_start <- proc.time()[3]
  wait_time <- 0.2 # sec
  while (proc.time()[3] - wait_start < wait_time) {}
}

wait_time_long <- function()
{
  wait_start <- proc.time()[3]
  wait_time <- 1 # sec
  while (proc.time()[3] - wait_start < wait_time) {}
}
