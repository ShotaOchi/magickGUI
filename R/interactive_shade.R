#' interactive shading
#'
#' Using image_shade of 'magick' interactively.
#' azimuth and elevation are parameters of image_shade. See reference manual of 'magick' for detail.
#' @param image a magick image object
#' @param color Set to true to shade the red, green, and blue components of the image
#' @param range_max_azimuth define maximum in slider of azimuth
#' @param range_min_azimuth define maximum in slider of azimuth
#' @param range_max_elevation define maximum in slider of elevation
#' @param range_min_elevation define maximum in slider of elevation
#' @param resolution resolution of slider
#' @param return_param If return_param is TRUE, returns values of azimuth and elevation. If return_param is FALSE, returns a magick image object.
#' @param scale geometry to be passed to image_scale function of magick package. image is scaled just for preview and result image is not scaled if scale is given.
#' @return a magick image object or values of azimuth and elevation
#' @author Shota Ochi
#' @export
#' @examples
#' \donttest{
#' if (interactive())
#' {
#'   interactive_shade(wizard)
#' }
#' }
interactive_shade <- function(image, color = FALSE, range_max_azimuth, range_min_azimuth, range_max_elevation, range_min_elevation, resolution = 0.1, return_param = FALSE, scale)
{
  # image must be convreted into png to avoid the error of tkimage.create function
  image_original <- image
  image <- image_convert(as.list(image)[[1]], format = "png")
  if (!missing(scale))
  {
    image <- image_scale(image, scale)
  }
  
  # set range of sliders
  iminfo <- image_info(image)
  if (missing(range_max_azimuth))
  {
    range_max_azimuth <- as.integer(max(iminfo$width, iminfo$height)) 
  }
  if (missing(range_min_azimuth))
  {
    range_min_azimuth <- -as.integer(max(iminfo$width, iminfo$height)) 
  }
  if (missing(range_max_elevation))
  {
    range_max_elevation <- as.integer(max(iminfo$width, iminfo$height)) 
  }
  if (missing(range_min_elevation))
  {
    range_min_elevation <- -as.integer(max(iminfo$width, iminfo$height)) 
  }
  if (range_max_azimuth < range_min_azimuth)
  {
    stop("range_max_azimuth is smaller than range_min_azimuth.")
  }
  if (range_max_elevation < range_min_elevation)
  {
    stop("range_max_elevation is smaller than range_min_elevation.")
  }
  
  # make initial output
  iniv_azimuth <- as.integer((range_max_azimuth + range_min_azimuth) / 2)
  iniv_elevation <- as.integer((range_max_elevation + range_min_elevation) / 2)
  initial <- image_shade(image, iniv_azimuth, iniv_elevation, color)

  # set variable range
  range_azimuth <- c(range_min_azimuth, range_max_azimuth)
  range_elevation <- c(range_min_elevation, range_max_elevation)
  length_slider <- as.integer(iminfo$width * 0.6) # length of slider
  if (length_slider < 200)
  {
    length_slider <- 200
  }
  text_label_azimuth <- "Azimuth: "
  text_label_elevation <- "Elevation: "
  quit_waiting <- !is.null(getOption("unit_test_magickGUI"))
  temp <- tempfile(fileext = ".jpg")
  on.exit(unlink(temp), add = TRUE)
  image_write(initial, temp)
  image_tcl <- tkimage.create("photo", "image_tcl", file = temp)
  label_digits <- -as.integer(log(resolution, 10))
  label_digits <- ifelse(label_digits > 0, label_digits, 0)
  label_template <- sprintf("%%.%df", label_digits)

  # configure widgets
  win1 <- tktoplevel()
  on.exit(tkdestroy(win1), add = TRUE)
  win1.frame1 <- tkframe(win1)
  win1.frame2 <- tkframe(win1)
  win1.im <- tklabel(win1, image = image_tcl)
  win1.frame1.label <- tklabel(win1.frame1, text = sprintf("%s%s", text_label_azimuth, sprintf(label_template, iniv_azimuth)))
  win1.frame2.label <- tklabel(win1.frame2, text = sprintf("%s%s", text_label_elevation, sprintf(label_template, iniv_elevation)))
  slider_value_azimuth <- tclVar(iniv_azimuth)
  slider_value_elevation <- tclVar(iniv_elevation)
  command_slider_azimuth <- function(...)
  {
    assign("slider_value_azimuth", slider_value_azimuth, inherits = TRUE)
  }
  command_slider_elevation <- function(...)
  {
    assign("slider_value_elevation", slider_value_elevation, inherits = TRUE)
  }
  win1.frame1.slider <- tkscale(win1.frame1, from = range_azimuth[1], to = range_azimuth[2], variable = slider_value_azimuth, orient = "horizontal", length = length_slider, command = command_slider_azimuth, resolution = resolution, showvalue = 0)
  win1.frame2.slider <- tkscale(win1.frame2, from = range_elevation[1], to = range_elevation[2], variable = slider_value_elevation, orient = "horizontal", length = length_slider, command = command_slider_elevation, resolution = resolution, showvalue = 0)
  temp_val <- c(iniv_azimuth, iniv_elevation)
  update_image <- function()
  {
    temp_image <- image_shade(image, temp_val[1], temp_val[2], color)
    image_write(temp_image, temp)
    image_tcl <- tkimage.create("photo", "image_tcl", file = temp)
    tkconfigure(win1.im, image = image_tcl)
  }
  command_button <- function(...)
  {
    assign("quit_waiting", TRUE, inherits = TRUE)
  }
  win1.button <- tkbutton(win1, text = "OK", command = command_button)
  tkpack(win1.im, side = "top")
  tkpack(win1.frame1.label, side = "left", anchor = "c")
  tkpack(win1.frame1.slider, side = "left", anchor = "c")
  tkpack(win1.frame1, side = "top", anchor = "c")
  tkpack(win1.frame2.label, side = "left", anchor = "c")
  tkpack(win1.frame2.slider, side = "left", anchor = "c")
  tkpack(win1.frame2, side = "top", anchor = "c")
  tkpack(win1.button, side = "top", anchor = "c", pady = 20)
  pre_slider_values <- c(as.numeric(tclvalue(slider_value_azimuth)), as.numeric(tclvalue(slider_value_elevation)))
  if (quit_waiting)
  {
    wait_test <- TRUE
    while (wait_test)
    {
      wait_test <- FALSE
      tryCatch({
        tkwm.state(win1)
      },
      error = function(e) assign("wait_test", TRUE, inherits = TRUE)
      )
    }
    wait_time_long()
    tkdestroy(win1.button)
  }
  tkwm.state(win1, "normal")
  while (TRUE)
  {
    tryCatch({
      tkwm.state(win1) 
      },
      error = function(e) assign("quit_waiting", TRUE, inherits = TRUE)
    )
    if (quit_waiting) break
    temp_val <- c(as.numeric(tclvalue(slider_value_azimuth)), as.numeric(tclvalue(slider_value_elevation)))
    if (any(temp_val != pre_slider_values))
    {
      temp_label_azimuth <- sprintf("%s%s", text_label_azimuth, sprintf(label_template, temp_val[1]))
      temp_label_elevation <- sprintf("%s%s", text_label_elevation, sprintf(label_template, temp_val[2]))
      tkconfigure(win1.frame1.label, text = temp_label_azimuth)
      tkconfigure(win1.frame2.label, text = temp_label_elevation)
      update_image()
      pre_slider_values <- temp_val
    }
  }
  val_res <- pre_slider_values
  names(val_res) <- c("azimuth", "elevation")
  if (return_param)
  {
    return(val_res)
  }
  return(image_shade(image_original, val_res[1], val_res[2], color))
}
