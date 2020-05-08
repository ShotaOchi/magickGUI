#' interactive canny edge detection
#'
#' Using image_canny of 'magick' interactively.
#' radius, sigma, lower\%, and upper\% are parameters of image_canny. See reference manual of 'magick' for detail.
#' @param image a magick image object
#' @param range_max_radius define maximum in slider of radius. must be positive.
#' @param range_max_sigma define maximum in slider of sigma. must be positive.
#' @param resolution resolution of slider
#' @param return_param If return_param is TRUE, returns values of radius, sigma, lower\%, and upper\% represented in the format of 'magick'. If return_param is FALSE, returns a magick image object.
#' @return a magick image object or values of radius, sigma, lower\%, and upper\% represented in the format of 'magick'
#' @author Shota Ochi
#' @export
#' @examples
#' \dontrun{
#' interactive_canny(wizard)
#' }
interactive_canny <- function(image, range_max_radius = 30, range_max_sigma = 2, resolution = 0.1, return_param = FALSE)
{
  # make initial output
  iniv_radius <- 0
  iniv_sigma <- 1
  iniv_lower <- 10
  iniv_upper <- 30
  initial <- image_canny(image, geometry_canny_magickGUI(iniv_radius, iniv_sigma, iniv_lower, iniv_upper))

  # set variable range
  iminfo <- image_info(image)
  range_radius <- c(0, range_max_radius)
  range_sigma <- c(0, range_max_sigma)
  range_lower <- c(0, 100)
  range_upper <- c(0, 100)
  length_slider <- as.integer(iminfo$width * 0.6) # length of slider
  if (length_slider < 200)
  {
    length_slider <- 200
  }
  text_label_radius <- "radius: "
  text_label_sigma <- "sigma: "
  text_label_lower <- "lower%: "
  text_label_upper <- "upper%: "
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
  win1.frame3 <- tkframe(win1)
  win1.frame4 <- tkframe(win1)
  win1.im <- tklabel(win1, image = image_tcl)
  win1.frame1.label <- tklabel(win1.frame1, text = sprintf("%s%s", text_label_radius, sprintf(label_template, iniv_radius)))
  win1.frame2.label <- tklabel(win1.frame2, text = sprintf("%s%s", text_label_sigma, sprintf(label_template, iniv_sigma)))
  win1.frame3.label <- tklabel(win1.frame3, text = sprintf("%s%s", text_label_lower, sprintf(label_template, iniv_lower)))
  win1.frame4.label <- tklabel(win1.frame4, text = sprintf("%s%s", text_label_upper, sprintf(label_template, iniv_upper)))
  slider_value_radius <- tclVar(iniv_radius)
  slider_value_sigma <- tclVar(iniv_sigma)
  slider_value_lower <- tclVar(iniv_lower)
  slider_value_upper <- tclVar(iniv_upper)
  command_slider_radius <- function(...)
  {
    assign("slider_value_radius", slider_value_radius, inherits = TRUE)
  }
  command_slider_sigma <- function(...)
  {
    assign("slider_value_sigma", slider_value_sigma, inherits = TRUE)
  }
  command_slider_lower <- function(...)
  {
    assign("slider_value_lower", slider_value_lower, inherits = TRUE)
  }
  command_slider_upper <- function(...)
  {
    assign("slider_value_upper", slider_value_upper, inherits = TRUE)
  }
  win1.frame1.slider <- tkscale(win1.frame1, from = range_radius[1], to = range_radius[2], variable = slider_value_radius, orient = "horizontal", length = length_slider, command = command_slider_radius, resolution = resolution, showvalue = 0)
  win1.frame2.slider <- tkscale(win1.frame2, from = range_sigma[1], to = range_sigma[2], variable = slider_value_sigma, orient = "horizontal", length = length_slider, command = command_slider_sigma, resolution = resolution, showvalue = 0)
  win1.frame3.slider <- tkscale(win1.frame3, from = range_lower[1], to = range_lower[2], variable = slider_value_lower, orient = "horizontal", length = length_slider, command = command_slider_lower, resolution = resolution, showvalue = 0)
  win1.frame4.slider <- tkscale(win1.frame4, from = range_upper[1], to = range_upper[2], variable = slider_value_upper, orient = "horizontal", length = length_slider, command = command_slider_upper, resolution = resolution, showvalue = 0)
  temp_val <- c(iniv_radius, iniv_sigma, iniv_lower, iniv_upper)
  update_image <- function()
  {
    temp_image <- image_canny(image, geometry_canny_magickGUI(temp_val[1], temp_val[2], temp_val[3], temp_val[4]))
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
  tkpack(win1.frame3.label, side = "left", anchor = "c")
  tkpack(win1.frame3.slider, side = "left", anchor = "c")
  tkpack(win1.frame3, side = "top", anchor = "c")
  tkpack(win1.frame4.label, side = "left", anchor = "c")
  tkpack(win1.frame4.slider, side = "left", anchor = "c")
  tkpack(win1.frame4, side = "top", anchor = "c")
  tkpack(win1.button, side = "top", anchor = "c", pady = 20)
  pre_slider_values <- c(as.numeric(tclvalue(slider_value_radius)), as.numeric(tclvalue(slider_value_sigma)), as.numeric(tclvalue(slider_value_lower)), as.numeric(tclvalue(slider_value_upper)))
  if (quit_waiting)
  {
    wait_test <- TRUE
    while (wait_test)
    {
      wait_test <- FALSE
      tryCatch({
        tkinvoke(win1.button)
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
    temp_val <- c(as.numeric(tclvalue(slider_value_radius)), as.numeric(tclvalue(slider_value_sigma)), as.numeric(tclvalue(slider_value_lower)), as.numeric(tclvalue(slider_value_upper)))
    if (any(temp_val != pre_slider_values))
    {
      temp_label_radius <- sprintf("%s%s", text_label_radius, sprintf(label_template, temp_val[1]))
      temp_label_sigma <- sprintf("%s%s", text_label_sigma, sprintf(label_template, temp_val[2]))
      temp_label_lower <- sprintf("%s%s", text_label_lower, sprintf(label_template, temp_val[3]))
      temp_label_upper <- sprintf("%s%s", text_label_upper, sprintf(label_template, temp_val[4]))
      tkconfigure(win1.frame1.label, text = temp_label_radius)
      tkconfigure(win1.frame2.label, text = temp_label_sigma)
      tkconfigure(win1.frame3.label, text = temp_label_lower)
      tkconfigure(win1.frame4.label, text = temp_label_upper)
      update_image()
      pre_slider_values <- temp_val
    }
  }
  val_res <- pre_slider_values
  if (return_param)
  {
    return(geometry_canny_magickGUI(val_res[1], val_res[2], val_res[3], val_res[4]))
  }
  return(image_canny(image, geometry_canny_magickGUI(val_res[1], val_res[2], val_res[3], val_res[4])))
}
