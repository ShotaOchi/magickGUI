#' interactive motion blurring
#'
#' Using image_motion_blur of 'magick' interactively.
#' radius and sigma and angle are parameters of image_motion_blur. See reference manual of 'magick' for detail.
#' @param image a magick image object
#' @param range_max_radius define maximum in slider of radius. must be positive.
#' @param range_max_sigma define maximum in slider of sigma. must be positive.
#' @param range_max_angle define maximum in slider of angle. must be positive.
#' @param resolution resolution of slider
#' @param return_param If return_param is TRUE, returns values of radius and sigma and angle. If return_param is FALSE, returns a magick image object.
#' @param scale geometry to be passed to image_scale function of magick package. image is scaled just for preview and result image is not scaled if scale is given.
#' @return a magick image object or values of radius, sigma, and angle
#' @author Shota Ochi
#' @export
#' @examples
#' \donttest{
#' if (interactive())
#' {
#'   interactive_motion_blur(wizard)
#' }
#' }
interactive_motion_blur <- function(image, range_max_radius = 100, range_max_sigma = 100, range_max_angle = 360, resolution = 0.1, return_param = FALSE, scale)
{
  # image must be convreted into png to avoid the error of tkimage.create function
  image_original <- image
  image <- image_convert(as.list(image)[[1]], format = "png")
  if (!missing(scale))
  {
    image <- image_scale(image, scale)
  }
  
  # make initial output
  iniv <- 0
  initial <- image_motion_blur(image, iniv, iniv, iniv)

  # set variable range
  iminfo <- image_info(image)
  range_radius <- c(0, range_max_radius)
  range_sigma <- c(0, range_max_sigma)
  range_angle <- c(0, range_max_angle)
  length_slider <- as.integer(iminfo$width * 0.6) # length of slider
  if (length_slider < 200)
  {
    length_slider <- 200
  }
  text_label_radius <- "Radius: "
  text_label_sigma <- "Sigma: "
  text_label_angle <- "Angle: "
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
  win1.im <- tklabel(win1, image = image_tcl)
  win1.frame1.label <- tklabel(win1.frame1, text = sprintf("%s%s", text_label_radius, sprintf(label_template, iniv)))
  win1.frame2.label <- tklabel(win1.frame2, text = sprintf("%s%s", text_label_sigma, sprintf(label_template, iniv)))
  win1.frame3.label <- tklabel(win1.frame3, text = sprintf("%s%s", text_label_angle, sprintf(label_template, iniv)))
  slider_value_radius <- tclVar(iniv)
  slider_value_sigma <- tclVar(iniv)
  slider_value_angle <- tclVar(iniv)
  command_slider_radius <- function(...)
  {
    assign("slider_value_radius", slider_value_radius, inherits = TRUE)
  }
  command_slider_sigma <- function(...)
  {
    assign("slider_value_sigma", slider_value_sigma, inherits = TRUE)
  }
  command_slider_angle <- function(...)
  {
    assign("slider_value_angle", slider_value_angle, inherits = TRUE)
  }
  win1.frame1.slider <- tkscale(win1.frame1, from = range_radius[1], to = range_radius[2], variable = slider_value_radius, orient = "horizontal", length = length_slider, command = command_slider_radius, resolution = resolution, showvalue = 0)
  win1.frame2.slider <- tkscale(win1.frame2, from = range_sigma[1], to = range_sigma[2], variable = slider_value_sigma, orient = "horizontal", length = length_slider, command = command_slider_sigma, resolution = resolution, showvalue = 0)
  win1.frame3.slider <- tkscale(win1.frame3, from = range_angle[1], to = range_angle[2], variable = slider_value_angle, orient = "horizontal", length = length_slider, command = command_slider_angle, resolution = resolution, showvalue = 0)
  temp_val <- rep(iniv, 3)
  update_image <- function()
  {
    temp_image <- image_motion_blur(image, temp_val[1], temp_val[2], temp_val[3])
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
  tkpack(win1.button, side = "top", anchor = "c", pady = 20)
  pre_slider_values <- c(as.numeric(tclvalue(slider_value_radius)), as.numeric(tclvalue(slider_value_sigma)), as.numeric(tclvalue(slider_value_angle)))
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
    temp_val <- c(as.numeric(tclvalue(slider_value_radius)), as.numeric(tclvalue(slider_value_sigma)), as.numeric(tclvalue(slider_value_angle)))
    if (any(temp_val != pre_slider_values))
    {
      temp_label_radius <- sprintf("%s%s", text_label_radius, sprintf(label_template, temp_val[1]))
      temp_label_sigma <- sprintf("%s%s", text_label_sigma, sprintf(label_template, temp_val[2]))
      temp_label_angle <- sprintf("%s%s", text_label_angle, sprintf(label_template, temp_val[3]))
      tkconfigure(win1.frame1.label, text = temp_label_radius)
      tkconfigure(win1.frame2.label, text = temp_label_sigma)
      tkconfigure(win1.frame3.label, text = temp_label_angle)
      update_image()
      pre_slider_values <- temp_val
    }
  }
  val_res <- pre_slider_values
  names(val_res) <- c("radius", "sigma", "angle")
  if (return_param)
  {
    return(val_res)
  }
  return(image_motion_blur(image_original, val_res[1], val_res[2], val_res[3]))
}
