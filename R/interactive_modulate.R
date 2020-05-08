#' interactive modulating
#'
#' Using image_modulate of 'magick' interactively.
#' brightness and saturation and hue are parameters of image_modulate. See reference manual of 'magick' for detail.
#' @param image a magick image object
#' @param range_max_brightness define maximum in slider of brightness. must be positive.
#' @param range_max_saturation define maximum in slider of saturation. must be positive.
#' @param range_max_hue define maximum in slider of hue. must be positive.
#' @param resolution resolution of slider
#' @param return_param If return_param is TRUE, returns values of brightness and saturation and hue. If return_param is FALSE, returns a magick image object.
#' @return a magick image object or values of brightness and saturation
#' @author Shota Ochi
#' @export
#' @examples
#' \donttest{
#' interactive_modulate(wizard)
#' }
interactive_modulate <- function(image, range_max_brightness = 200, range_max_saturation = 200, range_max_hue = 200, resolution = 0.1, return_param = FALSE)
{
  # make initial output
  iniv <- 100
  initial <- image_modulate(image, iniv, iniv, iniv)

  # set variable range
  iminfo <- image_info(image)
  range_brightness <- c(0, range_max_brightness)
  range_saturation <- c(0, range_max_saturation)
  range_hue <- c(0, range_max_hue)
  length_slider <- as.integer(iminfo$width * 0.6) # length of slider
  if (length_slider < 200)
  {
    length_slider <- 200
  }
  text_label_brightness <- "Brightness: "
  text_label_saturation <- "Saturation: "
  text_label_hue <- "Hue: "
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
  win1.frame1.label <- tklabel(win1.frame1, text = sprintf("%s%s", text_label_brightness, sprintf(label_template, iniv)))
  win1.frame2.label <- tklabel(win1.frame2, text = sprintf("%s%s", text_label_saturation, sprintf(label_template, iniv)))
  win1.frame3.label <- tklabel(win1.frame3, text = sprintf("%s%s", text_label_hue, sprintf(label_template, iniv)))
  slider_value_brightness <- tclVar(iniv)
  slider_value_saturation <- tclVar(iniv)
  slider_value_hue <- tclVar(iniv)
  command_slider_brightness <- function(...)
  {
    assign("slider_value_brightness", slider_value_brightness, inherits = TRUE)
  }
  command_slider_saturation <- function(...)
  {
    assign("slider_value_saturation", slider_value_saturation, inherits = TRUE)
  }
  command_slider_hue <- function(...)
  {
    assign("slider_value_hue", slider_value_hue, inherits = TRUE)
  }
  win1.frame1.slider <- tkscale(win1.frame1, from = range_brightness[1], to = range_brightness[2], variable = slider_value_brightness, orient = "horizontal", length = length_slider, command = command_slider_brightness, resolution = resolution, showvalue = 0)
  win1.frame2.slider <- tkscale(win1.frame2, from = range_saturation[1], to = range_saturation[2], variable = slider_value_saturation, orient = "horizontal", length = length_slider, command = command_slider_saturation, resolution = resolution, showvalue = 0)
  win1.frame3.slider <- tkscale(win1.frame3, from = range_hue[1], to = range_hue[2], variable = slider_value_hue, orient = "horizontal", length = length_slider, command = command_slider_hue, resolution = resolution, showvalue = 0)
  temp_val <- rep(iniv, 3)
  update_image <- function()
  {
    temp_image <- image_modulate(image, temp_val[1], temp_val[2], temp_val[3])
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
  pre_slider_values <- c(as.numeric(tclvalue(slider_value_brightness)), as.numeric(tclvalue(slider_value_saturation)), as.numeric(tclvalue(slider_value_hue)))
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
    temp_val <- c(as.numeric(tclvalue(slider_value_brightness)), as.numeric(tclvalue(slider_value_saturation)), as.numeric(tclvalue(slider_value_hue)))
    if (any(temp_val != pre_slider_values))
    {
      temp_label_brightness <- sprintf("%s%s", text_label_brightness, sprintf(label_template, temp_val[1]))
      temp_label_saturation <- sprintf("%s%s", text_label_saturation, sprintf(label_template, temp_val[2]))
      temp_label_hue <- sprintf("%s%s", text_label_hue, sprintf(label_template, temp_val[3]))
      tkconfigure(win1.frame1.label, text = temp_label_brightness)
      tkconfigure(win1.frame2.label, text = temp_label_saturation)
      tkconfigure(win1.frame3.label, text = temp_label_hue)
      update_image()
      pre_slider_values <- temp_val
    }
  }
  val_res <- pre_slider_values
  names(val_res) <- c("brightness", "saturation", "hue")
  if (return_param)
  {
    return(val_res)
  }
  return(image_modulate(image, val_res[1], val_res[2], val_res[3]))
}
