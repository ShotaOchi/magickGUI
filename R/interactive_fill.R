#' interactive filling
#'
#' Using image_fill of 'magick' interactively.
#' point and fuzz are parameters of image_fill. See reference manual of 'magick' for detail.
#' @param image a magick image object
#' @param color a valid color string such as "navyblue" or "#000080". Use "none" for transparency.
#' @param refcolor if set, fuzz color distance will be measured against this color, not the color of the starting point. Any color (within fuzz color distance of the given refcolor), connected to starting point will be replaced with the color. If the pixel at the starting point does not itself match the given refcolor (according to fuzz) then no action will be taken.
#' @param resolution resolution of slider of fuzz
#' @param return_param If return_param is TRUE, returns a list values of point and fuzz. If return_param is FALSE, returns a magick image object.
#' @return a magick image object or a list of values of point and fuzz
#' @author Shota Ochi
#' @export
#' @examples
#' \donttest{
#' interactive_fill(wizard, "black")
#' }
interactive_fill <- function(image, color, refcolor = NULL, resolution = 0.1, return_param = FALSE)
{
  # image must be convreted into png because of the bug in tcltk package
  image <- image_convert(image, format = "png")
  
  # make initial output
  inix <- 1
  iniy <- 1
  inifuzz <- 0
  initial <- image_fill(image, color, geometry_point(inix, iniy), inifuzz, refcolor)

  # set variable range
  iminfo <- image_info(image)
  range_x <- c(0, iminfo$width)
  range_y <- c(0, iminfo$height)
  range_fuzz <- c(0, 100)
  length_slider <- as.integer(iminfo$width * 0.6) # length of slider
  if (length_slider < 200)
  {
    length_slider <- 200
  }
  text_label_x <- "X: "
  text_label_y <- "Y: "
  text_label_fuzz <- "Fuzz: "
  quit_waiting <- !is.null(getOption("unit_test_magickGUI"))
  temp <- tempfile(fileext = ".jpg")
  on.exit(unlink(temp), add = TRUE)
  image_write(initial, temp)
  image_tcl <- tkimage.create("photo", "image_tcl", file = temp)
  label_digits <- -as.integer(log(resolution, 10))
  label_digits <- ifelse(label_digits > 0, label_digits, 0)
  label_template <- sprintf("%%.%df", label_digits)
  label_template_point <- sprintf("%%.%df", 0)

  # configure widgets
  win1 <- tktoplevel()
  on.exit(tkdestroy(win1), add = TRUE)
  win1.frame1 <- tkframe(win1)
  win1.frame2 <- tkframe(win1)
  win1.frame3 <- tkframe(win1)
  win1.im <- tklabel(win1, image = image_tcl)
  win1.frame1.label <- tklabel(win1.frame1, text = sprintf("%s%s", text_label_x, sprintf(label_template_point, inix)))
  win1.frame2.label <- tklabel(win1.frame2, text = sprintf("%s%s", text_label_y, sprintf(label_template_point, iniy)))
  win1.frame3.label <- tklabel(win1.frame3, text = sprintf("%s%s", text_label_fuzz, sprintf(label_template, inifuzz)))
  slider_value_x <- tclVar(inix)
  slider_value_y <- tclVar(iniy)
  slider_value_fuzz <- tclVar(inifuzz)
  command_slider_x <- function(...)
  {
    assign("slider_value_x", slider_value_x, inherits = TRUE)
  }
  command_slider_y <- function(...)
  {
    assign("slider_value_y", slider_value_y, inherits = TRUE)
  }
  command_slider_fuzz <- function(...)
  {
    assign("slider_value_fuzz", slider_value_fuzz, inherits = TRUE)
  }
  win1.frame1.slider <- tkscale(win1.frame1, from = range_x[1], to = range_x[2], variable = slider_value_x, orient = "horizontal", length = length_slider, command = command_slider_x, resolution = 1, showvalue = 0)
  win1.frame2.slider <- tkscale(win1.frame2, from = range_y[1], to = range_y[2], variable = slider_value_y, orient = "horizontal", length = length_slider, command = command_slider_y, resolution = 1, showvalue = 0)
  win1.frame3.slider <- tkscale(win1.frame3, from = range_fuzz[1], to = range_fuzz[2], variable = slider_value_fuzz, orient = "horizontal", length = length_slider, command = command_slider_fuzz, resolution = resolution, showvalue = 0)
  temp_val <- c(inix, iniy, inifuzz)
  update_image <- function()
  {
    temp_image <- image_fill(image, color, geometry_point(temp_val[1], temp_val[2]), temp_val[3], refcolor)
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
  pre_slider_values <- c(as.numeric(tclvalue(slider_value_x)), as.numeric(tclvalue(slider_value_y)), as.numeric(tclvalue(slider_value_fuzz)))
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
    temp_val <- c(as.numeric(tclvalue(slider_value_x)), as.numeric(tclvalue(slider_value_y)), as.numeric(tclvalue(slider_value_fuzz)))
    if (any(temp_val != pre_slider_values))
    {
      temp_label_x <- sprintf("%s%s", text_label_x, sprintf(label_template_point, temp_val[1]))
      temp_label_y <- sprintf("%s%s", text_label_y, sprintf(label_template_point, temp_val[2]))
      temp_label_fuzz <- sprintf("%s%s", text_label_fuzz, sprintf(label_template, temp_val[3]))
      tkconfigure(win1.frame1.label, text = temp_label_x)
      tkconfigure(win1.frame2.label, text = temp_label_y)
      tkconfigure(win1.frame3.label, text = temp_label_fuzz)
      update_image()
      pre_slider_values <- temp_val
    }
  }
  val_res <- list(point = geometry_point(pre_slider_values[1], pre_slider_values[2]), fuzz = pre_slider_values[3])
  if (return_param)
  {
    return(val_res)
  }
  return(image_fill(image, color, val_res$point, val_res$fuzz, refcolor))
}
