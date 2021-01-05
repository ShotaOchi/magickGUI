#' interactive annotation
#'
#' Using image_annotate of 'magick' interactively.
#' location, degrees, size, weight, and kerning are parameters of image_annotate. See reference manual of 'magick' for detail.
#' @param image a magick image object
#' @param text character vector of length equal to 'image' or length 1
#' @param gravity string with gravity value from gravity_types.
#' @param font string with font family such as "sans", "mono", "serif", "Times", "Helvetica", "Trebuchet", "Georgia", "Palatino" or "Comic Sans".
#' @param style value of style_types for example "italic"
#' @param decoration value of decoration_types for example "underline"
#' @param color a valid color string such as "navyblue" or "#000080". Use "none" for transparency.
#' @param strokecolor a color string adds a stroke (border around the text)
#' @param boxcolor a color string for background color that annotation text is rendered on.
#' @param range_max_size define maximum of size in slider. must be positive.
#' @param range_max_weight define maximum of weight in slider. must be positive.
#' @param range_max_kerning define maximum of kerning in slider. must be positive.
#' @param resolution resolution of slider
#' @param return_param If return_param is TRUE, returns a list of values of location, degrees, size, weight, and kerning. If return_param is FALSE, returns a magick image object.
#' @param scale geometry to be passed to image_scale function of magick package. image is scaled just for preview and result image is not scaled if scale is given.
#' @return a magick image object or a list of values of location, degrees, size, weight, and kerning
#' @author Shota Ochi
#' @export
#' @examples
#' \donttest{
#' interactive_annotate(wizard, "hello")
#' }
interactive_annotate <- function(image, text, gravity = "northwest", font = "", style = "normal", decoration = NULL, color = NULL, strokecolor = NULL, boxcolor = NULL, range_max_size = 1000, range_max_weight = 850, range_max_kerning = 300, resolution = 0.1, return_param = FALSE, scale)
{
  # image must be convreted into png to avoid the error of tkimage.create function
  image_original <- image
  image <- image_convert(as.list(image)[[1]], format = "png")
  
  # make initial output
  inix <- 0
  iniy <- 0
  inidegrees <- 0
  inisize <- 10
  iniweight <- 400
  inikerning <- 0
  initial <- image_annotate(image, text, gravity, geometry_point(inix, iniy), inidegrees, inisize, font, style, iniweight, inikerning, decoration, color, strokecolor, boxcolor)
  is_missing_scale <- missing(scale)

  # set variable range
  iminfo <- image_info(image)
  range_x <- c(-iminfo$width, iminfo$width)
  range_y <- c(-iminfo$height, iminfo$height)
  range_degrees <- c(0, 360)
  range_size <- c(0, range_max_size)
  range_weight <- c(0, range_max_weight)
  range_kerning <- c(0, range_max_kerning)
  length_slider <- as.integer(iminfo$width * 0.6) # length of slider
  if (length_slider < 200)
  {
    length_slider <- 200
  }
  text_label_x <- "X: "                           # text shown in label
  text_label_y <- "Y: "
  text_label_degrees <- "Degrees: "
  text_label_size <- "Size: "
  text_label_weight <- "Weight: "
  text_label_kerning <- "Kerning: "
  quit_waiting <- !is.null(getOption("unit_test_magickGUI"))
  temp <- tempfile(fileext = ".jpg")
  on.exit(unlink(temp), add = TRUE)
  if (!is_missing_scale)
  {
    image_write(image_scale(initial, scale), temp)
  } else
  {
    image_write(initial, temp)
  }
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
  win1.frame5 <- tkframe(win1)
  win1.frame6 <- tkframe(win1)
  win1.im <- tklabel(win1, image = image_tcl)
  win1.frame1.label <- tklabel(win1.frame1, text = sprintf("%s%s", text_label_x, sprintf(label_template, inix)))
  win1.frame2.label <- tklabel(win1.frame2, text = sprintf("%s%s", text_label_y, sprintf(label_template, iniy)))
  win1.frame3.label <- tklabel(win1.frame3, text = sprintf("%s%s", text_label_degrees, sprintf(label_template, inidegrees)))
  win1.frame4.label <- tklabel(win1.frame4, text = sprintf("%s%s", text_label_size, sprintf(label_template, inisize)))
  win1.frame5.label <- tklabel(win1.frame5, text = sprintf("%s%s", text_label_weight, sprintf(label_template, iniweight)))
  win1.frame6.label <- tklabel(win1.frame6, text = sprintf("%s%s", text_label_kerning, sprintf(label_template, inikerning)))
  slider_value_x <- tclVar(inix)
  slider_value_y <- tclVar(iniy)
  slider_value_degrees <- tclVar(inidegrees)
  slider_value_size <- tclVar(inisize)
  slider_value_weight <- tclVar(iniweight)
  slider_value_kerning <- tclVar(inikerning)
  command_slider_x <- function(...)
  {
    assign("slider_value_x", slider_value_x, inherits = TRUE)
  }
  command_slider_y <- function(...)
  {
    assign("slider_value_y", slider_value_y, inherits = TRUE)
  }
  command_slider_degrees <- function(...)
  {
    assign("slider_value_degrees", slider_value_degrees, inherits = TRUE)
  }
  command_slider_size <- function(...)
  {
    assign("slider_value_size", slider_value_size, inherits = TRUE)
  }
  command_slider_weight <- function(...)
  {
    assign("slider_value_weight", slider_value_weight, inherits = TRUE)
  }
  command_slider_kerning <- function(...)
  {
    assign("slider_value_kerning", slider_value_kerning, inherits = TRUE)
  }
  win1.frame1.slider <- tkscale(win1.frame1, from = range_x[1], to = range_x[2], variable = slider_value_x, orient = "horizontal", length = length_slider, command = command_slider_x, resolution = 1, showvalue = 0)
  win1.frame2.slider <- tkscale(win1.frame2, from = range_y[1], to = range_y[2], variable = slider_value_y, orient = "horizontal", length = length_slider, command = command_slider_y, resolution = 1, showvalue = 0)
  win1.frame3.slider <- tkscale(win1.frame3, from = range_degrees[1], to = range_degrees[2], variable = slider_value_degrees, orient = "horizontal", length = length_slider, command = command_slider_degrees, resolution = resolution, showvalue = 0)
  win1.frame4.slider <- tkscale(win1.frame4, from = range_size[1], to = range_size[2], variable = slider_value_size, orient = "horizontal", length = length_slider, command = command_slider_size, resolution = resolution, showvalue = 0)
  win1.frame5.slider <- tkscale(win1.frame5, from = range_weight[1], to = range_weight[2], variable = slider_value_weight, orient = "horizontal", length = length_slider, command = command_slider_weight, resolution = resolution, showvalue = 0)
  win1.frame6.slider <- tkscale(win1.frame6, from = range_kerning[1], to = range_kerning[2], variable = slider_value_kerning, orient = "horizontal", length = length_slider, command = command_slider_kerning, resolution = resolution, showvalue = 0)
  temp_val <- c(inix, iniy, inidegrees, inisize, iniweight, inikerning)
  update_image <- function()
  {
    temp_image <- image_annotate(image, text, gravity, geometry_point(temp_val[1], temp_val[2]), temp_val[3], temp_val[4], font, style, temp_val[5], temp_val[6], decoration, color, strokecolor, boxcolor)
    if (!is_missing_scale)
    {
      image_write(image_scale(temp_image, scale), temp)
    } else
    {
      image_write(temp_image, temp)
    }
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
  tkpack(win1.frame5.label, side = "left", anchor = "c")
  tkpack(win1.frame5.slider, side = "left", anchor = "c")
  tkpack(win1.frame5, side = "top", anchor = "c")
  tkpack(win1.frame6.label, side = "left", anchor = "c")
  tkpack(win1.frame6.slider, side = "left", anchor = "c")
  tkpack(win1.frame6, side = "top", anchor = "c")
  tkpack(win1.button, side = "top", anchor = "c", pady = 20)
  pre_slider_value <- c(as.numeric(tclvalue(slider_value_x)), as.numeric(tclvalue(slider_value_y)), as.numeric(tclvalue(slider_value_degrees)), as.numeric(tclvalue(slider_value_size)), as.numeric(tclvalue(slider_value_weight)), as.numeric(tclvalue(slider_value_kerning)))
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
    temp_val <- c(as.numeric(tclvalue(slider_value_x)), as.numeric(tclvalue(slider_value_y)), as.numeric(tclvalue(slider_value_degrees)), as.numeric(tclvalue(slider_value_size)), as.numeric(tclvalue(slider_value_weight)), as.numeric(tclvalue(slider_value_kerning)))
    if (any(temp_val != pre_slider_value))
    {
      temp_label_x <- sprintf("%s%s", text_label_x, sprintf(label_template, temp_val[1]))
      temp_label_y <- sprintf("%s%s", text_label_y, sprintf(label_template, temp_val[2]))
      temp_label_degrees <- sprintf("%s%s", text_label_degrees, sprintf(label_template, temp_val[3]))
      temp_label_size <- sprintf("%s%s", text_label_size, sprintf(label_template, temp_val[4]))
      temp_label_weight <- sprintf("%s%s", text_label_weight, sprintf(label_template, temp_val[5]))
      temp_label_kerning <- sprintf("%s%s", text_label_kerning, sprintf(label_template, temp_val[6]))
      tkconfigure(win1.frame1.label, text = temp_label_x)
      tkconfigure(win1.frame2.label, text = temp_label_y)
      tkconfigure(win1.frame3.label, text = temp_label_degrees)
      tkconfigure(win1.frame4.label, text = temp_label_size)
      tkconfigure(win1.frame5.label, text = temp_label_weight)
      tkconfigure(win1.frame6.label, text = temp_label_kerning)
      update_image()
      pre_slider_value <- temp_val
    }
  }
  val_res <- list(location = geometry_point(pre_slider_value[1], pre_slider_value[2]), degrees = pre_slider_value[3], size = pre_slider_value[4], weight = pre_slider_value[5], kerning = pre_slider_value[6])
  if (return_param)
  {
    return(val_res)
  }
  return(image_annotate(image_original, text, gravity, val_res[[1]], val_res[[2]], val_res[[3]], font, style, val_res[[4]], val_res[[5]], decoration, color, strokecolor, boxcolor))
}
