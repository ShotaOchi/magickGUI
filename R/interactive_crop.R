#' interactive cropping
#'
#' Using image_crop of 'magick' interactively.
#' @param image a magick image object
#' @param color color of background. a valid color string such as "navyblue" or "#000080". "none" is not allowed.
#' @param return_param If return_param is TRUE, returns a value of geometry. If return_param is FALSE, returns a magick image object.
#' @return a magick image object or a value of geometry.
#' @author Shota Ochi
#' @export
#' @examples
#' \donttest{
#' interactive_crop(wizard)
#' }
interactive_crop <- function(image, color = "white", return_param = FALSE)
{
  # image must be convreted into png to avoid the error of tkimage.create function
  image_original <- image
  image <- as.list(image)[[1]] %>% image_convert(format = "png")
  
  if (color == "none")
  {
    stop("setting color as none is not allowed.")
  }
  
  # make initial output
  iminfo <- image_info(image)
  inix <- 0
  iniy <- 0
  iniwidth <- as.integer(iminfo["width"] / 2) + 1
  iniheight <- as.integer(iminfo["height"] / 2) + 1
  blank <- image_blank(iminfo["width"], iminfo["height"], color = color)
  pre_initial <- image_crop(image, geometry_area(iniwidth, iniheight, inix, iniy))
  initial <- image_composite(blank, pre_initial, offset = geometry_point(inix, iniy))

  # set variable range
  range_x <- c(0, iminfo$width - 1)
  range_y <- c(0, iminfo$height - 1)
  range_width <- c(1, iminfo$width)
  range_height <- c(1, iminfo$height)
  length_slider <- as.integer(iminfo$width * 0.6) # length of slider
  if (length_slider < 200)
  {
    length_slider <- 200
  }
  text_label_x <- "X: "                           # text shown in label
  text_label_y <- "Y: "
  text_label_width <- "Width: "
  text_label_height <- "Height: "
  quit_waiting <- !is.null(getOption("unit_test_magickGUI"))
  temp <- tempfile(fileext = ".jpg")
  on.exit(unlink(temp), add = TRUE)
  image_write(initial, temp)
  image_tcl <- tkimage.create("photo", "image_tcl", file = temp)
  temp2 <- tempfile(fileext = ".jpg")
  on.exit(unlink(temp2), add = TRUE)
  image_write(image, temp2)
  image_ori_tcl <- tkimage.create("photo", "image_ori_tcl", file = temp2)
  label_digits <- 0
  label_template <- sprintf("%%.%df", label_digits)
  
  # configure widgets
  win1 <- tktoplevel()
  on.exit(tkdestroy(win1), add = TRUE)
  win1.frame0 <- tkframe(win1)
  win1.frame1 <- tkframe(win1)
  win1.frame2 <- tkframe(win1)
  win1.frame3 <- tkframe(win1)
  win1.frame4 <- tkframe(win1)
  win1.frame0.label1 <- tklabel(win1.frame0, text = "Original", image = image_ori_tcl, compound = "bottom")
  win1.frame0.label2 <- tklabel(win1.frame0, text = "Cropped", image = image_tcl, compound = "bottom")
  win1.frame1.label <- tklabel(win1.frame1, text = sprintf("%s%s", text_label_x, sprintf(label_template, inix)))
  win1.frame2.label <- tklabel(win1.frame2, text = sprintf("%s%s", text_label_y, sprintf(label_template, iniy)))
  win1.frame3.label <- tklabel(win1.frame3, text = sprintf("%s%s", text_label_width, sprintf(label_template, iniwidth)))
  win1.frame4.label <- tklabel(win1.frame4, text = sprintf("%s%s", text_label_height, sprintf(label_template, iniheight)))
  slider_value_x <- tclVar(inix)
  slider_value_y <- tclVar(iniy)
  slider_value_width <- tclVar(iniwidth)
  slider_value_height <- tclVar(iniheight)
  command_slider_x <- function(...)
  {
    assign("slider_value_x", slider_value_x, inherits = TRUE)
  }
  command_slider_y <- function(...)
  {
    assign("slider_value_y", slider_value_y, inherits = TRUE)
  }
  command_slider_width <- function(...)
  {
    assign("slider_value_width", slider_value_width, inherits = TRUE)
  }
  command_slider_height <- function(...)
  {
    assign("slider_value_height", slider_value_height, inherits = TRUE)
  }
  win1.frame1.slider <- tkscale(win1.frame1, from = range_x[1], to = range_x[2], variable = slider_value_x, orient = "horizontal", length = length_slider, command = command_slider_x, resolution = 1, showvalue = 0)
  win1.frame2.slider <- tkscale(win1.frame2, from = range_y[1], to = range_y[2], variable = slider_value_y, orient = "horizontal", length = length_slider, command = command_slider_y, resolution = 1, showvalue = 0)
  win1.frame3.slider <- tkscale(win1.frame3, from = range_width[1], to = range_width[2], variable = slider_value_width, orient = "horizontal", length = length_slider, command = command_slider_width, resolution = 1, showvalue = 0)
  win1.frame4.slider <- tkscale(win1.frame4, from = range_height[1], to = range_height[2], variable = slider_value_height, orient = "horizontal", length = length_slider, command = command_slider_height, resolution = 1, showvalue = 0)
  temp_val <- c(inix, iniy, iniwidth, iniheight)
  update_image <- function()
  {
    temp_image <- image_crop(image, geometry_area(temp_val[3], temp_val[4], temp_val[1], temp_val[2]))
    temp_image <- image_composite(blank, temp_image, offset = geometry_point(temp_val[1], temp_val[2]))
    image_write(temp_image, temp)
    image_tcl <- tkimage.create("photo", "image_tcl", file = temp)
    tkconfigure(win1.frame0.label2, image = image_tcl)
  }
  command_button <- function(...)
  {
    assign("quit_waiting", TRUE, inherits = TRUE)
  }
  win1.button <- tkbutton(win1, text = "OK", command = command_button)
  tkpack(win1.frame0.label1, side = "left", anchor = "c")
  tkpack(win1.frame0.label2, side = "left", anchor = "c")
  tkpack(win1.frame0, side = "top", anchor = "c")
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
  pre_slider_value <- c(as.numeric(tclvalue(slider_value_x)), as.numeric(tclvalue(slider_value_y)), as.numeric(tclvalue(slider_value_width)), as.numeric(tclvalue(slider_value_height)))
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
    temp_val <- c(as.numeric(tclvalue(slider_value_x)), as.numeric(tclvalue(slider_value_y)), as.numeric(tclvalue(slider_value_width)), as.numeric(tclvalue(slider_value_height)))
    if (any(temp_val != pre_slider_value))
    {
      temp_label_x <- sprintf("%s%s", text_label_x, sprintf(label_template, temp_val[1]))
      temp_label_y <- sprintf("%s%s", text_label_y, sprintf(label_template, temp_val[2]))
      temp_label_width <- sprintf("%s%s", text_label_width, sprintf(label_template, temp_val[3]))
      temp_label_height <- sprintf("%s%s", text_label_height, sprintf(label_template, temp_val[4]))
      tkconfigure(win1.frame1.label, text = temp_label_x)
      tkconfigure(win1.frame2.label, text = temp_label_y)
      tkconfigure(win1.frame3.label, text = temp_label_width)
      tkconfigure(win1.frame4.label, text = temp_label_height)
      update_image()
      pre_slider_value <- temp_val
    }
  }
  val_res <- geometry_area(pre_slider_value[3], pre_slider_value[4], pre_slider_value[1], pre_slider_value[2])
  if (return_param)
  {
    return(val_res)
  }
  return(image_crop(image_original, val_res))
}
