#' interactive image compositing
#'
#' Using image_composite of 'magick' interactively.
#' offset is a parameter of image_composite. see reference manual of 'magick' for detail.
#' @param image a magick image object
#' @param composite_image composition image
#' @param operator string with a composite operator
#' @param compose_args additional arguments needed for some composite operations
#' @param resolution resolution of slider
#' @param return_param If return_param is TRUE, returns values of offset. If return_param is FALSE, returns a magick image object.
#' @param scale geometry to be passed to image_scale function of magick package. image is scaled just for preview and result image is not scaled if scale is given.
#' @return magick a image object or values of offset
#' @author Shota Ochi
#' @export
#' @examples
#' \donttest{
#' interactive_composite(wizard, rose)
#' }
interactive_composite <- function(image, composite_image, operator = "atop", compose_args = "", resolution = 1, return_param = FALSE, scale)
{
  # image must be convreted into png to avoid the error of tkimage.create function
  image_original <- image
  image <- image_convert(as.list(image)[[1]], format = "png")
  composite_image_original <- composite_image
  composite_image <- image_convert(as.list(composite_image)[[1]], format = "png")
  
  # make initial output
  iniv <- 0
  initial <- image_composite(image, composite_image, operator = operator, offset = geometry_point(iniv, iniv), compose_args = compose_args)
  is_missing_scale <- missing(scale)

  # set variable range
  iminfo <- image_info(image)
  iminfo_composite <- image_info(composite_image)
  range_x <- c(-iminfo_composite[["width"]], iminfo[["width"]])
  range_y <- c(-iminfo_composite[["height"]], iminfo[["height"]])
  length_slider <- as.integer(iminfo$width * 0.6) # length of slider
  if (length_slider < 200)
  {
    length_slider <- 200
  }
  text_label_x <- "x: "
  text_label_y <- "y: "
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
  win1.im <- tklabel(win1, image = image_tcl)
  win1.frame1.label <- tklabel(win1.frame1, text = sprintf("%s%s", text_label_x, sprintf(label_template, iniv)))
  win1.frame2.label <- tklabel(win1.frame2, text = sprintf("%s%s", text_label_y, sprintf(label_template, iniv)))
  slider_value_x <- tclVar(iniv)
  slider_value_y <- tclVar(iniv)
  command_slider_x <- function(...)
  {
    assign("slider_value_x", slider_value_x, inherits = TRUE)
  }
  command_slider_y <- function(...)
  {
    assign("slider_value_y", slider_value_y, inherits = TRUE)
  }
  win1.frame1.slider <- tkscale(win1.frame1, from = range_x[1], to = range_x[2], variable = slider_value_x, orient = "horizontal", length = length_slider, command = command_slider_x, resolution = resolution, showvalue = 0)
  win1.frame2.slider <- tkscale(win1.frame2, from = range_y[1], to = range_y[2], variable = slider_value_y, orient = "horizontal", length = length_slider, command = command_slider_y, resolution = resolution, showvalue = 0)
  temp_val <- iniv
  update_image <- function()
  {
    temp_image <- image_composite(image, composite_image, operator = operator, offset = geometry_point(temp_val[1], temp_val[2]), compose_args = compose_args)
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
  tkpack(win1.button, side = "top", anchor = "c", pady = 20)
  pre_slider_values <- c(as.numeric(tclvalue(slider_value_x)), as.numeric(tclvalue(slider_value_y)))
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
    temp_val <- c(as.numeric(tclvalue(slider_value_x)), as.numeric(tclvalue(slider_value_y)))
    if (any(temp_val != pre_slider_values))
    {
      temp_label_x <- sprintf("%s%s", text_label_x, sprintf(label_template, temp_val[1]))
      temp_label_y <- sprintf("%s%s", text_label_y, sprintf(label_template, temp_val[2]))
      tkconfigure(win1.frame1.label, text = temp_label_x)
      tkconfigure(win1.frame2.label, text = temp_label_y)
      update_image()
      pre_slider_values <- temp_val
    }
  }
  val_res <- pre_slider_values
  names(val_res) <- c("x", "y")
  if (return_param)
  {
    return(geometry_point(val_res[1], val_res[2]))
  }
  return(image_composite(image_original, composite_image_original, operator = operator, offset = geometry_point(val_res[1], val_res[2]), compose_args = compose_args))
}
