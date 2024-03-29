#' interactive despeckling
#'
#' Using image_despeckle of 'magick' interactively.
#' times is a parameter of image_despeckle. See reference manual of 'magick' for detail.
#' @param image a magick image object
#' @param range_max define maximum in slider. must be positive.
#' @param resolution resolution of slider
#' @param return_param If return_param is TRUE, returns value of times. If return_param is FALSE, returns a magick image object.
#' @param scale geometry to be passed to image_scale function of magick package. image is scaled just for preview and result image is not scaled if scale is given.
#' @return a magick image object or value of times
#' @author Shota Ochi
#' @export
#' @examples
#' \donttest{
#' if (interactive())
#' {
#'   interactive_despeckle(wizard)
#' }
#' }
interactive_despeckle <- function(image, range_max = 50, resolution = 1, return_param = FALSE, scale)
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
  initial <- image_despeckle(image, iniv)

  # set variable range
  iminfo <- image_info(image)
  range_times <- c(0,range_max)
  length_slider <- as.integer(iminfo$width * 0.6) # length of slider
  if (length_slider < 200)
  {
    length_slider <- 200
  }
  text_label <- "Times: "                           # text shown in label
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
  win1.im <- tklabel(win1, image = image_tcl)
  win1.frame1.label <- tklabel(win1.frame1, text = sprintf("%s%s", text_label, sprintf(label_template, iniv)))
  slider_value <- tclVar(iniv)
  command_slider <- function(...)
  {
    assign("slider_value", slider_value, inherits = TRUE)
  }
  win1.frame1.slider <- tkscale(win1.frame1, from = range_times[1], to = range_times[2], variable = slider_value, orient = "horizontal", length = length_slider, command = command_slider, resolution = resolution, showvalue = 0)
  temp_val <- iniv
  update_image <- function()
  {
    temp_image <- image_despeckle(image, temp_val)
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
  tkpack(win1.button, side = "top", anchor = "c", pady = 20)
  pre_slider_value <- as.numeric(tclvalue(slider_value))
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
    wait_time()
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
    temp_val <- as.numeric(tclvalue(slider_value))
    if (temp_val != pre_slider_value)
    {
      temp_label <- sprintf("%s%s", text_label, sprintf(label_template, temp_val))
      tkconfigure(win1.frame1.label, text = temp_label)
      update_image()
      pre_slider_value <- temp_val
    }
  }
  val_res <- pre_slider_value
  names(val_res) <- "times"
  if (return_param)
  {
    return(val_res)
  }
  return(image_despeckle(image_original, val_res))
}
