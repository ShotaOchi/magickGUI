#' interactive thresholding
#'
#' Using image_threshold of 'magick' interactively.
#' type and channel are parameters of image_threshold. See reference manual of 'magick' for detail.
#' @param image a magick image object
#' @param type type of thresholding, either one of lat, black or white
#' @param channel a value specifying which channel(s) to set
#' @param resolution resolution of slider
#' @param return_param if TRUE, returns threshold value. if FALSE, returns magick image object.
#' @return a magick image object or threshold value
#' @author Shota Ochi
#' @export
#' @examples
#' \donttest{
#' interactive_threshold(wizard)
#' }

interactive_threshold <- function(image, type = c("black", "white"), channel = NULL, resolution = 0.1, return_param = FALSE)
{
  # make initial output
  iniv <- "0"
  initial <- image_threshold(image, type = type, threshold = paste(iniv, "%", sep = ""), channel = channel)

  # set variable range
  iminfo <- image_info(image)
  range_thr <- c(0,100)                               # threshold of image_threshold is a percentage.
  length_slider <- as.integer(iminfo["width"] * 0.6)  # length of slider
  if (length_slider < 200)
  {
    length_slider <- 200
  }
  text_label <- "Threshold: "                         # text shown in label
  quit_waiting <- !is.null(getOption("unit_test_magickGUI"))
  temp <- tempfile(fileext = ".jpg")
  on.exit(unlink(temp), add = TRUE)
  image_write(initial, temp)
  image_tcl <- tkimage.create("photo", "image_tcl", file = temp)

  # configure widgets
  iminfo <- image_info(image)
  win1 <- tktoplevel()
  on.exit(tkdestroy(win1), add = TRUE)
  win1.frame1 <- tkframe(win1)
  win1.im <- tklabel(win1, image = image_tcl)
  win1.frame1.label <- tklabel(win1.frame1, text = sprintf("%s%s %%", text_label, iniv))
  slider_value <- tclVar(iniv)
  command_slider <- function(...)
  {
    assign("slider_value", slider_value, inherits = TRUE)
  }
  win1.frame1.slider <- tkscale(win1.frame1, from = range_thr[1], to = range_thr[2], variable = slider_value, orient = "horizontal", length = length_slider, command = command_slider, resolution = resolution, showvalue = 0)
  command_button <- function(...)
  {
    assign("quit_waiting", TRUE, inherits = TRUE)
  }
  temp_val <- iniv
  update_image <- function()
  {
    temp_image <- image_threshold(image, type = type, threshold = paste(temp_val, "%", sep = ""), channel = channel)
    image_write(temp_image, temp)
    image_tcl <- tkimage.create("photo", "image_tcl", file = temp)
    tkconfigure(win1.im, image = image_tcl)
  }
  win1.button <- tkbutton(win1, text = "OK", command = command_button)
  tkpack(win1.im)
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
      temp_label <- sprintf("%s%s %%", text_label, formatC(temp_val))
      tkconfigure(win1.frame1.label, text = temp_label)
      update_image()
      pre_slider_value <- temp_val
    }
  }
  val_res <- paste(formatC(pre_slider_value), "%", sep = "")
  names(val_res) <- "threshold"
  if (return_param)
  {
    return(val_res)
  }
  return(image_threshold(image, type = type, threshold = val_res, channel = channel))
}
