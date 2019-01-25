#' interactive thresholding
#'
#' Using image_threshold of 'magick' interactively.
#' type and channel are parameters of image_threshold. see reference manual of 'magick' for detail.
#' @param image a magick image object
#' @param type type of thresholding, either one of lat, black or white
#' @param channel a value specifying which channel(s) to set
#' @param scale scale factor for size of image shown in plot. scale does not affect result.
#' @param return_thr if TRUE, returns threshold value. if FALSE, returns magick image object.
#' @return magick a image object or threshold value
#' @author Shota Ochi
#' @export
#' @examples
#' \dontrun{
#' interactive_threshold(wizard)
#' }

interactive_threshold <- function(image, type = c("black", "white"), channel = NULL, scale = 1, return_thr = FALSE)
{
  # make initial output
  iniv <- "0"
  initial <- image_threshold(image, type = type, threshold = paste(iniv, "%", sep = ""), channel = channel)

  # set variable range
  range_thr <- c(0,100) # threshold of image_threshold is a percentage.

  # configure widgets
  win1 <- tktoplevel()
  on.exit(tkdestroy(win1))
  im_tcl <- function(val)
  {
    return(function() plot(image_threshold(image, type = type, threshold = sprintf("%s%%", val), channel = channel)))
  }
  win1.im <- tkrplot(win1, fun = im_tcl(iniv), hscale = scale, vscale = scale)
  tkpack(win1.im)
  text_label <- "Threshold: "
  win1.label <- tklabel(win1, text = sprintf("%s%s %%", text_label, iniv))
  tkpack(win1.label, side = "top", anchor = "c")
  slider_value <- tclVar(iniv)
  command_slider <- function(...)
  {
    assign("slider_value", slider_value, inherits = TRUE)
  }
  length_slider <- 200 * scale
  win1.slider <- tkscale(win1, from = range_thr[1], to = range_thr[2], variable=slider_value, orient="horizontal", length=length_slider, command=command_slider)
  quit_waiting <- FALSE
  command_button <- function(...)
  {
    assign("quit_waiting", TRUE, inherits = TRUE)
  }
  win1.button <- tkbutton(win1, text = "OK", command = command_button)
  tkpack(win1.slider, side = "top")
  tkpack(win1.button, side = "top", anchor = "c", pady = 20)
  pre_slider_value <- as.numeric(tclvalue(slider_value))
  while (TRUE)
  {
    tryCatch({
      tkwm.state(win1) 
      },
      error = function(e) assign("quit_waiting", TRUE, inherits = TRUE)
    )
    if (quit_waiting) break
    if (pre_slider_value != as.numeric(tclvalue(slider_value))) {
      temp_slider_value <- as.numeric(tclvalue(slider_value))
      temp_label <- sprintf("%s%s %%", text_label, formatC(temp_slider_value))
      tkconfigure(win1.label, text = temp_label)
      tempim <- tkrplot(win1, fun = im_tcl(temp_slider_value), hscale = scale, vscale = scale)
      tkconfigure(win1.im, image = tempim$image)
      pre_sliderValue <- temp_slider_value
    }
  }
  val_res <- pre_slider_value
  if (return_thr)
  {
    return(sprintf("%s%%", formatC(val_res)))
  }
  return(image_threshold(image, type = type, threshold = sprintf("%f%%", val_res), channel = channel))
}
