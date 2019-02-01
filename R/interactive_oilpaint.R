#' interactive oil painting
#'
#' Using image_oilpaint of 'magick' interactively.
#' radius is a parameter of image_oilpaint. see reference manual of 'magick' for detail.
#' @param image a magick image object
#' @param range_max define maximum in slider. must be positive.
#' @param scale scale factor for size of image shown in plot. scale does not affect result.
#' @param resolution resolution of slider
#' @param return_param if TRUE, returns value of radius. if FALSE, returns magick image object.
#' @return magick a image object or value of radius
#' @author Shota Ochi
#' @export
#' @examples
#' \dontrun{
#' interactive_oilpaint(wizard)
#' }

interactive_oilpaint <- function(image, range_max = 10, scale = 1, resolution = 0.1, return_param = FALSE)
{
  # make initial output
  iniv <- 0
  initial <- image_oilpaint(image, iniv)

  # set variable range
  range_radius <- c(0,range_max)
  length_slider <- 200 * scale                        # length of slider
  digits <- log(resolution, 10)
  digits <- ifelse(digits < 0, -digits, 0)            # digits of resolution
  text_label <- "Radius: "                         # text shown in label
  quit_waiting <- !is.null(getOption("unit_test_magickGUI"))
  
  # configure widgets
  win1 <- tktoplevel()
  on.exit(tkdestroy(win1))
  im_tcl <- function(val)
  {
    return(function() plot(image_oilpaint(image, val)))
  }
  win1.im <- tkrplot(win1, fun = im_tcl(iniv), hscale = scale, vscale = scale)
  tkpack(win1.im)
  format_val <- function(val)
  {
    res <- as.numeric(val)
    
  }
  win1.label <- tklabel(win1, text = sprintf("%s%s %%", text_label, formatC(iniv)))
  tkpack(win1.label, side = "top", anchor = "c")
  slider_value <- tclVar(iniv)
  command_slider <- function(...)
  {
    assign("slider_value", slider_value, inherits = TRUE)
  }

  win1.slider <- tkscale(win1, from = range_radius[1], to = range_radius[2], variable = slider_value, orient = "horizontal", length = length_slider, command = command_slider, resolution = resolution, showvalue = 0)
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
  if (return_param)
  {
    return(val_res)
  }
  return(image_oilpaint(image, val_res))
}
