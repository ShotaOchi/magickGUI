#' interactive charcoal filtering
#'
#' Using image_charcoal of 'magick' interactively.
#' radius and sigma are parameters of image_charcoal. see reference manual of 'magick' for detail.
#' @param image a magick image object
#' @param range_max_radius define maximum in slider of radius. must be positive.
#' @param range_max_sigma define maximum in slider of sigma. must be positive.
#' @param resolution resolution of slider
#' @param return_param if TRUE, returns values of radius and sigma. if FALSE, returns magick image object.
#' @return magick a image object or values of radius and sigma
#' @author Shota Ochi
#' @export
#' @examples
#' \dontrun{
#' interactive_charcoal(wizard)
#' }

interactive_charcoal <- function(image, range_max_radius = 5, range_max_sigma = 5, resolution = 0.1, return_param = FALSE)
{
  # make initial output
  iniv <- 0
  initial <- image_charcoal(image, iniv, iniv)

  # set variable range
  iminfo <- image_info(image)
  range_radius <- c(0, range_max_radius)
  range_sigma <- c(0, range_max_sigma)
  length_slider <- as.integer(iminfo["width"] * 0.6) # length of slider
  if (length_slider < 200)
  {
    length_slider <- 200
  }
  text_label_radius <- "Radius: "
  text_label_sigma <- "Sigma: "
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
  win1.im <- tklabel(win1, image = image_tcl)
  win1.frame1.label <- tklabel(win1.frame1, text = sprintf("%s%s", text_label_radius, sprintf(label_template, iniv)))
  win1.frame2.label <- tklabel(win1.frame2, text = sprintf("%s%s", text_label_sigma, sprintf(label_template, iniv)))
  slider_value_radius <- tclVar(iniv)
  slider_value_sigma <- tclVar(iniv)
  command_slider_radius <- function(...)
  {
    assign("slider_value_radius", slider_value_radius, inherits = TRUE)
  }
  command_slider_sigma <- function(...)
  {
    assign("slider_value_sigma", slider_value_sigma, inherits = TRUE)
  }
  win1.frame1.slider <- tkscale(win1.frame1, from = range_radius[1], to = range_radius[2], variable = slider_value_radius, orient = "horizontal", length = length_slider, command = command_slider_radius, resolution = resolution, showvalue = 0)
  win1.frame2.slider <- tkscale(win1.frame2, from = range_sigma[1], to = range_sigma[2], variable = slider_value_sigma, orient = "horizontal", length = length_slider, command = command_slider_sigma, resolution = resolution, showvalue = 0)
  temp_val <- iniv
  update_image <- function()
  {
    temp_image <- image_charcoal(image, temp_val[1], temp_val[2])
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
  tkpack(win1.button, side = "top", anchor = "c", pady = 20)
  pre_slider_values <- c(as.numeric(tclvalue(slider_value_radius)), as.numeric(tclvalue(slider_value_sigma)))
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
    wait_start <- proc.time()[3]
    wait_time <- getOption("unit_test_magickGUI_wait_time")
    while (proc.time()[3] - wait_start < wait_time) {}
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
    temp_val <- c(as.numeric(tclvalue(slider_value_radius)), as.numeric(tclvalue(slider_value_sigma)))
    if (any(temp_val != pre_slider_values))
    {
      temp_label_radius <- sprintf("%s%s", text_label_radius, sprintf(label_template, temp_val[1]))
      temp_label_sigma <- sprintf("%s%s", text_label_sigma, sprintf(label_template, temp_val[2]))
      tkconfigure(win1.frame1.label, text = temp_label_radius)
      tkconfigure(win1.frame2.label, text = temp_label_sigma)
      update_image()
      pre_slider_values <- temp_val
    }
  }
  val_res <- pre_slider_values
  names(val_res) <- c("radius", "sigma")
  if (return_param)
  {
    return(val_res)
  }
  return(image_charcoal(image, val_res[1], val_res[2]))
}
