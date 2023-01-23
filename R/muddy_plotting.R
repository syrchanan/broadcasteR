#' Convert RGBa values to HSLa values
#'
#' Convert RGBa values to HSLa values by using the RGBa color model.
#'
#' @param r An integer between 0 and 255 representing the red value of the color
#' @param g An integer between 0 and 255 representing the green value of the color
#' @param b An integer between 0 and 255 representing the blue value of the color
#' @param a A value between 0 and 1 representing the alpha of the color
#' @param pct An indicator of whether the rgb values are percentages or scaled 0-255. Defaults to `FALSE`. If rgb values are input as percentages, set the parameters equal to a decimal between 0 and 1.
#'
#' @return Returns h/s/l/a in a vector, h = 0-360 deg, s = 0.0 - 1 (0-100%), l = 0.0 - 1 (0-100%)
#' @export
rgba_to_hsla <- function(r, g, b, a = 1, pct = F) {
  if (pct == F) {
    r <- r/255
    g <- g/255
    b <- b/255
  }
  val_max <- max(c(r, g, b))
  val_min <- min(c(r, g, b))
  h <- s <- l <- (val_max + val_min) / 2
  if (val_max == val_min){
    h <- s <- 0
  } else {
    d <- val_max - val_min
    s <- ifelse(l > 0.5, d / (2 - val_max - val_min), d / (val_max + val_min))
    if (val_max == r) { h <- (g - b) / d + (ifelse(g < b, 6, 0)) }
    if (val_max == g) { h <- (b - r) / d + 2 }
    if (val_max == b) { h <- (r - g) / d + 4 }
    h <- (h / 6) * 360
  }
  return(c(h=h, s=s, l=l, a=a))
}



#' Convert HSLa values to RGBa values
#'
#' Convert HSLa values to RGBa values by using the HSLa color model.
#'
#' @param h A value between 0 and 360 representing the whole degree of the hue
#' @param s A value between 0.0 - 1, (0 - 100%) representing the saturation of the color
#' @param l A value between 0.0 - 1, (0 - 100%) representing the lightness of the color
#' @param a A value between 0 and 1 representing the alpha of the color
#' @param pct An indicator of whether the rgb output values are scaled 0-255 or percentages. Defaults to `FALSE`. If rgb values are output as percentages, set the output will be a decimal between 0 and 1.
#'
#' @return A list of 4 values representing the RGBa values of the color
#' @export
hsla_to_rgba <- function(h, s, l, a = 1, pct = F) {
  h <- h / 360
  r <- g <- b <- 0.0
  if (s == 0) {
    r <- g <- b <- l
  } else {
    hue_to_rgb <- function(p, q, t) {
      if (t < 0) { t <- t + 1.0 }
      if (t > 1) { t <- t - 1.0 }
      if (t < 1/6) { return(p + (q - p) * 6.0 * t) }
      if (t < 1/2) { return(q) }
      if (t < 2/3) { return(p + ((q - p) * ((2/3) - t) * 6)) }
      return(p)
    }
    q <- ifelse(l < 0.5, l * (1.0 + s), l + s - (l*s))
    p <- 2.0 * l - q
    r <- hue_to_rgb(p, q, h + 1/3)
    g <- hue_to_rgb(p, q, h)
    b <- hue_to_rgb(p, q, h - 1/3)
  }
  if (pct == F) {
    r <- r*255
    b <- b*255
    g <- g*255
  }
  return(c(r=r,g=g,b=b,a=a))
}
