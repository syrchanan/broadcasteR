#' Convert RGBa values to HSLa values
#'
#' Converts colors from the **RGBa (Red, Green, Blue, alpha) color model**
#' to the **HSLa (Hue, Saturation, Lightness, alpha) color model**.
#' This function takes individual red, green, blue, and alpha component values
#' and returns their HSLa equivalents.
#'
#' The RGBa model defines colors by combining varying intensities of red, green,
#' and blue light. The alpha channel specifies the opacity. HSLa, on the other
#' hand, describes color based on its hue (the pure color), saturation (how
#' vivid the color is), lightness (how bright or dark the color is), and alpha.
#' HSLa is often preferred for color manipulation due to its more intuitive
#' representation of color attributes.
#'
#' @param r A numeric value representing the **red** component.
#'   Accepted range depends on `pct`: if `pct = FALSE` (default), it's an
#'   integer between `0` and `255`; if `pct = TRUE`, it's a decimal between
#'   `0` and `1`.
#' @param g A numeric value representing the **green** component.
#'   Accepted range is the same as for `r`.
#' @param b A numeric value representing the **blue** component.
#'   Accepted range is the same as for `r`.
#' @param a A numeric value representing the **alpha (opacity)** component.
#'   Must be a decimal between `0` (fully transparent) and `1` (fully opaque).
#'   Defaults to `1`.
#' @param pct A logical value. If `TRUE`, `r`, `g`, and `b` are expected to be
#'   input as **percentages** (i.e., decimal values between `0` and `1`).
#'   If `FALSE` (default), they are expected as integers scaled `0-255`.
#'
#' @return A **numeric vector** of four values:
#' \describe{
#'   \item{`h`}{**Hue**: A value between `0` and `360` degrees, representing the
#'     color's position on the color wheel. `0` (or `360`) is red, `120` is green,
#'     `240` is blue.} 
#'   \item{`s`}{**Saturation**: A value between `0.0` and `1.0` (inclusive),
#'     representing the intensity or purity of the color. `0.0` is grayscale,
#'     `1.0` is full saturation.}
#'   \item{`l`}{**Lightness**: A value between `0.0` and `1.0` (inclusive),
#'     representing the brightness of the color. `0.0` is black, `1.0` is white,
#'     `0.5` is normal lightness.}
#'   \item{`a`}{**Alpha**: The input alpha value, between `0` and `1`.}
#' }
#'
#' @seealso \code{\link{hsla_to_rgba}} for the reverse conversion.
#' @examples
#' # Convert a pure red color (non-percentage input)
#' rgba_to_hsla(r = 255, g = 0, b = 0, a = 1)
#' # Expected output: h=0, s=1, l=0.5, a=1
#'
#' # Convert a light blue with 50% opacity (percentage input)
#' rgba_to_hsla(r = 0.5, g = 0.75, b = 1, a = 0.5, pct = TRUE)
#' # Expected output (approx): h=210, s=1, l=0.75, a=0.5
#'
#' # Convert a gray color (should have 0 saturation)
#' rgba_to_hsla(r = 128, g = 128, b = 128)
#' # Expected output: h=0, s=0, l=0.5, a=1
#' @export
rgba_to_hsla <- function(r, g, b, a = 1, pct = F) {
  # Normalize RGB values to 0-1 range if not already percentages
  if (pct == F) {
    r <- r / 255
    g <- g / 255
    b <- b / 255
  }

  # Find the maximum and minimum values among R, G, B
  val_max <- max(c(r, g, b))
  val_min <- min(c(r, g, b))

  # Initialize H, S, L (L is calculated first)
  h <- s <- l <- (val_max + val_min) / 2

  # If max and min are equal, it's a shade of gray (no hue or saturation)
  if (val_max == val_min) {
    h <- s <- 0
  } else {
    # Calculate the difference for saturation and hue calculations
    d <- val_max - val_min

    # Calculate Saturation (s)
    s <- ifelse(l > 0.5, d / (2 - val_max - val_min), d / (val_max + val_min))

    # Calculate Hue (h) based on which RGB component is max
    if (val_max == r) {
      h <- (g - b) / d + (ifelse(g < b, 6, 0))
    } else if (val_max == g) {
      h <- (b - r) / d + 2
    } else { # val_max == b
      h <- (r - g) / d + 4
    }
    # Convert hue to degrees (0-360)
    h <- (h / 6) * 360
  }
  # Return the HSLa values in a named vector
  return(c(h = h, s = s, l = l, a = a))
}

#' Convert HSLa values to RGBa values
#'
#' Converts colors from the **HSLa (Hue, Saturation, Lightness, alpha) color model**
#' to the **RGBa (Red, Green, Blue, alpha) color model**.
#' This function takes hue, saturation, lightness, and alpha component values
#' and returns their RGBa equivalents.
#'
#' HSLa is often used for its intuitive way of defining colors. This function
#' allows you to translate those HSLa values back into the RGBa format, which
#' is commonly used for display on screens.
#'
#' @param h A numeric value representing the **hue**. Must be between `0` and `360`
#'   (inclusive) degrees.
#' @param s A numeric value representing the **saturation**. Must be between `0.0`
#'   and `1.0` (inclusive). `0.0` is grayscale, `1.0` is full saturation.
#' @param l A numeric value representing the **lightness**. Must be between `0.0`
#'   and `1.0` (inclusive). `0.0` is black, `1.0` is white, `0.5` is normal lightness.
#' @param a A numeric value representing the **alpha (opacity)**. Must be between
#'   `0` (fully transparent) and `1` (fully opaque). Defaults to `1`.
#' @param pct A logical value. If `TRUE`, the output `r`, `g`, and `b` values
#'   will be **percentages** (i.e., decimal values between `0` and `1`).
#'   If `FALSE` (default), they will be scaled to integers between `0` and `255`.
#'
#' @return A **numeric vector** of four values:
#' \describe{
#'   \item{`r`}{**Red** component, scaled `0-255` or `0-1` based on `pct`.}
#'   \item{`g`}{**Green** component, scaled `0-255` or `0-1` based on `pct`.}
#'   \item{`b`}{**Blue** component, scaled `0-255` or `0-1` based on `pct`.}
#'   \item{`a`}{**Alpha** component, the input alpha value, between `0` and `1`.}
#' }
#'
#' @seealso \code{\link{rgba_to_hsla}} for the reverse conversion.
#' @examples
#' # Convert a pure green color
#' hsla_to_rgba(h = 120, s = 1, l = 0.5, a = 1)
#' # Expected output: r=0, g=255, b=0, a=1
#'
#' # Convert a pale orange with 75% opacity (output as percentages)
#' hsla_to_rgba(h = 30, s = 0.7, l = 0.8, a = 0.75, pct = TRUE)
#' # Expected output (approx): r=0.99, g=0.86, b=0.69, a=0.75
#'
#' # Convert a fully saturated blue with 25% lightness
#' hsla_to_rgba(h = 240, s = 1, l = 0.25)
#' # Expected output: r=0, g=0, b=127.5, a=1
#' @export
hsla_to_rgba <- function(h, s, l, a = 1, pct = F) {
  # Normalize hue to 0-1 range for internal calculations
  h <- h / 360

  r <- g <- b <- 0.0 # Initialize RGB components

  # If saturation is 0, it's a grayscale color (R=G=B=L)
  if (s == 0) {
    r <- g <- b <- l
  } else {
    # Define an internal helper function for hue to RGB conversion
    # @param p Intermediate value derived from lightness
    # @param q Intermediate value derived from lightness and saturation
    # @param t Hue value (0-1) offset for R, G, B
    # @return A single RGB component value (0-1)
    hue_to_rgb <- function(p, q, t) {
      if (t < 0) {
        t <- t + 1.0
      }
      if (t > 1) {
        t <- t - 1.0
      }
      if (t < 1 / 6) {
        return(p + (q - p) * 6.0 * t)
      }
      if (t < 1 / 2) {
        return(q)
      }
      if (t < 2 / 3) {
        return(p + ((q - p) * ((2 / 3) - t) * 6))
      }
      return(p)
    }

    # Calculate intermediate values p and q
    q <- ifelse(l < 0.5, l * (1.0 + s), l + s - (l * s))
    p <- 2.0 * l - q

    # Calculate R, G, B components using the helper function with hue offsets
    r <- hue_to_rgb(p, q, h + 1 / 3)
    g <- hue_to_rgb(p, q, h)
    b <- hue_to_rgb(p, q, h - 1 / 3)
  }

  # Scale RGB values to 0-255 range if pct is FALSE
  if (pct == F) {
    r <- r * 255
    b <- b * 255
    g <- g * 255
  }
  # Return the RGBa values in a named vector
  return(c(r = r, g = g, b = b, a = a))
}