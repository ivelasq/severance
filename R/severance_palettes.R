# List of Severance color palettes and the order in which they are printed

#' Complete list of palettes.
#'
#' Use names(severance) to return all possible palette names. Current choices are:
#' \code{Dinner}, \code{Hell}, \code{HideAndSeek}, \code{Jazz01}, \code{Jazz02}, \code{TheYouYouAre}
#' @export
severance_palettes <- list(
  Dinner = c("#88AAC6", "#113847", "#4A5B65", "#D6E3EC", "#21475A"),
  Hell = c("#9BAAAF", "#11304F", "#989B6C", "#3F4738", "#195972", "#4D251D"),
  HideAndSeek = c("#405B76", "#E6F0F1", "#694F42", "#1F334B", "#9B4D40", "#171F22"),
  Jazz01 = c("#D29292", "#525832", "#F98F5B", "#7E8273", "#F17983"),
  Jazz02 = c("#6B8C81", "#0E171E", "#BD9A5A", "#3E2423", "#8797A7", "#393F3B"),
  TheYouYouAre = c("#122921", "#CFDED7", "#173746", "#806655", "#0A3C79")
)

#' Severance palette generator
#'
#' These are a handful of color palettes from the show Severance
#'
#' @param n Number of colors desired. If omitted, uses all colours.
#' @param name Name of desired palette. Choices are:
#' \code{Dinner}, \code{Hell}, \code{HideAndSeek}, \code{Jazz01}, \code{Jazz02}, \code{TheYouYouAre}
#' @param type Either "continuous" or "discrete". Use continuous if you want
#'   to automatically interpolate between colors.
#' @return A vector of colors.
#' @export
#' @keywords colors
#' @examples
#' severance_palette("Jazz02")
#' severance_palette("Hell")
#' severance_palette("TheYouYouAre")
#' severance_palette("Jazz01", 3)
#'
#' # If you need more colors than normally found in a palette, you
#' # can use a continuous palette to interpolate between existing
#' # colors
#' pal <- severance_palette(name = "Jazz02", n = 2, type = "continuous")
severance_palette <-
  function(name, n, type = c("discrete", "continuous")) {
    type <- match.arg(type)

    pal <- severance_palettes[[name]]
    if (is.null(pal))
      stop("Palette not found.")

    if (missing(n)) {
      n <- length(pal)
    }

    if (type == "discrete" && n > length(pal)) {
      stop("Number of requested colors greater than what palette can offer")
    }

    out <- switch(type,
                  continuous = grDevices::colorRampPalette(pal)(n),
                  discrete = pal[1:n])
    structure(out, class = "palette", name = name)
  }
