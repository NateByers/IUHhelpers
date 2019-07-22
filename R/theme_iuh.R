#' ggplot2 theme for IU Health graphs
#' @importFrom ggplot2 theme theme_dark theme_gray
#' @export
#' @author Joe Walters
#' @examples
#' library(ggplot2)
#' 
#' ggplot(mpg, aes(class, fill = drv)) + geom_bar() + theme_iuh() + 
#'   scale_fill_iuh() + ggtitle("IU Health")
theme_iuh <- function(dark = FALSE) {
  
  if("extrafont" %in% (.packages())) {
    if("extrafontdb" %in% library()$results[, 1]) {
      imported_fonts <- read.csv(paste0(find.package("extrafontdb"),
                                        "/fontmap/fonttable.csv"),
                                 stringsAsFactors = FALSE)
      
      if("Calisto MT" %in% imported_fonts$FullName) {
        font_title <- "Calisto MT"
      } else {
        font_title <- NULL
      }
      
      if("Franklin Gothic Book" %in% imported_fonts$FullName) {
        font_text <- "Franklin Gothic Book"
      } else {
        font_text <- NULL
      }
      
    } else {
      font_title <- NULL
      font_text <- NULL
    }
  } else {
    font_title <- NULL
    font_text <- NULL
  }
  
  if(dark) {
    iuh_theme <- theme_dark()
  } else {
    iuh_theme <- theme_gray()
  }
  
  iuh_theme +
    ggplot2::theme(panel.border = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_blank(),
                   legend.title = element_blank(),
                   legend.text = element_text(
                     color = "#231f20", family = font_text, size = 11),
                   legend.background = element_blank(),
                   plot.title = element_text(
                     color = "#981E32", family = font_title, size = 16, face = "bold", h = 0),
                   plot.subtitle = element_text(
                     color = "#981E32", family = font_title, size = 14, h = 0),
                   plot.caption = element_text(
                     color = "#231f20", family = font_title, size = 8),
                   plot.tag = element_text(
                     color = "#231f20", family = font_title, size = 8),
                   axis.title = element_text(
                     color = "#231f20", family = font_text, size = 12, face = "bold"),
                   axis.text = element_text(
                     color = "#231f20", family = font_text, size = 10),
                   strip.text = element_text(
                     color = "#231f20", family = font_title, size = 12))
  
}

#' IU Health color palette (discrete)
#' @rdname scale_iuh
#' @param fill Use the fill palette.
#' @export
iuh_pal <- function(fill=TRUE) {
  
  max_n <- 14
  
  f <- function(n) {
    check_pal_n(n, max_n)
    if (n == 1) {
      color_names <- data.frame(color = "PMS 201", stringsAsFactors = FALSE)
    } else if (n == 2) {
      color_names <- data.frame(color = c("IUH Cream", "PMS 201"),
                                stringsAsFactors = FALSE)
    } else if (n > 2) {
      primary_colors <- c("Warm Gray 6", "IUH Cream", "PMS 201")
      
      secondary <- iuh_colors$color[iuh_colors$type == "secondary"]
      
      color_names <- data.frame(color = c(primary_colors, secondary),
                                stringsAsFactors = FALSE)
    }
    
    color_names %>%
      dplyr::left_join(iuh_colors, "color") %>%
      dplyr::pull(hex)
  }
  
  attr(f, "max_n") <- max_n
  f
}

#' IU Health color scales
#' 
#' @rdname scale_iuh
#' @export
#' @importFrom ggplot2 discrete_scale
scale_color_iuh <- function(...) {
  ggplot2::discrete_scale("color", "iuh", iuh_pal(), ...)
}

#' @rdname scale_iuh
#' @export
scale_colour_iuh <- scale_color_iuh

#' IU Health color fill scales
#' @rdname scale_iuh
#' @export
scale_fill_iuh <- function(...) {
  ggplot2::discrete_scale("fill", "iuh", iuh_pal(), ...)
}

# copied from ggthemes package
check_pal_n <- function(n, max_n) {
  if (n > max_n) {
    warning("This palette can handle a maximum of ", max_n, " values.",
            "You have supplied ", n, ".")
  } else if (n < 0) {
    stop("`n` must be a non-negative integer.")
  }
}
