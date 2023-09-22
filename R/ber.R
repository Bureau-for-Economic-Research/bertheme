#' Bureau for Economic Research theme
#'
#' Theme based on the plots in \emph{Bureau for Economic Research} reporting.
#'
#' This theme should be used with \code{\link{scale_color_ber}()} and \code{\link{scale_fill_ber}()}.
#'
#' @references
#'
#' \url{https://ber.ac.za}
#'
#' @param base_size Numeric value specifying the base size for plot elements. Default is 12.
#' @param base_family Character string specifying the font family for base plot elements. Default is "sans".
#' @param title_size Numeric value specifying the size of the plot title. Default is 14.
#' @param title_family Character string specifying the font family for the plot title. Default is "Helvetica".
#' @param subtitle_size Numeric value specifying the size of the plot subtitle. Default is 12.
#' @param caption_size Numeric value specifying the size of the plot caption. Default is 12.
#' @param subtitle_color Character string specifying the color of the plot subtitle. Default is "#FF790F".
#' @param title_color Character string specifying the color of the plot title. Default is "#0154C3".
#' 
#' @family themes ber
#' 
#' @section Usage:
#' To use `theme_ber`, simply add it to your ggplot2 plot chain. For instance:
#' ```R
#' ggplot(data, aes(x, y)) + 
#'   geom_point() + 
#'   theme_ber()
#' ```
#'
#' @seealso 
#' Other related themes and utilities in this package.
#'
#' @examples 
#' \dontrun{
#' library(ggplot2)
#' data(mpg)
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   theme_ber(title_size = 16, title_family = "Times", title_color = "#FF0000")
#' }
#'
#' @return 
#' A ggplot2 theme.
#'
#' @export
theme_ber <- function(base_size = 12,
                      base_family = "sans",
                      title_size = 14,
                      title_family = "Helvetica",
                      subtitle_size = 12,
                      caption_size = 12, 
                      subtitle_color = "#FF790F",
                      title_color = "#0154C3"
                      ) {
  
  theme_foundation(base_size = base_size, base_family = base_family) +
    theme(
      line = element_line(linetype = 1, colour = "black"),
      rect = element_rect(fill = "white", linetype = 0, colour = NA),
      text = element_text(colour = "black"),
      title = element_text(family = title_family,
                           size = rel(2)),
      panel.background = element_rect(fill = "white"), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      # panel.grid.major.x = element_blank(), 
      panel.grid.major.y = element_line(colour = "grey"),
      axis.text = element_text(size = 9, 
                               color = "black"),
      axis.title = element_text(size = 12, colour = "black"),
      axis.ticks = element_blank(),
      # axis.line.x = element_line(colour = "black", size = 0.3),
      # axis.line.y = element_line(colour = "black", size = 0.3),
      legend.position = "bottom", 
      legend.key = element_blank(),
      legend.title = element_blank(),
      legend.key.width = unit(1, "cm"),
      legend.text = element_text(size = 10, 
                                 colour = "black"), # change 8 to 10 if too small
      legend.box = "horizontal",
      legend.box.just = "center", 
      strip.background = element_rect(fill = "white"), #this is for when you do facet_rep_wrap or facet_wrap
      strip.text = element_text(colour = "#002b60"),
      plot.title = element_text(size = title_size, color = title_color, face = "bold"),
      plot.subtitle = element_text(size = subtitle_size, face = "bold", color = subtitle_color), 
      plot.caption = element_text(size = caption_size, color = "darkgrey")
    )
}

#' Bureau for Economic Research color palette (discrete)
#'
#' The Bureau for Economic Research uses many different color palettes in its plots. Most of these are captured in this package.
#'  
#' @section Palettes:
#' The following palettes are available:
#'
#' \describe{
#'   \item{core}{A core set of colors combining blues with contrasting shades: "#002E60", "#FF790F", "#0154C3", "#0F76FF", "#61223B", "#b79962"}
#'   \item{darks}{Darker shades ideal for backgrounds or to emphasize certain data points: "#FF790F", "#61223B", "#002E5F", "#A24800", "#3A1423", "#001C39"}
#'   \item{blue_and_gold}{A blend of blues and golds for a classic, regal appearance: "#0F76FF", "#B79961", "#0154C3", "#0046A2", "#735D35", "#013275"}
#'   \item{expanded_core}{An extended version of the core palette with additional variations: "#FF790F", "#002E60", "#0154C3", "#0F76FF", "#FF8D44", "#275A87", "#2370D2","#3B8FFA"}
#'   \item{navy_alt}{Shades of navy, perfect for a subdued, professional look: "#002E60", "#0D386B", "#1E4D7C", "#316490", "#4B7FAA", "#6DA1C6", "#94C3E3", "#C1E1F5"}
#' }
#'
#' @param palette \code{character} The color palette to use. One of the following: \code{core}, \code{darks}, \code{blue_and_gold}, \code{expanded_core}, or \code{navy_alt}.
#' 
#' @details 
#' These palettes are designed to work well in various combinations, allowing for flexibility in plotting while maintaining visual coherence. Users can select the palette most appropriate for their data visualization needs.
#'
#' @seealso 
#' Other related color utilities in this package.
#'
#' @family colour ber
#' @export
ber_pal <- function(palette = "core"){
  palettes <- bertheme_data()
  
  if (palette %in% names(palettes)) {
    colors <- palettes[[palette]]
    max_n <- length(colors)
    f <- manual_pal(unname(colors))
    attr(f, "max_n") <- max_n
    f
  } else {
    stop(sprintf("palette %s not a valid palette.", palette))
  }
}

#' Bureau for Economic Research and fill scales
#'
#' Colour and fill scales which use the palettes in \code{\link{ber_pal}()}.
#' These scales should be used with \code{\link{theme_ber}()}.
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @inheritParams ber_pal
#' @family colour ber
#' @rdname scale_ber
#' @export
scale_colour_ber <- function(palette = "core", ...) {
  discrete_scale("colour", "ber", ber_pal(palette), ...)
}

#' @rdname scale_ber
#' @export
scale_color_ber <- scale_colour_ber

#' @rdname scale_ber
#' @export
scale_fill_ber <- function(palette = "core", ...) {
  discrete_scale("fill", "ber", ber_pal(palette), ...)
}

#' Add Event Annotations to ggplot2 Plots
#'
#' \code{geom_event} provides a convenient way to highlight a specific period or event
#' on a ggplot2 plot using a rectangle and label.
#'
#' @param start Starting date of the event.
#' @param end Ending date of the event.
#' @param label Label to annotate the event.
#'
#' @return A list containing \code{geom_rect} and \code{annotate} functions to 
#'         shade the region of interest and label it respectively.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' data <- data.frame(Date = as.Date('2020-01-01') + 0:364, Value = rnorm(365))
#' ggplot(data, aes(x = Date, y = Value)) +
#'   geom_line() +
#'   scale_colour_ber("core") +
#'   geom_event(start = "2020-02-01", end = "2020-04-28", label = "Summer", fill = "#0154C3") +
#'   theme_ber()
#' }
#'
#' @seealso \code{\link[ggplot2]{geom_rect}}, \code{\link[ggplot2]{annotate}} for the underlying functions.
#'
#' @export
geom_event <- function(start, 
                       end, 
                       label = "",
                       label_size = 4,
                       label_color = "#002E60",
                       fill = "darkgrey", 
                       vjust = 1.5,
                       alpha = 0.05) {
  list(

    geom_rect(data = data.frame(Date = as.Date(c(start, end))),
              aes(xmin = as.Date(start), xmax = as.Date(end), ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = fill, 
              alpha = alpha),
    
    annotate("text", x = as.Date(start), y = Inf, label = label, 
             vjust = vjust, 
             hjust = 1,
             color = label_color, size = 3)
    
  )
}