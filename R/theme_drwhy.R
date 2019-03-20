
#' DrWhy Theme for ggplot objects
#'
#' @param mode Default is a theme designed for vertical layout. Other possible vaules are `vertical` and `blank`.
#'
#' @return theme for ggplot2 objects
#' @export
#'
#' @rdname theme_drwhy
theme_drwhy <- function(mode = "regular") {

  theme_bw() %+replace%
    theme(
      strip.background = element_blank(),
      strip.text = element_text(color = "#371ea3",
                                size = 12,
                                hjust = 0,
                                margin = margin(0, 0, 1, 0)),

      axis.ticks = element_blank(),
      axis.title = element_text(color = "#371ea3"),
      axis.text = element_text(color = "#371ea3",
                               size = 10),

      legend.direction = "horizontal",
      legend.justification = c(1, 0),
      legend.position = c(1, 0.98),
      legend.background = element_blank(),
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(color = "#371ea3",
                                 size = 10),

      panel.background = element_blank(),
      panel.border = element_blank(),

      plot.background = element_blank(),
      plot.title = element_text(color = "#371ea3",
                                face = "bold",
                                hjust = 0)
    ) %+replace%

    if (mode == "regular") {
      theme(
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(color = "grey90",
                                          size = 0.5,
                                          linetype = 1),
        panel.grid.minor.y = element_line(color = "grey90",
                                          size = 0.5,
                                          linetype = 1),

        complete = TRUE
      )

    } else if (mode == "vertical") {
      theme(
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey90",
                                          size = 0.5,
                                          linetype = 1),
        panel.grid.minor.x = element_line(color = "grey90",
                                          size = 0.5,
                                          linetype = 1),

        complete = TRUE
      )

    } else if (mode == "blank") {
      theme(
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),

        complete = TRUE)

    }
}



#' @export
#' @rdname theme_drwhy
#' @param n number of colors for color palette
theme_drwhy_colors <- function(n = 2) {
  if (n == 1) return(c("#371ea3"))
  if (n == 2) return(c("#8bdcbe", "#4378bf"))
  if (n == 3) return(c("#8bdcbe", "#f05a71", "#4378bf"))
  if (n == 4) return(c("#8bdcbe", "#f05a71", "#4378bf", "#ffa58c"))
  if (n == 5) return(c("#8bdcbe", "#f05a71", "#4378bf", "#ae2c87", "#ffa58c"))
  if (n == 6) return(c("#8bdcbe", "#f05a71", "#46bac2", "#ae2c87", "#ffa58c", "#4378bf"))
  if (n == 7) return(c("#8bdcbe", "#f05a71", "#371ea3", "#46bac2", "#ae2c87", "#ffa58c", "#4378bf"))
  c("#8bdcbe", "#f05a71", "#371ea3", "#46bac2", "#ae2c87", "#ffa58c", "#4378bf")[((0:(n-1)) %% 7) + 1]
}



#' @export
#' @rdname theme_drwhy
theme_drwhy_colors_gradient <- function() {
  c("#c7f5bf", "#371ea3")
}
