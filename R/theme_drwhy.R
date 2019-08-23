#' @title  DrWhy Theme for ggplot objects
#'
#' @return theme for ggplot2 objects
#' @rdname theme_drwhy
theme_drwhy <- function() {
  theme_bw(base_line_size = 0) %+replace%
    theme(
      # upper stip
      strip.background = element_blank(),
      strip.text = element_text(color = "#371ea3", size = 12, hjust = 0, margin = margin(0, 0, 1, 0)),

      # axes
      axis.ticks = element_blank(),
      axis.title = element_text(color = "#371ea3"),
      axis.text = element_text(color = "#371ea3", size = 10),

      # legend
      legend.direction = "horizontal",
      legend.justification = c(1, 0),
      legend.position = c(1, 0.97),
      legend.background = element_blank(),
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(color = "#371ea3", size = 10),

      # panel
      panel.background = element_blank(),
      panel.border = element_blank(),

      # plot backgroud and title
      plot.background = element_blank(),
      plot.title = element_text(color = "#371ea3", face = "bold", hjust = 0),

      complete = TRUE,

      # changeable values
      axis.line.x = element_line(color = "#371ea3"),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", size = 0.5, linetype = 1),
      panel.grid.minor.y = element_line(color = "grey90", size = 0.5, linetype = 1)
    )

}

# #' @rdname theme_drwhy
# theme_drwhy_vertical <- function() {
#   theme_drwhy() %+replace%
#     theme(
#       axis.line.x = element_blank(),
#       axis.ticks.x = element_blank(),
#       panel.grid.major.y = element_blank(),
#       panel.grid.minor.y = element_blank(),
#       panel.grid.major.x = element_line(color = "grey90", size = 0.5, linetype = 1),
#       panel.grid.minor.x = element_line(color = "grey90", size = 0.5, linetype = 1)
#     )
#
# }
#
#
# theme_drwhy_blank <- function() {
#   theme_drwhy() %+replace%
#     theme(
#       axis.line.x = element_blank(),
#       axis.ticks.x = element_blank(),
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor.x = element_blank(),
#       panel.grid.major.y = element_blank(),
#       panel.grid.minor.y = element_blank()
#     )
# }


#' @param n number of colors to get
#'
#' @rdname theme_drwhy
theme_drwhy_colors <- function(n = 2) {
  if (n == 1) return(c("#371ea3"))
  if (n == 2) return(c("#371ea3", "#8bdcbe"))
  if (n == 3) return(c("#8bdcbe", "#f05a71", "#4378bf"))
  if (n == 4) return(c("#8bdcbe", "#f05a71", "#4378bf", "#ffa58c"))
  if (n == 5) return(c("#8bdcbe", "#f05a71", "#4378bf", "#ae2c87", "#ffa58c"))
  if (n == 6) return(c("#8bdcbe", "#f05a71", "#46bac2", "#ae2c87", "#ffa58c", "#4378bf"))
  if (n == 7) return(c("#8bdcbe", "#f05a71", "#371ea3", "#46bac2", "#ae2c87", "#ffa58c", "#4378bf"))
  c("#8bdcbe", "#f05a71", "#371ea3", "#46bac2", "#ae2c87", "#ffa58c", "#4378bf")[((0:(n-1)) %% 7) + 1]
}
