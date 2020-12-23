### Functions to generate barplots

#' Barplot with labels
#'
#' Function creating a visualization based on \code{geom_bar()} with count and percent labels.
#' @param dataframe disaggregated data to plot
#' @param xvar variable to appear on the x axis; percents are computed based on this
#' @param fillvar variable to be used for color
#' @param title optional string to be set as the chart title
#' @import dplyr ggplot2
#' @importFrom magrittr %>%
#' @importFrom scales percent
#' @export
#' @examples
#' data(diamonds, package = 'ggplot2')
#' gg_barplot_pct(diamonds, xvar = cut, fillvar = color, title = 'Diamond color and cut')

gg_barplot_pct <- function(dataframe, xvar, fillvar, title = NULL) {

  xvar <- enquo(xvar)
  fillvar <- enquo(fillvar)

  dataframe %>%
    group_by(!!xvar, !!fillvar) %>%
    summarise(n = n()) %>%
    group_by(!!xvar) %>%
    mutate(
      pct = .data$n / sum(.data$n)
      , pct_str = .data$pct %>% scales::percent()
    ) %>%
    ggplot(aes(x = !!xvar, y = .data$n, fill = !!fillvar)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = paste0(.data$n, ' (', .data$pct_str, ')')), position = position_stack(vjust = 0.5)) +
    labs(title = title)
}
