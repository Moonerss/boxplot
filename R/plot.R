#' @title plot box plot for shiny
#' @author Erjie Zhao
#' @param plot_dat data with four column: genes, name, value, Group
#' @param log whether log10 convert
#' @param plot_by_group plot by group info
#' @param show_point whether show point
#' @param point_jitter point whether jitter
#' @param show_outliers show outliers
#' @param show_errorbar show error bar
#' @param xlab x lab
#' @param ylab y lab
#' @param main title
#' @param theme ggplot theme
#' @param legend_title legend title
#' @param show_legend whether show legend
#' @param legend_in whether legend in plot
#' @param legend_position legend position
#' @param legend_x x position
#' @param legend_y y position
#' @param title_size title size
#' @param axis_font_size axis text size
#' @param legend_title_size legend title size
#' @param axis_x_font_angle x axis text angle
#' @param multiple whether compare multiple group
#' @param multiple_method the method to compare multiple group
#' @param multiple_x the p value x position
#' @param multiple_y the p value y position
#' 

plot_box <- function(plot_dat, log = TRUE, plot_by_group = FALSE,
                     show_point = FALSE, point_jitter = FALSE,
                     show_outliers = TRUE, show_errorbar = TRUE,
                     xlab = 'Sample', ylab = 'log10(Value+1)',
                     main = 'Boxplot', theme = 'theme_classic()',
                     legend_title = 'Group', show_legend = TRUE,
                     legend_in = FALSE, legend_position = 'right',
                     legend_x = NULL, legend_y = NULL,
                     title_size = 20, axis_font_size = 9,
                     legend_title_size = 10, axis_x_font_angle = 0,
                     labs_title_size = 10,
                     y_lim = c(NA, NA),
                     multiple = F, multiple_method = c('Anova', 'kruskal.test'),
                     multiple_x = NULL, multiple_y = NULL,
                     two_compare = F, two_method = c('wilcox.test', 't.test'),
                     signf_method = c('p', 'star')) {
  if (log) {
    plot_dat$value <- log10(plot_dat$value + 1)
  }
  ## whether plot by group
  if (plot_by_group) {
    p <- ggplot(plot_dat) +
      aes(x = Group, y = value, fill = Group)
  } else {
    p <- ggplot(plot_dat) +
      aes(x = name, y = value, fill = Group)
  }
  if (show_point) {
    p <- p + geom_point()
    if (point_jitter) {
      p <- p + geom_jitter(width = 0.25)
    }
  }
  if (show_errorbar) {
    p <- p + stat_boxplot(geom = 'errorbar', width = 0.25)
  }
  if (!show_outliers) {
    p <- p + geom_boxplot(outlier.shape = NA)
  } else {
    p <- p + geom_boxplot()
  }
  
  ## y lim
  if (is.na(y_lim[1])) {
    if (is.na(y_lim[2])) {
      p <- p
    } else {
      p <- p + ylim(NA, y_lim[2])
    }
  } else {
    if (is.na(y_lim[2])) {
      p <- p + ylim(y_lim[1], NA)
    } else {
      p <- p + ylim(y_lim[1], y_lim[2])
    }
  }
  
  ## add two compare
  
  
  ## add lab
  p <- p + labs(title = main, x = xlab, y = ylab, fill = legend_title)
  ## add theme
  p <- p + base::eval(rlang::parse_expr(theme))
  ## legend
  if (!show_legend) {
    p <- p + theme(legend.position = 'none')
  } else {
    if (!legend_in) {
      p <- p + theme(legend.position = legend_position)
    } else {
      p <- p + theme(legend.position = c(legend_x, legend_y))
    }
  }
  ## font size
  p <- p +
    theme(axis.text = element_text(size = axis_font_size),
          axis.title = element_text(size = labs_title_size),
          plot.title = element_text(size = title_size, hjust = 0.5),
          axis.text.x = element_text(angle = axis_x_font_angle, hjust = 0.5, vjust = 0.5),
          legend.title = element_text(size = legend_title_size))
  
  return(p)
}