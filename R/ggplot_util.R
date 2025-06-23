#' Apply consistent theme to ggplot visualizations
#'
#' @param p ggplot object
#' @param title Optional title for the plot
#' @param subtitle Optional subtitle for the plot
#' @param x_label Optional x-axis label
#' @param y_label Optional y-axis label
#' @param rotate_x_labels Whether to rotate x-axis labels (default: FALSE)
#' @param rotate_y_labels Whether to rotate y-axis labels (default: FALSE)
#'
#' @return ggplot object with applied theme
#' @export
apply_naa_theme <- function(p, 
                          title = NULL, 
                          subtitle = NULL,
                          x_label = NULL,
                          y_label = NULL,
                          rotate_x_labels = FALSE,
                          rotate_y_labels = FALSE) {
  
  # Base theme
  p <- p + ggplot2::theme_minimal() +
    ggplot2::theme(
      # Text elements
      plot.title = ggplot2::element_text(
        size = 14,
        face = "bold",
        hjust = 0,
        margin = ggplot2::margin(b = 10)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 12,
        hjust = 0,
        margin = ggplot2::margin(b = 15)
      ),
      axis.title = ggplot2::element_text(
        size = 11,
        face = "bold"
      ),
      axis.text = ggplot2::element_text(
        size = 10,
        color = "gray30"
      ),
      
      # Grid and background
      panel.grid.major = ggplot2::element_line(
        color = "gray90",
        linewidth = 0.5
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(
        fill = "white",
        color = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = "white",
        color = NA
      ),
      
      # Margins and spacing
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      panel.spacing = ggplot2::unit(1, "lines"),
      
      # Legend
      legend.title = ggplot2::element_text(
        size = 11,
        face = "bold"
      ),
      legend.text = ggplot2::element_text(
        size = 10
      ),
      legend.position = "right",
      legend.background = ggplot2::element_rect(
        fill = "white",
        color = NA
      )
    )
  
  # Add title and subtitle if provided
  if (!is.null(title)) {
    p <- p + ggplot2::labs(title = title)
  }
  if (!is.null(subtitle)) {
    p <- p + ggplot2::labs(subtitle = subtitle)
  }
  
  # Add axis labels if provided
  if (!is.null(x_label)) {
    p <- p + ggplot2::labs(x = x_label)
  }
  if (!is.null(y_label)) {
    p <- p + ggplot2::labs(y = y_label)
  }
  
  # Rotate labels if requested
  if (rotate_x_labels) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
  if (rotate_y_labels) {
    p <- p + ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1))
  }
  
  p
}
