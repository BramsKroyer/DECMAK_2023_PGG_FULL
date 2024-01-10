# ------ PLOT FUNCTION: RECOVERY
recov_plot <- function(true, infer, plot_lab, plot_col) {
  
  # library(ggplot2)
  
  df <- data.frame(true, infer)
  
  pl <- ggplot(df, aes(x = true,
                       y = infer)) + 
    geom_point(color = 'darkslategray3',
               alpha = 0.9) +
    geom_abline(intercept=0, 
                slope=1, 
                linetype=2) +
    geom_smooth(method = "lm", 
                se = T, 
                formula = "y ~ x",
                color = 'darkslategray4',
                alpha = 0.3) +
    theme_minimal() +
    xlab(plot_lab[1]) +
    ylab(plot_lab[2]) +
    labs(color = "") +
    ggtitle(paste0("'", plot_lab[2], "' compared to '", plot_lab[1], "'"))+
    theme(plot.title = element_text(face = "bold"))
  
  return(pl)
  
}