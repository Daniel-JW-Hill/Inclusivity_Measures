
#Takes results on contributions and plots them as a chart and saves them in results

plot_chart = function(results, 
                      contributions_of,
                      sample,
                      outcome,
                      model,
                      ineq_label,
                      x_labels,
                      reference_value) {
    
  # Compute percentage difference
  results$percentage_diff = results[[contributions_of]]
  results = results[results$X != "overall", ]  # Removing the "overall" row
  results$X2 = x_labels
  
  results = results[order(results$percentage_diff, decreasing = FALSE), ]
  results$X2 = factor(results$X2, levels = results$X2)
  
  # Plotting
  plot = ggplot(results, aes(x = X2, y = percentage_diff, fill = factor(ifelse(percentage_diff < 0, "lower", "higher")))) + 
    geom_bar(stat = "identity", color = "black") +
    scale_fill_manual(values = c("lower" = "grey70", "higher" = "grey70"), guide = FALSE)  + 
    labs(y = "% difference to mean estimate") +  
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  # Save the plot
  ggsave(here("Results", paste("Changes in ", ineq_label, " - ", outcome, " - ", sample, " - ", model, ".png")),
         plot = plot, width = 15, height = 10, units = 'cm')
}

  
        