
#Takes results on contributions and plots them as a chart and saves them in results

plot_chart = function(results, 
                      contributions_of,
                      sample,
                      outcome,
                      model,
                      ineq_label,
                      x_labels,
                      reference_value) {
  
  
  results$percentage_diff =  results[,contributions_of]
  results = results[-which(results$X == "overall"),]
  results$X = factor(results$X, levels = rev(results$X))
  
  plot = ggplot(results, aes(x = X, y = percentage_diff, fill = factor(ifelse(percentage_diff < 0, "lower", "higher")))) + 
                geom_bar(stat = "identity") +
                scale_fill_manual(values = c("lower" = "lightcoral", "higher" = "lightblue"), guide = FALSE) +
                labs(y = paste("Percentage difference in", contributions_of), y = "Values", 
                     title = paste("Changes in", contributions_of, "in", ineq_label,"\n",outcome,"\n",sample, " - ",model))+
                scale_x_discrete(labels = rev(x_labels))+
                theme(axis.title.y=element_blank(),
                      panel.grid.major = element_blank(),
                      panel.background = element_rect(fill = "white"),
                      axis.text.y = element_text(size = 14),
                      axis.text.x = element_text(size = 14))+
                scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
                coord_flip() 
  
  ggsave(here("Results", paste("Changes in ", ineq_label," - ",outcome, " - ", sample, " - ", model, ".png")),
    plot = plot)
  
              
}