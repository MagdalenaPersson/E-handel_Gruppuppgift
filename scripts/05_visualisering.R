library(ggplot2)

source("scripts/04_analysis.R")


library(ggrepel)

p_category_summary <- ggplot(category_summary,aes(x = avg_order, 
                                                  y = total_sales, 
                                                  size = antal_order, 
                                                  color = product_category)) +
                        geom_point(alpha = 0.7) +
                        geom_text_repel(
                          aes(label = product_category),
                          size = 5,              
                          box.padding = 0.5,
                          point.padding = 0.5,
                          segment.color = "grey50"
                        ) +
                        labs(
                          title = "Sales vs Average Order Value",
                          x = "Average Order Value",
                          y = "Total Sales",
                          size = "Number of Orders"
                        ) +
                        guides(color = "none")
                      

p_category_summary

ggsave("output/figures/category_sales_analysis.png", p_category_summary, width = 8, height = 5)