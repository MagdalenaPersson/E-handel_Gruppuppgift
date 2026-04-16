library(ggplot2)
source("scripts/04_analysis.R")
library(ggrepel)

# Visualisering till Fråga 1

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

# Visualisering till Fråga 2

scale_factor <- max(rabattgrupp_summary$antal) / 
  max(rabattgrupp_summary$medelvarde_order)


p_rabattgrupp_summary <- ggplot(rabattgrupp_summary, aes(x = rabattgrupp)) +
  geom_col(aes(y = antal, fill = "Antal ordrar")) +
  geom_line(aes(y = medelvarde_order * scale_factor, 
                color = "Medel ordervärde", 
                group = 1), 
            size = 1.2) +
  scale_y_continuous(
    name = "Antal ordrar",
    sec.axis = sec_axis(~ . / scale_factor, name = "Medel ordervärde")
  ) +
  scale_fill_manual(
    values = c("Antal ordrar" = "lightgrey"),
    guide = "none"   # 🔹 removes bar legend
  ) +
  scale_color_manual(
    name = "",  # optional: removes legend title
    values = c("Medel ordervärde" = "blue")
  ) +
  labs(
    title = "Antal ordrar och medel ordervärde per rabattnivå",
    x = "Rabattgrupp"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

p_rabattgrupp_summary

ggsave("output/figures/discount_sales_volume_analysis.png", p_rabattgrupp_summary, width = 8, height = 5)


# Visualisering till Fråga 3

### Plot 1 


ggplot(rabatt_impact_summary,
       aes(x = medel_quantity,
           y = medel_efter_rabatt,
           size = antal_ordrar,
           fill = rabattgrupp)) +
  geom_point(
    shape = 21,
    color = "grey80",
    stroke = 0.4,
    alpha = 0.85
  ) +
  scale_size(
    range = c(4, 10),
    guide = guide_legend(title = "Antal ordrar")
  ) +
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~ customer_type, nrow = 1) +
  labs(
    title = "Hur kundtyp och rabattnivå påverkar orderstorlek och ordervärde",
    x = "Medel antal produkter per order",
    y = "Medel ordervärde efter rabatt",
    fill = "Rabattgrupp"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.background = element_rect(fill = "grey95", color = NA),
    panel.border = element_rect(color = "grey80", linewidth = 0.6),
    panel.grid.minor = element_blank()
  ) +
  guides(
    size = guide_legend(
      position = "top",
      override.aes = list(size = 2)
    ),
    fill = guide_legend(
      position = "bottom",
      override.aes = list(shape = 21, size = 5, color = "grey80")
    )
  )


#### Plot 2

plot_data <- order_customer_type %>%
  pivot_longer(
    cols = c(medel_quantity, medelvarde_order, median_order),
    names_to = "metric",
    values_to = "value"
  )


plot_data <- plot_data %>%
  mutate(rabattgrupp = factor(rabattgrupp, levels = c(
    "Ingen rabatt",
    "Mycket låg (0-5%)",
    "Låg (5-10%)",
    "Medel (10-15%)",
    "Medel-hög (15-20%)",
    "Hög (över 20%)"
  )))

ggplot(plot_data %>% filter(metric == "medel_quantity"),
       aes(x = customer_type,
           y = rabattgrupp,
           fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Genomsnittlig quantity per kundtyp och rabattnivå",
    x = "Kundtyp",
    y = "Rabattgrupp",
    fill = "Medel quantity"
  ) +
  theme_minimal()

