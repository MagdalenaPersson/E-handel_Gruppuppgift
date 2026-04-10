# Analys - frågeställning 1 och 2
source("clean_data.R")
source("variabler.R")


# Frågeställning 1: Försäljning per produktkategori
orders_clean %>%
  group_by(product_category) %>%
  summarise(total_sales = sum(sales_amount)) %>%
  arrange(desc(total_sales))

orders_clean %>%
  group_by(product_category) %>%
  summarise(
    antal_order = n(),
    medelvarde_order = mean(sales_amount),
    median_order = median(sales_amount)
  ) %>%
  arrange(desc(medelvarde_order))


# Frågeställning 2: Rabatt och ordervärde
orders_clean %>%
  group_by(discount_pct) %>%
  summarise(
    antal = n(),
    medelvarde_order = mean(sales_amount)
  ) %>%
  arrange(discount_pct)

orders_clean %>%
  mutate(rabattgrupp = case_when(
    discount_pct == 0 ~ "Ingen rabatt",
    discount_pct < 0.1 ~ "Låg (under 10%)",
    discount_pct < 0.2 ~ "Medel (10-20%)",
    TRUE ~ "Hög (över 20%)"
  )) %>%
  group_by(rabattgrupp) %>%
  summarise(antal = n(), medelvarde_order = mean(sales_amount)) %>%
  arrange(desc(medelvarde_order))
