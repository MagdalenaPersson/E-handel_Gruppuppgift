library(tidyverse)

orders_clean <- orders_clean %>% 
  mutate(
    discounted_price = unit_price * (1 - discount_pct),
    sales_amount = quantity * unit_price,
    discount_amount = (unit_price - discounted_price) * quantity,
    avg_price_after_discount = discounted_price
  )