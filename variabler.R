library(tidyverse)
orders_clean <- orders_clean %>% 
  mutate(sales_amount = quantity * unit_price,
         discounted_price = unit_price * (1 - discount_pct),
         discount_amount = (unit_price - discounted_price) * quantity,
         avg_price_after_discount = sales_amount / quantity
  )