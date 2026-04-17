source("scripts/02_clean_data.R")

orders_clean <- orders_clean %>% 
  mutate(sales_amount = quantity * unit_price,
         discounted_price = unit_price * (1 - discount_pct),
         discount_amount = (unit_price - discounted_price) * quantity,
         total_after_discount = sales_amount - discount_amount
  )