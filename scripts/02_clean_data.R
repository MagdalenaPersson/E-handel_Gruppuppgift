orders <- read_csv(here::here("data/ecommerce_orders.csv"))


# city behöver städas - inkonsekvent stora/små bokstäver
# inga å, ä, ö
# payment_method och campaign_source behöver städas

orders_clean <- orders %>%
  mutate(
    order_date = as.Date(order_date),
    city = str_trim(city),
    city = str_to_title(city),
    payment_method = str_trim(payment_method),
    payment_method = str_to_title(payment_method),
    campaign_source = str_trim(campaign_source),
    campaign_source = str_to_title(campaign_source),
    campaign_source = case_when(
      campaign_source == "Social" ~ "Social Media", 
      TRUE ~ campaign_source
    )
  )


orders_clean %>% count(payment_method) %>% arrange(payment_method)
orders_clean %>% count(campaign_source) %>% arrange(campaign_source)
orders_clean %>% count(city) %>% arrange(city)


# Hitta saknade värden
colSums(is.na(orders))

# city: 21 saknade värden
# payment_method: 25 saknade värden
# campaign_source: 31 saknade värden
# discount_pct: 27 saknade värden
# shipping_days: 22 saknade värden

# discount_pct är den variabel 
# med saknade värden som är relevant för våra frågeställningar
# 27 av 1000 värden saknas och vi väljer att ta bort dessa rader

orders_clean <- orders_clean %>%
  filter(!is.na(discount_pct))



