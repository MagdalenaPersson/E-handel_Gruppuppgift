library(tidyverse)

# Läs in data
# here::here skapar en relativ filväg till datamappen
orders <- read_csv(here::here("data/raw/ecommerce_orders.csv"))

# Visa datasetets storlek
dim(orders)
glimpse(orders)

# Datasettet har 1000 rader och 16 kolumner
# 11 kategoriska, 1 datum, 4 doubles

# Hitta saknade värden
colSums(is.na(orders))

# city: 21 saknade värden
# payment_method: 25 saknade värden
# campaign_source: 31 saknade värden
# discount_pct: 27 saknade värden
# shipping_days: 22 saknade värden

# Kolla unika värden i regionala. kategoriska variabler
orders %>% count(region)
orders %>% count(city) %>% arrange(city)

# city behöver städas - inkonsekvent stora/små bokstäver
# inga å, ä, ö

# Kolla unika värden i övriga kategoriska variabler
orders %>% count(product_category) %>% arrange(product_category)
orders %>% count(customer_segment) %>% arrange(customer_segment)
orders %>% count(customer_type) %>% arrange(customer_type)
orders %>% count(payment_method) %>% arrange(payment_method)
orders %>% count(campaign_source) %>% arrange(campaign_source)
orders %>% count(returned) %>% arrange(returned)

# payment_method och campaign_source behöver städas

# Viktigaste variabler för vår analys:

# Frågeställning 1 - Produktkategorier och försäljning:
# - product_category: huvudvariabeln för gruppering
# - quantity: antal sålda enheter
# - unit_price: styckpris
# - sales_amount behöver skapas (quantity * unit_price)

# Frågeställning 2 - Samband mellan rabatt och ordervärde:
# - discount_pct: rabatt (27 saknade värden - behöver hanteras)
# - unit_price och quantity: för att beräkna ordervärde
# - discounted_price behöver skapas (unit_price * (1 - discount_pct))







