# Dataset efter städning: 973 ordrar (27 rader med saknade discount_pct borttagna)

# Analys - frågeställning 1, 2 och 3


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

category_summary <- orders_clean %>%
  group_by(product_category) %>%
  summarise(
    total_sales = sum(sales_amount),
    avg_order = mean(sales_amount),
    antal_order = n()
  )
  
  category_summary

# Electronics dominerar med 161 790 kr i total försäljning, nästan tre gånger mer än Home 
# på andra plats. Electronics har också högst medelordervärde (691 kr) och näst flest ordrar (234). 
  # Beauty har lägst försäljning på alla mått.
  
  
  # Frågeställning 2: Rabatt och ordervärde
  orders_clean %>%
    group_by(discount_pct) %>%
    summarise(
      antal = n(),
      medelvarde_order = mean(sales_amount)
    ) %>%
    arrange(discount_pct)
  
  rabattgrupp_summary <- orders_clean %>%
    mutate(rabattgrupp = case_when(
      discount_pct == 0 ~ "Ingen rabatt",
      discount_pct < 0.05 ~ "Mycket låg (0-5%)",
      discount_pct < 0.10 ~ "Låg (5-10%)",
      discount_pct < 0.15 ~ "Medel (10-15%)",
      discount_pct < 0.20 ~ "Medel-hög (15-20%)",
      TRUE ~ "Hög (över 20%)"
    )) %>%
    group_by(rabattgrupp) %>%
    summarise(antal = n(), medelvarde_order = mean(sales_amount)) %>%
    arrange(desc(medelvarde_order))
  
  rabattgrupp_summary <- rabattgrupp_summary %>%
    mutate(rabattgrupp = factor(rabattgrupp, levels = c(
      "Ingen rabatt",
      "Mycket låg (0-5%)",
      "Låg (5-10%)",
      "Medel (10-15%)",
      "Medel-hög (15-20%)",
      "Hög (över 20%)"
    )))
  
  
  rabattgrupp_summary
  
  
  # De flesta ordrar har små rabatter — 613 av 973 ordrar har under 10% rabatt.
  # Endast 19 ordrar har hög rabatt (över 20%), vilket är för få för att
  # dra säkra slutsatser om den gruppen.
  
  # Ordrar utan rabatt har högst medelordervärde (383 kr).
  # Rabatter ökar försäljningsvolymen, men minskar ordervärdet
  # Det finns inget tydligt linjärt samband mellan rabattnivå och ordervärde.
  
  # Frågeställning 3: Leder rabatter till högre ordervärde — är det lönsamt att ge rabatt?
  
  # Börjar med att kolla om rabatter korrelerade med kategorier
  orders_clean %>%
    group_by(product_category) %>%
    summarise(
      medel_rabatt = mean(discount_pct),
      median_rabatt = median(discount_pct)
    ) %>%
    arrange(desc(medel_rabatt))
  
  # Medel-rabatten ligger mellan 6.7% och 7.4% för alla kategorier.
  # Rabatter är därmed inte koncentrerade till någon specifik kategori. 
  
  #Undersöker om intäkter per kund i gruppen liten eller ingen rabatt, 
  #jämfört med intäkt per kund i gruppen för stora rabatter. 
  orders_clean %>%
    filter(discount_pct <= 0.05 | discount_pct >= 0.15) %>%
    mutate(rabattgrupp = case_when(
      discount_pct <= 0.05 ~ "Låg (0-5%)",
      TRUE ~ "Hög (15% och över)"
    )) %>%
    group_by(rabattgrupp, customer_id) %>%
    summarise(
      total_intakt_per_kund = sum(total_after_discount),
      antal_ordrar = n()
    ) %>%
    group_by(rabattgrupp) %>%
    summarise(
      antal_kunder = n(),
      medel_intakt_per_kund = mean(total_intakt_per_kund),
      medel_ordrar_per_kund = mean(antal_ordrar)
    )
  # Kunder med låg rabatt (0-5%) är värda nästan dubbelt så mycket som kunder med hög rabatt:
  #Låg rabatt: 1000 kr per kund, 2.01 ordrar per kund
  #Hög rabatt: 665 kr per kund, 1.20 ordrar per kund
  
  #Undersöker intäkter per kund - inga rabatter jmf med 15% eller mer
  #(här är grupperna mer jämförbara i storlek)
  orders_clean %>%
    filter(discount_pct == 0.0 | discount_pct >= 0.15) %>%
    mutate(rabattgrupp = case_when(
      discount_pct == 0.0 ~ "Ingen rabatt",
      TRUE ~ "Hög (15% och över)"
    )) %>%
    group_by(rabattgrupp, customer_id) %>%
    summarise(
      total_intakt_per_kund = sum(total_after_discount),
      antal_ordrar = n()
    ) %>%
    group_by(rabattgrupp) %>%
    summarise(
      antal_kunder = n(),
      medel_intakt_per_kund = mean(total_intakt_per_kund),
      medel_ordrar_per_kund = mean(antal_ordrar)
    )
  #medel_ordrar_per_kund är nästan identisk: 1.19 vs 1.20
  #medel_intakt_per_kund är fortfarande högre för ingen rabatt: 732 kr vs 655 kr
  #Det betyder att höga rabatter inte får kunder att lägga fler ordrar 
  #jämfört med kunder utan rabatt alls. 
  
  # Verkar som kunder med låga rabatter handlar oftare.
  #Kunden med flest ordrar har bara lagt 6 ordrar, så verkar inte vara något outliner som drar upp medlet
  # 99% av VIP-kunder får rabatt, med ett medelvärde på 11.4%
  # 91% av nya kunder får rabatt mot 88% av återkommande

#    Slutsats frågeställning 3: 
#    Kunder med låg eller inga rabatter (0-5%) är de mer lönsamma (snittar 
#    2.01 ordrar per kund och 1000 kr i intäkt per kund) än 
#    kunder med höga rabatter (+15%) (snittar 1.20 ordrar, 655 kr per kund).
#    Höga rabatter verkar inte löna sig. De driver varken fler ordrar eller
#    högre intäkt per kund jämfört med ingen eller låg rabatt.
#
# Begränsning:
#  Vi kan se samband men inte förklara vad som orsakar vad (kausalitet).
#  Vi skulle behöva data över längre tid för att avgöra om rabatter driver 
#  lojalitet. 
#  Vi saknar information om marginaler. Vi vet inte vad produkterna kostar att sälja.
#  (En rabatt på en högmarginalprodukt kan fortfarande vara lönsam medan samma rabatt 
#  på en lågmarginalprodukt kan ge förlust.

# ---------------- viktiga del för visualiseringar (ta inte bort) :)
  
  rabatt_impact_summary <- orders_clean %>%
    mutate(rabattgrupp = case_when(
      discount_pct == 0 ~ "Ingen rabatt",
      discount_pct < 0.05 ~ "Mycket låg (0-5%)",
      discount_pct < 0.10 ~ "Låg (5-10%)",
      discount_pct < 0.15 ~ "Medel (10-15%)",
      discount_pct < 0.20 ~ "Medel-hög (15-20%)",
      TRUE ~ "Hög (över 20%)"
    )) %>%
    group_by(rabattgrupp, customer_type) %>%
    summarise(
      antal_ordrar = n(),
      medel_quantity = mean(quantity),
      medel_efter_rabatt = mean(total_after_discount),
      .groups = "drop"
    ) %>%
    arrange(desc(medel_quantity)) %>%
    mutate(rabattgrupp = factor(rabattgrupp, levels = c(
      "Ingen rabatt",
      "Mycket låg (0-5%)",
      "Låg (5-10%)",
      "Medel (10-15%)",
      "Medel-hög (15-20%)",
      "Hög (över 20%)"
    )))
  
  rabatt_impact_summary
  
  # För nya kunder:
  # - En hög rabatt fungerar som ett incitament att köpa en dyrare vara
  # - En medelhög rabatt motiverar istället till att köpa fler varor till lägre pris
  # - Jämfört med ingen rabatt finns ingen tydlig fördel
  # - Däremot kan rabatter fungera som ett incitament för att återkomma som kund
  
  # För återkommande kunder:
  # - Det bästa genomsnittliga ordervärdet uppnås utan rabatt
  # - Höga rabatter motiverar till fler varor per köp
  # - Men detta sänker det genomsnittliga ordervärdet och ger sämst utfall
  # - Rabatter under 15 % verkar öka beställningsfrekvensen
  
  # För VIP-kunder:
  # - Låga rabatter är mest effektiva
  # - De bibehåller ett högt ordervärde samtidigt som kunderna köper fler varor
  # - Höga rabatter har liknande effekt som ingen rabatt och rabbat till 5 %
  # - Rekommenderad maxrabatt är cirka 15 %
  
  #
  # För att fullt ut förstå kundbeteendet krävs vidare analys av hur rabatter
  # påverkar kundernas utveckling från återkommande kunder till VIP-kunder
  