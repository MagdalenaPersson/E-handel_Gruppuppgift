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
  
  # Leder rabatt till fler köp per order?
  orders_clean %>%
    mutate(rabattgrupp = case_when(
      discount_pct == 0 ~ "Ingen rabatt",
      discount_pct < 0.05 ~ "Mycket låg (0-5%)",
      discount_pct < 0.10 ~ "Låg (5-10%)",
      discount_pct < 0.15 ~ "Medel (10-15%)",
      discount_pct < 0.20 ~ "Medel-hög (15-20%)",
      TRUE ~ "Hög (över 20%)"
    )) %>%
    group_by(rabattgrupp) %>%
    summarise(
      antal_ordrar = n(),
      medel_quantity = mean(quantity),
      medel_efter_rabatt = mean(total_after_discount)
    ) %>%
    arrange(desc(medel_quantity))
  
  # Rabatter driver inte fler enheter per order (medel 1.31-1.47, minimal skillnad)
  # Ordervärdet efter rabatt är högst för ordrar utan rabatt (617 kr)
  # och lägst för gruppen med hög rabatt över 20% (422 kr)
  
  # Lockar rabatter nya kunder?
  customer_summary <- orders_clean %>%
    group_by(customer_type) %>%
    summarise(
      antal = n(),
      andel_med_rabatt = mean(discount_pct > 0),
      medel_rabatt = mean(discount_pct)
    ) %>%
    arrange(desc(andel_med_rabatt))
  
  # 99% av VIP-kunder får rabatt, med ett medelvärde på 11.4%
  # 91% av nya kunder får rabatt mot 88% av återkommande
  # Det verkar som att företaget använder rabatter för att belöna sina bästa kunder men 
  # vi kan inte säga om rabatter faktiskt leder till att kunder kommer tillbaka eller blir VIP 
  # det kan lika gärna vara så att företaget ger VIP-kunder rabatt för att de redan är lojala.
  
  # Ordrar och kundgrupper - Jämför gruppen med ingen eller max 5% rabatt, med gruppen med 15% rabatt eller högre
  orders_clean %>%
    filter(discount_pct <= 0.05 | discount_pct >= 0.15) %>%
    mutate(rabattgrupp = case_when(
      discount_pct <= 0.05 ~ "Låg (0-5%)",
      TRUE ~ "Hög (15% och över)"
    )) %>%
    group_by(rabattgrupp, customer_type) %>%
    summarise(
      antal = n(),
      andel_med_rabatt = mean(discount_pct > 0),
      medel_rabatt = mean(discount_pct)
    ) %>%
    arrange(rabattgrupp, customer_type)
  
  
  # Höga rabatter ges oftare till VIP-kunder
  # än andra kundergrupper. Resultatet bör dock tolkas med försiktighet 
  # eftersom högrabattgruppen endast innehåller 85 ordrar.
  
  
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
  
  #Kollar om det är något outlier som drar upp antalet ordrar per kund för
  #kundgruppen med liten rabatt (0-5%, 2.01 ordrar per kund), jmf med
  #kundgruppen utan rabatt (1.19 ordrar per kund)
  orders_clean %>%
    filter(discount_pct > 0 & discount_pct <= 0.05) %>%
    group_by(customer_id) %>%
    summarise(
      antal_ordrar = n(),
      medel_rabatt = mean(discount_pct),
      total_intakt = sum(total_after_discount)
    ) %>%
    arrange(desc(antal_ordrar))
  
  # Verkar som kunder med låga rabatter handlar oftare.
  #Kunden med flest ordrar har bara lagt 6 ordrar,
  #så verkar inte vara något outliner som drar upp medlet
  
  
  # Viktig affärsinsikt:
  # Rabatter påverkar kundtyper olika – särskilt beteendet mellan nya och återkommande kunder.
  
  # Frågeställning 3: Är det lönsamt att ge rabatter?
  #
  # För att undersöka detta tittade vi på frya delområden:
  #
  # 1. Är rabatter kopplade till specifika kategorier?
  #    Medel-rabatten ligger mellan 6.7% och 7.4% för alla produktkategorier.
  #    Rabatter är jämnt fördelade mellan kategorier, vilket betyder att
  #    rabatterna inte är kategoribundna.
  #
  # 2. Leder rabatter till fler köp per order?
  #    Vi tittade på om kunder köper mer per order vid högre rabatt
  #    Medel antal enheter per order varierar mellan 1.31 och 1.47 mellan
  #    rabattgrupperna — en minimal skillnad.
  #    Rabatter driver alltså inte kunder att köpa fler enheter per order.
#
# 3. Lockar rabatter nya kunder eller belönar de lojala kunder?
#    99% av VIP-kunder får rabatt med ett medelvärde på 11.4%, jämfört med
#    91% av nya kunder och 88% av återkommande kunder.
#    Det tyder på att företaget främst använder rabatter för att belöna
#    sina bästa kunder snarare än för att locka nya.
#    Vi kan dock inte avgöra kausalitet, skapar rabatter lojala kunder eller 
#    får lojala kunder mer rabatt.
#
# 4. Är kunder med höga rabatter mer lönsamma än kunder utan rabatt?
#   
#    Jämförelse 1 - Låg rabatt (0-5%) jmf med hög rabatt (15%+):
#    Kunder med låga rabatter är de mest lönsamma. De lägger flest ordrar
#    (2.01 per kund) och ger högst intäkt (1000 kr per kund), jämfört med
#    kunder med höga rabatter (1.20 ordrar, 655 kr per kund).
#    Varför lågrabattkunder är så lojala kan vi inte förklara med denna data.

#    Jämförelse 2 - Ingen rabatt vs hög rabatt (15%+):
#    Kunder utan rabatt och med hög rabatt lägger ungefär lika många ordrar
#    (1.19 vs 1.20 per kund), men kunder utan rabatt ger högre intäkt
#    (732 kr vs 655 kr per kund). Höga rabatter driver alltså inte lojalitet.
#
#
# Rekommendation till företaget:
#    Höga rabatter verkar inte löna sig. De driver varken fler ordrar eller
#    högre intäkt per kund jämfört med ingen eller låg rabatt.
#    Det kan vara värt att se över rabattstrategin och undersöka vidare
#    varför kunder med låga rabatter är så lojala.
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
  