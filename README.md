# E-handel Gruppuppgift

## Arbetsfördelning
- Dataförståelse: Elisabeth
- Datastädning: Magdalena
- Analys: Katja & Elisabeth
- Visualisering: Wendy
- Rapport: Daniel
- Presentation: Magdalena

## Projekt
Utforskande dataanalys i R av e-handelsdata 

##Frågeställningar
1. Vilka produktkategorier verkar driva högst försäljning?
2. Finns det samband mellan rabatt och ordervärde?
3. Är det lönsamt att ge rabatt?

## Kom igång
1. Klona repot från GitHub
2. Öppna `ecommerce_analysis.Rproj` i RStudio
3. Installera nödvändiga paket (se nedan)
4. Lägg originalfilen `ecommerce_orders.csv` i projektet
5. Kör `run_full_analysis.R` för att köra alla scripts i sekvens

## Paket
install.packages("tidyverse")
install.packages("here")
install.packages("ggrepel")

## Filstruktur
project/ecommerce_analysis.Rproj
├── .gitignore
├── .RData
├── .RHistory
├── README.md
├── run_full_analysis.R          # Kör hela analysen i sekvens
├── data/
│   └── ecommerce_orders.csv     # Rådata
├── output/
│   └── figures/                 # Auto-genererade grafer från 05_visualisering.R
├── report/
│   ├── report.qmd               # Quarto-källfil för rapporten
│   ├── report.html              # Renderad HTML-rapport
│   └── Report_files/            # Resurser genererade av Quarto
└── scripts/
    ├── 01_data_understanding.R  # Utforskning och översikt av rådata
    ├── 02_clean_data.R          # Datarensning och förbehandling
    ├── 03_variabler.R           # Variabeldefinitioner och transformationer
    ├── 04_analysis.R            # Statistisk analys
    └── 05_visualisering.R       # Genererar grafer till output/figures/

