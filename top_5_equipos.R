library(tidyverse)
library(highcharter)  

datikos <- "./datos/top250-00-19.csv"

df_completo <- read_csv(datikos)

top_5_equipos <- df_completo %>%
  group_by(Team_to) %>%
  summarise(gasto_total = sum(Transfer_fee, na.rm = TRUE)) %>%
  slice_max(gasto_total, n = 5)



hchart(top_5_equipos, "pie", hcaes(x = Team_to, y = gasto_total)) %>%
  hc_add_theme(hc_theme_economist()) %>% 
  hc_size(height = 500)