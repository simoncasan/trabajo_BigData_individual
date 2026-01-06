library(tidyverse)


datikos <- "./datos/top250-00-19.csv"

df_completo <- read_csv(datikos)

top10_x_año <- df_completo %>% 
  mutate(Transfer_fee = as.numeric(Transfer_fee)) %>% 
  mutate(Season = parse_number(Season) + 1) %>%
  group_by(Season) %>%
  slice_max(order_by = Transfer_fee, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  select(Season, Transfer_fee)

ev_precio_medio <- top10_x_año %>%
  group_by(Season) %>%
  summarise(
    precio_medio = mean(Transfer_fee, na.rm = TRUE),
    n_fichajes = n()
  ) %>%
  ungroup()

  
grafico_lineal <- ggplot(ev_precio_medio, aes(x = Season, y = precio_medio)) +
  geom_line(color = "dodgerblue3", linewidth = 1.2) +
  geom_point(color = "dodgerblue3", size = 2) +
  scale_x_continuous(breaks = seq(min(ev_precio_medio$Season),max(ev_precio_medio$Season),by = 1)) +
  scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 8)) +
  labs(title = "Evolución del precio medio de los top 10 fichajes",
    subtitle = "2001–2019",
    x = "Temporada",
    y = "Precio medio (€)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

grafico_lineal

