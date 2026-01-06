library(tidyverse)
library(magick)


datikos <- "./datos/top250-00-19.csv"

df_completo <- read_csv(datikos)

liga_ranking <- df_completo %>%
  mutate(League_from = str_replace(League_from,
                              "Rel. Ligue 1", "Ligue 1")) %>%
  group_by(League_from) %>%
  summarise(
    precio_medio = mean(Transfer_fee, na.rm = TRUE),
    n_fichajes = n()
  ) %>%
  arrange(desc(precio_medio))



edad_ranking <- df_completo %>%
  group_by(Age) %>%
  summarise(
    precio_medio = mean(Transfer_fee, na.rm = TRUE),
    n_fichajes = n()
  ) %>%
  arrange(desc(precio_medio))


posicion_ranking <- df_completo %>%
  group_by(Position) %>%
  summarise(
    precio_medio = mean(Transfer_fee, na.rm = TRUE),
    n_fichajes = n()
  ) %>%
  arrange(desc(precio_medio))



jugadores <- "./datos/database.csv"

df_jugadores <- read_csv(jugadores)

df_jugadores <- df_jugadores %>%
  mutate(Age = as.numeric(str_extract(Age, "^[0-9]+")))%>%
  filter(str_detect(Position, "LW"))%>%
  filter(Age==33)%>%
  slice(-3)  #me aparecía un jugador repetido y por eso lo elimino


james <- image_read("./imagenes/james.jpeg") %>% image_scale(.,"700")
peña <- image_read("./imagenes/peñ.jpeg") %>% image_scale(.,"700")

equipos <- c(james, peña)
image_animate(image_scale(equipos), fps = 0.25)




