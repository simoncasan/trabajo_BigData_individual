library(tidyverse)
library(ggimage)

datikos <- "./datos/top250-00-19.csv"

df_completo <- read_csv(datikos)


top_15_fichajes <- df_completo %>%
  select(Name, Season, Transfer_fee) %>% 
  mutate(Transfer_fee = as.numeric(Transfer_fee)) %>% 
  arrange(desc(Transfer_fee)) %>%
  head(15)



df_caras <- data.frame(Name = c(
  "Neymar",
  "Kylian Mbappé",
  "Philippe Coutinho",
  "Cristiano Ronaldo",
  "Ousmane Dembélé",
  "Paul Pogba",
  "Gareth Bale",
  "Cristiano Ronaldo",
  "Gonzalo Higuaín",
  "Neymar",
  "Romelu Lukaku",
  "Luis Suárez",
  "Virgil van Dijk",
  "Zinédine Zidane",
  "Kevin De Bruyne"
),
Foto = c(
  "ney",
  "mbappe",
  "coutinho",
  "elbicho",
  "dembele",
  "pogba",
  "bale",
  "elbicho",
  "pipita",
  "ney",
  "lakaka",
  "suarez",
  "van di",
  "zizu",
  "debruine"
)
) %>%
  mutate(logo = file.path("imagenes", paste0(Foto, ".png")))



top_15_completo <- top_15_fichajes %>%
  left_join(df_caras, by = "Name") %>%
  arrange(desc(Transfer_fee)) %>%
  mutate(
    etiqueta = paste0(Name, " (", Season, ") #", row_number()),
    logo = file.path("imagenes", paste0(Foto, ".png"))
  )

top_15_completo <- top_15_completo[-c(2, 6, 11, 14), ] #borro esas filas maualmente porque no se porque se me duplicaban esos fichajes





mi_grafico <- ggplot(top_15_completo, aes(x = reorder(etiqueta, Transfer_fee), y = Transfer_fee)) +
  geom_col(fill = "dodgerblue3", width = 0.6) +
  coord_flip() +
  geom_image(aes(image = logo, y = 0), size = 0.05) +
  geom_text(
    aes(label = Season, y = max(Transfer_fee) * 0.02),
    hjust = 0,
    size = 3,
    color = "black"
  ) +
  geom_text(
    aes(label = scales::comma(Transfer_fee)),
    hjust = -0.2,
    fontface = "bold",
    size = 3.5) +
  scale_y_continuous(labels = scales::comma) + 
  expand_limits(y = 0) + 
  labs(
    title = "Top 15 Fichajes Mas Caros",
    subtitle = "Desde 2000-2018",
    x = "Jugadores", 
    y = "Precio Pagado") +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(10, 20, 10, 20))

mi_grafico
