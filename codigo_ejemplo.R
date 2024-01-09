# Cargar paquetes necesarios
library("tidyverse")


# Importar los datos
paes_2023 <- read_csv("Library/Mobile Documents/com~apple~CloudDocs/Teaching/ISUC/2024_1_nucleo_r2hs/data/paes_2023.csv")


# Explorar resultados


resultados_comuna_dependencia <- paes_2023 %>% 
  mutate(puntaje_total = (lenguaje + matematicas_1 + matematicas_2)/3 ) %>%
  group_by(comuna, dependencia) %>%
  summarise(promedio_puntaje_total = mean(puntaje_total))  


# Explorar resultados
resultados_comuna_dependencia %>% arrange(desc(promedio_puntaje_total)) %>% print(n=100)


# Visualizar datos
resultados_comuna_dependencia %>% ggplot(aes(
    y = reorder(interaction(comuna, dependencia), promedio_puntaje_total),
    x = promedio_puntaje_total,
    fill = factor(dependencia)
  )) +
  geom_bar(stat = "identity") +
  theme(axis.text.y = element_text(size = 2)) +
  labs(x="Promedio Puntaje Total", y="Dependencia por comuna", fill = "Dependencia")





