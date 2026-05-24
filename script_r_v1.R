
###########SCRIPT RESULTADOS TFM#############

library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(writexl)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

####OE1. SINTESIS LITERATURA CIENTIFICA####
S1_study <- read_excel("C:/Users/annac/Escritorio/OneDrive - Universidad de Alcala/01 MURE i Doctorat/14. PEX y TFM/TFM/Tratamiento datos/Data_treatment_v1.xlsx", 
                                      sheet = "1_study") %>% 
  filter(!country %in% c("Australia", "California", "Chile", "EEUU", "SouthAfrica"))

#tengo que poner los paises por our_id y quitar lo que no sea mediterranean basis!!!!

###### OE1.1 Evolución temporal del número de publicaciones por tipo de estudio ####
files_merged_12052026 <- read_excel("C:/Users/annac/Escritorio/OneDrive - Universidad de Alcala/01 MURE i Doctorat/14. PEX y TFM/TFM/Tratamiento datos/files_merged_12052026.xlsx", 
                                    sheet = "Sheet1")
files_merged_articles <- files_merged_12052026 %>% 
  filter(str_detect(Document.Type, regex("article", ignore_case = TRUE)))

data_OE1_1 <- S1_study %>%            #aqui asocio por el our_id el año de publicacion con mis datos de estudio
  left_join(files_merged_articles %>% select(our_id, Year), by = "our_id")

ggplot(data_OE1_1, aes(x = Year, fill = study_type)) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = c(seq(1990, 2026, by = 6), max(data_OE1_1$Year, na.rm = TRUE))) +
  labs(title = "Evolución temporal de estudios por tipo",
       x = "Año",
       y = "Número de estudios",
       fill = "Tipo de estudio") +
  theme_classic()

table(data_OE1_1$study_type)
prop.table(table(data_OE1_1$study_type)) * 100

#quito categoria palaeoecology (solo 3 estudios) y ordeno categorias
data_OE1_2 <- data_OE1_1 %>%
  filter(study_type != "palaeoecology") %>%
  mutate (study_type = fct_relevel(study_type, 
                                  "field", 
                                  "remote sensing", 
                                  "field;remote sensing"))

#etiquetas
etiquetas <- data_OE1_2 %>%
  group_by(Year) %>%
  summarise(total = n(), .groups = 'drop')

#grafico con etiquetas
ggplot(data_OE1_2, aes(x = Year)) +
  geom_bar(aes(fill = study_type), position = "stack", width = 0.7, color = "white", linewidth = 0.1) +
  geom_text(data = etiquetas, aes(x = Year, y = total, label = total),
            vjust = -0.5, size = 3.5, family = "Arial", color = "black") +
  scale_x_continuous(breaks = c(seq(1990, 2026, by = 1))) +
  scale_y_continuous(breaks = function(x) seq(0, max(x) + 1, by = 1)) +
  scale_fill_manual(values = c("field" = "indianred2", "remote sensing" = "cadetblue2", "field;remote sensing" = "darkolivegreen2"),
                    labels = c("field" = "Campo", "remote sensing" = "Teledetección", "field;remote sensing" = "Campo + Teledetección")) +
  labs(x = "Año de publicación",
       y = "Número de estudios",
       fill = "Tipo de estudio") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(family = "Arial", size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

###### OE1.2 Asociación entre study_level y study_type: ¿el tipo de estudio varia segun si se estudia a nivel de comunidad o especie? ####
#para el global de 107 estudios
solo_vegetacion <- subset(data_OE1_2, !is.na(study_level))
relacion_ST_SL <- xtabs(~ study_level + study_type, data = solo_vegetacion)
relacion_ST_SL
global <- prop.table(relacion_ST_SL) * 100     # % respecto el total de estudios
global
portipoestudio <- prop.table(relacion_ST_SL, margin = 2) * 100   # % respecto el total de cada tipo de estudio
portipoestudio
write.csv2(global, "OE1.2_global")
write.csv2(portipoestudio, "OE1.2_portipoestudio")

###### OE1.3 Asociación entre study_type y nº incendios: el tipo de estudio varia en relación al nº de incendios estudiados? ####
ST_nfires <- data_OE1_1 %>%                      #aqui con paleoecologia 
  mutate(n_fires = as.numeric(n_fires)) %>%
  group_by(study_type) %>%
  summarise(total_incendios = sum(n_fires, na.rm = TRUE))    #quitar NA's para el gráfico


ST_nfires_excludepaleo <- data_OE1_2 %>%                      #aqui sin paleoecologia
  mutate(n_fires = as.numeric(n_fires)) %>% 
  group_by(study_type) %>%
  summarise(total_incendios = sum(n_fires, na.rm = TRUE))

ggplot(ST_nfires_excludepaleo, aes(x = study_type, y = total_incendios, fill = study_type)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = total_incendios), vjust = -0.5, family = "Arial", size = 3.5) +
  scale_x_discrete(labels = c("field" = "Campo", "remote sensing" = "Teledetección", "field;remote sensing" = "Campo + Teledetección")) +
  scale_fill_manual(values = c("field" = "indianred2", "remote sensing" = "cadetblue2", "field;remote sensing" = "darkolivegreen2")) +
  labs(x = "Tipo de estudio",
       y = "Número de incendios") +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(family = "Arial", size = 11),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 8)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )



####OE2. VARIABLES Y FACTORES MÁS IMPORTANTES####
S3_measures <- read_excel("C:/Users/annac/Escritorio/OneDrive - Universidad de Alcala/01 MURE i Doctorat/14. PEX y TFM/TFM/Tratamiento datos/Data_treatment_v1.xlsx", 
                       sheet = "3_measures") %>% 
  filter(!country %in% c("Australia", "California", "Chile", "EEUU", "SouthAfrica"))

######OE2.1 Variables respuesta más importantes estudiadas####

#qué tipo de variables son las más estudiadas? 
round(prop.table(table(S3_measures$variable_type)) * 100, 1)

#qué subtipo de variables son las más estudiadas?
#para vegetación
subtipos_vegetacion <- S3_measures %>% 
  filter(variable_type == "vegetation") %>% 
  group_by(variable_subtype) %>% 
  summarise(percentatge = round((n() / nrow(.)) * 100, 1))

ggplot(subtipos_vegetacion, aes(x = reorder(variable_subtype, percentatge), y = percentatge)) +
  geom_bar(stat = "identity", fill = "green") +
  scale_x_discrete(limits = levels(reorder(subtipos_vegetacion$variable_subtype, subtipos_vegetacion$percentatge)),
                   labels = c("structure" = "estructura", "regeneration" = "regeneración", "diversity" = "diversidad", "composition" = "composición",
                               "spectral response" = "respuesta espectral", "ecological processes" = "procesos ecológicos", "functional traits" = "rasgos funcionales",
                               "ecosystem services" = "servicios ecosistémicos", "chemical properties" = "propiedades químicas",
                               "biological properties" = "propiedades biologicas", "others" = "otras", "interactions" = "interacciones", "landscape" = "paisaje")) +
  coord_flip() +
  labs(
    tag = "a) vegetación",
    x = "Subtipo de variable",
    y = "Porcentaje (%)"
  ) +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(r = 6)))

#3 primeros subtipos ver variable respuesta más medida (>10%)

#para suelos
subtipos_suelos <- S3_measures %>% 
  filter(variable_type == "soil") %>% 
  group_by(variable_subtype) %>% 
  summarise(percentatge = round((n() / nrow(.)) * 100, 1))

ggplot(subtipos_suelos, aes(x = reorder(variable_subtype, percentatge), y = percentatge)) +
  geom_bar(stat = "identity", fill = "brown") +
  scale_x_discrete(limits = levels(reorder(subtipos_suelos$variable_subtype, subtipos_suelos$percentatge)),
                   labels = c("structure" = "estructura", "regeneration" = "regeneración", "diversity" = "diversidad", "physical properties" = "propiedades físicas",
                   "composition" = "composición", "ecological processes" = "procesos ecológicos", "chemical properties" = "propiedades químicas",
                              "biological properties" = "propiedades biologicas", "others" = "otras", "erosion" = "erosión")) +
  coord_flip() +
  labs(
    tag = "b) suelos",
    x = "Subtipo de variable",
    y = "Porcentaje (%)"
  ) +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(r = 6)))

#3 primeros subtipos ver variables respuesta más medidas (>10%)


######OE2.2 Moderadores más importantes estudiados####
#solo descriptivo para asociar moderador a paper y facilitarme luego la agrupacion de moderadores (consultar papers si es necesario
#no puedo dar un conteo tipo moderador / paper porque hemos agrupado moderadores, asi que esto es solo descriptivo
moderators_paper <- S3_measures %>% 
  separate_rows(moderator_type, sep = ";") %>% 
  distinct(our_id, moderator_type) %>%  #Elimino filas duplicadas para que solo se quede con los moderadores por our_id (por paper).
  add_count(moderator_type, name = "n_papers")  # Recuento de moderador por paper

#conocer moderadores por tipo en global, esto ya si es útil
moderators_global <- S3_measures %>%            
  separate_rows(moderator_type, sep = ";") %>% 
  count(moderator_type, name = "n_papers")

write_xlsx(moderators_global, "OE2.2_moderators")


#minusculas moderadores

####OE3. MAPEAR ESTUDIOS POR PAIS Y CRUZAR CON INCENDIOS######
S2_fire <- read_excel("C:/Users/annac/Escritorio/OneDrive - Universidad de Alcala/01 MURE i Doctorat/14. PEX y TFM/TFM/Tratamiento datos/Data_treatment_v1.xlsx", 
                       sheet = "2_fire") %>% 
filter(!country %in% c("Australia", "California", "Chile", "EEUU", "SouthAfrica"))

#exclude fire_id NA's, mantain "all", and fire_id 1, 2, 3, etc. without duplicates 1, 1,...
nfires_country <- S2_fire %>%
  filter(!is.na(fire_id)) %>%   #esclude NA's from fire_id (no info.)
  distinct(country, fire_id, .keep_all = TRUE) %>% #keep "all" and eliminate duplicates in fire_id
  group_by(country) %>%
  summarise(
    n_fires = n()
  )

world <- ne_countries(scale = "medium", returnclass = "sf")   #cargo paises del mundo
mbasis <- c("Algeria", "France", "Greece", "Israel",
                   "Italy", "Morocco",
                   "Portugal", "Spain", "Turkey")

mbasis_world <- world %>% filter(name %in% mbasis)    #defino y filtro por mis paises

mapa_mb_w <- left_join(mbasis_world, nfires_country, by = c("name" = "country"))

# 4. DIBUIXAR EL MAPA
ggplot(data = mapa_mb_w) +
  geom_sf(aes(label = n_fires), linewidth = 0.2) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey90", direction = -1) +
  geom_sf_label(aes(label = n_fires), size = 3.5, fontface = "bold", label.size = 0.2) +
  coord_sf(xlim = c(-10, 40), ylim = c(28, 48), expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Estudios de recuperación postincendio por país",
    fill = "Nº estudios",
    x = "Longitud",
    y = "Latitud"
  )
