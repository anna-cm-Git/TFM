
####SCRIPT RESULTADOS TFM####

library(readxl)
library(tidyverse)
library(writexl)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

####OE1. SINTESIS LITERATURA CIENTIFICA####
S1_study <- read_excel("C:/Users/annac/Escritorio/OneDrive - Universidad de Alcala/01 MURE i Doctorat/14. PEX y TFM/TFM/Tratamiento datos/Data_treatment_v4.xlsx", 
                                      sheet = "1_study")
S2_fire <- read_excel("C:/Users/annac/Escritorio/OneDrive - Universidad de Alcala/01 MURE i Doctorat/14. PEX y TFM/TFM/Tratamiento datos/Data_treatment_v4.xlsx", 
                      sheet = "2_fire")
S2_fire_MB <- S2_fire %>%            #MB indica que es la tabla de datos solo con paises de la Mediterranean Basis
  select(our_id, country) %>% 
  filter(!country %in% c("Australia", "California", "Chile", "EEUU", "SouthAfrica")) %>% 
  distinct(our_id, .keep_all = TRUE)

S1_study_MB <- S1_study %>%
  inner_join(S2_fire_MB, by = "our_id")

##### OE1.1 Evolución temporal del número de publicaciones por tipo de estudio #####
files_merged_12052026 <- read_excel("C:/Users/annac/Escritorio/OneDrive - Universidad de Alcala/01 MURE i Doctorat/14. PEX y TFM/TFM/Tratamiento datos/files_merged_12052026.xlsx", 
                                    sheet = "Sheet1")
files_merged_articles <- files_merged_12052026 %>% 
  filter(str_detect(Document.Type, regex("article", ignore_case = TRUE))) #que se quede solo los articulos

data_OE1_1 <- S1_study_MB %>%            #aqui asocio por el our_id el año de publicacion con mis datos de estudio
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
    text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

##### OE1.2 Asociación entre study_level y study_type: ¿el tipo de estudio varia segun si se estudia a nivel de comunidad o especie? #####
#para el global de estudios
solo_vegetacion <- subset(data_OE1_2, !is.na(study_level) & study_level != "NA")
relacion_ST_SL <- xtabs(~ study_level + study_type, data = solo_vegetacion)
relacion_ST_SL
global <- prop.table(relacion_ST_SL) * 100     # % respecto el total de estudios
portipoestudio <- prop.table(relacion_ST_SL, margin = 2) * 100   # % respecto el total de cada tipo de estudio
portipoestudio
write.csv2(global, "OE1.2_global.csv")
write.csv2(portipoestudio, "OE1.2_portipoestudio.csv")

##### OE1.3 Asociación entre study_type y nº incendios: el tipo de estudio varia en relación al nº de incendios estudiados? #####
ST_nfires <- data_OE1_1 %>%                      #aqui con paleoecologia 
  mutate(n_fires = as.numeric(n_fires)) %>%
  group_by(study_type) %>%
  summarise(total_incendios = round(mean(n_fires, na.rm = TRUE), 1))    #quitar NA's y all's para el gráfico


ST_nfires_excludepaleo <- data_OE1_2 %>%                      #aqui sin paleoecologia
  mutate(n_fires = as.numeric(n_fires)) %>% 
  group_by(study_type) %>%
  summarise(total_incendios = round(mean(n_fires, na.rm = TRUE), 1))

ggplot(ST_nfires_values, aes(x = study_type, y = total_incendios, fill = study_type)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = total_incendios), vjust = -0.5, family = "Arial", size = 3.5) +
  scale_x_discrete(labels = c("field" = "Campo", "remote sensing" = "Teledetección", "field;remote sensing" = "Campo + Teledetección")) +
  scale_fill_manual(values = c("field" = "indianred2", "remote sensing" = "cadetblue2", "field;remote sensing" = "darkolivegreen2")) +
  labs(x = "Tipo de estudio",
       y = "Número de incendios") +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(size = 11),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 8)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )                                                       #grafico mitjana n_fires

#tengo dos valores que me suben la media mucho para remote sensing, si los elimino?
ST_nfires_values <- data_OE1_2 %>%
  slice(-35, -62) %>% 
  mutate(n_fires = as.numeric(n_fires)) %>% 
  group_by(study_type) %>%
  summarise(total_incendios = round(mean(n_fires, na.rm = TRUE), 1))

#cuantos NA's tengo?
cuantoNA <- data_OE1_2 %>%
  mutate(n_fires = as.numeric(n_fires)) %>%  
  summarise(na_n_fires = sum(is.na(n_fires)))

(21/271)*100

####OE2. VARIABLES Y FACTORES MÁS IMPORTANTES####
S3_measures <- read_excel("C:/Users/annac/Escritorio/OneDrive - Universidad de Alcala/01 MURE i Doctorat/14. PEX y TFM/TFM/Tratamiento datos/Data_treatment_v4.xlsx",
                          sheet = "3_measures")
S3_measures_MB <- S3_measures %>%
  inner_join(S2_fire_MB, by = "our_id")

write_xlsx(S3_measures_MB, "S3_measures_MB.xlsx")

#####OE2.1 Variables respuesta más importantes estudiadas#####
#qué tipo de variables son las más estudiadas? 
round(prop.table(table(S3_measures_MB$variable_type)) * 100, 1)

#qué subtipo de variables son las más estudiadas?
#para vegetación, primero filtramos los datos por "vegetacion"
S3_measures_veg <- S3_measures_MB %>%
  filter(variable_type == "vegetation")

######Vegetación######
#para vegetación, tenemos que clasificar variables respuesta en categorias: structure, abundance, diversity,
#vegetation function, y spectral response
#Es una función que te clasifica la columna que le des ("var") en diferentes
#categorias segun las palabras que aparecen en la variable:

subcategories_veg <- function (var) {
  
  stru <- "magni|pathl|altit|topol|stand| age|cork|distan|branch|profil|rother|thick|width|agdb|weight|thd|tdd|dbh|foliar|length|crown|size|canopy|litter|diamet|height|density|prese|mass|structure|cover|wood|volume|area"
  abun <- "abund|occup|pres"
  dive <- "evenn|choro|dominan|equitatib|diversity|shannon|brillouin|simpson|eveness|richness|similarity|iap|sef|compo|fugac|allele|heterozig"
  vefu <- "ecos|flux|serot|disper|remain|c/n|transp|photosy|predawn|load|nrest|input|resis|resil|conduc|sequ|storag|carbon|elong|mortal|death|serotinity|dead|kill|burn|liv|leaf area|efficienc|assimil|nectar|respro|sprout|germin|viabil|pollen|surviv|time|seed|recruit|produ|regene|stomat|rate|18|13|15|xilo|grow|cone|shoot|new"
  spre <- "normali|season|nvi|rfdi|polari|ndvi|evi|fpar|land|nbr|rri|rr|ndre|indic|vari|reflectance|band|change|pixel|ndwi|siwsi|^rvi"
  
  # ^ esto es para indicar que se fija en que el termino esta al principio
  
  varlower <- str_to_lower(var) #pasar a minusculas
  
    case_when(     #orden importa: lo q tiene menos opciones primero = lo prioritario
      str_detect(varlower, abun) ~ "abundance",
      str_detect(varlower, spre) ~ "spectral response",
      str_detect(varlower, dive) ~ "diversity",
      str_detect(varlower, vefu) ~ "vegetation function",
      str_detect(varlower, stru) ~ "structure",
            varlower %in% c("-", "NA", "NaN") ~ "none") #NA's se llamen none)    
      #me deja las que no clasifica con el nombre original                 
}

#aplico la funcion a mi tabla

V_clasificacion <- S3_measures_veg %>% 
  mutate(across(
    .cols = c(response_variable), 
    .fns = ~ subcategories_veg(.x), 
    .names = "{.col}_clean"
  )) %>% 
  relocate(response_variable_clean, .before = response_units)

#cambios manuales
V_clasificacion[23, 3] = "structure"
V_clasificacion[106, 3] = "structure"
V_clasificacion[107, 3] = "structure"
V_clasificacion[115, 3] = "spectral response"
V_clasificacion[120, 3] = "diversity"
V_clasificacion[186, 3] = "vegetation function"
V_clasificacion[187, 3] = "vegetation function"
V_clasificacion[192, 3] = "diversity"
V_clasificacion[233, 3] = "diversity"
V_clasificacion[242, 3] = "structure"
V_clasificacion[256, 3] = "structure"
V_clasificacion[257, 3] = "structure"
V_clasificacion[260, 3] = "structure"   
V_clasificacion[273, 3] = "spectral response"
V_clasificacion[301, 3] = "structure"
V_clasificacion[303, 3] = "structure"
V_clasificacion[325, 3] = "structure"
V_clasificacion[326, 3] = "structure"
V_clasificacion[337, 3] = "structure"
V_clasificacion[339, 3] = "structure"
V_clasificacion[394, 3] = "abundance"   
V_clasificacion[432, 3] = "structure"   
V_clasificacion[450, 3] = "structure"   
V_clasificacion[451, 3] = "structure"   
V_clasificacion[457, 3] = "diversity"   
V_clasificacion[458, 3] = "diversity" 
V_clasificacion[470, 3] = "diversity"   
V_clasificacion[485, 3] = "vegetation function"    
V_clasificacion[492, 3] = "spectral response"
V_clasificacion[493, 3] = "spectral response"
V_clasificacion[497, 3] = "structure"
V_clasificacion[498, 3] = "structure"
V_clasificacion[499, 3] = "structure"#a partir de aqui añades cambios manuales para los datos que te han pasado
V_clasificacion[510, 3] = "vegetation function"
V_clasificacion[532, 3] = "diversity"
V_clasificacion[533, 3] = "diversity"
V_clasificacion[548, 3] = "diversity"
V_clasificacion[620, 3] = "structure"   
V_clasificacion[644, 3] = "diversity"
V_clasificacion[682, 3] = "structure"
V_clasificacion[684, 3] = "structure"
V_clasificacion[687, 3] = "diversity"
V_clasificacion[711, 3] = "structure"
V_clasificacion[725, 3] = "structure"
V_clasificacion[726, 3] = "structure"
V_clasificacion[758, 3] = "diversity"
V_clasificacion[762, 3] = "structure"
V_clasificacion[771, 3] = "structure"
V_clasificacion[784, 3] = "abundance"
V_clasificacion[820, 3] = "structure"
V_clasificacion[823, 3] = "abundance"
V_clasificacion[837, 3] = "vegetation function"

#acuerdate anna si añades mas datos agrupar variables y revisar antes de correr el
#script de cambios manuales que vayas añadiendo

#pongo bien mis spectral responses para luego reclasificarlos por separado
V_clasificacion[169, 3] = "spectral response"
V_clasificacion[791, 3] = "spectral response"
V_clasificacion[792, 3] = "spectral response"
V_clasificacion[793, 3] = "spectral response"
V_clasificacion[794, 3] = "spectral response"
V_clasificacion[795, 3] = "spectral response"
V_clasificacion[843, 3] = "spectral response"
V_clasificacion[844, 3] = "spectral response"
V_clasificacion[845, 3] = "spectral response"

subtipos_vegetacion <- V_clasificacion %>% 
  filter(variable_type == "vegetation") %>% 
  group_by(response_variable_clean) %>% 
  summarise(percentage = round((n() / nrow(.)) * 100, 1))

#faltaria cambiar el ggplot
ggplot(subtipos_vegetacion, aes(x = reorder(response_variable_clean, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "green") +
  scale_x_discrete(limits = levels(reorder(subtipos_vegetacion$response_variable_clean, subtipos_vegetacion$percentage)),
                   labels = c("abundance" = "abundancia", "diversity" = "diversidad", "spectral response" = "respuesta espectral",
                   "structure" = "estructura", "vegetation function" = "función de la vegetación")) +
  coord_flip() +
  labs(
    tag = "a) vegetación",
    x = "Subtipo de variable",
    y = "Porcentaje (%)"
  ) +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(r = 6)))

#ahora, como interpreto variables spectral response?
spectral_responses <- V_clasificacion %>%
  filter(response_variable_clean == "spectral response")

SPRE_clasificacion <- spectral_responses %>% 
  mutate(response_variable_clean2 = case_when(
    str_detect(tolower(comments), "regeneration|flux") ~ "vegetation function",
    str_detect(tolower(response_variable), "ndvi|ndwi|ndre|evapotr|flux|recov|disper|nbr|season") ~ "vegetation function",
    str_detect(tolower(response_variable), "cover|ascend|desce") ~ "structure",
    variable_subtype %in% c("regeneration", "functional traits", "ecological processes") ~ "vegetation function",
    variable_subtype == "structure" ~ "structure",
    variable_subtype == "composition" ~ "composition",
    TRUE ~ variable_subtype)) %>% 
    relocate(response_variable_clean2, .before = response_units)

SPRE_clasificacion[14,4] = "structure"
SPRE_clasificacion[35,4] = "vegetation function"
SPRE_clasificacion[86,4] = "vegetation function"
SPRE_clasificacion[92,4] = "composition"

subtipos_spre_veg <- SPRE_clasificacion %>% 
  group_by(response_variable_clean2) %>% 
  summarise(percentage = round((n() / nrow(.)) * 100, 1))

#3 primeros subtipos ver variable respuesta más medida, exporto un excel y lo miro manual
write_xlsx(V_clasificacion, "V_class.xlsx")
write_xlsx(SPRE_clasificacion, "SPRE_class.xlsx")

######Suelos######
S3_measures_soil <- S3_measures_MB %>%
  filter(variable_type == "soil")

#categorias: propiedades físicas, químicas, y biológicas. Procesos ecosistémicos e hidrológicos.
subcategories_soil <- function (var) {
  
  phys <- "diam|textur|sand|silt|clay|compact|bulk|porosity|infiltr|humidit|water|moist|temperat|hydraulic conduct|stab"
  chem <- "total|ph|electrical|cation|extract|nutrient|^som|labile|avail|exchang|organic|carbon|^n|nitrog|c/n|
  ^soc|^toc|^na|sodiu|^c|^p|^mg|magnes|^ca|^k|nh4|no2|humic|fluv|n:p"
  biol <- "plfa|micro|bacter|dna|enzymat|respirat|biomass|invertebr|diversit|fung|activ|abund|rich|shann|qbs|gluco|aryl"
  ecos <- "cycl|decompo|flux|multi"
  hidr <- "run|eros|yield|sedim|loss"

  varlower <- str_to_lower(var) #pasar a minusculas
  
  case_when(     #orden importa: lo q tiene menos opciones primero = lo prioritario
    str_detect(varlower, hidr) ~ "hydrological processes",
    str_detect(varlower, ecos) ~ "ecosystem processes",
    str_detect(varlower, biol) ~ "biological properties",
    str_detect(varlower, phys) ~ "physical properties",
    str_detect(varlower, chem) ~ "chemical properties",
    varlower %in% c("-", "NA", "NaN") ~ "none") #NA's se llamen none)    
  #me deja las que no clasifica con el nombre original                 
}

#aplico funcion a mi tabla
S_clasificacion <- S3_measures_soil %>% 
  mutate(across(
    .cols = c(response_variable), 
    .fns = ~ subcategories_soil(.x), 
    .names = "{.col}_clean"
    )) %>% 
  relocate(response_variable_clean, .before = response_units)

#cambios manuales
S_clasificacion[27, 3] = "ecosystem processes"
S_clasificacion[31, 3] = "hydrological processes"
S_clasificacion[109, 3] = "biological properties"
S_clasificacion[133, 3] = "hydrological processes"
S_clasificacion[143, 3] = "hydrological processes"
S_clasificacion[210, 3] = "physical properties"
S_clasificacion[240, 3] = "hydrological processes"    #a partir de aqui añades cambios manuales para los datos que te han pasado

subtipos_suelos <- S_clasificacion %>% 
  filter(variable_type == "soil") %>% 
  group_by(response_variable_clean) %>% 
  summarise(percentage = round((n() / nrow(.)) * 100, 1))

#representacion grafica
ggplot(subtipos_suelos, aes(x = reorder(response_variable_clean, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "brown") +
  scale_x_discrete(limits = levels(reorder(subtipos_suelos$response_variable_clean, subtipos_suelos$percentage)),
                   labels = c("physical properties" = "propiedades físicas", "chemical properties" = "propiedades químicas",
                              "biological properties" = "propiedades biologicas", "ecosystem processes = procesos ecosistémicos",
                              "hydrological processes" = "procesos hidrológicos")) +
  coord_flip() +
  labs(
    tag = "b) suelos",
    x = "Subtipo de variable",
    y = "Porcentaje (%)"
  ) +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(r = 6)))

#3 primeros subtipos ver variables respuesta más medidas (>10%)
write_xlsx(S_clasificacion, "S_class.xlsx")

#####OE2.2 Moderadores más importantes estudiados#####
#solo descriptivo para asociar moderador a paper y facilitarme luego la agrupacion de moderadores (consultar papers si es necesario
#no puedo dar un conteo tipo moderador / paper porque hemos agrupado moderadores, asi que esto es solo descriptivo
S3_measures2 <- read_excel("C:/Users/annac/Escritorio/OneDrive - Universidad de Alcala/01 MURE i Doctorat/14. PEX y TFM/TFM/Tratamiento datos/Data_treatment_v3.xlsx",
                          sheet = "3_measures")  #parte antes de que me pasaran datos
S3_measures_MB2 <- S3_measures2 %>%
  inner_join(S2_fire_MB, by = "our_id")

moderators_paper <- S3_measures_MB2 %>% 
  separate_rows(moderator_type, sep = ";") %>% 
  distinct(our_id, moderator_type)  #Elimino filas duplicadas para que solo se quede con los moderadores por our_id (por paper).

#clasificacion de moderadores

subcategories_mods <- function (var) {
  
  spat <- "spat|autocov|coord|eucli|surround"
  fire <- "sever|nbr|intens|freq|recurr|return|fire|ocurr|size|burnt"
  time <- "tslf|time|year|after|month|date|succe"
  envi <- "habita|environ|landscape type|ecos|orient|elev|rough|hli|slop|curv|altitu|aspec|posit|expos|topogr|bedroc|morpho|site|plot|geo|subcatch"
  sowa <- "fung|bact|organ|^ph|moist|soil|water|stream|rock|^som|^toc|nutr|permea|humi|avail|^som|^toc"
  clim <- "^rain|clim|droug|aridit|season|thorn|preci|tempe"
  vege <- "bryop|moss|leaf|defol|ndvi|stem|herb|fun|litter|bark|shrub|sapling|wood|forb|gramin|trunk|canop|specie|densi|richn|veget|cover|fores|plant|heigh|dbh|diam|basal|tree|stand age|^age|life|regen|recov|seed|cone"
  huma <- "manag|treatm|logg|thinn|land use|use|prescrib|pile|human"
  
  varlower <- str_to_lower(var) #pasar a minusculas
  
  case_when(     #orden importa: lo q tiene menos opciones primero = lo prioritario
    str_detect(varlower, spat) ~ "spatial factors",
    str_detect(varlower, clim) ~ "climate",
    str_detect(varlower, time) ~ "time since fire",
    str_detect(varlower, envi) ~ "environmental and site conditions",
    str_detect(varlower, huma) ~ "use and human management",
    str_detect(varlower, sowa) ~ "soil traits and water availability",
    str_detect(varlower, fire) ~ "fire regime and traits",
    str_detect(varlower, vege) ~ "vegetation traits",

        varlower %in% c("-", "NA", "NaN") ~ "none",    #NA's se llamen none
    TRUE~var)
}

#aplico funcion a mi tabla
M_clasificacion <- moderators_paper %>% 
  mutate(across(
    .cols = c(moderator_type), 
    .fns = ~ subcategories_mods(.x), 
    .names = "{.col}_clean"
  ))

#cambios manuales
M_clasificacion[68, 3] = "spatial factors"
M_clasificacion[90, 3] = "vegetation traits"
M_clasificacion[99, 3] = "vegetation traits"
M_clasificacion[100, 3] = "vegetation traits"
M_clasificacion[101, 3] = "vegetation traits"
M_clasificacion[102, 3] = "vegetation traits"
M_clasificacion[103, 3] = "vegetation traits"
M_clasificacion[105, 3] = "fire regime and traits"
M_clasificacion[106, 3] = "fire regime and traits"
M_clasificacion[107, 3] = "vegetation traits"
M_clasificacion[115, 3] = "vegetation traits"
M_clasificacion[157, 3] = "vegetation traits"
M_clasificacion[215, 3] = "fire regime and traits"
M_clasificacion[250, 3] =  "time since fire"     #measures vegetation recovery
M_clasificacion[294, 3] = "vegetation traits"
M_clasificacion[316, 3] = "vegetation traits"
M_clasificacion[368, 3] = "spatial factors"
M_clasificacion[369, 3] = "spatial factors"
M_clasificacion[371, 3] = "fire regime and traits"
M_clasificacion[384, 3] = "use and human management"
M_clasificacion[424, 3] = "vegetation traits"
M_clasificacion[438, 3] = "vegetation traits"
M_clasificacion[442, 3] = "vegetation traits"
M_clasificacion[443, 3] = "vegetation traits"
M_clasificacion[480, 3] = "spatial factors"
M_clasificacion[518, 3] = "vegetation traits"    

#lo mismo con los papers que me han pasado
S3_measures3 <- read_excel("C:/Users/annac/Escritorio/OneDrive - Universidad de Alcala/01 MURE i Doctorat/14. PEX y TFM/TFM/Tratamiento datos/Data_treatment_v4_1.xlsx",
                           sheet = "3_measures")  #solo datos de los otros reviewers, no mios
S3_measures_MB3 <- S3_measures3 %>%
  inner_join(S2_fire_MB, by = "our_id")

moderators_colab <- S3_measures_MB3 %>%    
  separate_rows(moderator_type, sep = ";") %>% 
  distinct(our_id, moderator_type)

#funcion
subcategories_mods2 <- function (var) {
  
  spat <- "spat|autocov|coord|eucli|surround|spac|distan"
  fire <- "dama|consup|sever|nbr|intens|freq|recurr|return|fire|ocurr|size|burnt"
  time <- "tslf|time|year|after|month|date|succe"
  envi <- "lith|zon|locat|mounta|inclin|habita|environ|landscape type|ecos|orient|elev|rough|hli|slop|curv|altitu|aspec|posit|expos|topogr|bedroc|morpho|site|plot|geo|subcatch"
  sowa <- "content|activi|microb|elect|silt|clay|sand|ground|edaph|mycorr|runo|ash|bare gr|fung|bact|organ|^ph|moist|soil|water|stream|rock|^som|^toc|^n|^c/n|^p|^k|^ca|nutr|permea|humi|avail|^som|^toc"
  clim <- "spei|win|monthly|radiat|^air|^vpd|^rain|clim|droug|aridit|season|thorn|preci|tempe|rainfall|rain"
  vege <- "green|compet|neigh|stump|respr|pre-veg|bryop|moss|leaf|defol|ndvi|stem|herb|fun|litter|bark|shrub|sapling|wood|forb|gramin|trunk|canop|specie|densi|richn|veget|cover|fores|plant|heigh|dbh|diam|basal|tree|stand age|^age|\\bage\\b|life|regen|recov|seed|cone"
  huma <- "harvesti|exploit|lulc|interve|manag|treatm|logg|thinn|land use|use|prescrib|pile|human"
  
  varlower <- str_to_lower(var) #pasar a minusculas
  
  case_when(     #orden importa: lo q tiene menos opciones primero = lo prioritario
    str_detect(varlower, spat) ~ "spatial factors",
    str_detect(varlower, clim) ~ "climate",
    str_detect(varlower, time) ~ "time since fire",
    str_detect(varlower, envi) ~ "environmental and site conditions",
    str_detect(varlower, huma) ~ "use and human management",
    str_detect(varlower, sowa) ~ "soil traits and water availability",
    str_detect(varlower, fire) ~ "fire regime and traits",
    str_detect(varlower, vege) ~ "vegetation traits",
    
    varlower %in% c("-", "NA", "NaN") ~ "none",    #NA's se llamen none
    TRUE~var)
}

#aplico a los datos de las otras reviewers
M_clasificacion2 <- moderators_colab %>% 
  mutate(across(
    .cols = c(moderator_type), 
    .fns = ~ subcategories_mods2(.x), 
    .names = "{.col}_clean"
  ))

#cambios manuales
M_clasificacion2[7,3] = "vegetation traits"
M_clasificacion2[9,3] = "soil traits and water availability"
M_clasificacion2[38,3] = "fire regime and traits"
M_clasificacion2[45,3] = "soil traits and water availability"
M_clasificacion2[73,3] = "time since fire"
M_clasificacion2[89,3] = "vegetation traits"
M_clasificacion2[93,3] = "leaf mesh size"
M_clasificacion2[105,3] = "vegetation traits"
M_clasificacion2[106,3] = "vegetation traits"
M_clasificacion2[140,3] = "environmental and site conditions"
M_clasificacion2[143,3] = "vegetation traits"
M_clasificacion2[152,3] = "environmental and site conditions"
M_clasificacion2[161,3] = "vegetation traits"
M_clasificacion2[190,3] = "vegetation traits"
M_clasificacion2[384,3] = "vegetation traits"
M_clasificacion2[207,3] = "climate"
M_clasificacion2[233,3] = "fire regime and traits"
M_clasificacion2[239,3] = "soil traits and water availability"
M_clasificacion2[255,3] = "vegetation traits"
M_clasificacion2[258,3] = "vegetation traits"
M_clasificacion2[259,3] = "vegetation traits"
M_clasificacion2[273,3] = "vegetation traits"
M_clasificacion2[290,3] = "vegetation traits"
M_clasificacion2[304,3] = "vegetation traits"
M_clasificacion2[305,3] = "vegetation traits"
M_clasificacion2[309,3] = "vegetation traits"
M_clasificacion2[310,3] = "vegetation traits"
M_clasificacion2[314,3] = "vegetation traits"
M_clasificacion2[318,3] = "vegetation traits"
M_clasificacion2[340,3] = "vegetation traits"
M_clasificacion2[404,3] = "vegetation traits"
M_clasificacion2[453,3] = "soil traits and water availability"
M_clasificacion2[466,3] = "use and human management"
M_clasificacion2[495,3] = "soil traits and water availability"
M_clasificacion2[523,3] = "vegetation traits"
M_clasificacion2[548,3] = "vegetation traits"
M_clasificacion2[557,3] = "nutritional mode"
M_clasificacion2[559,3] = "vegetation traits"
M_clasificacion2[562,3] = "vegetation traits"
M_clasificacion2[564,3] = "fire regime and traits"
M_clasificacion2[565,3] = "vegetation traits"

#union de bases de datos mia y reviewers
M_classTotal <- bind_rows(M_clasificacion, M_clasificacion2)

#porcentaje de papers que analiza cada tipo de moderador
total_papers <- n_distinct(M_classTotal$our_id)
subtipo_mods <- M_classTotal %>%
  distinct(our_id, moderator_type_clean) %>% 
  filter(!is.na(moderator_type_clean) & moderator_type_clean != "NA") %>%    #elimina vacios y donde pone NA
  count(moderator_type_clean, name = "num_papers") %>% 
  mutate(percentage = round((num_papers/total_papers)*100, 1)) %>% 
  arrange(desc(percentage)) #desc = orden descendente de mayor a menor

Mods_final <- subtipo_mods %>%  
  slice(-9) %>% #elimino una fila vacia, no habia moderadores
  mutate(moderator_type_clean = if_else(percentage < 1 | moderator_type_clean == "spatial factors", "others", moderator_type_clean)) %>%  #moderadores < 1% a "others"
  group_by(moderator_type_clean) %>% 
  summarise(num_papers = sum(num_papers),
            percentage = sum(percentage)) %>%
  arrange(desc(percentage))

#representacion grafica
ggplot(Mods_final, aes(x = reorder(moderator_type_clean, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "blue") +
  scale_x_discrete(limits = levels(reorder(Mods_final$moderator_type_clean, Mods_final$percentage)),
                   labels = c("time since fire" = "tiempo desde el incendio", "vegetation traits" = "atributos de la vegetación",
                              "fire regime and traits" = "caracteristicas y regimen del incendio",
                              "environmental and site conditions" = "condiciones ambientales y del sitio",
                              "use and human management" = "uso y gestion antropica",
                              "soil traits and water availability" = "caracteristicas del suelo y disponibilidad de agua",
                              "climate" = "clima", "spatial factors" = "distribucion espacial", "others" = "otros")) +
  coord_flip() +
  labs(
    x = "Tipos de Variables explicativas",
    y = "Porcentaje de estudios (%)"
  ) +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(r = 6)))

####OE3. MAPEAR ESTUDIOS POR PAIS Y CRUZAR CON INCENDIOS####
S2_fire <- read_excel("C:/Users/annac/Escritorio/OneDrive - Universidad de Alcala/01 MURE i Doctorat/14. PEX y TFM/TFM/Tratamiento datos/Data_treatment_v4.xlsx", 
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
  geom_sf(aes(fill = n_fires), color = "black", linewidth = 0.2) +
  geom_sf_label(aes(label = n_fires),
                fill = "white", color = "black", size = 3.5,
                fontface = "bold", label.size = 0.2) +
  scale_fill_viridis_c(option = "magma", na.value = "grey90", direction = -1) +
  coord_sf(xlim = c(-15, 40), ylim = c(25, 50), expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Estudios de recuperación postincendio por país",
    fill = "Nº estudios",
    x = "Longitud",
    y = "Latitud"
  )
