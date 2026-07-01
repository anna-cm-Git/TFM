
####SCRIPT RESULTADOS TFM####

library(readxl)
library(tidyverse)
library(writexl)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

citation()
citation("rnaturalearthdata")

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
solo_vegetacion <- subset(data_OE1_1, !is.na(study_level) & study_level != "NA")
relacion_ST_SL <- xtabs(~ study_level + study_type, data = solo_vegetacion)
relacion_ST_SL
global <- prop.table(relacion_ST_SL) * 100     # % respecto el total de estudios
global

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
  vefu <- "sap flow|ecos|flux|serot|disper|remain|c/n|transp|photosy|predawn|load|nrest|input|resis|resil|conduc|sequ|storag|carbon|elong|mortal|death|serotinity|dead|kill|burn|liv|leaf area|efficienc|assimil|nectar|respro|sprout|germin|viabil|pollen|surviv|time|seed|recruit|produ|regene|stomat|rate|18|13|15|xilo|grow|cone|shoot|new"
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
  relocate(response_variable_clean, .before = response_units) %>%
  mutate(             #cambios manuales para variables mal clasificadas
    response_variable_clean = case_when(
      our_id == 152 & response_variable == "Nº of trees" ~ "structure",
      our_id == 381 & response_variable == "seeders cover" ~ "structure",
      our_id == 381 & response_variable == "resprouters cover" ~ "structure",
      our_id == 426 & response_variable == "TESC90-06 (transitions to early-successional communities)" ~ "spectral response",
      our_id == 429 & response_variable == "vegetation type" ~ "diversity",
      our_id == 75 & response_variable == "Carbon isotope composition, δ13C" ~ "vegetation function",
      our_id == 75 & response_variable == "nitrogen isotope composition, δ15N" ~ "vegetation function",
      our_id == 647 & response_variable == "herbaceous species frequency" ~ "diversity",
      our_id == 738 & response_variable == "species occurrence" ~ "diversity",
      our_id == 743 & response_variable == "number of understorey species" ~ "diversity",
      our_id == 778 & response_variable == "dead stem density" ~ "structure",
      our_id == 778 & response_variable == "dead stem basal area" ~ "structure",
      our_id == 778 & response_variable == "fire-scorched stems" ~ "structure",
      our_id == 803 & response_variable == "species importance value (IV)" ~ "spectral response",
      our_id == 866 & response_variable == "resprouts cover" ~ "structure",
      our_id == 866 & response_variable == "resprouts mean height" ~ "structure",
      our_id == 892 & response_variable == "surviving trees DBH" ~ "structure",
      our_id == 892 & response_variable == "surviving trees height" ~ "structure",
      our_id == 904 & response_variable == "Tree height diversity (THD) of living stems" ~ "structure",
      our_id == 904 & response_variable == "tree diameter diversity (TDD) of living stems" ~ "structure",
      our_id == 273 & response_variable == "pine stems" ~ "abundance",
      our_id == 69 & response_variable == "dominant plants height" ~ "structure",
      our_id == 630 & response_variable == "Seedling growth (Height)" ~ "structure",
      our_id == 630 & response_variable == "Seedling growth (Trunk diameter)" ~ "structure",
      our_id == 706 & response_variable == "Number of species" ~ "diversity",
      our_id == 706 & response_variable == "Number of plant functional types" ~ "diversity",
      our_id == 798 & response_variable == "Life-forms" ~ "diversity",
      our_id == 817 & response_variable == "Crown defoliation" ~ "vegetation function",
      our_id == 819 & response_variable == "Vegetation recovery level" ~ "spectral response",
      our_id == 819 & response_variable == "Disturbance Index" ~ "spectral response",
      our_id == 68 & response_variable == "proportion of seeders" ~ "structure",
      our_id == 68 & response_variable == "proportion of fire-sensitive species" ~ "structure",
      our_id == 68 & response_variable == "seeder to resprouter ratio" ~ "structure",
      our_id == 527 & response_variable == "Mean species number per relevé" ~ "diversity",
      our_id == 527 & response_variable == "Total species number" ~ "diversity",
      our_id == 527 & response_variable == "Mean diversity (Shannon Wiener Index)" ~ "diversity",
      our_id == 536 & response_variable == "Total plant species richness" ~ "diversity",
      our_id == 102 & response_variable == "Aboveground carbon stock" ~ "structure",
      our_id == 457 & response_variable == "Sørensen similarity index" ~ "diversity",
      our_id == 809 & response_variable == "Diameter of cork tree trunks (ground validation)" ~ "structure",
      our_id == 809 & response_variable == "Floristic composition (species presence/cover)" ~ "diversity",
      our_id == 809 & response_variable == "Number of species per plot" ~ "diversity",
      our_id == 821 & response_variable == "Pinus halepensis seedling density" ~ "vegetation function",
      our_id == 123 & response_variable == "N concentration in dead (burned) wood" ~ "structure",
      our_id == 162 & response_variable == "Understory vegetation cover (germinating)" ~ "structure",
      our_id == 198 & response_variable == "Woody species cover" ~ "structure",
      our_id == 264 & response_variable == "Temporal turnover (species turnover composition component)" ~ "diversity",
      our_id == 264 & response_variable == "Temporal turnover (species turnover metrics)" ~ "diversity",
      our_id == 264 & response_variable == "dispersal ability variation over time" ~ "vegetation function",
      our_id == 320 & response_variable == "basal area" ~ "structure",
      our_id == 344 & response_variable == "radial growth" ~ "vegetation function",
      our_id == 506 & response_variable == "annual growth" ~ "vegetation function",
      our_id == 510 & response_variable == "mean plant mortality" ~ "vegetation function",
      our_id == 510 & response_variable == "mean number of plants" ~ "abundance",
      our_id == 515 & response_variable == "degree of serotiny" ~ "vegetation function",
      our_id == 996 & response_variable == "Life-form spectra" ~ "diversity",
      our_id == 617 & response_variable == "recovery rate" ~ "spectral response",
      our_id == 422 & response_variable == "Vegetation cover resilience" ~ "spectral response",
      our_id == 423 & response_variable == "Tree cover fraction" ~ "spectral response",
      our_id == 423 & response_variable == "Shrub cover fraction" ~ "spectral response",
      our_id == 423 & response_variable == "Background cover fraction" ~ "spectral response",
      our_id == 423 & response_variable == "Normalised Difference Tree-Shrub Fraction (NDTSF)" ~ "spectral response",
      our_id == 551 & response_variable == "Latent heat flux" ~ "spectral response",
      our_id == 551 & response_variable == "Evapotranspiration difference" ~ "spectral response",
      our_id == 851 & response_variable == "Vegetation density" ~ "spectral response",
      our_id == 776 & response_variable == "NDVI" ~ "spectral response",
      our_id == 422 & response_variable == "Vegetation recovery index" ~ "spectral response",
      our_id == 551 & response_variable == "Actual evapotranspiration" ~ "spectral response",
      our_id == 842 & response_variable == "Re-growth intensity" ~ "spectral response",
      our_id == 884 & response_variable == "vegetation recovery probability" ~ "spectral response",
      our_id == 1018 & response_variable == "Vegeration regrowth" ~ "spectral response",
      our_id == 1018 & response_variable == "Tree cover" ~ "spectral response",
      TRUE ~ response_variable_clean 
    ))

subtipos_vegetacion <- V_clasificacion %>% 
  filter(variable_type == "vegetation") %>% 
  group_by(response_variable_clean) %>% 
  summarise(percentage = round((n() / nrow(.)) * 100, 1))

#faltaria cambiar el ggplot
ggplot(subtipos_vegetacion, aes(x = reorder(response_variable_clean, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "green") +
  scale_x_discrete(limits = levels(reorder(subtipos_vegetacion$response_variable_clean, subtipos_vegetacion$percentage)),
                   labels = c("abundance" = "Abundancia", "diversity" = "Diversidad", "spectral response" = "Respuesta espectral",
                   "structure" = "Estructura", "vegetation function" = "Función de la vegetación")) +
  coord_flip() +
  labs(
    tag = "a)",
    x = "Subtipo de variable",
    y = "Porcentaje (%)"
  ) +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(r = 6)))

#exporto Excel para agrupar (nuevas categorias) variables más medidas
write_xlsx(V_clasificacion, "V_class.xlsx")

######Suelos######
S3_measuressuelo <- read_excel("C:/Users/annac/Escritorio/OneDrive - Universidad de Alcala/01 MURE i Doctorat/14. PEX y TFM/TFM/Tratamiento datos/Data_treatment_v4.xlsx",
                          sheet = "3_measures")

S3_measures_soil <- S3_measuressuelo %>%
  filter(variable_type == "soil")

#categorias: propiedades físicas, químicas, y biológicas. Procesos ecosistémicos e hidrológicos.
subcategories_soil <- function (var) {
  
  phys <- "osmo|^dry|field|mecha|emiss|humificatio|partic|ash|bare|cover|diam|textur|sand|silt|clay|compact|bulk|porosity|infiltr|humidit|water|moist|temperat|hydraulic conduct|stab"
  chem <- "polys|lignin|aromat|mangan|zinc|copper|lead|cobalt|arsen|mercu|ammon|chain|molec|iron|total|ph|electrical|cation|extract|nutrient|^som|labile|avail|exchang|organic|carbon|^n|nitrog|c/n|^al|^mn|^fe|^zn|^cu|\\bb\\b|^cr|^si|^s|^soc|^toc|^na|sodiu|^c|^p|^mg|magnes|^ca|^k|nh4|no2|humic|fluv|n:p"
  biol <- "colon|presen|sphoro|eukar|prokar|mycor|mineraliza|wood|popul|cellul|invert|phosphatase|metabo|ureas|litter|glomal|plfa|micro|bacter|dna|enzymat|respirat|biomass|invertebr|diversit|simpson|fung|activ|abund|rich|shann|qbs|gluco|aryl"
  ecos <- "cycl|decompo|flux|multi"
  hidr <- "connec|convergenc|flat|ls-|pond|flow|absolu|rough|run|eros|yield|sedim|loss"

  varlower <- str_to_lower(var) #pasar a minusculas
  
  case_when(     #orden importa: lo q tiene menos opciones primero = lo prioritario
    str_detect(varlower, hidr) ~ "hydrological processes",
    str_detect(varlower, ecos) ~ "ecosystem processes",
    str_detect(varlower, biol) ~ "biological properties",
    str_detect(varlower, phys) ~ "physical properties",
    str_detect(varlower, chem) ~ "chemical properties",
    varlower %in% c("-", "NA", "NaN") ~ "none",
    TRUE ~ var) #NA's se llamen none)    
  #me deja las que no clasifica con el nombre original                 
}

#aplico funcion a mi tabla
S_clasificacion <- S3_measures_soil %>% 
  mutate(across(
    .cols = c(response_variable), 
    .fns = ~ subcategories_soil(.x), 
    .names = "{.col}_clean"
    )) %>% 
  relocate(response_variable_clean, .before = response_units) %>% 
  mutate(             #cambios manuales para variables mal clasificadas
    response_variable_clean = case_when(
      our_id == 52 & response_variable == "forest floor standing weight" ~ "ecosystem processes",
      our_id == 65 & response_variable == "Titanium (Ti) and Zirconium (Zr)" ~ "hydrological processes",
      our_id == 169 & response_variable == "mineralizing C capacity (index)" ~ "ecosystem processes",
      our_id == 169 & response_variable == "C descomposition efficiency (index)" ~ "biological properties",  
      our_id == 388 & response_variable == "AIC-SSY relationship" ~ "hydrological processes", 
      our_id == 508 & response_variable == "Microaggregate size distribution" ~ "physical properties",    
      our_id == 406 & response_variable == "n-Fatty acid ratio" ~ "biological properties",  
      our_id == 406 & response_variable == "n-alkane ratio" ~ "biological properties",  
      our_id == 112 & response_variable == "Soil Quality Index (SQI)" ~ "soil quality index",     
      our_id == 228 & response_variable == "PCA.Aromatic non-specific compounds (relative abundance in topsoil)" ~ "chemical properties",  
      our_id == 228 & response_variable == "PCA.N-compounds (relative abundance in topsoil)" ~ "chemical properties",   
      our_id == 228 & response_variable == "PCA.Lignin-derived compounds (relative abundance in topsoil)" ~ "chemical properties",  
      our_id == 228 & response_variable == "PCA.Polysaccharide-derived compounds (relative abundance in topsoil)" ~ "chemical properties",  
      our_id == 228 & response_variable == "PCA.Hydroaromatic steroids (relative abundance in topsoil" ~ "chemical properties",    
      our_id == 162 & response_variable == "Ash" ~ "biological properties",  
      our_id == 223  & response_variable == "Needle cover" ~ "chemical properties", 
      our_id == 324  & response_variable == "Cover of leaves and thin branches < 2 cm (Lcov)" ~ "biological properties", 
      our_id == 324  & response_variable == "Cover of lying necromass > 2 cm (Ncov)" ~ "biological properties", 
      our_id == 361 & response_variable == "Specific UV Absorbance" ~ "physical properties",    
      our_id == 428 & response_variable == "water holding capacity" ~ "chemical properties",   
      our_id == 428 & response_variable == "Bulk chemical composition (C types)" ~ "chemical properties",   
      our_id == 815 & response_variable == "Soil Quality Index (SQI)" ~ "soil quality index",
      our_id == 339 & response_variable == "weight" ~ "physical properties",
      TRUE ~ response_variable_clean 
    ))

subtipos_suelos <- S_clasificacion %>% 
  filter(variable_type == "soil") %>% 
  group_by(response_variable_clean) %>% 
  summarise(percentage = round((n() / nrow(.)) * 100, 1))

#representacion grafica
ggplot(subtipos_suelos, aes(x = reorder(response_variable_clean, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "brown") +
  scale_x_discrete(limits = levels(reorder(subtipos_suelos$response_variable_clean, subtipos_suelos$percentage)),
                   labels = c("physical properties" = "Propiedades físicas", "chemical properties" = "Propiedades químicas",
                              "biological properties" = "Propiedades biológicas", "ecosystem processes" = "Procesos ecosistémicos",
                              "hydrological processes" = "Procesos hidrológicos", "soil quality index" = "Calidad del suelo")) +
  coord_flip() +
  labs(
    tag = "b)",
    x = "Subtipo de variable",
    y = "Porcentaje (%)"
  ) +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(r = 6)))

#exporto Excel para agrupar (nuevas categorias) variables más medidas
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
  )) %>%
  mutate(             #cambios manuales
    moderator_type_clean = case_when(
      our_id == 266 & moderator_type == "distance to unburned woodlands" ~ "spatial factors",
      our_id == 378 & moderator_type == "CVH" ~ "vegetation traits",
      our_id == 426 & moderator_type == "montado prefire patterns" ~ "vegetation traits",
      our_id == 426 & moderator_type == "mean contiguity index" ~ "vegetation traits",
      our_id == 426 & moderator_type == "mean patch ratius of gyration" ~ "vegetation traits",
      our_id == 426 & moderator_type == "normalized landscape shape index" ~ "vegetation traits",
      our_id == 426 & moderator_type == "splitting density" ~ "vegetation traits",
      our_id == 426 & moderator_type == "burned area distribution index" ~ "fire regime and traits",
      our_id == 426 & moderator_type == "burned area aggregation index" ~ "fire regime and traits",
      our_id == 426 & moderator_type == "effective mesh size" ~ "vegetation traits",
      our_id == 443 & moderator_type == "pine climatic niches" ~ "vegetation traits",
      our_id == 617 & moderator_type == "prefire vegetation state" ~ "vegetation traits",
      our_id == 687 & moderator_type == "unburnt remnant patches" ~ "fire regime and traits",
      our_id == 788 & moderator_type == " VARI index" ~ "time since fire",
      our_id == 911 & moderator_type == " prefire stand age" ~ "vegetation traits",
      our_id == 98 & moderator_type == " postfire pine age" ~ "vegetation traits",
      our_id == 315 & moderator_type == "distance to the unburned area" ~ "spatial factors",
      our_id == 315 & moderator_type == " distance to the nearest trap" ~ "spatial factors",
      our_id == 315 & moderator_type == " burned area" ~ "fire regime and traits",
      our_id == 388 & moderator_type == "C factor" ~ "use and human management",
      our_id == 956 & moderator_type == "branches" ~ "vegetation traits",
      our_id == 212 & moderator_type == "prefire vegetation condition" ~ "vegetation traits",
      our_id == 785 & moderator_type == "prefire vegetationcover" ~ "vegetation traits",
      our_id == 785 & moderator_type == "prefire vegetation moisture and structure" ~ "vegetation traits",
      our_id == 640 & moderator_type == "distance from burn perimeter" ~ "spatial factors",
      our_id == 817 & moderator_type == "defoliation" ~ "vegetation traits",
      TRUE ~ moderator_type_clean)) 

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
  huma <- "terrac|harvesti|exploit|lulc|interve|manag|treatm|logg|thinn|land use|use|prescrib|pile|human"
  
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
  )) %>%
  mutate(             #cambios manuales
    moderator_type_clean = case_when(
      our_id == 79 & moderator_type == "pine pre-fire basal area" ~ "vegetation traits",
      our_id == 79 & moderator_type == "extent of erosion" ~ "soil traits and water availability",
      our_id == 801 & moderator_type == "composite burn index (severity)" ~ "fire regime and traits",
      our_id == 856 & moderator_type == "flood- and erosion-control" ~ "soil traits and water availability",
      our_id == 557 & moderator_type == " study periods (pre-fire, post-fire, and regrowth)" ~ "time since fire",
      our_id == 564 & moderator_type == " vegetation height (from \"rock cover\" to 16 m)" ~ "vegetation traits",
      our_id == 571 & moderator_type == " mesh size" ~ "leaf mesh size",
      our_id == 96 & moderator_type == "pre-fire sapling density" ~ "vegetation traits",
      our_id == 96 & moderator_type == "pre-fire seedling presence" ~ "vegetation traits",
      our_id == 340 & moderator_type == "Landscape configuration" ~ "environmental and site conditions",
      our_id == 457 & moderator_type == "pre-fire vegetation structure" ~ "vegetation traits",
      our_id == 809 & moderator_type == "area (Area 4 vs Area 6)" ~ "environmental and site conditions",
      our_id == 822 & moderator_type == "sound cone crop size" ~ "vegetation traits",
      our_id == 162 & moderator_type == "pine seed input (post fire and post harvest)" ~ "vegetation traits",
      our_id == 482 & moderator_type == "distance from trunk" ~ "vegetation traits",
      our_id == 221 & moderator_type == "AI15 index (P*I15)" ~ "climate",
      our_id == 238 & moderator_type == "fire class (nunber of fires, time since last fire, time since penultimate fire)" ~ "fire regime and traits",
      our_id == 238 & moderator_type == "ground cover composition (no vascular cryptogams, bare soil, rocky outcrop cover)" ~ "soil traits and water availability",
      our_id == 264 & moderator_type == "dispersal ability categoty (low, moderate, high)" ~ "vegetation traits",
      our_id == 267 & moderator_type == "tree size" ~ "vegetation traits",
      our_id == 267 & moderator_type == "tree location" ~ "vegetation traits",
      our_id == 269 & moderator_type == " prefire plant diameter" ~ "vegetation traits",
      our_id == 320 & moderator_type == " pine size class" ~ "vegetation traits",
      our_id == 335 & moderator_type == "prefire overstorey structure" ~ "vegetation traits",
      our_id == 335 & moderator_type == "prefire understorey structure" ~ "vegetation traits",
      our_id == 335 & moderator_type == "post-fire overstorey structure" ~ "vegetation traits",
      our_id == 335 & moderator_type == " post-fire understorey structure" ~ "vegetation traits",
      our_id == 344 & moderator_type == "initial target tree size" ~ "vegetation traits",
      our_id == 350 & moderator_type == "woody debris after fire" ~ "vegetation traits",
      our_id == 387 & moderator_type == "post-fire vegetation recovery" ~ "vegetation traits",
      our_id == 506 & moderator_type == "previous size of the individuals" ~ "vegetation traits",
      our_id == 815 & moderator_type == "time since the soil treatment" ~ "soil traits and water availability",
      our_id == 851 & moderator_type == " Structure type" ~ "use and human management",
      our_id == 384 & moderator_type == "Na" ~ "soil traits and water availability",
      our_id == 347 & moderator_type == "previous stand age" ~ "vegetation traits",
      our_id == 305 & moderator_type == "forest type (mono-specific eucalypt plantation, mono-specific maritime pine plantation, mixed eucalypt-pine stand)" ~ "vegetation traits",
      our_id == 304 & moderator_type == "edibility" ~ "nutritional mode",
      our_id == 302 & moderator_type == "heat load index" ~ "environmental and site conditions",
      our_id == 302 & moderator_type == "number of mature surviving trees" ~ "vegetation traits",
      our_id == 302 & moderator_type == "no. of fires" ~ "fire regime and traits",
      our_id == 302 & moderator_type == "vegetation cover (ferns, herbs, coarse woody debris, stones, bare soil and litter)" ~ "vegetation traits",
      our_id == 884 & moderator_type == " pre-fire vegetation" ~ "vegetation traits",
      TRUE ~ moderator_type_clean)) 

#union de bases de datos mia y revisoras
M_classTotal <- bind_rows(M_clasificacion, M_clasificacion2) %>% 
  mutate(moderator_type_clean = if_else(moderator_type_clean == "climate",
                                        "environmental and site conditions", #que clima sea condiciones ambientales y de sitio
                                        moderator_type_clean))

#exporto Excel 
write_xlsx(M_classTotal, "M_classTotal.xlsx")
#agrupacion variables más medidas (nuevas categorias) mediante codigo
vegetationtraitsClass<- M_classTotal %>% 
  filter(moderator_type_clean == "vegetation traits") %>% 
  distinct()

vegetationtraits <- function (var) {
  
  index <- "index|ndvi|evi|spectr|satellit"
  lifor <- "life form|regenerat|trait|function|strateg|resprout|seed|raunki|phenolog"
  diver <- "divers|richness|shannon|simpson|evenness"
  spide <- "species|taxa|taxon|flora|ulex|rosmarinus|cistus|pinus|quercus"
  vegty <- "vegetation type|pre-fire veg|communit|forest|shrub|grass|woodland|biome|ecosystem"
  vestr <- "cover|basal|diamet|height|structur|biomass|snag|densit|canopy|lai|leaf area|size"
  varlower <- str_to_lower(var) #pasar a minusculas
 
  case_when(     # el orden importa: lo q tiene menos opciones primero = lo prioritario
    str_detect(varlower, index) ~ "remote sensing index",
    str_detect(varlower, lifor) ~ "life form and functional traits",
    str_detect(varlower, diver) ~ "diversity and richness",
    str_detect(varlower, spide) ~ "species identity",
    str_detect(varlower, vegty) ~ "vegetation type",
    str_detect(varlower, vestr) ~ "vegetation structure",
    
    varlower %in% c("-", "na", "nan") ~ "none",    # NA's se llamen none (en minuscula por str_to_lower)
    is.na(varlower) ~ "none",                      # Por si entra un NA real de R
    TRUE ~ var)
  }
  
VegTraits_class <- vegetationtraitsClass %>% 
  mutate(across(
    .cols = c(moderator_type), 
    .fns = ~ vegetationtraits(.x), 
    .names = "{.col}_clean2"
  )) %>% 
  count(moderator_type_clean2, sort = TRUE)

#porcentaje de papers que analiza cada tipo de moderador
total_papers <- n_distinct(M_classTotal$our_id)
subtipo_mods <- M_classTotal %>%
  distinct(our_id, moderator_type_clean) %>% 
  filter(!is.na(moderator_type_clean) & moderator_type_clean != "NA") %>%    #elimina vacios y donde pone NA
  count(moderator_type_clean, name = "num_papers") %>% 
  mutate(percentage = round((num_papers/total_papers)*100, 1)) %>% 
  arrange(desc(percentage)) #desc = orden descendente de mayor a menor

Mods_final2 <- subtipo_mods %>%
  slice(-8,-9) %>%  #elimino filas vacias, no habia moderadores
  mutate(moderator_type_clean = if_else(percentage < 1 | moderator_type_clean == "spatial factors", "others", moderator_type_clean)) %>%  #moderadores < 1% a "others"
  group_by(moderator_type_clean) %>% 
  summarise(num_papers = sum(num_papers),
            percentage = sum(percentage)) %>%
  arrange(desc(percentage))

#representacion grafica
ggplot(Mods_final2, aes(x = reorder(moderator_type_clean, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "blue") +
  scale_x_discrete(limits = levels(reorder(Mods_final2$moderator_type_clean, Mods_final2$percentage)),
                   labels = c("time since fire" = "Tiempo desde el incendio", "vegetation traits" = "Características de la vegetación",
                              "fire regime and traits" = "Características y regímen del incendio",
                              "environmental and site conditions" = "Clima, fisiografia, paisaje y sitio",
                              "use and human management" = "Gestión y uso humano",
                              "soil traits and water availability" = "Características del suelo y disponibilidad de agua",
                              "spatial factors" = "Distribución espacial", "others" = "Otras")) +
  coord_flip() +
  labs(
    x = "Tipos de Variables explicativas",
    y = "Porcentaje de estudios (%)"
  ) +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(r = 6)))

####OE3. MAPEAR ESTUDIOS POR PAIS Y CRUZAR CON INCENDIOS####
studies_90_26 <- data_OE1_1 %>%
  group_by(country) %>% 
  summarise(n_studies = n()) %>% 
  slice(-7) %>% 
  mutate(percentage = (n_studies / sum(n_studies)) * 100) %>%
  arrange(desc(n_studies))

world <- ne_countries(scale = "medium", returnclass = "sf")   #cargo paises del mundo
mbasis <- c("Algeria", "France", "Greece", "Israel","Italy", "Morocco","Portugal",
            "Spain", "Turkey", "Albania", "Bosnia and Herzegovina","Bulgaria",
            "Croatia", "Egypt", "Iraq", "Kosovo", "Lebanon", "Libya","Macedonia",
            "Malta", "Montenegro", "Palestina", "Serbia", "Slovenia","Syria",
            "Tunisia")

mbasis_world <- world %>% filter(name %in% mbasis)    #defino y filtro por mis paises

mapa_mb_w <- left_join(mbasis_world, studies_90_26, by = c("name" = "country"))

#DIBUIXAR EL MAPA
ggplot()+
  geom_sf(data = world, fill = "grey95", color = "darkgrey", linewidth = 0.2) +
  geom_sf(data = mapa_mb_w, aes(fill = n_studies), color = "black", linewidth = 0.2) +
  geom_sf_label(data = mapa_mb_w, aes(label = n_studies),
                fill = "white", color = "black", size = 3.5,
                fontface = "bold", label.size = 0.2) +
  scale_fill_viridis_c(option = "magma", na.value = "grey70", direction = -1) +
  coord_sf(xlim = c(-15, 40), ylim = c(25, 50), expand = FALSE) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#D6EAF8", color = NA), 
        panel.grid.major = element_line(color = "white", linewidth = 0.3)) +
  labs(
    fill = "Nº estudios",
    x = "Longitud",
    y = "Latitud"
  )

#EFIS NUMERO DE INCENDIOS (Start date: 01/10/2008, End date: 11/06/2026)
fires_EFIS <- read_excel("C:/Users/annac/Escritorio/OneDrive - Universidad de Alcala/01 MURE i Doctorat/14. PEX y TFM/TFM/Tratamiento datos/Distribucion geografica/EFIS_data/fires_EFIS.xlsx")
n_fires_MB <- fires_EFIS %>%
  select(-sclerophillous_vegetation_percent, -transitional_vegetation_percent, -other_natural_percent,
         -agriculture_percent, -artificial_percent, -other_percent, -natura2k_percent) %>% 
  filter(broadleaved_forest_percent > 0 | coniferous_forest_percent > 0 | mixed_forest_percent > 0) %>% 
  group_by(country) %>% 
  summarise(
    num_incendios08_26 = n_distinct(id)) %>% 
  arrange(desc(num_incendios08_26))

#Numero de estudios del 2008 al 2026
studies_08_26 <- data_OE1_1 %>%
  filter(Year >= 2008,
         Year <= 2026) %>% 
  group_by(country) %>% 
  summarise(num_estudios08_26 = n())

#cruzo nº incendios con nº estudios (2008 - 2026) -->  a tener en cuenta meses de inicio y final de los datos
study_fire_cross <- left_join(n_fires_MB, studies_08_26, by = "country") %>% 
  filter(!is.na(num_estudios08_26)) %>% 
  mutate(country_es = case_when(
    country == "Spain" ~ "España",
    country == "Italy" ~ "Italia",
    country == "France" ~ "Francia",
    country == "Greece" ~ "Grecia",
    country == "Turkey" ~ "Turquía",
    country == "Algeria" ~ "Argelia",
    country == "Morocco" ~ "Marruecos",
    TRUE ~ country))

write_xlsx(study_fire_cross, "cruce_st_fires_08_26.xlsx")

#grafico de puntos incendios - estudios 2008 - 2026
ggplot(study_fire_cross, aes(x = num_incendios08_26, y = num_estudios08_26)) +
  geom_smooth(data = subset(study_fire_cross, country_es != "España"), method = "lm", color = "darkkhaki", linetype = "dashed", se = FALSE) +
  geom_smooth(method = "lm", color = "darksalmon", linetype = "dashed", se = FALSE) +
  geom_point(color = "royalblue", size = 3.2) +
  geom_text(aes(label = country_es), vjust = -1, hjust = -0.1, size = 3) +
  annotate("text", x = 5650, y = 42, label = "Tendencia \ngeneral", color = "darksalmon", size = 3) +
  annotate("text", x = 5650, y = 16, label = "Tendencia \nsin España", color = "darkkhaki", size = 3) +
  labs(x = "Número de incendios",y = "Número de estudios") +
  theme_minimal()

######Severidad, bosque afectado y manejo#####
S2_severity <- read_excel("C:/Users/annac/Escritorio/OneDrive - Universidad de Alcala/01 MURE i Doctorat/14. PEX y TFM/TFM/Tratamiento datos/Data_treatment_v4.xlsx", 
                      sheet = "2_fire") %>% 
  filter(!country %in% c("Australia", "California", "Chile", "EEUU", "SouthAfrica")) %>%
  distinct(our_id, fire_id, .keep_all = TRUE) %>% 
  mutate(severity = if_else(severity %in% c("low;medium;high", 
                                             "low;moderate;high", 
                                             "medium;high;low"), "all", severity)) %>% 
  mutate(severity = gsub("medium", "moderate", severity)) %>% 
  group_by(severity) %>%
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  mutate(porcentaje = round((total / sum(total)) * 100, 2))

S2_foresttype <- read_excel("C:/Users/annac/Escritorio/OneDrive - Universidad de Alcala/01 MURE i Doctorat/14. PEX y TFM/TFM/Tratamiento datos/Data_treatment_v4.xlsx", 
                                           sheet = "2_fire") %>% 
  filter(!country %in% c("Australia", "California", "Chile", "EEUU", "SouthAfrica")) %>% 
  distinct(our_id, fire_id, forest_type, .keep_all = TRUE) %>% 
  group_by(forest_type) %>% 
  summarise(total = n())

S2_postfireMG <- read_excel("C:/Users/annac/Escritorio/OneDrive - Universidad de Alcala/01 MURE i Doctorat/14. PEX y TFM/TFM/Tratamiento datos/Data_treatment_v4.xlsx", 
                            sheet = "2_fire") %>% 
  filter(!country %in% c("Australia", "California", "Chile", "EEUU", "SouthAfrica")) %>% 
  distinct(our_id, fire_id, postfire_management, .keep_all = TRUE) %>%
  mutate(postfire_management = gsub("; ", ";", postfire_management)) %>%
  group_by(postfire_management) %>%
  mutate(postfire_management = na_if(postfire_management, "NA")) %>%
  summarise(total = n()) %>% 
  mutate(porcentaje = round((total / sum(total)) * 100, 2))
