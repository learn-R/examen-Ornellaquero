#Desafío

#Paquetes
pacman::p_load(tidyverse, #Universo de librerías para manipular datos
               haven, #Para cargar datos
               dplyr,#Para manipular datos 
               sjmisc,#Para explorar datos
               magrittr, #Para el operador pipeline (%>%)
               sjPlot,
               srvyr,
               survey,
               sjlabelled)

#Abrir base de datos
EME <- read_dta("input/Base de datos Full VI EME (extracto).dta")

#explorar base de datos
head(EME$region)

#Seleccionar variables
EME <- select(EME, macrozona=region, Enc_rph, ganancia_final_mensual, conta_completa,
              registro_SII, CISE, Factor_EME)   

##Recodificar
EME <- mutate(EME, macrozona = case_when(macrozona %in% c(1:3, 15) ~ "1",
                                         macrozona %in% c(4:8, 16) ~ "2",
                                         macrozona %in% c(9, 10, 14) ~ "3",
                                         macrozona %in% c(11, 12) ~ "4",
                                         TRUE~NA_character_))

frq(EME$macrozona)
                                                     
#Objeto encuesta
EME <- EME %>% 
  group_by(macrozona)%>% 
  mutate(stratn = sum(Factor_EME)) %>%  #Calculamos el total de personas por estrato
  ungroup() #desagrupamos

EME_proc <- EME %>%
  as_survey_design(ids = Enc_rph, 
                   strata = macrozona,
                   fpc = stratn,
                   weights = Factor_EME)


