#Examen
#Ornella Quero


# 1. Cargar paquetes ------------------------------------------------------

pacman::p_load(tidyverse,
               haven,
               dplyr,
               sjmisc,
               sjPlot,
               srvyr,
               survey,
               magrittr,
               sjlabelled)


# 2. Cargar datos ---------------------------------------------------------

elsoc_2021 <- read_sav("input/data/ELSOC_W05_v1.0_SPSS.sav")

# 3. Explorar base de datos -----------------------------------------------

names(elsoc_2021)
head(elsoc_2021$region)

# 4. Seleccionar variables ------------------------------------------------

datare <- elsoc_2021 %>% select(r16, r09, r12_04, c37_05, m0_sexo, m0_edad, region)


# 5.  Procesar variables --------------------------------------------------

#r16: Grado de confianza
datare$r16<- car::recode(datare$r16,"c(-888,-999, -777, -666)= NA")

datare<-
  datare%>% 
  mutate(r16_proc = case_when(r16>=2 & r16<=3 ~"Poca o Algo",
                              r16>=4 & r16<=5 ~ "Bastante o Mucha",
                              r16==1 ~ "Nada" ))

#r09: Grado de simpatia por [PER/HAI/VEN] que viven en Chile

datare$r09<- car::recode(datare$r09,"c(-888,-999, -777, -666)= NA")

datare<-
  datare%>% 
  mutate(r09_proc = case_when(r09>=2 & r09<=3 ~"Poco o Algo",
                              r09>=4 & r09<=5 ~ "Bastante o Mucho",
                              r09==1 ~ "Muy poco o nada" ))

#r12_04:Grado de acuerdo: Con llegada de [PER/HAI/VEN] aumenta el desempleo

datare$r12_04<- car::recode(datare$r12_04,"c(-888,-999, -777, -666)= NA")

datare<-
  datare%>% 
  mutate(r12_04_proc = case_when(r12_04>=1 & r12_04<=2 ~"En desacuerdo",
                                 r12_04>=4 & r12_04<=5 ~ "De acuerdo",
                                 r12_04==3 ~ " Ni en desacuerdo ni de acuerdo" ))

#c37_05: Grado de acuerdo: restricciones ingreso migrantes

datare$c37_05<- car::recode(datare$c37_05,"c(-888,-999, -777, -666)= NA")

datare<-
  datare%>% 
  mutate(c37_05_proc = case_when(c37_05>=1 & c37_05<=2 ~"En desacuerdo",
                                 c37_05>=4 & c37_05<=5 ~ "De acuerdo",
                                 c37_05==3 ~ " Ni en desacuerdo ni de acuerdo" ))

#m0_sexo: Sexo del entrevistado
datare <- 
  datare %>% 
  mutate(m0_sexo = case_when(m0_sexo == 1 ~ "Hombre",
                             m0_sexo == 2 ~ "Mujer"))
frq(datare$m0_sexo, out = "viewer")

#m0_edad por tramos
datare <- 
  datare %>% 
  mutate(edad_tramo = case_when(m0_edad <=39 ~  "Joven",
                                m0_edad > 39 & m0_edad <=59 ~ "Adulto",
                                m0_edad > 59 ~ "Adulto mayor",
                                TRUE ~ NA_character_))
frq(datare$edad_tramo, out = "viewer")

datare<-
  datare %>% 
  mutate(sexo_edad_tramo = case_when(m0_sexo == "Hombre" & m0_edad <=39 ~  "Hombre joven",
                                     m0_sexo == "Mujer" & m0_edad <=39 ~  "Mujer joven",
                                     m0_sexo == "Hombre" & (m0_edad > 39 & m0_edad <=59) ~ "Hombre adulto",
                                     m0_sexo == "Mujer" & (m0_edad > 39 & m0_edad <=59) ~ "Mujer adulta",
                                     m0_sexo == "Hombre" & m0_edad > 59 ~ "Adulto mayor",
                                     m0_sexo == "Mujer" & m0_edad > 59 ~ "Adulta mayor",
                                     TRUE ~ NA_character_))
frq(datare$sexo_edad_tramo, out = "viewer")

#regi??n
datare <-
  datare %>% 
  mutate(region = case_when
         (region %in% c(Arica y Parinacota, Trapac??, Antofagasta, Atacama) ~ "Macrozona Norte",
          region %in% c(Coquimbo, Valpara??so) ~ "Macrozona Centro",
          region %in% c(Libertador General Bernardo Ohiggins, Maule, ??uble, Biob??o) ~ "Macrozona Centro Sur",
          region %in% c(La Araucan??a, Los R??os, Los Lagos) ~ "Macrozona Sur",
          region %in% c(Aysen, Magallanes) ~ "Macrozona Austral",
          TRUE~NA_character_))

datare <-
  datare %>%
  mutate(region = as.numeric(.$region)) %>% 
  mutate(region = case_when(region == "Arica y Parinacota" ~ "15", 
                            region == "Trapac??" ~ "1", 
                            region == "Antofagasta" ~ "2",
                            region == "Atacama" ~ "3",
                            TRUE~NA_character_))

frq(datare$region, out = "viewer")
  
  # 7. Eliminar NA ----------------------------------------------------------

nrow(datare)
datare <- na.omit(datare) 
nrow(datare)
