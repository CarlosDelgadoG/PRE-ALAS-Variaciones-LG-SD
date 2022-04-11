library(tidyverse)
library(data.table)

base_long<- readRDS("DATOS/base_long.RDS")
base_long <-sjmisc::set_na(base_long,na=c(-888,-999))

#Atributos
frame_atributos <- readRDS("DATOS/frame_atributos.RDS")

#ESCALA PHQ-9
base_long$s11_01_rec <- car::recode(base_long$s11_01-1,"4=3")
base_long$s11_02_rec <- car::recode(base_long$s11_02-1,"4=3")
base_long$s11_03_rec <- car::recode(base_long$s11_03-1,"4=3")
base_long$s11_04_rec <- car::recode(base_long$s11_04-1,"4=3")
base_long$s11_05_rec <- car::recode(base_long$s11_05-1,"4=3")
base_long$s11_06_rec <- car::recode(base_long$s11_06-1,"4=3")
base_long$s11_07_rec <- car::recode(base_long$s11_07-1,"4=3")
base_long$s11_08_rec <- car::recode(base_long$s11_08-1,"4=3")
base_long$s11_09_rec <- car::recode(base_long$s11_09-1,"4=3")

base_long$s11_phq9<- with(base_long,(s11_01_rec+s11_02_rec+s11_03_rec+s11_04_rec+s11_05_rec+s11_06_rec+s11_07_rec+s11_08_rec+s11_09_rec))
attr(base_long$s11_phq9,"label") <- "Escala sintomatología depresiva"

#EDAD

base_long$m0_edad_fac <-factor( with(base_long,
                              case_when(m0_edad < 35 ~1,
                                        m0_edad > 34 & m0_edad < 60 ~2,
                                        m0_edad >59 ~3)),
                                labels = c("18-34 años","35-59 años","Más de 60 años"))
# ESCALA PHQ-9 
base_long$s11_phq9_bin <- with(base_long,
                                case_when(s11_phq9 <10~0,
                                          s11_phq9 >9 ~1))
attr(base_long$s11_phq9_bin,"label") <- "Escala PHQ-9 presenta síntomas"


#Sexo del entrevistado
base_long$m0_sexo_fac <- factor(base_long$m0_sexo,
                                 labels = c("Hombre","Mujer"))

attr(base_long$m0_sexo_fac,"label")<- "Sexo entrevistado"

  #En tratamiento por depresión
base_long$s14_fac <- factor(base_long$s14,
                             labels = c("No","Sí"),
                             levels = c(2,1))

attr(base_long$s14_fac,"label") <- "Tratamiento por Depresión"
#Satisfacción con la vida
base_long$s01_fac <- factor(car::recode(base_long$s01,"c(1,2)=1;3=2;c(4,5)=3"),
                             levels = c(1,2,3),
                             labels = c("Instatisfecho",
                                        "Ni satisfecho ni insatisfecho",
                                        "Satisfecho"))

setattr(base_long$s01_fac,"label","Satisfacción con la vida")

#Satisfaccion con ingreso

base_long$m16_fac<- factor(car::recode(base_long$m16, "c(1,2)=1;3=2;c(4,5)=3"),
labels = c("Instatisfecho", "Ni instatisfecho ni satisfecho","Satisfecho"))

setattr(base_long$m16_fac,"label","Satisfacción con ingreso")

#Percepción subjetiva de la salud
base_long$s03_fac <- factor(car::recode(base_long$s03, "c(1,2)=1;c(3,4,5)=2"),
                             labels = c("Mala-Regular","Buena-Excelente"))
setattr(base_long$s03_fac, "label","Percepción subjetiva estado salud")
#Sobrecarga endeudamiento
base_long$m43_fac<- factor(car::recode(base_long$m43, "c(1,2)=1;c(3,4)=2"),
                            labels = c("No Sobrecargado","Sobrecargado"))
setattr(base_long$m43_fac,"label","Sobrecarga endeudamiento")
#Grado en que mi vida se acerca a mi ideal
base_long$s02_fac <- factor(car::recode(base_long$s02, "c(1,2)=1;3=2;c(4,5)=3"),
                             labels = c("Se aleja de mi ideal","No se acerca ni se aleja","Se acerca a mi ideal"))
setattr(base_long$s02_fac,"label","Grado en que vida se acerca a ideal")

#Actividad principal

base_long$m02_fac<-factor(car::recode(base_long$m02, "c(1,2,3)=1;7=2;5=3;6=4;c(4,8,9)=5"),
                           labels = c("Trabajo Remunerado","Trabajo Doméstico No Remunerado","Jubilado","Desempleado","Otras Categorías"))

setattr(base_long$m02_fac,"label","Actividad principal entrevistado")
#Nivel educacional
base_long$m01_fac<- factor(car::recode(base_long$m01, "c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4"),
                            labels = c("Básica","Media","Técnica","Universitaria"))
setattr(base_long$m01_fac, "label","Nivel educacional")
#Estado civil 
base_long$m36_fac <- factor(car::recode(base_long$m36,"c(1,2,3)=1;c(4,5,6,7,8)=2;else=NA"),
                             labels = c("En Pareja","Soltero"))
setattr(base_long$m36_fac,"label","Estado Civil")
#Comportamiento prosocial
base_long$c07_prosocial <- with(base_long,(c07_01+c07_02+c07_03+c07_04+c07_05+c07_06+c07_07+c07_08))

attr(base_long$c07_prosocial,"label") <- "Escala comportamiento pro-social"

# ESTRESORES
base_long$s13_01_rec <-car::recode(base_long$s13_01-1, "0=1;1=0")
base_long$s13_02_rec <-car::recode(base_long$s13_02-1, "0=1;1=0")
base_long$s13_03_rec <-car::recode(base_long$s13_03-1, "0=1;1=0")
base_long$s13_04_rec <-car::recode(base_long$s13_04-1, "0=1;1=0")
base_long$s13_05_rec <-car::recode(base_long$s13_05-1, "0=1;1=0")
base_long$s13_06_rec <-car::recode(base_long$s13_06-1, "0=1;1=0")
base_long$s13_07_rec <-car::recode(base_long$s13_07-1, "0=1;1=0")
base_long$s13_08_rec <-car::recode(base_long$s13_08-1, "0=1;1=0")

base_long$s13_estresores <- with(base_long,(s13_01_rec+s13_02_rec+s13_03_rec+s13_04_rec+s13_05_rec+s13_06_rec+s13_07_rec+
                                                s13_08_rec))
attr(base_long$s13_estresores,"label")<- "Cantidad de eventos estresores"


# IMC ---------------------------------------------------------------------

base_long$s_imc <- with(base_long, s06/(s05/100)^2)
# SACAR OBSERVACIONES SOSPECHOSAS
base_long$s_imc[c(1765,2613,2697,6793)] <-NA 
#Attributos
setattr(base_long$s_imc,"label","Indice de masa corporal")
base_long$s_imc_tramos <- factor(with(base_long, case_when(
                            s_imc >18.4 & s_imc < 25 ~ 1,
                            s_imc >24.9 & s_imc < 30 ~ 2,
                            s_imc > 29.9 & s_imc <35 ~ 3,
                            s_imc>34.9 & s_imc < 40 ~ 4,
                            s_imc > 40~5)),
                            labels = c("Normal", "Sobre Peso", "Obesidad I", "Obesidad II", "Obesidad III"))
base_long$s_imc_fac <- factor(with(base_long, case_when(
  s_imc >18.4 & s_imc < 25 ~ 1,
  s_imc >24.9 & s_imc < 30 ~ 2,
  s_imc > 29.9  ~ 3)),
  labels = c("Normal", "Sobre Peso", "Obesidad"))

setattr(base_long$s_imc_fac,"label","Tramos IMC")



# Jornada laboral ---------------------------------------------------------

base_long$m12_fac <- factor(with(base_long,case_when(
                          m12 <25 ~1,
                          m12 > 24 & m12 <46 ~2,
                          m12 >45 ~3)),
                          labels = c("Menos de 25 horas", "25 a 45 horas","Más de 45 horas"))
setattr(base_long$m12_fac,"label","Jornada Laboral")

# Horas de cuidados de familia

base_long$m17_fac <- factor(with(base_long,case_when(
                                            m17 < 25 ~1,
                                            m17 > 24 & m17 < 46~2,
                                            m17 > 45 ~3)),
                            labels = c("Menos de 25 horas","25 a 45 horas","Más de 45 horas"))

setattr(base_long$m17_fac,"label","Horas al cuidado de familia")
# Tramos Cigarros ---------------------------------------------------------

base_long$s08_fac<- factor(with(base_long, case_when(
                                            s08== 0 ~1,
                                            s08 >0  ~2)),
                    labels = c("No Fumador", "Fumador"))

setattr(base_long$s08_fac,"label","Fuma o No Fuma")

# Frecuencia actividad física
base_long$s04_fac <- factor(with(base_long,
                                 car::recode(s04,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3")),
                            labels = c("Más de 3 veces por semana","1 a 8 veces al mes","Nunca o casi nunca"))

setattr(base_long$s04_fac,"label","Frecuencia de actividad física")

# Consumo de alcohol
base_long$s09 <- factor(base_long$s09, labels = unlist(frame_atributos[frame_atributos$var=="s09",]$labels)[-c(1,2)])

setattr(base_long$s09,"label","Consume Alcohol")

# Frecuencia para hablar con familiares
base_long$s12_fac <- factor(with(base_long,
                                 car::recode(s12,"c(1,2)=1;3=2;c(4,5)=3")),
                            labels = c("Nunca o pocas veces","Algunas veces","Siempre o Casi Siempre"))

setattr(base_long$s12_fac,"label","Habla de problemas con amigos y familiares" )


# Número de confidentes
base_long$r13_fac <- factor(with(base_long,
                                 car::recode(r13_nredes,"c(0,1)=1;c(2,3)=2;c(4,5)=3")),
                            labels = c("Ningún o 1 confidente","2 o 3 Confidentes","4 o 5 Confidentes"))

setattr(base_long$r13_fac,"label","Número de confidentes")

# Cantidad de Amigos Cercanos
base_long$r15_fac <- factor(car::recode(base_long$r15,"1='No tiene amigos';c(2,3)='De 1 a 5 amigos';c(4,5)='Más de 5 amigos'"),
                            levels = c('No tiene amigos','De 1 a 5 amigos','Más de 5 amigos'))

setattr(base_long$r15_fac, "label","Cantidad de amigos cercanos")

# Socialización primaria
base_long$m49_fac <- factor(with(base_long,
                                 car::recode(m49, "c(1,2,4,5,6)=1;3=2")),
                            labels = c("Otras Categorías","Ambos Padres"))

setattr(base_long$m49_fac,"label","Socialización Primaria")

# LUGAR DE ULTIMA ATENCION
base_long$s16_fac <- factor(car::recode(base_long$s16,"c(1,2)=1;c(3,4)=2;c(5,6,7)=3"),
                            label=c("Público","Privado","Otro"))
setattr(base_long$s16_fac,"label","Tipo establecimiento última atención médica")

# TIPO DE PREVISIÓN SOCIAL

base_long$s18_fac <- factor(car::recode(base_long$s18,"c(1,2,3,4,5,6)=1; c(8)=2;c(7,9,10)=3"),
                            label=c("Fonasa","Isapre","Otro"))
setattr(base_long$s18_fac,"label","Tipo de previsión social")

# DIAGNOSTICOS DE ENFERMEDADES
base_long$s19_diag <- 8-select(base_long,s19_01,s19_02,s19_04,s19_05)%>%
  rowSums()

setattr(base_long$s19_diag,"label","Cantidad de diagnósticos de enfermades físicas")

base_long$s19_diag_fac <- factor(with(base_long,
                                      case_when(s19_diag ==0~0,
                                                s19_diag >0~1)),
                                 labels=c("Ninguna","Alguna"))

setattr(base_long$s19_diag_fac,"label","Presenta diagnóstico de enfermedad física")

base_long$ola_num <- as.numeric(car::recode(base_long$ola, "2016=1;2017=2;2018=3;2019=4"))-1

#EVVENTOS ESTRESORES FAC
base_long$s13_estresores_fac <- factor(with(base_long,
                                     case_when(s13_estresores==0 ~1,
                                               s13_estresores %in% c(1,2) ~2,
                                               s13_estresores >2 ~3)),
                                  labels=c("0 eventos", "1 o 2 eventos","3 o más eventos"))

# COMPORTAMIENTO PROSOCIAL FAC
base_long$c07_ps_fac <- factor(with(base_long,
                             case_when(c07_prosocial < 16~1,
                                       c07_prosocial>15~2)),
                              labels = c("Bajo","Alto"))

#COMPORTAMIENTO PROSOCIAL

# Guardar datos -----------------------------------------------------------

elsoc_2016 <- filter(base_long,ola==2016,!is.na(ponderador01))
elsoc_2017 <- filter(base_long,ola==2017,!is.na(ponderador01))
elsoc_2018 <- filter(base_long,ola==2018,!is.na(ponderador01))
elsoc_2019 <- filter(base_long,ola==2019,!is.na(ponderador01))

muestra_1 <- base_long%>%
  filter(tipo_atricion==1,!is.na(ponderador01))
base_long<- base_long%>%
            filter(!is.na(ponderador01))



var_panel <- function(var,sub=1,repe=4){
  var_tidy<- as.name(var)
  muestra_1%>%
    rename(variable=!!var_tidy)%>%
    group_by(idencuesta)%>%
    summarise(variable= variable[sub])%>%
    pull(variable)%>%
    rep(., each=repe)
} 

#FUMADOR PARA LONG
muestra_1$s08_l<-var_panel("s08_fac")

# INDICE DE MASA CORPORAL
imc <-muestra_1%>%
  group_by(idencuesta)%>%
  summarise(imc =mean(s_imc,na.rm=TRUE))

imc$imc[is.nan(imc$imc)]<- NA

muestra_1$s_imc_l <- rep(imc$imc,each=4)

# CANTIDAD DE EVENTOS ESTRESORES

estresores <- muestra_1%>%
  group_by(idencuesta)%>%
  summarise(estresores=mean(s13_estresores,na.rm=TRUE))

estresores$estresores[is.nan(estresores$estresores)] <- NA

muestra_1$s13_estresores_l <- rep(estresores$estresores,each=4)

#FRECUENCIA DE ACTIVIDAD FISICA
muestra_1$s04_l<-var_panel("s04_fac")

# CONSUMO DE ALCOHOL
muestra_1$s09_l <- var_panel("s09")

# DIAGNOSITICO DE ALGUNA ENFERMEDAD
muestra_1$s19_l <- var_panel("s19_diag_fac",sub=2)

# TIPO DE PREVISION SOCIAL
muestra_1$s18_l <- var_panel("s18_fac",sub = 2)

# FRECUENCIA PARA HABLAR PROBLEMAS
muestra_1$s12_l <- var_panel("s12_fac")

# CANTIDAD DE AMIGOS CERCANOS
muestra_1$r15_l <- as.numeric(var_panel("r15_fac",sub = 2))

saveRDS(base_long,"DATOS/base_long_reco.RDS")
saveRDS(muestra_1,"DATOS/muestra_1.RDS")
saveRDS(elsoc_2016,"DATOS/elsoc_16.RDS")
saveRDS(elsoc_2017,"DATOS/elsoc_17.RDS")
saveRDS(elsoc_2018,"DATOS/elsoc_18.RDS")
saveRDS(elsoc_2019,"DATOS/elsoc_19.RDS")
rm(frame_atributos)