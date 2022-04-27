library(tidyverse)
library(readxl)
library(lme4)


# CARGAR DATOS ------------------------------------------------------------
muestra_1 <- readRDS("DATOS/muestra_1.RDS")
var_pred <- read_excel("MODELOS/variables predictoras.xlsx")
bm_long <- select(muestra_1, idencuesta,ola_num,s11_phq9,s11_phq9_bin, !!var_pred$long) 

source("FUNCIONES/FUNCIONES.R", encoding = 'UTF-8')


# Crear logistica mixta multivariada --------------------------------------


mlg_multi <- gen_mod(var_pred$long, 'MLG')
saveRDS(mlg_multi, file='MODELOS/mlg_multi.RDS')

# COMPUTAR MODELOS --------------------------------------------------------

lista_mlgi <- lapply(var_pred$long, function(i){gen_mod(var_pred$long,'MLG',i)})


# GUARDAR MODELOS ---------------------------------------------------------


for(i in 1:length(var_pred$long)){
  saveRDS(lista_mlgi[[i]],paste('MODELOS/mlg_multi_i',var_pred$long[i],'.RDS',sep=''))  
}

resumenes_mlg <- lapply(1:length(lista_mlgi), function(i){summary(lista_mlgi[[i]])})

saveRDS(resumenes_mlg,file='MODELOS/resumenes_mlginter.RDS')



# Obtener interacciones ---------------------------------------------------
get_interlg <- function(i){
  nm_coef <- dimnames(resumenes_mlg[[i]]$coefficients)[[1]]
  vc_sub <- grepl(":ola_num",nm_coef)
  
  valores_summary <-as.tibble(resumenes_mlg[[i]]$coefficients)[vc_sub,]
  tabla<- tibble('nombres_coef'= nm_coef[vc_sub],
                 'valor'=unlist(valores_summary[,1]),
                 'error'=unlist(valores_summary[,2]),
                 'Z'=unlist(valores_summary[,3]),
                 'valor_p'=unlist(valores_summary[,4]))
  
  return(tabla)
}


inter_mlgi <- bind_rows(lapply(1:length(resumenes_mlg),function(i){get_interlg(i)}))%>%
  mutate('sig'=valor_p<.1)

saveRDS(inter_mlgi,'MODELOS/inter_mlgi.RDS')

