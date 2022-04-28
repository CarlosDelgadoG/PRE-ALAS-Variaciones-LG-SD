library(tidyverse)
modelos_archivos <-list.files('MODELOS',pattern = '.RDS')

nombres_mmi<-  modelos_archivos[str_which(modelos_archivos,'mlm_multi_i')]

vars_mmi<-  str_replace(nombres_mmi, '\\.RDS','')%>%
  str_replace('mlm_multi_i','')




# Cargar archivos ---------------------------------------------------------

modelos <- lapply(paste('MODELOS/',nombres_mmi,sep = ''), function(i){readRDS(i)})
names(modelos) <- vars_mmi


# Computar IC Para modelos con interacciones ------------------------------

ic_modelos <- lapply(1:length(modelos), function(i){confint(modelos[[i]])})

saveRDS(ic_modelos,file='MODELOS/ic_modelos_mlm.RDS')

# Tabla con todas las interacciones ---------------------------------------


get_inter <- function(i){
  tabla <- tibble('coef'=dimnames(ic_modelos[[i]])[[1]][-c(1:(length(fixef(mlm_multi))+2))],
                  'L'=ic_modelos[[i]][-c(1:(length(fixef(mlm_multi))+2)),1],
                  'U'=ic_modelos[[i]][-c(1:(length(fixef(mlm_multi))+2)),2])%>%
    mutate('var'= rep(names(modelos)[[i]]))
  return(tabla)  
}

#Crear tabla
interacciones <-bind_rows(lapply(1:length(ic_modelos),function(i){get_inter(i)}))

saveRDS(interacciones,file='MODELOS/inter_mlm.RDS')