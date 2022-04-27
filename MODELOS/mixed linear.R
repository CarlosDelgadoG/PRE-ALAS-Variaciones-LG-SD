
# Mixto Multivariable -----------------------------------------------------

mlm_multi <- gen_mod(var_pred$long, 'LMM')
saveRDS(mlm_multi,file = 'MODELOS/mlm_multi.RDS')



# IC Para LMM -------------------------------------------------------------

# IC Para LMM -------------------------------------------------------------

tabla_IC <- confint(mlm_multi)
saveRDS(tabla_IC,file = 'MODELOS/mlm_tabla_ic.RDS')






# INTERACIONES LM ---------------------------------------------------------

lista_mmi <- lapply(var_pred$long, function(i){gen_mod(var_pred$long,'LMM',i)})

for(i in 1:length(var_pred$long)){
  saveRDS(lista_mmi[[i]],paste('MODELOS/mlm_multi_i',var_pred$long[i],'.RDS',sep=''))  
}



# Recuperar nombres -------------------------------------------------------

modelos_archivos <-list.files('MODELOS',pattern = '.RDS')

nombres_mmi<-  modelos_archivos[str_which(modelos_archivos,'mlm_multi_i')]

vars_mmi<-  str_replace(nombres_mmi, '\\.RDS','')%>%
  str_replace('mlm_multi_i','')


# Cargar archivos ---------------------------------------------------------

modelos <- lapply(paste('MODELOS/',nombres_mmi,sep = ''), function(i){readRDS(i)})
names(modelos) <- vars_mmi


# Computar IC Para modelos con interacciones ------------------------------

ic_modelos <- lapply(1:length(modelos), function(i){confint(modelos[[i]])})


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