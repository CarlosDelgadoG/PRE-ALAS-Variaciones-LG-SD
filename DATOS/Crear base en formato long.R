library(tidyverse)
load("ELSOC_2016_2019.RData")

long<- pivot_longer(elsoc_wide_2016_2019,!idencuesta:cuestion_mig, names_to = c(".value","ola"), names_pattern = '(.*?)(_w\\d+)',values_drop_na = TRUE )
long$ola <- car::recode(long$ola,"'_w01'=2016;'_w02'=2017;'_w03'=2018;'_w04'=2019",as.factor = TRUE)
long<- pivot_longer(elsoc_wide_2016_2019,!idencuesta:cuestion_mig, names_to = c(".value","ola"), names_pattern = '(.*?)(_w\\d+)',values_drop_na = TRUE )

#SACAR ATRIBUTOS
long<- bind_cols(lapply(long, function(x) { attributes(x) <- NULL; x }))

long$ola <- car::recode(long$ola,"'_w01'=2016;'_w02'=2017;'_w03'=2018;'_w04'=2019",as.factor = TRUE)
long<- sjmisc::set_na(long,na=c(-888,-999))

saveRDS(long,"base_long.RDS")