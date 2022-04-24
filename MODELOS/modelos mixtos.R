bm_long <- select(muestra_1, idencuesta,ola_num,s11_phq9,s11_phq9_bin, !!var_pred$long)

mlm_multi <- gen_mod(var_pred$long, 'LMM')
saveRDS(mlm_multi,file = 'MODELOS/mlm_multi.RDS')


mlg_multi <- gen_mod(var_pred$long, 'MLG')
saveRDS(mlg_multi, file='MODELOS/mlg_multi.RDS')