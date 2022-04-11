 grafo_phq_bin<- function(base,var){
  
   var_tidy <- as.name(var) 

datos <-   base%>%
  group_by(!!var_tidy,s11_phq9_bin)%>%
  summarise(n=n()) %>%
  drop_na()%>%
  mutate(prop=round(100*n/sum(n),1),
         perc=paste(prop,"%")) 
  
ggp<-   ggplot(datos,aes(x=!!var_tidy,y=prop,fill=factor(s11_phq9_bin)))+
  labs(y="Proporción",
      x=attr(getElement(base,var),"label"),
       fill="Puntaje PHQ-9")+
  geom_col()+
  geom_text(aes(label=perc),color = "black",position = position_stack(vjust = .5))+
  scale_fill_discrete(labels=c("Sín síntomas","Con síntomas"))+
  theme_classic()+
  theme(legend.position = "top")
  
return(ggp)
 }
 
 
 # GRAFO PHQ ---------------------------------------------------------------
 grafo_phq <- 
   function(base,vars,etiq){
   
   to_long <- function(var){
     var_tidy <- as.name(var)
     base <- select(base, !!var_tidy)
     base <- mutate(base,
                    "valor"=rep(var,nrow(base)))
     colnames(base)<- c("valor","variable")
     base$valor <- car::recode(base$valor, "1=0;c(2,3,4)=1")
     base$valor <- factor(base$valor,
                          levels = c(0,1),
                          labels=c('Sin Síntoma','Con Síntoma'))
     tabla <-  base%>%   
       group_by(valor)%>%
       summarise(count=n())%>%
       drop_na()%>%
       mutate(prop=round(100*count/sum(count),1),
              perc=paste(prop,"%"),
              variable=factor(rep(var,nrow(.))))
     return(tabla)
   }
   
   nombres.facet<- function(viejos, nuevos){
     var.labs <- nuevos
     names(var.labs) <-viejos
     return(var.labs)
   }
   
   
   base_grafo<- bind_rows(lapply(vars, to_long))
   
   ggp<- ggplot(base_grafo,aes(x=valor,y=prop,fill=valor))+
     geom_col()+
     geom_text(aes(label=perc,vjust=-.5),color = "black",position=position_dodge())+
     labs(y="Proporción",
          x=element_blank())+
     
     scale_x_discrete(labels=stringr::str_wrap(levels(base_grafo$valor), width =7),na.translate=FALSE)+
     facet_wrap(~variable,
                labeller = labeller(variable=nombres.facet(viejos = vars,
                                                           nuevos = etiq)))+
     scale_y_continuous(labels = scales::percent_format(scale=1))+
     theme_classic()+
     theme(legend.position = "none")+
     scale_fill_manual(labels=c("Sin Síntoma", "Con Síntoma"),values = c("cornflowerblue","firebrick1"))
   return(ggp)
 }
 

 grafo_olas<- function(base,var,up=6){
   
   var_tidy <- as.name(var) 
   
   datos <-   base%>%
     group_by(ola,!!var_tidy,s11_phq9_bin)%>%
     summarise(n=n()) %>%
     drop_na()%>%
     mutate(prop=round(100*n/sum(n),1),
            perc=as.character(prop)) 
   
  y_max <- datos$prop[which.max(datos$prop)]
  
   ggp<-   ggplot(datos,aes(x=ola,y=prop,fill=factor(s11_phq9_bin)))+
     labs(y="Proporción",
          x=attr(getElement(base,var),"label"),
          fill="Puntaje PHQ-9")+
     geom_col(position="dodge",alpha=.7)+
     theme_classic()+
     theme(legend.position = "top")+
     facet_wrap(as.formula(paste("~ ",var)))+
     geom_text(aes(label=perc),color = "black",position = position_dodge(width = 0.9), vjust = -1,size=3)+
     scale_fill_manual(labels=c("Sin Síntomas", "Con Síntomas"),values = c("cornflowerblue","firebrick1"))+
     scale_y_continuous(labels = scales::percent_format(accuracy = 1,scale = 1),limits = c(0,y_max+up))
   return(ggp)  
 }
 
 grafo_box <- function(var){
   var_tidy <- as.name(var)
   muestra_1%>%
     select(ola,!!var_tidy,s11_phq9_bin)%>%
     drop_na()%>%
     ggplot(aes(x=ola,y=!!var_tidy,color=factor(s11_phq9_bin)))+
     geom_boxplot()+
     labs(x="Año de estudio",
          y=attr(getElement(base_long,var),"label"),
          color=element_blank())+
     scale_color_manual(labels=c("Sin Síntoma", "Con Síntoma"),values = c("cornflowerblue","firebrick1"))+
     theme_classic()+
     theme(legend.position = "top")
   
   
 }
 
 grafo_mediacion <- function(base,var,var_med){
   var_tidy <- as.name(var) 
   var_med_tidy <- as.name(var_med)
   datos <-   muestra_1%>%
     group_by(ola,!!var_tidy,!!var_med_tidy,s11_phq9_bin)%>%
     summarise(n=n()) %>%
     drop_na()%>%
     mutate(prop=round(100*n/sum(n),1),
            perc=as.character(prop))%>%
     filter(s11_phq9_bin==1)
   y_max <- datos$prop[which.max(datos$prop)]
   
   ggp <-   ggplot(datos,aes(x=ola,y=prop,color=!!var_med_tidy,group=!!var_med_tidy))+
     geom_line()+
     geom_point()+
     facet_wrap(as.formula(paste("~ ",var)))+
     labs(y="Proporción",
          x="Año de estudio",
          color=attr(getElement(base,var_med),"label"))+
     scale_color_brewer(type = 'qual', palette = 4, direction = 1)+
     theme_classic()+
     theme(legend.position = "top")+
     scale_y_continuous(labels = scales::percent_format(accuracy = 1,scale = 1))  
   return(ggp)
 }
 
 

# Significancia univariada ------------------------------------------------

 sig.univariada <- function(base,var){
   var.md <- as.name(var)
   if(is.factor(getElement(base_modelo,var))){
     tabla <- tibble("Variable"=var,
                     "Etiqueta"=attr(getElement(base_modelo,var),"label"),
                     "p.value"=round(as.numeric(glance(lm(aov(s11_phq9~!!var.md,data = base)))$p.value),3),
                     "Devianza"=round(deviance(lm(aov(s11_phq9~!!var.md,data = base)))),
                     "Función"="AOV")
   }else{
     tabla <- tibble("Variable"=var,
                     "Etiqueta"=attr(getElement(base_modelo,var),"label"),
                     "p.value"=round(as.numeric(glance(lm(s11_phq9~!!var.md,data = base))$p.value),3),
                     "Devianza"=round(deviance(lm(s11_phq9~!!var.md,data = base))),
                     "Función"="LM")
   }
   
   return(tabla)
   
 }
 
 grafo_media <- function(var){
   var_tidy<- as.name(var)
   muestra_1%>%
     group_by(ola,!!var_tidy)%>%
     summarise(media=mean(s11_phq9,na.rm=TRUE))%>%
     drop_na()%>%
     ggplot(aes(x=ola,y=media,group=!!var_tidy,color=!!var_tidy))+
     geom_point()+
     geom_line()+
     labs(y="Promedio puntaje PHQ-9",
          x="Año de Estudio")+
     facet_wrap(as.formula(paste("~",var)))+
     theme_classic()+
     theme(legend.position = "none")+ 
     scale_color_viridis(discrete = TRUE,option = "D")
 }
 
 datos_long <- function(var){
   var_tidy <- as.name(var)
   
   
   
   
   datos <- muestra_1 %>%
     mutate(s11_i= factor(car::recode(!!var_tidy, "0='Nunca';1='Algunos Días';
                            2='Más de la mitad de los días';
                            3='Casi todos los días'"),
                          levels = c('Nunca',
                                     'Algunos Días',
                                     'Más de la mitad de los días',
                                     'Casi todos los días')))%>%
     group_by(ola,s11_i)%>%
     summarise(n=n())%>%
     drop_na()%>%
     mutate(prop=n/sum(n),
            perc=as.character(round(prop*100,1)))
   datos$variable <- rep(var,16)
   return(datos)
 }
 
 grafo_long <- function(vars,etiq){
   
   #FUNCION PARA LAS ETIQUETAS DEL FACET
   nombres.facet<- function(viejos, nuevos){
     var.labs <- nuevos
     names(var.labs) <-viejos
     return(var.labs)
   }
   
   datos_plot<- bind_rows(lapply(vars, datos_long)) 
   ggplot(datos_plot,aes(x=s11_i, y=prop,fill=ola))+
     geom_col(position = "dodge2")+
     labs(x="Frecuencia síntomas",
          y="Proporción",
          fill=element_blank())+
     facet_wrap(~variable,
                labeller = labeller(variable=nombres.facet(viejos = vars,
                                                           nuevos = etiq)))+
     theme_classic()+
     theme(legend.position = "top")+
     scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
     scale_fill_viridis(discrete = TRUE,option = "D")+
     scale_x_discrete(labels=str_wrap(c(attr(getElement(datos_plot,"s11_i"),"levels")),7))
 }
 
 grafo_var <- function(var,up=10){
   
   var_tidy <- as.name(var)
   datos <- muestra_1%>%
     group_by(ola, !!var_tidy)%>%
     summarise(n=n())%>%
     mutate(prop=100*n/sum(n),
            lab=as.character(round(prop,1)))%>%
     ungroup()%>%
     filter(!prop==100)
   
   datos[,var] <- factor(getElement(datos,var), 
                         levels = c(levels(getElement(datos,var)), NA), 
                         labels = c(levels(getElement(datos,var)), "NS/NR"), 
                         exclude = NULL)
   
   
   
   y_max <- datos$prop[which.max(datos$prop)]
   
   ggp<-  ggplot(datos,aes(x=!!var_tidy,y=prop))+
     geom_col(fill="cornflowerblue")+
     geom_text(aes(label=lab,vjust=-.5),color = "black")+
     labs(x= attr(getElement(muestra_1,var),"label"),
          y="Frecuencia")+
     facet_wrap(~ola)+
     theme_classic()+
     scale_y_continuous(limits = c(0,y_max+up),
                        labels = scales::percent_format(accuracy = 1,scale = 1))+
     scale_x_discrete(labels=str_wrap(c(attr(getElement(datos,var),"levels")),7))
   return(ggp)
 }
 
 
 gen_mod <- function(var,mod){
   if(mod == 'LG'){
     mod <- glm(as.formula(paste("s11_phq9_bin",var,sep='~')),family = binomial,data=base_modelos)  
   }
   if(mod=='BIN'){
     mod <- glm(as.formula(paste("cbind(s11_phq9,27-s11_phq9)",var,sep='~')),family = binomial,data=base_modelos)       
   }
   if(mod=='PROPM'){
     mod <- MASS::polr(as.formula(paste("factor(s11_phq9)",var,sep='~')),data = base_modelos)
   }
   return(mod)
 }
 
 
 
 