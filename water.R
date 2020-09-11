library(mpmi)
library(corrplot)
library(reshape2)
library(ggplot2)
library(RColorBrewer) 


SDGsCities <- read.csv("/home/wattie/water-studio/data/tlacotepec_mim.csv")
write.csv(SDGsCities,file="tlacotepec_mim.csv")

####  Calculating Mutual information ###

MISDGsCities <- cmi(SDGsCities,na.rm = TRUE)

###

MISDGsCitiesValues <- MISDGsCities$bcmi
write.csv(MISDGsCitiesValues, file = "MISDGsCitiesValues.csv")


mp(MISDGsCitiesValues)

maxVal <- max(MISDGsCitiesValues)

NormMISDGsCitiesValues <- MISDGsCitiesValues/maxVal
write.csv(NormMISDGsCitiesValues, file = "NormMISDGsCitiesValues.csv")


corrplot(NormMISDGsCitiesValues, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

corrplot(NormMISDGsCitiesValues, 
         method="circle",
         col=brewer.pal(n=10, name="PuOr"),
         # Mostrar sólo la diagonal superior
         type="lower", 
         #color, tamaño y rotación de las etiquetas
         tl.col="red",
         tl.cex = 0.9, 
         tl.srt=0, 
         # no visualizar la diagonal,
         # (var con respecto a sí misma)
         diag=FALSE, 
         # aceptar cualquier matriz, mic en este caso
         #(no es un elemento de correlación)
         is.corr = T 
         
)



corrplot(NormMISDGsCitiesValues, 
         method="color",
         type="lower", 
         number.cex=0.6,
         # Agregar coeficiente de correlación
         addCoef.col = "black", 
         tl.col="red", 
         tl.srt=0, 
         tl.cex = 0.6,
         diag=FALSE, 
         is.corr = T 
)

cross_plot(NormMISDGsCitiesValues, input = "COLI_FEC", target = "N_NO3", plot_type = "percentual")


####

MISDGsCitiesZscore <- MISDGsCities$zvalues
write.csv(MISDGsCitiesZscore, file = "MISDGsCitiesZscores.csv")


mp(MISDGsCitiesZscore)

###################
library(funModeling)

SDGsCitiesFun <- read.csv("/home/wattie/water-studio/data/tlacotepec_mim.csv")


#SDGsCitiesFun[is.na(SDGsCitiesFun)] <- 0
length(SDGsCitiesFun)
df_status(SDGsCitiesFun)
di=data_integrity(SDGsCitiesFun)
di
summary(di)
plot_num(SDGsCitiesFun)
#correlation_table(SDGsCitiesFun, "has_SDGsCitiesFun")
variable_importance =  var_rank_info(SDGsCitiesFun, "TEMP_AGUA")
#describe(heart_disease$has_heart_disease)
#SDGsCitiesFun$COLI_TOT
variable_importance
ggplot(variable_importance, aes(x = reorder(var, gr), y = gr, fill = var)) + geom_bar(stat = "identity") + coord_flip() + theme_bw() + xlab("") + ylab("Variable Importance (based on Information Gain)") +   guides(fill = FALSE)



x = c('SAAM','OD_mg/L','COLI_TOT','pH_CAMPO','TEMP_AGUA','NI_TOT','E_COLI','HG_TOT','PB_TOT','CD_TOT','CR_TOT','AS_TOT','TURBIEDAD','SST','COLOR_VER','DUR_TOT','N_TOT','COLI_FEC','PO4_TOT','N_NH3','N_NO2','N_NO3')
#x
#count <- 0
 
for (i in seq_along(x)) {
  z[i] <- gsub(" ", "", paste('var_',x[i]))
  #z[i] = var_rank_info(SDGsCities,  x[i])
}

#z<-NULL;
variable_importance = data.frame()
#p <-matrix(length(z),)
for (j in seq_along(z)) {
  #z[i] <- gsub(" ", "", paste('var_',x[i]))
  variable_importance = var_rank_info(SDGsCities,  x[j])
  z[j] <- variable_importance
}
x[1]

for (i in seq_along(x)) {
  #z[i] <- gsub(" ", "", paste('var_',x[i]))
  #z[i] = var_rank_info(SDGsCities,  x[i])
  
  ggplot(var_rank_info(SDGsCities,  "SAAM"), aes(x = reorder(var, gr),y = gr, fill = var)) + geom_bar(stat = "identity") + coord_flip() + theme_bw() +     xlab("") + ylab("Variable Importance (based on Information Gain)"  ) + guides(fill = FALSE)

  
}



