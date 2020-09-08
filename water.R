library(mpmi)
library(corrplot)
library(reshape2)
library(ggplot2)

SDGsCities <- read.csv("/home/wattie/water-studio/data/metztitlan_2_mim.csv")
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

####

MISDGsCitiesZscore <- MISDGsCities$zvalues
write.csv(MISDGsCitiesZscore, file = "MISDGsCitiesZscores.csv")


mp(MISDGsCitiesZscore)


