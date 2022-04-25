#Carregar pacotes necessarios
library(ithir)
library(aqp)
library(spatstat)
library(maptools)
library(tripack)
library(matrixStats)
library(raster)
library(sf)
library(Cubist)
require(caret)
require(parallel)
require(doParallel)
require(Cubist)

#Selecionar area de trabalho e listar arquivos disponiveis
setwd("C:/Users/Neli Quiñonez/Google Drive/GEOCIS/IC/Merylin/dados_2nd_revision")
list.files()
citation()
#Carregar dados e dividir em 75% e 25%
cor.A <- read.csv("cor_PIRA_A_withSYSIFused_102230.csv", sep=",")
train.ind <- createDataPartition(1:nrow(cor.A), p = .75, list = FALSE)
train.cor <- cor.A[ train.ind,] 
test.cor <- cor.A[-train.ind,]

write.csv(train.cor, "train_dados_cor.csv")
write.csv(test.cor, "test_dados_cor.csv")

train.cor <- read.csv("train_dados_cor.csv")
test.cor <- read.csv("test_dados_cor.csv")



#Como eu ja tinha dividido e salvo no computador, carregar aqui
#Treinamento
points <- read.csv("train_dadosTESEdefinitivo.csv", sep=",")
head(points)

#Validacao
val <- read.csv("test_dadosTESEdefinitivo.csv", sep=",")
head(val)

#Separar dos dados as colunas correspondentes a cada covariavel tanto para treinamento 
#como para validacao. Se nao quiser dividir assim, pode chamar da tabela oriniginal com os []

Combined <- points[,12:17]
SYSIS2MSI <- cbind(points[,18:20], points[,24:26])
SYSIS2MSI9 <- points[,18:26]
head(SYSIS2MSI9)
SYSIL8OLI <- points[,27:32]
head(SYSIL8OLI)

Combinedtest <- val[,12:17]
head(Combinedtest)
SYSIS2MSItest <- cbind(val[,18:20], val[,24:26])
head(SYSIS2MSItest)
SYSIS2MSI9test <- val[,18:26]
head(SYSIS2MSI9test)
SYSIL8OLItest <- val[,27:32]
head(L8OLItest)


Combined.cor <- train.cor[,10:15]
SYSIS2MSI.cor <- cbind(train.cor[,16:18], train.cor[,22:24])
SYSIS2MSI9.cor <- train.cor[,16:24]
head(SYSIS2MSI9.cor)
SYSIL8OLI.cor <- train.cor[,25:30]
head(SYSIL8OLI.cor)

Combinedtest.cor <- test.cor[,10:15]
head(Combinedtest.cor)
SYSIS2MSItest.cor <- cbind(test.cor[,16:18], test.cor[,22:24])
head(SYSIS2MSItest.cor)
SYSIS2MSI9test.cor <- test.cor[,16:24]
head(SYSIS2MSI9test.cor)
SYSIL8OLItest.cor <- test.cor[,25:30]
head(SYSIL8OLItest.cor)


#Carregar os rasters das covariaveis que serao utilizados na predicao

load(file = "covsSYSIS2MSI.RData")
load(file = "covsSYSIL8.RData")
load(file = "covsSYSICombined.RData")

# Fit para o treinamento de argila utilizando o Cubist 
# SYSI combined
##Clay, Sand, SIlt, OM
{
  clay_Combined <- train(x = Combined,
                         y = points$Clay.gkg,
                         method = "cubist")

clay_val_combined <- predict(clay_Combined,
                             newdata = Combinedtest)
goof(val$Clay.gkg, clay_val_combined)

clay_Combined_geo <- train(x = Combined,
                       y = points$clay_geo,
                       method = "cubist")
clay_val_combined_geo <- predict(clay_Combined_geo,
                                 newdata = Combinedtest)
goof(val$clay_geo, clay_val_combined_geo)

#Sand Combined
sand_Combined <- train(x = Combined,
                       y = points$Sand.gkg,
                       method = "cubist")
sand_val_combined <- predict(sand_Combined,
                             newdata = Combinedtest)
goof(val$Sand.gkg, sand_val_combined)

sand_Combined_geo <- train(x = Combined,
                       y = points$sand_geo,
                       method = "cubist")

sand_val_combined_geo <- predict(sand_Combined_geo,
                                 newdata = Combinedtest)

goof(val$Sand.gkg, sand_val_combined_geo)

#Silt Combined
silt_Combined <- train(x = Combined,
                       y = points$Silt.gkg,
                       method = "cubist")
silt_val_combined <- predict(silt_Combined,
                             newdata = Combinedtest)

goof(val$Silt.gkg, silt_val_combined)

silt_Combined_geo <- train(x = Combined,
                           y = points$silt_geo,
                           method = "cubist")

silt_val_combined_geo <- predict(silt_Combined_geo,
                                 newdata = Combinedtest)

goof(val$Silt.gkg, silt_val_combined_geo)


om_Combined <- train(x = Combined,
                       y = points$OM.gkg,
                       method = "cubist")

om_val_combined <- predict(om_Combined,
                             newdata = Combinedtest)

goof(val$OM.gkg, om_val_combined)

om_Combined_geo <- train(x = Combined,
                           y = points$mo_geo,
                           method = "cubist")

om_val_combined_geo <- predict(om_Combined_geo,
                           newdata = Combinedtest)

goof(val$OM.gkg, om_val_combined_geo)
}

#Color combined
#Hue
{
  hue_Combined <- train(x = Combined.cor,
                        y = train.cor$Hue,
                        method = "cubist")
  
  hue_val_combined <- predict(hue_Combined,
                               newdata = Combinedtest.cor)
  goof(test.cor$Hue, hue_val_combined)
  
  
  #value
 value_Combined <- train(x = Combined.cor,
                         y = train.cor$value,
                         method = "cubist")
  value_val_combined <- predict(value_Combined,
                               newdata = Combinedtest.cor)
  goof(test.cor$value, value_val_combined)

  
  #chroma
  chroma_Combined <- train(x = Combined.cor,
                         y = train.cor$chroma,
                         method = "cubist")
  chroma_val_combined <- predict(chroma_Combined,
                               newdata = Combinedtest.cor)
  
  goof(test.cor$chroma, chroma_val_combined)
  
}

{#Hue
  hue_Combined3 <- train(x = Combined.cor[,1:3],
                        y = train.cor$Hue,
                        method = "cubist")
  
  hue_val_combined3 <- predict(hue_Combined3,
                              newdata = Combinedtest.cor[,1:3])
  goof(test.cor$Hue, hue_val_combined3)
  
  
  #value
  value_Combined3 <- train(x = Combined.cor[,1:3],
                          y = train.cor$value,
                          method = "cubist"
                          )
  value_val_combined3 <- predict(value_Combined3,
                                newdata = Combinedtest.cor[,1:3])
  goof(test.cor$value, value_val_combined3)
  
  
  #chroma
  chroma_Combined3 <- train(x = Combined.cor[,1:3],
                           y = train.cor$chroma,
                           method = "cubist",
                           )
  chroma_val_combined3 <- predict(chroma_Combined3,
                                 newdata = Combinedtest.cor[,1:3])
  
  goof(test.cor$chroma, chroma_val_combined3)
  
}

##SYSI Landsat
{
  clay_SYSIL8OLI <- train(x = SYSIL8OLI,
                        y = points$Clay.gkg,
                        method = "cubist",
                        )
  
  clay_val_SYSIL8OLI <- predict(clay_SYSIL8OLI,
                               newdata = SYSIL8OLItest)
  goof(val$Clay.gkg, clay_val_SYSIL8OLI)
  
  clay_SYSIL8OLI_geo <- train(x = SYSIL8OLI,
                             y = points$clay_geo,
                             method = "cubist",
                             )
  clay_val_SYSIL8OLI_geo <- predict(clay_SYSIL8OLI_geo,
                                   newdata = SYSIL8OLItest)
  goof(val$clay_geo, clay_val_SYSIL8OLI_geo)
  
  #Sand SYSIL8OLI
  sand_SYSIL8OLI <- train(x = SYSIL8OLI,
                         y = points$Sand.gkg,
                         method = "cubist",
                         )
  sand_val_SYSIL8OLI <- predict(sand_SYSIL8OLI,
                               newdata = SYSIL8OLItest)
  goof(val$Sand.gkg, sand_val_SYSIL8OLI)
  
  sand_SYSIL8OLI_geo <- train(x = SYSIL8OLI,
                             y = points$sand_geo,
                             method = "cubist",
                             )
  
  sand_val_SYSIL8OLI_geo <- predict(sand_SYSIL8OLI_geo,
                                   newdata = SYSIL8OLItest)
  
  goof(val$Sand.gkg, sand_val_SYSIL8OLI_geo)
  
  #Silt SYSIL8OLI
  silt_SYSIL8OLI <- train(x = SYSIL8OLI,
                         y = points$Silt.gkg,
                         method = "cubist",
                         )
  silt_val_SYSIL8OLI <- predict(silt_SYSIL8OLI,
                               newdata = SYSIL8OLItest)
  
  goof(val$Silt.gkg, silt_val_SYSIL8OLI)
  
  silt_SYSIL8OLI_geo <- train(x = SYSIL8OLI,
                             y = points$silt_geo,
                             method = "cubist",
                             )
  
  silt_val_SYSIL8OLI_geo <- predict(silt_SYSIL8OLI_geo,
                                   newdata = SYSIL8OLItest)
  
  goof(val$Silt.gkg, silt_val_SYSIL8OLI_geo)
  
  
  om_SYSIL8OLI <- train(x = SYSIL8OLI,
                       y = points$OM.gkg,
                       method = "cubist",
                       )
  
  om_val_SYSIL8OLI <- predict(om_SYSIL8OLI,
                             newdata = SYSIL8OLItest)
  
  goof(val$OM.gkg, om_val_SYSIL8OLI)
  
  om_SYSIL8OLI_geo <- train(x = SYSIL8OLI,
                           y = points$mo_geo,
                           method = "cubist",
                           )
  
  om_val_SYSIL8OLI_geo <- predict(om_SYSIL8OLI_geo,
                                 newdata = SYSIL8OLItest)
  
  goof(val$OM.gkg, om_val_SYSIL8OLI_geo)
}

#Color SYSI Landsat
{#Hue
  hue_SYSIL8OLI <- train(x = SYSIL8OLI.cor,
                        y = train.cor$Hue,
                        method = "cubist",
                        )
  
  hue_val_SYSIL8OLI <- predict(hue_SYSIL8OLI,
                              newdata = SYSIL8OLItest.cor)
  goof(test.cor$Hue, hue_val_SYSIL8OLI)
  
  
  #value
  value_SYSIL8OLI <- train(x = SYSIL8OLI.cor,
                          y = train.cor$value,
                          method = "cubist",
                          )
  value_val_SYSIL8OLI <- predict(value_SYSIL8OLI,
                                newdata = SYSIL8OLItest.cor)
  goof(test.cor$value, value_val_SYSIL8OLI)
  
  
  #chroma
  chroma_SYSIL8OLI <- train(x = SYSIL8OLI.cor,
                           y = train.cor$chroma,
                           method = "cubist",
                           )
  chroma_val_SYSIL8OLI <- predict(chroma_SYSIL8OLI,
                                 newdata = SYSIL8OLItest.cor)
  
  goof(test.cor$chroma, chroma_val_SYSIL8OLI)
  
}

{#Hue
  hue_SYSIL8OLI3 <- train(x = SYSIL8OLI.cor[,1:3],
                         y = train.cor$Hue,
                         method = "cubist",
                         )
  
  hue_val_SYSIL8OLI3 <- predict(hue_SYSIL8OLI3,
                               newdata = SYSIL8OLItest.cor[,1:3])
  goof(test.cor$Hue, hue_val_SYSIL8OLI3)
  
  
  #value
  value_SYSIL8OLI3 <- train(x = SYSIL8OLI.cor[,1:3],
                           y = train.cor$value,
                           method = "cubist",
                           )
  value_val_SYSIL8OLI3 <- predict(value_SYSIL8OLI3,
                                 newdata = SYSIL8OLItest.cor[,1:3])
  goof(test.cor$value, value_val_SYSIL8OLI3)
  
  
  #chroma
  chroma_SYSIL8OLI3 <- train(x = SYSIL8OLI.cor[,1:3],
                            y = train.cor$chroma,
                            method = "cubist",
                            )
  chroma_val_SYSIL8OLI3 <- predict(chroma_SYSIL8OLI3,
                                  newdata = SYSIL8OLItest.cor[,1:3])
  
  goof(test.cor$chroma, chroma_val_SYSIL8OLI3)
  
}
#SYSI S2 MSI
{
  clay_SYSIS2MSI <- train(x = SYSIS2MSI,
                          y = points$Clay.gkg,
                          method = "cubist",
                          )
  
  clay_val_SYSIS2MSI <- predict(clay_SYSIS2MSI,
                                newdata = SYSIS2MSItest)
  goof(val$Clay.gkg, clay_val_SYSIS2MSI)
  
  clay_SYSIS2MSI_geo <- train(x = SYSIS2MSI,
                              y = points$clay_geo,
                              method = "cubist",
                              )
  clay_val_SYSIS2MSI_geo <- predict(clay_SYSIS2MSI_geo,
                                    newdata = SYSIS2MSItest)
  goof(val$clay_geo, clay_val_SYSIS2MSI_geo)
  
  #Sand SYSIS2MSI
  sand_SYSIS2MSI <- train(x = SYSIS2MSI,
                          y = points$Sand.gkg,
                          method = "cubist",
                          )
  sand_val_SYSIS2MSI <- predict(sand_SYSIS2MSI,
                                newdata = SYSIS2MSItest)
  goof(val$Sand.gkg, sand_val_SYSIS2MSI)
  
  sand_SYSIS2MSI_geo <- train(x = SYSIS2MSI,
                              y = points$sand_geo,
                              method = "cubist",
                              )
  
  sand_val_SYSIS2MSI_geo <- predict(sand_SYSIS2MSI_geo,
                                    newdata = SYSIS2MSItest)
  
  goof(val$Sand.gkg, sand_val_SYSIS2MSI_geo)
  
  #Silt SYSIS2MSI
  silt_SYSIS2MSI <- train(x = SYSIS2MSI,
                          y = points$Silt.gkg,
                          method = "cubist",
                          )
  silt_val_SYSIS2MSI <- predict(silt_SYSIS2MSI,
                                newdata = SYSIS2MSItest)
  
  goof(val$Silt.gkg, silt_val_SYSIS2MSI)
  
  silt_SYSIS2MSI_geo <- train(x = SYSIS2MSI,
                              y = points$silt_geo,
                              method = "cubist",
                              )
  
  silt_val_SYSIS2MSI_geo <- predict(silt_SYSIS2MSI_geo,
                                    newdata = SYSIS2MSItest)
  
  goof(val$Silt.gkg, silt_val_SYSIS2MSI_geo)
  
  
  om_SYSIS2MSI <- train(x = SYSIS2MSI,
                        y = points$OM.gkg,
                        method = "cubist",
                        )
  
  om_val_SYSIS2MSI <- predict(om_SYSIS2MSI,
                              newdata = SYSIS2MSItest)
  
  goof(val$OM.gkg, om_val_SYSIS2MSI)
  
  om_SYSIS2MSI_geo <- train(x = SYSIS2MSI,
                            y = points$mo_geo,
                            method = "cubist",
                            )
  
  om_val_SYSIS2MSI_geo <- predict(om_SYSIS2MSI_geo,
                                  newdata = SYSIS2MSItest)
  
  goof(val$OM.gkg, om_val_SYSIS2MSI_geo)
}

{
  clay_SYSIS2MSI9 <- train(x = SYSIS2MSI9,
                          y = points$Clay.gkg,
                          method = "cubist",
                          )
  
  clay_val_SYSIS2MSI9 <- predict(clay_SYSIS2MSI9,
                                newdata = SYSIS2MSI9test)
  goof(val$Clay.gkg, clay_val_SYSIS2MSI9)
  
  clay_SYSIS2MSI_geo9 <- train(x = SYSIS2MSI9,
                              y = points$clay_geo,
                              method = "cubist",
                              )
  clay_val_SYSIS2MSI_geo9 <- predict(clay_SYSIS2MSI_geo9,
                                    newdata = SYSIS2MSI9test)
  goof(val$clay_geo, clay_val_SYSIS2MSI_geo9)
  
  #Sand SYSIS2MSI
  sand_SYSIS2MSI9 <- train(x = SYSIS2MSI9,
                          y = points$Sand.gkg,
                          method = "cubist",
                          )
  sand_val_SYSIS2MSI9 <- predict(sand_SYSIS2MSI9,
                                newdata = SYSIS2MSI9test)
  goof(val$Sand.gkg, sand_val_SYSIS2MSI9)
  
  sand_SYSIS2MSI_geo9 <- train(x = SYSIS2MSI9,
                              y = points$sand_geo,
                              method = "cubist",
                              )
  
  sand_val_SYSIS2MSI_geo9 <- predict(sand_SYSIS2MSI_geo9,
                                    newdata = SYSIS2MSI9test)
  
  goof(val$Sand.gkg, sand_val_SYSIS2MSI_geo9)
  
  #Silt SYSIS2MSI
  silt_SYSIS2MSI9 <- train(x = SYSIS2MSI9,
                          y = points$Silt.gkg,
                          method = "cubist",
                          )
  silt_val_SYSIS2MSI9 <- predict(silt_SYSIS2MSI9,
                                newdata = SYSIS2MSI9test)
  
  goof(val$Silt.gkg, silt_val_SYSIS2MSI9)
  
  silt_SYSIS2MSI_geo9 <- train(x = SYSIS2MSI9,
                              y = points$silt_geo,
                              method = "cubist",
                              )
  
  silt_val_SYSIS2MSI_geo9 <- predict(silt_SYSIS2MSI_geo9,
                                    newdata = SYSIS2MSI9test)
  
  goof(val$Silt.gkg, silt_val_SYSIS2MSI_geo9)
  
  
  om_SYSIS2MSI9 <- train(x = SYSIS2MSI9,
                        y = points$OM.gkg,
                        method = "cubist",
                        )
  
  om_val_SYSIS2MSI9 <- predict(om_SYSIS2MSI9,
                              newdata = SYSIS2MSI9test)
  
  goof(val$OM.gkg, om_val_SYSIS2MSI9)
  
  om_SYSIS2MSI_geo9 <- train(x = SYSIS2MSI9,
                            y = points$mo_geo,
                            method = "cubist",
                            )
  
  om_val_SYSIS2MSI_geo9 <- predict(om_SYSIS2MSI_geo9,
                                  newdata = SYSIS2MSI9test)
  
  goof(val$OM.gkg, om_val_SYSIS2MSI_geo9)
}

{#Hue
  hue_SYSIS2MSI <- train(x = SYSIS2MSI.cor,
                         y = train.cor$Hue,
                         method = "cubist",
                         )
  
  hue_val_SYSIS2MSI <- predict(hue_SYSIS2MSI,
                               newdata = SYSIS2MSItest.cor)
  goof(test.cor$Hue, hue_val_SYSIS2MSI)
  
  
  #value
  value_SYSIS2MSI <- train(x = SYSIS2MSI.cor,
                           y = train.cor$value,
                           method = "cubist",
                           )
  value_val_SYSIS2MSI <- predict(value_SYSIS2MSI,
                                 newdata = SYSIS2MSItest.cor)
  goof(test.cor$value, value_val_SYSIS2MSI)
  
  
  #chroma
  chroma_SYSIS2MSI <- train(x = SYSIS2MSI.cor,
                            y = train.cor$chroma,
                            method = "cubist",
                            )
  chroma_val_SYSIS2MSI <- predict(chroma_SYSIS2MSI,
                                  newdata = SYSIS2MSItest.cor)
  
  goof(test.cor$chroma, chroma_val_SYSIS2MSI)
  
}

{#Hue
  hue_SYSIS2MSI3 <- train(x = SYSIS2MSI.cor[,1:3],
                          y = train.cor$Hue,
                          method = "cubist",
                          )
  
  hue_val_SYSIS2MSI3 <- predict(hue_SYSIS2MSI3,
                                newdata = SYSIS2MSItest.cor[,1:3])
  goof(test.cor$Hue, hue_val_SYSIS2MSI3)
  
  
  #value
  value_SYSIS2MSI3 <- train(x = SYSIS2MSI.cor[,1:3],
                            y = train.cor$value,
                            method = "cubist",
                            )
  value_val_SYSIS2MSI3 <- predict(value_SYSIS2MSI3,
                                  newdata = SYSIS2MSItest.cor[,1:3])
  goof(test.cor$value, value_val_SYSIS2MSI3)
  
  
  #chroma
  chroma_SYSIS2MSI3 <- train(x = SYSIS2MSI.cor[,1:3],
                             y = train.cor$chroma,
                             method = "cubist",
                             )
  chroma_val_SYSIS2MSI3 <- predict(chroma_SYSIS2MSI3,
                                   newdata = SYSIS2MSItest.cor[,1:3])
  
  goof(test.cor$chroma, chroma_val_SYSIS2MSI3)
  
}

{#Hue
  hue_SYSIS2MSI9 <- train(x = SYSIS2MSI9.cor,
                          y = train.cor$Hue,
                          method = "cubist",
                          )
  
  hue_val_SYSIS2MSI9 <- predict(hue_SYSIS2MSI9,
                                newdata = SYSIS2MSI9test.cor)
  goof(test.cor$Hue, hue_val_SYSIS2MSI9)
  
  
  #value
  value_SYSIS2MSI9 <- train(x = SYSIS2MSI9.cor,
                            y = train.cor$value,
                            method = "cubist",
                            )
  value_val_SYSIS2MSI9 <- predict(value_SYSIS2MSI9,
                                  newdata = SYSIS2MSI9test.cor)
  goof(test.cor$value, value_val_SYSIS2MSI9)
  
  
  #chroma
  chroma_SYSIS2MSI9 <- train(x = SYSIS2MSI9.cor,
                             y = train.cor$chroma,
                             method = "cubist")
  chroma_val_SYSIS2MSI9 <- predict(chroma_SYSIS2MSI9,
                                   newdata = SYSIS2MSI9test.cor)
  
  goof(test.cor$chroma, chroma_val_SYSIS2MSI9)
  
}


#Predicao
clay_raster <- predict(Cubist_Combined, newdata = covsSYSICombined)
writeRaster(clay_raster, "clay_raster.tif")