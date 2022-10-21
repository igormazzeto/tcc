  #############################################################
  ### UNIVERSIDADE FEDERAL DE MINAS GERAIS                  ###
  ### INSTITUTO DE CIÊNCIAS EXATAS                          ###
  ### DEPARTAMENTO DE ESTATÍSTICA                           ###
  ### ESPECIALIZAÇÃO EM ESTATÍSTICA                         ###
  ### TRABALHO DE CONCLUSÃO DE CURSO                        ###
  ### TEMA: REGRESSÃO LINEAR - OTIMIZAÇÃO DE BETA ZERO      ###
  ### AUTOR: IGOR MAZZETO RESENDE SOARES                    ###
  ### ORIENTADOR: PROF. DR. MARCELO AZEVEDO COSTA           ###
  #############################################################
  
  ### DESCRIÇÃO DO PROBLEMA
  
  ###
  ###
  ###
  ###
  ###
  ###
  ###
  ###
  ###
  
  ### CARREGAMENTO DE PACOTES
  
  
  rm(list = ls(all = TRUE))
  
  if(!require(ggplot2)) install.packages("ggplot2") else library(ggplot2)
  if(!require(ggalt)) install.packages("ggalt") else library(ggalt)
  if(!require(dplyr)) install.packages("dplyr") else library(dplyr)
  if(!require(tidyverse)) install.packages("tidyverse") else library(tidyverse)
  if(!require(gridExtra)) install.packages("gridExtra") else library(gridExtra)
  if(!require(grid)) install.packages("grid") else library(grid)
  if(!require(xlsx)) install.packages("xlsx") else library(xlsx)
  if(!require(openxlsx)) install.packages("openxlsx") else library(openxlsx)
  if(!require(corrplot)) install.packages("corrplot") else library(corrplot)
  if(!require(kableExtra)) install.packages("kableExtra") else library(kableExtra)
  if(!require(psych)) install.packages("psych") else library(psych)
  if(!require(mvShapiroTest)) install.packages("mvShapiroTest") else library(mvShapiroTest)
  if(!require(MVar.pt)) install.packages(MVar.pt) else library(MVar.pt)
  if(!require(kableExtra)) install.packages("kableExtra") else library(kableExtra)
  if(!require(ppclust)) install.packages("ppclust") else library("ppclust")
  if(!require(NbClust)) install.packages("NbClust") else library("NbClust")
  if(!require(packHV)) install.packages("packHV") else library(packHV)
  if(!require(readxl)) install.packages("reaxl") else library(readxl)
  if(!require(MASS)) install.packages("MASS") else library(MASS)
  if(!require(exploreR)) install.packages("exploreR") else library(exploreR)
  if(!require(Hmisc)) install.packages("misc") else library(Hmisc)
  if(!require(moments)) install.packages(moments) else require(moments)
  if(!require(openxlsx)) install.packages(openxlsx) else require(openxlsx)
  if(!require(nortest)) install.packages(nortest) else require(nortest)
  if(!require(lmtest)) install.packages(lmtest) else require(lmtest)
  if(!require(forecast)) install.packages(forecast) else require(forecast)
  if(!require(lubridate)) install.packages(lubridate) else library(lubridate)
  #library(xlsx)
  if(!require(RColorBrewer)) install.packages("RColorBrewer") else library(RColorBrewer)
  #library(rcompanion)
  require(ggpubr)
  if(!require(rbcb)) install.packages("rbcb") else library(rbcb)
  if(!require(qqplotr)) install.packages("qqplotr") else library(qqplotr)
  if(!require(car)) install.packages("car") else library(car)
  if(!require(pROC)) install.packages("pROC") else library(pROC)
  if(!require(mlogit)) install.packages("mlogit") else library(mlogit)
  if(!require(readr)) install.packages("readr") else library(readr)
  if(!require(rpart)) install.packages("rpart") else library(rpart)
  if(!require(rpart.plot)) install.packages("rpart.plot") else library(rpart.plot)
  if(!require(randomForest)) install.packages("randomForest") else library(randomForest)
  if(!require(lpSolve)) install.packages("lpSolve") else library (lpSolve)
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  
  
  ### Leitura de dados
  
  ### MacOS
  # dados <- read_excel("7. Especialização/3. TCC/2. Arquivos 20.10.2022/BaseCompilada_TS14_2022.xlsx", 
  #                                       sheet = "DadosEscores")
  
  ###  Windows
  library(readxl)
  dados <- read_excel("BaseCompilada_TS14_2022.xlsx", 
                                        sheet = "DadosEscores")
  
  View(dados)  
  
  ### Estatísticas - resumo
  summary(dados)
  
  ### Transformação do IdAgente as char
  dados$IdAgente = as.character(dados$IdAgente)
  
  
  
  
  
  
  # par(mfrow=c(5,3))
  # plot(Compensacoes.Pagas ~ Volume.chuva, data=base, pch=19, col="dark blue")
  # plot(Compensacoes.Pagas ~ Descargas.atm, data=base, pch=19, col="dark blue")
  # plot(Compensacoes.Pagas ~ Vento, data=base, pch=19, col="dark blue")
  # plot(Compensacoes.Pagas ~ umidade, data=base, pch=19, col="dark blue")
  # plot(Compensacoes.Pagas ~ temperatura, data=base, pch=19, col="dark blue")
  # plot(Compensacoes.Pagas ~ AG1, data=base, pch=19, col="dark blue")
  # plot(Compensacoes.Pagas ~ AE1, data=base, pch=19, col="dark blue")
  # plot(Compensacoes.Pagas ~ AE2, data=base, pch=19, col="dark blue")
  # plot(Compensacoes.Pagas ~ VC1, data=base, pch=19, col="dark blue")
  # plot(Compensacoes.Pagas ~ DS1, data=base, pch=19, col="dark blue")
  # plot(Compensacoes.Pagas ~ DS2, data=base, pch=19, col="dark blue")
  # plot(Compensacoes.Pagas ~ AR1, data=base, pch=19, col="dark blue")
  # plot(Compensacoes.Pagas ~ as.factor(grupos2), data=base, pch=19, col="dark blue", xlab="Grupos")
  # plot(Compensacoes.Pagas ~ Unidades.Consumidoras, data=base, pch=19, col="dark blue")
  # packHV::hist_boxplot(base$Compensacoes.Pagas, main="", col="light blue", xlab="Compensacoes.Pagas")
  # mosaicplot(native.country ~ y, data=dados1);abline(h = p1, col = "red")
  #  matCor <- cor(dados, method="spearman")
  
  # corrplot(matCor, method = "ellipse", type="upper", 
  #          order="AOE", diag=FALSE, addgrid.col=NA,
  #          outline=TRUE)
  
  
  
  
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  
  ### Exemplo de programação linear
  
  # Set up problem -> max: x1 + 9 x2 + x3 
  
  # subject to
  # x1 + 2 x2 + 3 x3 <= 9
  # 3 x1 + 2 x2 + 2 x3 <= 15
  
  
  # f.obj <- c(1, 9, 1)
  # f.con <- matrix (c(1, 2, 3, 3, 2, 2), nrow=2, byrow=TRUE)
  # f.dir <- c("<=", "<=")
  # f.rhs <- c(9, 15)
  # lp ("max", f.obj, f.con, f.dir, f.rhs)
  # lp ("max", f.obj, f.con, f.dir, f.rhs)$solution
  
  
  

    
  
  
