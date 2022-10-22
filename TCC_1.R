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
  dados =  read_excel("7. Especialização/3. TCC/3. Códigos/tcc/BaseCompilada_TS14_2022.xlsx", 
                   sheet = "DadosEscores")
  
  ###  Windows
  # library(readxl)
  # dados <- read_excel("BaseCompilada_TS14_2022.xlsx", 
  #                                       sheet = "DadosEscores")
  
  # View(dados)  
  
  ### Variáveis do modelo ### ### ### ### ### ### ### ### ### ### ### ### ###
  ###                                                                     ###
  # X1: Extensão de rede menor que 230 kV                                 ###
  # X2: Extensão de rede superior que 230 kV                              ###
  # X3: Potência aparente total, em MVA, de equipamentos de subestação    ###
  # x4: PotÇencia reativa total, em Mvar, de equipamentos de subestação   ###
  # X5: Equipamentos de subestação com tensão inferior a 230 kV           ###
  # X6: Equipamentos de subestação com tensão superior a 230 kV           ###
  # X7: Módulos de manobra com tensão inferior a 230 kV                   ###
  # X8: Módulos de manobra com tensão igual ou superior a 230 kV          ###
  # X9: Qualiodade (não utilizada)                                        ###
  # Y : PMSO (variável resposta)                                          ###
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  
  ### Estatísticas - resumo
  summary(dados)
  
  ### Transformação do IdAgente as char
  dados$IdAgente = as.character(dados$IdAgente)
  
  ### dataset com variáveis do modelo
  db = with(dados,dplyr::select(dados, Concessionaria, Tipo,
                        Ano, PMSO, rede.menor.230,
                        rede.maior.230,MVA,Mvar,
                        modulos.sub.menor230,modulos.sub.maior230,
                        modulos.manobra.menor230,modulos.manobra.maior230))
  
  ### Modificacao dos nomes das variaveis
  names(db)[5]  = "X1"
  names(db)[6]  = "X2"
  names(db)[7]  = "X3"
  names(db)[8]  = "X4"
  names(db)[9]  = "X5"
  names(db)[10] = "X6"
  names(db)[11] = "X7"
  names(db)[12] = "X8"
  
  # View(db)
  
  ### Gráficos
  
  ### Histograma
  with(db,packHV::hist_boxplot(PMSO, main="Histograma", 
                       col="light blue",
                       xlab="PMSO"))
  
  ### Scatter plots
  #par(mfrow=c(4,2))
    plot(PMSO ~ X1, data=db, pch=19, col="dark blue", main =  'PMSO x X1')
    plot(PMSO ~ X2, data=db, pch=19, col="red", main =  'PMSO x X2')
    plot(PMSO ~ X3, data=db, pch=19, col="green", main =  'PMSO x X3')
    plot(PMSO ~ X4, data=db, pch=19, col="cyan", main =  'PMSO x X4')
    plot(PMSO ~ X5, data=db, pch=19, col="grey", main =  'PMSO x X5')
    plot(PMSO ~ X6, data=db, pch=19, col="orange", main =  'PMSO x X6')
    plot(PMSO ~ X7, data=db, pch=19, col="purple", main =  'PMSO x X7')
    plot(PMSO ~ X8, data=db, pch=19, col="black", main =  'PMSO x X8')

  ### Correlation plots
  dbcor = with(db,dplyr::select(db, PMSO,X1,X2,X3,X4,X5,X6,X7,X8))
    
  matCor <- cor(dbcor, method="spearman")
  corrplot(matCor, type="upper",
             order="AOE", diag=FALSE, addgrid.col=NA,
             outline=TRUE)
  ### Boxplots
  
  with(db,
  boxplot(PMSO ~ Ano,
          main="PMSO anual", 
          col="dark blue", 
          xlab="Ano",
          ylab="PMSO")
  )
  
  with(db,
       boxplot(PMSO ~ Concessionaria,
               main="PMSO por conc.", 
               col="green", 
               xlab="Conc.",
               ylab="PMSO")
  )
  
  with(db,
       boxplot(PMSO ~ Tipo,
               main="PMSO por tipo", 
               col="orange", 
               xlab="Tipo",
               ylab="PMSO")
  )
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  
  ### Linear Regressions
  
  ### Analisando o R2 de cada variavel
  exploreR::masslm(dbcor,"PMSO")
  
  ### modelo linear com todas as variaveis
  modelo = lm(PMSO ~ ., data=dbcor)
  summary(modelo)
  plot(modelo, lty=0, pch=19, col="blue")
  modelo$coefficients
  
  ### Shapiro-Wilk test | Null-hypothesis: population is normally distributed
  ### If p-value < alpha then nul-=hypothesis is rejected.
  
  shapiro.test(residuals(modelo))
  cat('Evidencias de que os residuos nao sao normalmente distribuidos')
  
    
  
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
  
  
  

    
  
  
