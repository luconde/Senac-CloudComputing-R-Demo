#####################################
# Instala as Bibliotecas
#####################################
install.packages("readxl")
install.packages("openxlsx")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("writexl")
install.packages("ggplot2")
install.packages("lmtest")
install.packages("caret")
install.packages("car")

#####################################
# Carrega as Bibliotecas
#####################################
rm(list= ls()) 
options(scipen=999)

source("fnLoadLibrary.R")
source("fnLoadData.R")

library(readxl)
library(openxlsx)
library(dplyr)
library(tidyverse)
library(writexl)
library(ggplot2)
library(lmtest)
library(caret)

#####################################
# Carrega os dados
#####################################
vTDadosOriginal = fnLoadData("dados.xlsx")


#####################################
# Executa a separação (70% para treinamento)
#####################################
vintNumeroDeLinhas = nrow(vTDadosOriginal)
vintNumeroDeLinhas70Pct = round(vintNumeroDeLinhas * .7, 0)
vintNumeroDeLinhas30Pct = vintNumeroDeLinhas - vintNumeroDeLinhas70Pct

# Coleta 70% das linhas para treinamento
vTDadosTrain = vTDadosOriginal[1:vintNumeroDeLinhas70Pct, ]

# Coleta os restantes 30% das linhas
vTDadosModel = vTDadosOriginal[(vintNumeroDeLinhas70Pct + 1): vintNumeroDeLinhas, ]

#####################################
# Cria a Regressao
#####################################

# Calcula a Equação (Regressão)
vModel01 <- lm(preco ~ quartos + banheiros + areautil + areatotal + anos, vTDadosTrain)

# Exibe o resultado
summary(vModel01)

# Armazena o resultado do Modelo
vSumModel01 = summary(vModel01)

#####################################
# Analisando o qualidade
#####################################

### Analisando a multicolinearidade
# Regra de ouro
# Pouca colinearidade < 5
# Alta colinearidade > 5
car::vif(vModel01)

### Analisando a significancia do modelo
# H0: Não há relação entre as variaveis independente e a dependentes (p-value >= 0.05)
# H1: Há relação entre as variaveis independentes e a dependente (p-value < 0.05)

# Também pode ser analisado em F-Statistics: 61.17 (QUanto mais perto de 100 melhor)
pf(vSumModel01$fstatistic[1], vSumModel01$fstatistic[2], vSumModel01$fstatistic[3], lower.tail = FALSE)


#####################################
# Previsão
#####################################

# Quartos: 4
# Banheiros: 3
# Area Util = 240m2
# Area Total = 331m2
# Anos de construção = 106 anos
predict(vModel01, data.frame(quartos = 4, banheiros = 3, areautil = 240, areatotal = 331, anos = 106))



