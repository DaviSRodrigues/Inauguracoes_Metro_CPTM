#setting tyhe environment
setwd('D:/OneDrive/Jupyter Notebooks/Estudos/Inauguracoes Metro CPTM')

#loading data
dados = read.csv('inauguracoes.csv')

head(dados)

dados$Data <- as.Date(dados$Inauguração, format = '%d/%m/%Y')

dados$Ano <- as.numeric(format(dados$Data,'%Y'))

dados$Mes <- as.numeric(format(dados$Data,'%m'))

dados$Dia <- as.numeric(format(dados$Data,'%d'))

head(dados)

dados[order(dados$Ano, dados$Mes, dados$Dia) & dados$Linha == 7, ]

dados[order(dados$Ano, dados$Mes, dados$Dia, decreasing = TRUE), ]

idade_linha <- aggregate(dados$Idade, by = list(dados$Linha), FUN = mean)

names(idade_linha)[1] = 'Linha'
names(idade_linha)[2] = 'Idade_Media'

idade_linha[order(idade_linha$Idade_Media, decreasing = TRUE), ]

idade_empresa <- aggregate(dados$Idade, by = list(dados$Construção), FUN = mean)

names(idade_empresa)[1] = 'Empresa'
names(idade_empresa)[2] = 'Idade_Media'

idade_empresa[order(idade_empresa$Idade_Media, decreasing = TRUE), ]

inaug_ano <- aggregate(dados$Ano, by = list(dados$Ano), FUN = function(x) length(x))

names(inaug_ano)[1] = 'Ano'
names(inaug_ano)[2] = 'Inauguracoes'

plot(dados$Ano, dados$Linha, main = "Inaugurações de Estaações da CPTM/Metrô",
     xlab = "Ano de Inauguração", ylab = "Linha", pch = 20, col = dados$Linha)

abline(lm(dados$Linha ~ dados$Ano), col = "red") # regression line (y~x)
lines(lowess(dados$Ano, dados$Linha), col = "blue") # lowess line (x,y)

axis(1, at = (1867:2019))
axis(2, at = (1:15))

library(ggplot2)

ggplot(dados, aes(x = dados$Ano, y = dados$Linha, color = dados$Linha)) + 
  geom_point(size = 3) +
  xlim(1867, 2019) +
  ylim(1, 15) +
  ggtitle('Inaugurações de Estações da CPTM/Metrô') + 
  xlab('Ano de Inauguração') +
  ylab('Linha') + 
  labs(colour = 'Linha')

ggplot(inaug_ano, aes(x = inaug_ano$Ano, y = inaug_ano$Inauguracoes, color = inaug_ano$Ano)) +
  geom_col() +
  xlim(1867, 2019) +
  ylim(0, 15) +
  ggtitle('Inaugurações de Estações da CPTM/Metrô') + 
  xlab('Ano de Inauguração') +
  ylab('Estações inauguradas') +
  labs(colour = 'Ano')