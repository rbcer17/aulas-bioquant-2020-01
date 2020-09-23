###########################Sepal.Length#####################################

#variavel dependente
Sepal.Length <- iris$Sepal.Length
#variavel independente
Species <- iris$Species

Tabela <- data.frame(Species, Sepal.Length)

#Teste de normalidade
shapiro.test (Tabela[Tabela$Species == "virginica",]$Sepal.Length)
shapiro.test (Tabela[Tabela$Species == "setosa",]$Sepal.Length)
shapiro.test (Tabela[Tabela$Species == "versicolor",]$Sepal.Length)

#Homocedasticidade
install.packages("lawstat")
library(lawstat)
levene.test(Tabela$Sepal.Length, group = Tabela$Species)
#Pelo teste de levene não consideramos que as amostras possuem homogeneidade de varianças.

###########################Sepal.Width#####################################

#variavel dependente
Sepal.Width <- iris$Sepal.Width
#variavel independente
Species <- iris$Species

Tabela <- data.frame(Species, Sepal.Width)

#Teste de normalidade
shapiro.test (Tabela[Tabela$Species == "virginica",]$Sepal.Width)
shapiro.test (Tabela[Tabela$Species == "setosa",]$Sepal.Width)
shapiro.test (Tabela[Tabela$Species == "versicolor",]$Sepal.Width)

#Homocedasticidade
install.packages("lawstat")
library(lawstat)
levene.test(Tabela$Sepal.Width, group = Tabela$Species)

resultado <- aov(Sepal.Width ~ Species, data = Tabela)
summary(resultado)
#F valor = 49.16, nivel de significancia de 2e-16, considerando 0,05 como referência rejeitamos a hipótese
#nula, as médias diferem entre si.

###########################Petal.Length#####################################
#variavel dependente
Petal.Length <- iris$Petal.Length
#variavel independente
Species <- iris$Species

Tabela <- data.frame(Species, Petal.Length)

#Teste de normalidade
shapiro.test (Tabela[Tabela$Species == "virginica",]$Petal.Length)
shapiro.test (Tabela[Tabela$Species == "setosa",]$Petal.Length)
shapiro.test (Tabela[Tabela$Species == "versicolor",]$Petal.Length)

#Homocedasticidade
install.packages("lawstat")
library(lawstat)
levene.test(Tabela$Petal.Length, group = Tabela$Species)
#Pelo teste de levene não consideramos que as amostras possuem homogeneidade de varianças.

###########################Petal.Width#####################################
#variavel dependente
Petal.Width <- iris$Petal.Width
#variavel independente
Species <- iris$Species

Tabela <- data.frame(Species, Petal.Width)

#Teste de normalidade
shapiro.test (Tabela[Tabela$Species == "virginica",]$Petal.Width)
shapiro.test (Tabela[Tabela$Species == "setosa",]$Petal.Width)
shapiro.test (Tabela[Tabela$Species == "versicolor",]$Petal.Width)
#as amostras não estão distribuidas normalmente de acordo com o teste de shapiro wilk.

##########################################################################

#Considerando todos os fatores analisados, aquele que melhor nos serve para diferenciar os grupos
#de acordo com suas médias é a largura da sepala(sepal.width)

