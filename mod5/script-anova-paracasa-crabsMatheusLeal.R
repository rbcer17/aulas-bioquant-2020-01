#Vamos usar o conjunto de dados crabs (caranguejos) do pacote MASS
#Primeiro instalamos o pacote MASS
#
install.packages("MASS")
#
#Vamos pedir ao R umas informacoes basicas sobre o pacote e sobre o conjunto de dados
#
??MASS
??crabs
#
#agora que o pacote foi instalado vamos carregar usando o comando library
#
library("MASS")
#
#Agora vamos visualizar quais variaveis ha dentro dos dados crabs
#
data(crabs) #baixar os dados do pacote crabs dentro do MASS
View (crabs)
head(crabs) #olhar as primeiras colunas
summary(crabs)
#
#Agora a pergunta para casa: monte uma anova testando se o comprimento do lobo frontal
# do caranguejo varia entre os sexos e entre especies, e se ha interacao entre as
#variaveis independentes sexo e especie
# Nao esqueca de testar as premissas de normalidade e homoscedascidade

#variavel dependente

Dependente <- c(crabs$FL)

#variaveis independentes

Indep1 <- c(crabs$sp)
Indep2 <- c(crabs$sex)

#tabela

crabs1 <- data.frame(Indep1, Indep2, Dependente)

#normalidade especie

shapiro.test(crabs1[crabs1$Indep1 == "2",]$Dependente)
shapiro.test(crabs1[crabs1$Indep1 == "1",]$Dependente)

#normalidade sexo

shapiro.test(crabs1[crabs1$Indep2 == "2",]$Dependente)
shapiro.test(crabs1[crabs1$Indep2 == "1",]$Dependente)

#Homocedasticidade

install.packages("lawstat")
library(lawstat)

levene.test(Dependente, group = Indep1)

levene.test(Dependente, group = Indep2)

resultado <- aov(Dependente ~ Indep1 + Indep2, data = crabs1)
summary(resultado)

resultado1 <- aov(Dependente ~ Indep1 + Indep2 + Indep1:Indep2, data = crabs1)
summary(resultado1)

posth <- TukeyHSD(resultado1)
posth
plot(posth)

