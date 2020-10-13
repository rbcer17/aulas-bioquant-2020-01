#Vamos usar o conjunto de dados crabs (caranguejos) do pacote MASS
#Primeiro instalamos o pacote MASS
#
install.packages("MASS")
#
#Vamos pedir ao R umas informacoes basicas sobre o pacote e sobre o conjunto de dados
#
library(MASS)
?MASS
help(MASS)
??crabs
help(MASS)
View(crabs)
#
#agora que o pacote foi instalado vamos carregar usando o comando library
#
library("MASS")
#
#Agora vamos visualizar quais variaveis ha dentro dos dados crabs
#
data(crabs) #baixar os dados do pacote crabs dentro do MASS
head(crabs) #olhar as primeiras colunas
summary(crabs)
#
#Agora a pergunta para casa: monte uma anova testando se o comprimento do lobo frontal
# do caranguejo varia entre os sexos e entre especies, e se ha interacao entre as
#variaveis independentes sexo e especie
#Primeiro vamos testar a normalidade das amostras com um shapiro test
shapiro.test(crabs$FL)
F <- subset(crabs, sex =="F")
M <- subset(crabs, sex=="M")
spb <- subset(crabs, sp=="B")
spo <- subset(crabs, sp=="O")
shapiro.test(F$FL)
shapiro.test(M$FL)
shapiro.test(spb$FL)
shapiro.test(spo$FL)
#Apesar de alguns valores serem >0.5 consideramos que as amostras possuem distribuição normal pois possuem valores próximos 
#teste homoscedascidade 
library(lawstat)
levene.test(crabs$FL, group= crabs$sp)
levene.test(crabs$FL, group = crabs$sex)#DUVIDA
#aov
help(crabs)
resultadocrabs <- aov(FL~ sp+sex+sp:sex, data:crabs)#erro
summary(resultadocrabs)
# Nao esqueca de testar as premissas de normalidade e homoscedascidade