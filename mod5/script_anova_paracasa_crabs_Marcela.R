#Vamos usar o conjunto de dados crabs (caranguejos) do pacote MASS
#Primeiro instalamos o pacote MASS
#
install.packages("MASS")
#
#Vamos pedir ao R umas informacoes basicas sobre o pacote e sobre o conjunto de dados
#
help(MASS)
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
head(crabs) #olhar as primeiras colunas
summary(crabs)
#
#Agora a pergunta para casa: monte uma anova testando se o comprimento do lobo frontal
# do caranguejo varia entre os sexos e entre especies, e se ha interacao entre as
#variaveis independentes sexo e especie
# Nao esqueca de testar as premissas de normalidade e homoscedascidade


##PREMISSAS##

##Teste de Levene - Homocedasticidade 
library(lawstat)
levene.test(crabs$FL, group = crabs$sp)
#p-value = 0.4472 > 0.05 = aceita-se H0, há homocedasticidade

levene.test(crabs$FL, group = crabs$sex)
#p-value = 0.9443 > 0.05 = aceita-se H0, há homocedasticidade


##Shapiro Wilk - Distribuicao normal 

#especie
shapiro.test(crabs[crabs$sp=="B",]$FL)
#p-value = 0.4989 > 0.05 = dados normais


shapiro.test(crabs[crabs$sp=="O",]$FL)
#p-value = 0.1592 > 0.05 = dados normais


#sexo
shapiro.test(crabs[crabs$sex=="F",]$FL)
#p-value = 0.4939 > 0.05 = dados normais

shapiro.test(crabs[crabs$sex=="M",]$FL)
#p-value = 0.6801  0.05 = dados normais


##ANOVA##


anova_crabs = aov(FL~ sp + sex , data=crabs)
summary(anova_crabs)


anova_crabs2 = aov(FL~ sp * sex , data=crabs)
summary(anova_crabs2)
# p-value(sp) = 4.63e-11 < 0.05 = valor significativo
# p-value(sp:sex) =  0.00416 < 0.05 = valor significativo



## Histograma residuos

hist(residuals(anova_crabs), col="darkgray")
## possui tendencia central = distribuicao dos residuos parece normal

##RESPOSTAS##

# O comprimento do lobo frontal possui variacao significativa entre as especies "O" e "B"
# Nao ha veriacao significativa de acordo com o sexo
# ha interacao entre as variaveis especie e sexo
