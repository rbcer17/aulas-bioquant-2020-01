#Vamos usar o conjunto de dados crabs (caranguejos) do pacote MASS
#Primeiro instalamos o pacote MASS
#
install.packages("MASS")
#
#Vamos pedir ao R umas informacoes basicas sobre o pacote e sobre o conjunto de dados
#
?MASS
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



#Vamos usar o conjunto de dados crabs (caranguejos) do pacote MASS
#Primeiro instalamos o pacote MASS

install.packages("MASS")


#Vamos pedir ao R umas informacoes basicas sobre o pacote e sobre o conjunto de dados

?MASS
??crabs


#agora que o pacote foi instalado vamos carregar usando o comando library


library("MASS")


#Agora vamos visualizar quais variaveis ha dentro dos dados crabs
#baixar os dados do pacote crabs dentro do MASS

data(crabs)

#olhar as primeiras colunas

head(crabs) 
summary(crabs)
View(crabs)


#Agora a pergunta para casa: monte uma anova testando se o comprimento do lobo frontal
# do caranguejo varia entre os sexos e entre especies, e se ha interacao entre as
#variaveis independentes sexo e especie
# Nao esqueca de testar as premissas de normalidade e homoscedascidade

#Normalidade#
## o teste Shapiro-wilk constata a normalidade dos dados#
shapiro.test(crabs[crabs$sp == "B",]$FL)
shapiro.test(crabs[crabs$sp == "O",]$FL)
shapiro.test(crabs[crabs$sex == "F",]$FL)
shapiro.test(crabs[crabs$sex == "M",]$FL)



##Vizualização os dados separadamente#

boxplot ( crabs$FL ~ sex , data = crabs,
          ordem = c ( " B " , " O " , " F " , " M " ), cor = " preto " ,
          ylab = " FL " , xlab = " sex " )

boxplot ( crabs$FL ~ sp , data = crabs,
          ordem = c ( " B " , " O " , " F " , " M " ), cor = " preto " ,
          ylab = " FL " , xlab = " sp " )

boxplot ( crabs$FL ~ sex+sp , data = crabs,
          ordem = c ( " B " , " O " , " F " , " M " ), cor = " preto " ,
          ylab = " FL " , xlab = " sex+sp " )


#Homoscedascidade#
# O teste Levene-type monstra que os dados são homocedasticos#

install.packages("lawstat")
library(lawstat)

levene.test(crabs$FL, group = crabs$sp)
levene.test(crabs$FL, group = crabs$sex)

#ANOVA#

#Há variação do comprimento do lobo frontal entre as espécies, porém não há 
#variação significativa entre os sexos, e não há  interaçao entre as
#variaveis independentes sexo e espécie#

resultado <- aov(FL~sp+sex+sp:sex, data = crabs)
summary(resultado)
