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

#determinando dados
FL<-crabs$FL #variavel resposta
sp<-crabs$sp #fator
sexo<-crabs$sex #fator

F<-subset(crabs, sex=="F") #dados relacionados as femeas
M<-subset(crabs, sex=="M") #dados relacionados aos machos

FLspB<-FL[1:100] #medidas de lobo frontal da especie B
FLspO<-FL[101:200] #medidas de lobo frontal da especie O

#teste de normalidade (shapiro)
shapiro.test(F$FL)
shapiro.test(M$FL)
shapiro.test(FLspB)
shapiro.test(FLspO)
#os dados apresentaram distribuicao normal (P>0.05)

#teste de homoscedascidade
library(car)
leveneTest(crabs$FL, group=crabs$sex, center=mean)
leveneTest(crabs$FL, group=crabs$sp, center=mean )
#as variancias sao homogeneas (P>0.05)

#teste ANOVA 
testeanova <- aov(FL~sex+sp+sex:sp, data=crabs)
summary(testeanova)
#o comprimeto do lobo frontal dos carangueijos nao varia em relacao ao sexo (P>0.05),
#mas varia em relacao a especie (P<0.05) e
#em relacao a interacao entre sexo e especie (P<0.05)