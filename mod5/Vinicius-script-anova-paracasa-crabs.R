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
head(crabs) #olhar as primeiras colunas
summary(crabs)
View(crabs)
#
#Agora a pergunta para casa: monte uma anova testando se o comprimento do lobo frontal
#do caranguejo varia entre os sexos e entre especies, e se ha interacao entre as
#variaveis independentes sexo e especie
#Nao esqueca de testar as premissas de normalidade e homoscedascidade
#Resposta:

#Normalidade FL: De acordo com os resultados os dados do comprimento do lobo frontal são normais
shapiro.test(crabs[crabs$sp=="B",]$FL)
shapiro.test(crabs[crabs$sp=="O",]$FL)
shapiro.test(crabs[crabs$sex=="M",]$FL)
shapiro.test(crabs[crabs$sex=="F",]$FL)

#Homocedasticidade: De acordo com os resultados, os dados tambem são homocedasticos.
library(lawstat)
levene.test(crabs$FL,group = crabs$sex)
levene.test(crabs$FL,group = crabs$sp)

#Anova: houve uma diferença significativa entre o comprimento do lobo frontal e a espécie (alfa < 0.001),
#e uma diferença significatica entre o sexo e a espécie (alfa < 0.01).
#Porem não houve diferença significativa entre o comprimento do lobo frontal e o sexo dos carangueijos.
resultado = aov(FL~sex+sp+sex:sp, data = crabs)
summary(resultado)
#Com isso podemos assumir que o comprimento do lobo frontal varia de acordo com a espécie de carangueijo, mas não varia de a cordo com o sexo,
#e também que o numero de indivíduos machos e femeas variam de acordo com a espécie do carangueijo.
