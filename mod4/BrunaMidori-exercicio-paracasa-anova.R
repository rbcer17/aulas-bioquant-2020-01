### Exercicio para casa: Analise de Variancia


## EXERCICIO 1


# Voce e um consultor ambiental e foi contratado para um determinado empreendimento.Para realizar os estudos, voce estratificou
# a area experimental em 4 estratos, e amostrou 200 arvores em cada estrato. Agora voce necessita saber se os estratos
# são signicativamente diferentes em relacao ao volume vegetal coletado, e quais estratos diferem entre si, para que as
# analises prossigam.
# Responda essas questoes com os metodos estatisticos adequados.
# OBS: Nao esqueca de responder as questoes levantadas apontando a hipotese nula e alternativa se for realizado um teste
# de hipoteses
# A planilha com os dados REFERENTES A ESSA QUESTAO e o exercicio-anova-1.xlsx

#inserindo dados
library(readxl)
exercicio_anova_1 <- read_excel("exercicio-anova-1.xlsx")
#transformando os dados em dados numericos
exercicio_anova_1$dados=as.numeric(exercicio_anova_1$dados)
#visualizacao dos dados
View(exercicio_anova_1)
#transformando a tabela de dados em data frame
exercicio_anova_1<-data.frame(exercicio_anova_1$dados, exercicio_anova_1$estratos)

#variavel dependente(continua)/divisao dos dados numericos em estratos
estrato1<-exercicio_anova_1$exercicio_anova_1.dados[1:200]
estrato2<-exercicio_anova_1$exercicio_anova_1.dados[201:400]
estrato3<-exercicio_anova_1$exercicio_anova_1.dados[401:600]
estrato4<-exercicio_anova_1$exercicio_anova_1.dados[601:800]
exercicio_anova_1.dados<-c(estrato1,estrato2,estrato3,estrato4)

#variavel independente(categorica)
exercicio_anova_1$exercicio_anova_1.estratos<- rep(c("Estrato 1","Estrato 2","Estrato 3","Estrato 4"),each=200)

#para visualizar os dados
str(exercicio_anova_1)
boxplot(exercicio_anova_1.dados~exercicio_anova_1.estratos,data=exercicio_anova_1 ,
        order=c("Estrato 1","Estrato 2", "Estrato 3","Estrato 4") , color="black" ,
        ylab="Dados", xlab="Estratos")

#teste de normalidade (H0: distribuicao normal; H1: populacao nao distribuida normalmente.)
shapiro.test(estrato1)
#estrato1 distribuido normalmente (p>0.05)
shapiro.test(estrato2)
#estrato2 distribuido normalmente (p>0.05)
shapiro.test(estrato3)
#estrato3 distribuido normalmente (p>0.05)
shapiro.test(estrato4)
#estrato4 distribuido normalmente (p>0.05)

#teste de homoscedascidade/homogeinidade das variancias (H0:variancias homogeneas; H1: variancias nao homogeneas)
library(car)
leveneTest(exercicio_anova_1.dados~exercicio_anova_1.estratos, data=exercicio_anova_1, center=mean)
#as variancias são homogeneas (p>0.05)

#teste ANOVA (H0:nao ha diferenca siginificativa; H1:ha diferenca significativa)
testeanova <- aov(exercicio_anova_1.dados~exercicio_anova_1.estratos, data=exercicio_anova_1)
summary(testeanova)
#existe diferenca significativa entre as variancias dos estratos (p<0.05)

## EXERCICIO 2



# Em um laboratorio foi realizada a coleta de plasma sanguineo de 20 pequenos roedores, e o grupo de pesquisa quer
# saber se determinado hormonio afeta o nivel de plasma sanguineo, se ha diferenca entre esse nivel para machos e 
# femeas e se existe diferenca entre machos e femeas na presenca de hormonio e/ou na ausencia dele.
# Responda as questoes levantadas utilizando metodos estatisticos adequados


# A planilha com os dados REFERENTES A ESSA QUESTAO e o exercicio-anova-2.xlsx


# OBS: Nao esqueca de responder as questoes levantadas apontando a hipotese nula e alternativa se for realizado um teste
# de hipoteses


# OBS 2: Na coluna "tratamentos", o numero 1 significa  "presenca do hormonio" e o numero 2"ausencia do hormonio";
# Na coluna "sexo", o numero 1 significa "macho" e o numero 2 "femea".

library(readxl)
exercicio_anova_2 <- read_excel("exercicio-anova-2.xlsx")
View(exercicio_anova_2)

#variaveis
ausencia<-exercicio_anova_2$Plasma[1:10]
presenca<-exercicio_anova_2$Plasma[11:20]

#teste de normalidade para o níveis de plasma em relacao aos tratamentos 
#(H0: distribuicao normal; H1: populacao nao distribuida normalmente.)
shapiro.test(ausencia)
shapiro.test(presenca)
#dados distribuidos normalmente (p>0.05)

#teste de normalidade para os niveis de plasma em relacao ao sexo
#(H0: distribuicao normal; H1: populacao nao distribuida normalmente.)
shapiro.test(exercicio_anova_2[exercicio_anova_2$Sexo =="1",]$Plasma)
shapiro.test(exercicio_anova_2[exercicio_anova_2$Sexo =="2",]$Plasma)
#dados distribuidos normalmente (p>0.05)

#teste de homoscedacidade
#(H0:variancias homogeneas; H1: variancias nao homogeneas)
library(car)
leveneTest(exercicio_anova_2$Plasma, group = exercicio_anova_2$Sexo, center=mean)
leveneTest(exercicio_anova_2$Plasma, group =,exercicio_anova_2$Tratamento, center=mean)
#variancia dos dados em relacao ao sexo sao homogeneas
#variancia dos dados em relacao ao tratamento nao sao homogeneas

plasma<-exercicio_anova_2$Plasma
tratamento<-exercicio_anova_2$Tratamento
sexo<-exercicio_anova_2$Sexo
as.character(tratamento)
as.character(sexo)
#teste de ANOVA
testeanova <- aov(plasma~tratamento+sexo+tratamento:sexo, data=exercicio_anova_2)
summary(testeanova)
#o tratamento afeta o nivel de plasma (p<0.05)
#nao ha diferenca no nivel de plasma para machos e femeas (p>0.05)
#nao ha diferenca entre machos e femeas na presenca e ausencia do hormonio (p>0.05)