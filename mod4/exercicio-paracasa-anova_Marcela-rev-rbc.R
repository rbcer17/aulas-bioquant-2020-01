### Exercicio para casa: Analise de Variancia

################################ EXERCICIO 1################################


# Voce e um consultor ambiental e foi contratado para um determinado empreendimento.
#Para realizar os estudos, voce estratificou a area experimental em 4 estratos,
#e amostrou 200 arvores em cada estrato. Agora voce necessita saber se os estratos
# são signicativamente diferentes em relacao ao volume vegetal coletado,
#e quais estratos diferem entre si, para que as analises prossigam.
# Responda essas questoes com os metodos estatisticos adequados.
# OBS: Nao esqueca de responder as questoes levantadas apontando 
#a hipotese nula e alternativa se for realizado um teste de hipoteses

# A planilha com os dados REFERENTES A ESSA QUESTAO e o exercicio-anova-1.xlsx
library(readxl)
exercicio_anova_1 <- read_excel("C:/Users/User/Downloads/Biologia Quantitativa/exercicio-anova-1.xlsx")
View(exercicio_anova_1)
exercicio_anova_1$dados = as.numeric(exercicio_anova_1$dados)


#PREMISSAS#

# Shapiro Wilk - Distribuicao normal 
Estrato_1=exercicio_anova_1$dados [1:200]
shapiro.test(Estrato_1)
# p-value = 0.3357 > 0.05 = dados dentro da normalidade

Estrato_2=exercicio_anova_1$dados [201:400]
shapiro.test(Estrato_2)
# p-value = 0.4302 > 0.05 = dados dentro da normalidade

Estrato_3=exercicio_anova_1$dados [401:600]
shapiro.test(Estrato_3)
#p-value = 0.1172 > 0.05 = dados dentro da normalidade

Estrato_4=exercicio_anova_1$dados [601:800]
shapiro.test(Estrato_4)
#p-value = 0.4134 > 0.05 = dados dentro da normalidade


# Teste de Levene - Homocedasticidade 
# H0 = variancia entre os grupos igual
library(lawstat)
levene.test(exercicio_anova_1$dados, group = exercicio_anova_1$estratos)
# p-value = 0.419 > 0.05 = aceita-se H0, as vari?ncias s?o iguais (homocedasticas)


#ANOVA#
#H0: medias dos grupos iguais
#H1: m?dia de pelo menos 1 grupo ? diferente
resultado_anova = aov(dados~estratos, data=exercicio_anova_1)
summary(resultado_anova)


#Box plot#
install.packages("ggplot2")
library("ggplot2")

exercicio_anova_1$estratos <- ordered(exercicio_anova_1$estratos,
                         levels = c("Estrato 1", "Estrato 2", "Estrato 3", "Estrato 4"))

exercicio1= as.data.frame(exercicio_anova_1)


# n funcionou
ggplot(exercicio1$estratos, x = "estratos", y = "dados", 
          color = "estratos", palette = c("#999999","#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Estrato 1", "Estrato 2", "Estrato 3", "Estrato 4"),
          ylab = "dados", xlab = "estratos")

# n funcionou
ggplot(exercicio_anova_1$estratos, aes(x=dose, y=len, color=estratos)) + 
       scale_color_brewer(palette="Dark2") +
       order = c("Estrato 1", "Estrato 2", "Estrato 3", "Estrato 4")

  

#Tukey multiple pairwise-comparisons#
TUKEY <-TukeyHSD(resultado_anova)
TUKEY
#Apenas a diferença (Estrato 2-Estrato 1) nao e significativa, com valor de p ajustado de 0.7383912



################################EXERCICIO 2################################


# Em um laboratorio foi realizada a coleta de plasma sanguineo de 20 pequenos roedores, 
#e o grupo de pesquisa quer saber se determinado hormonio afeta o nivel de plasma sanguineo,
#se ha diferenca entre esse nivel para machos e femeas e 
#se existe diferenca entre machos e femeas na presenca de hormonio e/ou na ausencia dele.
# Responda as questoes levantadas utilizando metodos estatisticos adequados

# OBS: Nao esqueca de responder as questoes levantadas apontando a hipotese nula e alternativa se for realizado um teste
# de hipoteses
# H0 = Nao ha diferen?a entre os tratamentos
# H1 = h? diferen?a entre os tratamentos

# OBS 2: Na coluna "tratamentos", o numero 1 significa  "presenca do hormonio" e o numero 2"ausencia do hormonio";
# Na coluna "sexo", o numero 1 significa "macho" e o numero 2 "femea".


# A planilha com os dados REFERENTES A ESSA QUESTAO e o exercicio-anova-2.xlsx
library(readxl)
exercicio_anova_2 <- read_excel("C:/Users/User/Downloads/Biologia Quantitativa/exercicio-anova-2.xlsx")
View(exercicio_anova_2)

exercicio_anova_2$Plasma = as.numeric(exercicio_anova_2$Plasma)
exercicio_anova_2$Tratamento = as.character(exercicio_anova_2$Tratamento)

#PREMISSAS#

# Shapiro Wilk - Distribuicao normal (tratamento)

shapiro.test(exercicio_anova_2$Plasma) 
# p-value = 0.06956 > 0.05 = dados dentro da normalidade

tratamento_1=c(exercicio_anova_2$Plasma [1:10])
shapiro.test(tratamento_1)
# p-value = 0.7438  > 0.05 = dados dentro da normalidade

tratamento_2 = c(exercicio_anova_2$Plasma [11:20])
shapiro.test(tratamento_2)
#p-value = 0.5864  > 0.05 = dados dentro da normalidade

# Shapiro Wilk - Distribuicao normal (sexo)
shapiro.test(exercicio_anova_2[exercicio_anova_2$Sexo =="1",]$Plasma)
#p-value = 0.06643 > 0.05 = dados dentro da normalidade

shapiro.test(exercicio_anova_2[exercicio_anova_2$Sexo =="2",]$Plasma)
# p-value = 0.09318 > 0.05 = dados dentro da normalidade


# Teste de Levene - Homocedasticidade 
library(lawstat)

levene.test(exercicio_anova_2$Plasma, group =,exercicio_anova_2$Tratamento)
#p-value = 0.02734 < 0.05 = regeita-se H0, variância entre os tratamentos é diferente

levene.test(exercicio_anova_2$Plasma, group =,exercicio_anova_2$Sexo)
#p-value = 0.6856 > 0.05 = aceita-se H0, variância entre os sexos é igual

#BOXPLOT#
library(ggplot2)
boxplot(Plasma ~ Tratamento:Sexo,
        data = exercicio_anova_2,
        xlab = "tratamento x Sexo",
        ylab = "Plasma")


#ANOVA#
Modelo_2= lm(Plasma ~ Tratamento + Sexo + Tratamento:Sexo,
           data= exercicio_anova_2)
anova(Modelo_2) 
#7.943e-07 < 0.05 = Tratamento possui diferença significativa

summary(Modelo_2) 

#HISTOGRAMA#
hist(residuals(Modelo_2),
     col="darkgray")
#distribuição dos residuos possui tendencia central, parece normal

#RESPOSTAS#

#A presença do hormonio afeta o nivel de plasma sanguineo? SIM

#Ha diferenca entre esse nivel para machos e femeas? NAO

#Existe diferenca entre machos e femeas na presenca de hormonio e/ou na ausencia dele? NAO
