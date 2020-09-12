#Atividade MOD3: Teste de T student#
###Exercicio 1
#Um ALUNO DE PIBIC DA UnB TEM QUE VERIFICAR SE SUA POPULACAO DE AVES 
#PERTENCE A UMA POPULACAO CUJA MEDIA DE TAMANHO DAS ASAS   E 2.2


#dados=1.6,3.4,2.3,4.1,4.2,6.6,1.7,4.3#


###Exercicio 2
#Em um estudo, as seguintes aves de dois locais tiveram suas asas medidas  para verificar se sao da
#mesma populacao
#local1=16.3,21.2,15.0,10.8,13.9,23.5, 11.0,6.4,2.8,12.0
#local2=13.3,21.6,27.2,21.7,26.7,33.0,10.9,29.6,9.3,24.3
#Veja se hß uma diferenþa significativa entre as populacoes dos dois locais usando  a analiise correta
#lembrando de  verificar os pre requisitos necessarios para tal analise


###Exercicio 3
#Uma pesquisadora quer verificar se determinada racao tem efeito sobre o peso de frangos.
#Com isso ele gerou dois bancos de dados: um antes da utilizacao da racao e outro depois.
#Verifique se de fato a racao tem efeito significativo sobre essa populacao aplicando
#o teste  correto,lembrando de  verificar os pre requisitos necessarios para tal analise. Cada individuo está pareado na sequencia antes e depois, ou seja o primeiro peso de cada sequencia corresponde ao individuo 1, o segundo ao individuo 2, e assim por diante.


#antes=5166,6080,7290,7031,6700,8908,4214,5135,5002,4900,8043,6205,8800
#depois=6310,6295,4497,5182,4273,6591,6425,4600,5407,5509,4900,5100,4900

#Exercicio1#
dado1<- c(1.6,3.4,2.3,4.1,4.2,6.6,1.7,4.3)
mean(dado1)
#A media do tamanho das asas das aves do aluno de pibic é 3.525, diferente da população de controle, que é 2.2#

#Exercicio2#
Local1<- c(16.3,21.2,15.0,10.8,13.9,23.5, 11.0,6.4,2.8,12.0)
local2<- c(13.3,21.6,27.2,21.7,26.7,33.0,10.9,29.6,9.3,24.3)
shapiro.test(Local1)
shapiro.test(local2)
var.test(Local1,local2)
summary(Local1)
summary(local2)
boxplot(Local1)
boxplot(local2)
boxplot(Local1,local2,  main= "diferença entre as populações", names=c("Local1", "local2") )
t.test(Local1,local2,   var.equal = TRUE)

#Exercico3#
antes<- c(5166,6080,7290,7031,6700,8908,4214,5135,5002,4900,8043,6205,8800)
depois<- c(6310,6295,4497,5182,4273,6591,6425,4600,5407,5509,4900,5100,4900)
shapiro.test(antes)
shapiro.test(depois)
var.test(antes,depois)
summary(antes)
summary(depois)
boxplot(antes)
boxplot(depois)
boxplot(antes,depois, main= "Peso dos frangos", names=c("antes","depois"))
t.test(antes, depois,    var.equal = TRUE)
