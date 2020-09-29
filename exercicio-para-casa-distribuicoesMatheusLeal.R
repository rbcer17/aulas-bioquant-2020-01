###Exercicio 1
#Um ALUNO DE PIBIC DA UnB TEM QUE VERIFICAR SE SUA POPULACAO DE AVES 
#PERTENCE A UMA POPULACAO CUJA MEDIA DE TAMANHO DAS ASAS   E 2.2


dados = c(1.6,3.4,2.3,4.1,4.2,6.6,1.7,4.3)
mean(dados)

#A hipótese nula diz que a média da população é igual a 2.2, mas os dados da população
#nos dão o valor de média igual a 3.525. Sendo assim, a hipótese alternativa é verdadeira.


###Exercicio 2
#Em um estudo, as seguintes aves de dois locais tiveram suas asas medidas  para verificar se sao da
#mesma populacao
local1=c(16.3,21.2,15.0,10.8,13.9,23.5, 11.0,6.4,2.8,12.0)
local2=c(13.3,21.6,27.2,21.7,26.7,33.0,10.9,29.6,9.3,24.3)
#Veja se hß uma diferenþa significativa entre as populacoes dos dois locais usando  a analiise correta
#lembrando de  verificar os pre requisitos necessarios para tal analise

shapiro.test(local1)
shapiro.test(local2)
#Os p-valores foram acima de 0.05, portanto assumiremos a hipótese nula, os grupos 
#seguem uma distribuição normal.Dessa forma usaremos o teste t de student para comparar as médias
#as populações.
t.test(local1,local2)
#O p-valor foi abaixo de 0.05, assim assumiremos H1, existe diferença significativa entre
#as populações.

###Exercicio 3
#Uma pesquisadora quer verificar se determinada racao tem efeito sobre o peso de frangos.
#Com isso ele gerou dois bancos de dados: um antes da utilizacao da racao e outro depois.
#Verifique se de fato a racao tem efeito significativo sobre essa populacao aplicando
#o teste  correto,lembrando de  verificar os pre requisitos necessarios para tal analise. Cada individuo está pareado na sequencia antes e depois, ou seja o primeiro peso de cada sequencia corresponde ao individuo 1, o segundo ao individuo 2, e assim por diante.


antes=c(5166,6080,7290,7031,6700,8908,4214,5135,5002,4900,8043,6205,8800)
depois=c(6310,6295,4497,5182,4273,6591,6425,4600,5407,5509,4900,5100,4900)

shapiro.test(antes)
shapiro.test(depois)
#ambos os grupos estão distribuidos normalmente.
#considerando a hipotese alternativa sendo a média dos pesos dos frangos antes
#é diferente a média dos pesos dos frangos depois, usaremos o teste t pareado,
#para comparar as médias com elementos pareados.
t.test(antes, depois, paired = TRUE)

#O p-valor foi de 0.068 assumimos assim a hipótese nula, temos menos de 95%
#de chance de certeza em dizer que a ração alterou o peso dos frangos.