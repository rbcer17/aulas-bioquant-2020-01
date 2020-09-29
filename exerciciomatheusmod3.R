###Exercicio 1
#Um ALUNO DE PIBIC DA UnB TEM QUE VERIFICAR SE SUA POPULACAO DE AVES 
#PERTENCE A UMA POPULACAO CUJA MEDIA DE TAMANHO DAS ASAS   E 2.2

dados=c(1.6,3.4,2.3,4.1,4.2,6.6,1.7,4.3)
mean(dados)

#A hipótese nula propõe que a média dos dados descritos é de 2,2, porém a média real possui o valor de 3.525
#portanto a hipótese alternativa é verdadeira

###Exercicio 2
#Em um estudo, as seguintes aves de dois locais tiveram suas asas medidas  para verificar se sao da
#mesma populacao
local1=c(16.3,21.2,15.0,10.8,13.9,23.5,11.0,6.4,2.8,12.0)
local2=c(13.3,21.6,27.2,21.7,26.7,33.0,10.9,29.6,9.3,24.3)
#Veja se hß uma diferenþa significativa entre as populacoes dos dois locais usando  a analiise correta
#lembrando de  verificar os pre requisitos necessarios para tal analise

shapiro.test(local1)
shapiro.test(local2)
#Como os valores de p para local1=0.9503 e local2=0.428
#São maiores que 0.05 assumimos que os dados vêm de uma distibuição normal e
#Não há como rejeitar a hipótese nula 



###Exercicio 3
#Uma pesquisadora quer verificar se determinada racao tem efeito sobre o peso de frangos.
#Com isso ele gerou dois bancos de dados: um antes da utilizacao da racao e outro depois.
#Verifique se de fato a racao tem efeito significativo sobre essa populacao aplicando
#o teste  correto,lembrando de  verificar os pre requisitos necessarios para tal analise. Cada individuo está pareado na sequencia antes e depois, ou seja o primeiro peso de cada sequencia corresponde ao individuo 1, o segundo ao individuo 2, e assim por diante.

antes=c(5166,6080,7290,7031,6700,8908,4214,5135,5002,4900,8043,6205,8800)
depois=c(6310,6295,4497,5182,4273,6591,6425,4600,5407,5509,4900,5100,4900)

shapiro.test(antes)
shapiro.test(depois)

#De acordo com os testes, ambos os conjuntos de dados estão em uma distribuição normal
#Para verificar se a ração tem de fato efeito sobre o peso = hipótese alternativa em que as médias
#dos pesos seriam diferentes entre si deve se aplicar um teste t pareado
#entre os dois conjuntos de dados
t.test(antes, depois, paired=TRUE)

#o valor de p foi de 0.06837 
#A hipótese alternativa coloca que as médias dos pesos é diferente
#com este valor de p podemos assumir a hipótese nula, há menos de 95% de chance de assumirmos que ração afetou os pesos dos frangos 











