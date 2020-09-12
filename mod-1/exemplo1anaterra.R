#Primeiro exercicio de Biologia Quantitativa
#O simbolo jogo da velha e usado para indicar linha de comentario
#se houver o simbolo jogo da velha a linha e ignorada pelo R
#O R tem um conjunto de dados exemplo chamado trees
#Vamos visualizar:
View(trees)
#Agora vamos buscar os dados sumarizados
summary(trees)
#Exercicio para os alunos:
#use o comando plot do R para visualizar os dados deste conjunto de dados
plot(trees) 
#para aplicar o comando plot, escrevo "plot(objeto)" e aperto ctrl + enter para rodar no console. É possível visualizar o gráfico gerado na aba "plots"
#use os comandos de media e variancia para obter a media e variancia da
#altura, volume e diametro das 30 arvores
mean(trees$Girth)
#Para obter a média, é necessário utilizar o comando "mean". O argumento que será utilizado é a base de dados "trees". Para selecionar um vetor dessa lista, usamos "$". No caso, queremos obter apenas a média da circunferência. 
mean(trees$Height)
mean(trees$Volume)
#Para obter a variância, utilizamos o comando "var". Da mesma maneira, utilizamos o argumento "trees" e usamos "$" para selecionar o vetor desejado. 
var(trees$Girth)
var(trees$Height)
var(trees$Volume)
#grave o script com um novo nome e extensao .R
#sincronize com o seu repositorio no github e faca um pull request para 
#o repositorio mestre
#edite o documento do teams fazendo o seu passo a passo
