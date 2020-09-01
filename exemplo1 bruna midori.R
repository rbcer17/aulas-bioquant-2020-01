#Exercício da Bruna Midori
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
#use os comandos de media e variancia para obter a media e variancia da
#altura, volume e diametro das 30 arvores
mean(trees$Girth)
mean(trees$Height)
mean(trees$Volume)

var(trees$Girth)
var(trees$Height)
var(trees$Volume)
#grave o script com um novo nome e extensao .R
#sincronize com o seu repositorio no github e faca um pull request para 
#o repositorio mestre
#edite o documento do teams fazendo o seu passo a passo