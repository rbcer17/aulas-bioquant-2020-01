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

#use os comandos de media e variancia para obter a media e variancia da altura,volume e diametro das 30 arvores

apply(trees,2,mean) 
apply(trees,2,var)
#função apply(nome do dataset,linha ou coluna, função2)= aplica a função2 em todas as linhas(1) ou colunas(2)

#grave o script com um novo nome e extensao .R
#sincronize com o seu repositorio no github e faca um pull request para 
#o repositorio mestre
#edite o documento do teams fazendo o seu passo a passo