install.packages('igraph')

library(igraph)

#Lendo a lista de arestas disponibilizada no github e baixada no WD

lista_arestas <- read.csv(file = "got-edges.csv",
                          header = TRUE, sep = ",",
                          dec = ',')

is.data.frame(lista_arestas)

head(lista_arestas)

#Lendo a lista de vértices de arestas disponibilizada no github e baixada no WD

lista_vertices <- read.csv(file = "got-nodes.csv",
                           header = TRUE, sep = ",",
                           dec = ',')

#Verificando se o dataframe foi gerado

is.data.frame(lista_vertices)

head(lista_vertices)

#Grafo

grafo <- graph_from_data_frame(lista_arestas,
                               directed = FALSE,
                               vertices = lista_vertices)

tkplot(grafo,
       layout = layout.lgl,
       vertex.color="light green")

#Atributos dos vértices

vertex_attr(grafo)

#Atributos das arestas

edge_attr(grafo)

#Tirando o label do grafo

tkplot(grafo,
       layout = layout.lgl,
       vertex.color = "light green",
       vertex.label = NA)

#Resumo do grafo

summary(grafo)

degree(grafo)

graus <- degree(grafo)

graus

maiorGrau <- max (graus)

maiorGrau

intermediacaoVertices <- betweenness(grafo)

intermediacaoVertices

maiorIntermediacaoVertices <- max(intermediacaoVertices)

maiorIntermediacaoVertices

diameter(grafo)

diametro <- get_diameter(grafo)

diametro

hist(diametro)

maiorDiametro <- max (diametro)

maiorDiametro

#

sort(degree(grafo), decreasing = TRUE) [1:10]

sort(degree(grafo), decreasing = FALSE) [1:30]

#

sort(closeness(grafo), decreasing = TRUE) [1:10]

sort(betweenness(grafo), decreasing = TRUE) [1:10]

#

degree(grafo)["Robert"]



degree(grafo)[degree(grafo) > 30]

#

degree(grafo)[degree(grafo) == 12]

#

mean(degree(grafo))

#

vertex_attr(grafo)

#

plot(grafo,
     vertex.label = ifelse(V(grafo)$name %in% c("Tyrion", "Jon", "Sansa"),
                           V(grafo)$name,
                           NA),
     vertex.size = ifelse(V(grafo)$name %in% c("Tyrion", "Jon", "Sansa"),
                          30,
                          7),
     vertex.color = ifelse(V(grafo)$name %in% c("Tyrion", "Jon","Sansa"),
                           "red",
                           NA))

#

par(mfrow = c(1,2)
    ,bty  = "n")

#

hist(degree(grafo)
    ,col  ="lightblue"
    ,main = "Aula 25-04-202")

#

stripchart(degree(grafo)
           ,method = "stack"
           ,pch    = 16
           ,cex    = 1.2
           ,at     = 0
           ,col    = "lightblue")

#

eigen_centrality(grafo)

#

sort(eigen_centrality(grafo) $vector, decreasing = TRUE) [1:10]

#

neighbors(grafo,"Jon")

#

degree(grafo)[degree(grafo) == 1]

#

neighborhood(grafo, order = 1, "Amory")

#

neighborhood(grafo, order = 2, "Amory")

#

neighborhood(grafo, order = 1, "Oberyn")

#

grafo_sub <- subgraph.edges(grafo,
                            E(grafo)[inc(c("Amory", "Oberyn"))])

#

plot(grafo_sub, vertex.color="lightgreen", vertez.size=40)


grafo_sub <- subgraph.edges(grafo,
                            E(grafo)[inc(c("Tyrion", "Jon"))])


plot(grafo_sub, vertex.color="lightgreen", vertez.size=40)




#

tkplot(grafo_sub,
       vertex.color="lightgreen",
       vertex.size=40)

#

comunidade1 <- cluster_edge_betweenness(grafo)
modularity(comunidade1)

#

plot(comunidade1
     ,grafo
     ,vertex.label= NA
     ,vertex.size = 10)


membros_da_comunidade1 <- membership(comunidade1)

#

table(membros_da_comunidade1)


plot(comunidade1
     ,grafo
     ,vertex.size = 20
     ,vertex.label = as.character(membros_da_comunidade1))

#

membros_da_comunidade1[membros_da_comunidade1 == 8]

#

membros_da_comunidade1[membros_da_comunidade1 == 6]

sort(degree(grafo), decreasing = TRUE) [1:11]

degree(grafo)[degree(grafo) == 7]



neighbors(grafo, "Janos")

neighborhood(grafo, order = 2, "Janos")

eigen_centrality(grafo)

comunidade2 <- cluster_fast_greedy(grafo)
modularity(comunidade2)

membros_da_comunidade2 <- membership(comunidade2)
membros_da_comunidade2
table(membros_da_comunidade2)
