# step-by-step direction 
# https://cran.r-project.org/web/packages/bibliometrix/vignettes/bibliometrix-vignette.html

setwd("/Users/Ray_Mac/Documents/R Yang/R_Projects/Lit_Review/data")
library(bibliometrix) 
library(igraph)
D <- readFiles("savedrecs_cat_eval_032619.txt")
M <- convert2df(D, dbsource="isi",format="plaintext")

results <- biblioAnalysis(M, sep = ";")
S <- summary(object = results, k = 10, pause = FALSE)
# plot(x = results, k = 10, pause = FALSE)

# cited references of the first article
M$CR[1]

# top five citations (most frequently cited)
CR <- citations(M, field = "article", sep = ";")
top_5 <- data.frame(CR$Cited[1:5])
names(CR$Cited[1:5]) <- c("Paper", "Frequency")
print(top_5)

# author dominance ranking
DF <- dominance(results, k = 10)
DF

# Authors’ h-index
indices <- Hindex(M, field = "author", elements="ZUCKERMAN EW", sep = ";", years = 10)
indices$H
indices$CitationList
# top 10 
authors=gsub(","," ",names(results$Authors)[1:10])
indices <- Hindex(M, field = "author", elements=authors, sep = ";", years = 50)
indices$H

topAU <- authorProdOverTime(M, k = 10, graph = TRUE)
## Table: Author's productivity per year
head(topAU$dfAU)

# networ analysis 
A <- cocMatrix(M, Field = "SO", sep = ";")
sort(Matrix::colSums(A), decreasing = TRUE)[1:5]

# Citation network
A <- cocMatrix(M, Field = "CR", sep = ".  ")
# Author network
A <- cocMatrix(M, Field = "AU", sep = ";")
# Author keyword network
A <- cocMatrix(M, Field = "DE", sep = ";")
# Keyword Plus network
A <- cocMatrix(M, Field = "ID", sep = ";")

# Bibliographic coupling: tie exists if two papers cite at least one common source
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ".  ")
net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 10, Title = "Authors' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)

# Bibliographic co-citation: tie exists if two papers are cited in a third article are cited in a common third-article
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
g <- graph_from_adjacency_matrix(as.matrix(NetMatrix), mode="undirected") %>% eigen_centrality()
net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 10, Title = "Authors' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)

# Keyword co-occurrences
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

CS <- conceptualStructure(M,field="ID", method="CA", minDegree=4, k.max=8, stemming=FALSE, labelsize=10, documents=10)

# Direct Citation Plot
histResults <- histNetwork(M, min.citations = 10, sep = ";")
net <- histPlot(histResults, n=15, size = 20, labelsize=10, size.cex=TRUE, arrowsize = 0.5, color = TRUE)

