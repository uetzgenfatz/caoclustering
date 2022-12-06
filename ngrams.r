## ---------------------------------------------
## Quantitative analysis of the CAO data 
## Author: Magnus B. Birkenes
## Modifications and updates by Oliver Schallert
## Date Created: 2022-12-06
## ---------------------------------------------

library(quanteda)
library(qlcVisualize)
library(readtext)
library(quanteda.textstats)
library(rworldmap)

# qlcVisualize is an experimental Package by Michael Cysouw. We use it for visualizing the distance matrix as color hues. Unfortunately, this package is not available via CRAN, so you have to install it yourself:

# devtools::install_github("cysouw/qlcVisualize")

# Specifiy or change wd, if you like

loc <- read.csv("/Users/uetzelsche/Desktop/CAO_Ngram/data/orte.csv", sep=",", stringsAsFactors = FALSE, na.strings="", header = TRUE)

rownames(loc) <- loc$id
loc$id <- NULL

setwd("/Users/uetzelsche/Desktop/CAO_Ngram/")

texts <- readtext(paste0(getwd(), "/corpus"), encoding = "UTF-8")

# texts (all)
corpus <- corpus(texts, docid_field = "doc_id")

# Tokenize, remove punctuation
ngrams <- tokens(corpus, what = "character", remove_separators=TRUE, remove_punct = TRUE)

# Convert to lowercase
ngrams <- tokens_tolower(ngrams)

# Character trigrams. Additionally, we perform a check if the output is correct

toks_ngram <- tokens_ngrams(ngrams, n = 2:4)
head(toks_ngram[[1]], 30)

# Count
dfm <- dfm(toks_ngram)

# Cosine similarity
cosine <- textstat_simil(dfm, method = "cosine")

# Keyness
keyness <- textstat_keyness(dfm)


# Cluster analyses

# hierarchical

cosine <- as.dist(cosine)
hclust <- hclust(1-cosine, method="ward.D")
hclust$labels <- docnames(dfm)
plot(hclust)

hclust2 <- cutree(hclust, 2)
hclust3 <- cutree(hclust, 3)
hclust4 <- cutree(hclust, 4)
hclust5 <- cutree(hclust, 5)
hclust6 <- cutree(hclust, 6)
hclust8 <- cutree(hclust, 8)


# k-means

kmeans2 <- kmeans(1-cosine, 2)
kmeans3 <- kmeans(1-cosine, 3)
kmeans4 <- kmeans(1-cosine, 4)
kmeans5 <- kmeans(1-cosine, 5)
kmeans6 <- kmeans(1-cosine, 6)

# Voronoi-plots
window <- qlcVisualize::hullToOwin(loc, shift = 0.1, alpha = 0.3)
v <- qlcVisualize::voronoi(loc, window)
cols <- qlcVisualize::heeringa(1-cosine)
names(cols)<-texts$doc_id
cols<-cols[rownames(loc)]
cols[is.na(cols)]<-"white"
cols<-c(cols, rep("white"))

# reset par
dev.off()

vmap(v,col=cols, border = NA, outer.border = NA)

# "Heeringa points"

# Remove empty rows
heeringa <- data.frame(id=names(cols), color=cols, row.names=NULL)
heeringa <- heeringa[heeringa$color != "white",]
rownames(heeringa) <- heeringa$id
heeringa$id <- NULL

# Draw heeringa points
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(6, 20), ylim = c(46, 55), asp = 1, main="")
geocluster <- merge(loc, heeringa, by="row.names")
points(geocluster$longitude, geocluster$latitude, col = as.character(geocluster$color), cex = 2, pch = 19)

# Draw cluster solutions: Either pick the hclust2-8 or kmeans2-6 objects

drawcluster <- function (object) {
  newmap <- getMap(resolution = "low")
  plot(newmap, xlim = c(9, 14), ylim = c(46, 54.5), asp = 1, main="")
  geocluster <- merge(loc, object, by="row.names")
  points(geocluster$longitude, geocluster$latitude, col = geocluster$y, pch = 20)
}
