# model creation
library(tidyverse)
library(readtext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda.textmodels)

# download
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download_zip <- "download.zip"

if(!file.exists(download_zip)){
        download.file(url, destfile = download_zip)
}

if(!dir.exists("final")){
        unzip(download_zip)
}

list <- unzip(download_zip, list = T)

# corpus 99k
path_twitter <- list[11,1]
path_news <- list[12,1]
path_blogs <- list[13,1]

con <- file(path_twitter, "r")
tw1 <- readLines(con, 33000)
close(con)

con <- file(path_news, "r")
ne1 <- readLines(con, 33000)
close(con)

con <- file(path_blogs, "r")
bl1 <- readLines(con, 33000)
close(con)

corpus_99k <- corpus(c(tw1, ne1, bl1))

# corpus 999k
con <- file(path_twitter, "r")
tw2 <- readLines(con, 330000, skipNul = TRUE)
close(con)

con <- file(path_news, "r")
ne2 <- readLines(con, 330000)
close(con)

con <- file(path_blogs, "r")
bl2 <- readLines(con, 330000)
close(con)

corpus_999k <- corpus(c(tw2, ne2, bl2))

# tokens 99k
toks99 <- tokens(corpus_99k, remove_punct = T, remove_numbers = T, remove_symbols = T,
                 remove_url = T)
# tokens 999k
toks999 <- tokens(corpus_999k, remove_punct = T, remove_numbers = T, remove_symbols = T,
                 remove_url = T)

# dfm 99k
dfm99 <- dfm(toks99)
ndoc(dfm99)
nfeat(dfm99)
print(dfm99)
head(featnames(dfm99), 20)
head(rowSums(dfm99), 10)
topfeatures(dfm99)

dfm99_prop <- dfm_weight(dfm99, scheme  = "prop")
print(dfm99_prop)


dfm99_tfidf <- dfm_tfidf(dfm99)
print(dfm99_tfidf)

dfm99_nostop <- dfm_select(dfm99, pattern = stopwords("en"), selection = "remove")
print(dfm99_nostop)

dfm99_long <- dfm_keep(dfm99, min_nchar = 3)
print(dfm99_long)
topfeatures(dfm99_long, 10)

dfm99_freq <- dfm_trim(dfm99, min_termfreq = 5)
print(dfm99_freq)

dfm99_docfreq <- dfm_trim(dfm99, max_docfreq = 0.1, docfreq_type = "prop")
print(dfm99_docfreq)
topfeatures(dfm99_docfreq)

# fcm 99k
fcm99 <- fcm(dfm99)
dim(fcm99)
topfeatures(fcm99)
head(fcm99, 10)

feat <- names(topfeatures(fcm99, 50))
fcm99_select <- fcm_select(fcm99, pattern = feat, selection = "keep")
dim(fcm99_select)

size <- log(colSums(dfm_select(dfm99, feat, selection = "keep")))

set.seed(144)
textplot_network(fcm99_select, min_freq = 0.8, vertex_size = size / max(size) * 3)


# n-grams 99k
gram2_99<- tokens_ngrams(toks99, n = 2)
gram3_99<- tokens_ngrams(toks99, n = 3)












# dfm 999k
dfm999 <- dfm(toks999)
ndoc(dfm999)
nfeat(dfm999)




