# model creation
library(tidyverse)
library(data.table)
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

corpus_99 <- corpus(c(tw1, ne1, bl1))

# tokens 99k
toks99 <- tokens(corpus_99, remove_punct = T, remove_numbers = T, remove_symbols = T,
                 remove_url = T)

toks99_stem <- tokens_wordstem(toks99, language = "en")


# n-grams
gram2 <- tokens_ngrams(toks99_stem, n = 2)
gram3 <- tokens_ngrams(toks99_stem, n = 3)

# dfm
dfm1 <- dfm(toks99_stem)
dfm2 <- dfm(gram2)
dfm3 <- dfm(gram3)

dfm1 <- dfm_trim(dfm1, min_termfreq = 8, max_docfreq = 0.05, docfreq_type = "prop")
dfm2 <- dfm_trim(dfm2, min_termfreq = 6, max_docfreq = 0.1, docfreq_type = "prop")
dfm3 <- dfm_trim(dfm3, min_termfreq = 3, max_docfreq = 0.2, docfreq_type = "prop")

# words
sums1 <- colSums(dfm1)
sums2 <- colSums(dfm2)
sums3 <- colSums(dfm3)

words1 <- data.table(word_1 = names(sums1), count = sums1)

words2 <- data.table(
  word_1 = sapply(strsplit(names(sums2), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums2), "_", fixed = TRUE), '[[', 2),
  count = sums2)

words3 <- data.table(
  word_1 = sapply(strsplit(names(sums3), "_", fixed = TRUE), '[[', 1),
  word_2 = sapply(strsplit(names(sums3), "_", fixed = TRUE), '[[', 2),
  word_3 = sapply(strsplit(names(sums3), "_", fixed = TRUE), '[[', 3),
  count = sums3)

setkey(words1, word_1)
setkey(words2, word_1, word_2)
setkey(words3, word_1, word_2, word_3)


