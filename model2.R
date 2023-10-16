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

# corpus 9.9k
path_twitter <- list[11,1]
path_news <- list[12,1]
path_blogs <- list[13,1]

con <- file(path_twitter, "r")
tw1 <- readLines(con, 330)
close(con)

con <- file(path_news, "r")
ne1 <- readLines(con, 330)
close(con)

con <- file(path_blogs, "r")
bl1 <- readLines(con, 330)
close(con)

corpus_99 <- corpus(c(tw1, ne1, bl1))

# tokens 9.9k
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

dfm1 <- dfm_trim(dfm1, min_termfreq = 5, max_docfreq = 0.05, docfreq_type = "prop")
dfm2 <- dfm_trim(dfm2, min_termfreq = 3, max_docfreq = 0.1, docfreq_type = "prop")
dfm3 <- dfm_trim(dfm3, min_termfreq = 2, max_docfreq = 0.2, docfreq_type = "prop")

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

# probabilities
words1_prob <- words1[, prob := count / sum(words1$count), by = word_1]


# trial
raw_input <- "come in a few"

inp <- tokens(raw_input, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE,
                      remove_url = TRUE)
inp <- tokens_wordstem(inp, language = "en")
inp_words <- as.character(inp[[1]])
inp_l <- length(inp_words)
        
if (inp_l >= 2) {
        w_2 <- inp_words[inp_l - 1]
        w_1 <- inp_words[inp_l]
} 

if (inp_l == 1) {
        w_1 <- inp_words[inp_l]
}        
        
if (inp_l == 0) {
        print("Input must contain at least one word.")
}        

words3_f <- words3 |> filter(word_1 == w_2) |> filter(word_2 == w_1) |> arrange(-count)
words2_f <- words2 |> filter(word_1 == w_1) |> arrange(-count)

if (nrow(words3_f)>0) {
        print(words3_f[1,3])
                
} else if (nrow(words2_f)>0){
        print(words2_f[1,2])
}












