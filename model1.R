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

# corpus 99k and 999k
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

# tokens
toks99 <- tokens(corpus_99k, remove_punct = T, remove_numbers = T, remove_symbols = T,
                 remove_url = T)

toks999 <- tokens(corpus_999k, remove_punct = T, remove_numbers = T, remove_symbols = T,
                 remove_url = T)

# dfm
dfm99 <- dfm(toks99)
ndoc(dfm99)
nfeat(dfm99)

dfm999 <- dfm(toks999)
ndoc(dfm999)
nfeat(dfm999)


