library(tidyverse)
library(readtext)
library(quanteda)
library(gt)

# download and unzip
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download_zip <- "download.zip"

if(!file.exists(download_zip)){
  download.file(url, destfile = download_zip)
}

list <- unzip(download_zip, list = T)

if(!dir.exists("final")){
  unzip(download_zip)
}

# basic exploratory analysis
path_twitter <- list[11,1]
path_news <- list[12,1]
path_blogs <- list[13,1]

file_size <- c(round(file.size(path_twitter)/1024^2, 1), round(file.size(path_news)/1024^2, 1), round(file.size(path_blogs)/1024^2, 1))

con <- file(path_twitter, "r")
tw1 <- readLines(con, 3)
close(con)

con <- file(path_news, "r")
ne1 <- readLines(con, 3)
close(con)

con <- file(path_blogs, "r")
bl1 <- readLines(con, 3)
close(con)

txt_raw <- readtext(list[10,1], docvarsfrom = "filenames", docvarnames = c("language", "source"))
corpus_raw <- corpus(txt_raw) # preparing raw corpus
doc_no <- ndoc(corpus_raw)
sentence_no <- c(nsentence(corpus_raw[3]), nsentence(corpus_raw[2]), nsentence(corpus_raw[1]))

# visualization of exploratory analysis outcome
source_file <- c("US Twitter", "US News", "US Blogs")
param <- data.frame(source_file, file_size, sentence_no)
table_gt <- gt(param)
print(table_gt)

example_text <- data.frame(tw1, ne1, bl1)
names(example_text) <- source_file
example_gt <- example_text|> gt()
print(example_gt)


# sub-setting corpus to be easier to handle in following steps
corpus_sentences <- corpus_reshape(corpus_raw, to = "sentences")

corpus_samples <- corpus_sample(corpus_sentences, size = 0.02*ndoc(corpus_sentences))
corpus_samples_stats <- summary(corpus_samples)

