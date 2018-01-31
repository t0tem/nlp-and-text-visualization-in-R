library(quanteda)
library(data.table)
library(ggplot2)
library(wordcloud)


#function to read the files in binary mode
read_file <- function(path) {
    con <- file(path, open = "rb")
    data <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
    close(con)
    data
}

bigtext <- read_file("t.txt")
bigtext <- iconv(bigtext, to = "ASCII", sub = "")

system.time(
    sentences <- tokens(bigtext, what = "sentence", verbose = TRUE)
)

sentences <- as.character(sentences)

system.time(
    tok <- 
        tokens_tolower(
            tokens(sentences, what = "word", remove_numbers = TRUE,
                   remove_punct = TRUE, remove_symbols = TRUE, 
                   remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE))
)

# optional steps of removing stop-words and word-stemming
tok <- tokens_remove(tok, stopwords("english"))
tok <- tokens_wordstem(tok, language = "english")



#building DFMs for tokens
build.dfm <- function(n) {
    dfm(tokens_ngrams(tok, n = n, concatenator = " "), tolower = FALSE)
}


#building DTs with frequencies
build.dt <- function(dfm) {
    dt <- data.table(ngram = featnames(dfm), count = colSums(dfm))
    dt <- dt[order(-count)]
    dt
}


system.time(dfm1 <- build.dfm(1L))
system.time(dfm2 <- build.dfm(2L))
system.time(dfm3 <- build.dfm(3L))

topfeatures(dfm1)
topfeatures(dfm2)
topfeatures(dfm3)


system.time(dt1 <- build.dt(dfm1))
system.time(dt2 <- build.dt(dfm2))
system.time(dt3 <- build.dt(dfm3))


plot_n_gram <- function(data, title) {
    ggplot(head(data, 20), aes(x = reorder(ngram, count), y = count)) +
        geom_col() + 
        theme(plot.title = element_text(hjust = 0.5),
              axis.title.x = element_blank()) +
        coord_flip() +
        ggtitle(title) + xlab(NULL)
}

g1 <- plot_n_gram(dt1, "Most frequent unigrams")
g2 <- plot_n_gram(dt2, "Most frequent bigrams")
g3 <- plot_n_gram(dt3, "Most frequent trigrams")

print(g1)
print(g2)
print(g3)



plot_wcloud <- function(data, scale, n, title) {
    layout(matrix(c(1, 2), nrow = 2), heights = c(2, 40))
    par(mar = rep(0, 4))
    plot.new()
    text(x = 0.5, y = 0.3, title)
    
    wordcloud(words = data$ngram,
              freq = data$count, 
              colors = brewer.pal(6, "Dark2"), 
              scale = c(scale, 0.2),
              rot.per = 0.3,
              max.words = n,
              random.order = FALSE)
}


plot_wcloud(dt1, 4, 200, "Unigrams Word Cloud")
plot_wcloud(dt2, 4, 120, "Bigrams Word Cloud")
