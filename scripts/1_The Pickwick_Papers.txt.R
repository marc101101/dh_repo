install.packages("textreuse")

library(textreuse)
library(tm)

# set working directory to pilot-corpus
setwd("")

# [1] IMPORT TEXTS

# 1.1 read in hypertext: Kipling, Rudyard - Complete 317000 tokens)
hyper_input <- readLines("../texte/1_The_Pickwick_Papers.txt")
hypertext <- toString(hyper_input) 
  #hypertext

# 1.2 read in top 10 hypotexts + all sonnets
input <- readLines("../all-in-one.txt")
hypotext <- toString(input) 
  #hypotext


# [2] PREPROCESSING PHASE 
# Although some preprocessing options are also applied automatically when tokenizing the hypotext
# as ngrams, it is safer to apply the tm-package preprocessing to BOTH, hypertext and hypotext before
# calculating the alignments. This way we make sure that whitespaces and punctuation marks are processed
# the same in both texts.

# 2.1 convert texts from string to corpus format, so that the tm-functions can be applied
hypertext_corpus <- Corpus(VectorSource(hypertext))
hypotext_corpus <- Corpus(VectorSource(hypotext))

# 2.2 transform to lower case
hypertext_corpus <- tm_map(hypertext_corpus, content_transformer(tolower)) 
hypotext_corpus <- tm_map(hypotext_corpus, content_transformer(tolower)) 

# 2.3 remove special characters and punctuation
#  keep intra word punctuation (e.g. aren't) and intra word dashes (worm-eaten)
hypertext_corpus <- tm_map(hypertext_corpus, removePunctuation, 
                           preserve_intra_word_contractions = TRUE, 
                           preserve_intra_word_dashes = TRUE)
hypotext_corpus <- tm_map(hypotext_corpus, removePunctuation, 
                          preserve_intra_word_contractions = TRUE, 
                          preserve_intra_word_dashes = TRUE)

# [3] ALIGNMENT

# specify input for hypertext and hypotext to be compared by the alignment algorithm
hypotext_ngrams <- tokenize_ngrams(toString(hypotext_corpus), n = 9)
hypertext <- toString(hypertext_corpus)
length(hypotext_ngrams)

#all in one - tests
hypotext_ngrams[30676] #last ngram in hamlet
hypotext_ngrams[47878] #last ngram in macbeth
hypotext_ngrams[69245] #last ngram in merchantvenice
hypotext_ngrams[85811] #last ngram in midsummer
hypotext_ngrams[107042] #last ngram in muchado
hypotext_ngrams[126828] #last ngram in juliuscaesar
hypotext_ngrams[143534] #last ngram in tempest
hypotext_ngrams[168175] #last ngram in romeojuliet
hypotext_ngrams[194448] #last ngram in kinglear
hypotext_ngrams[220851] #last ngram in othello
hypotext_ngrams[238443] #last ngram in sonnets



#----------------------
# START ANALYSIS HERE 
#----------------------

# Loop through alignments: ngram-hypotext vs. whole hypertext

count <- 1

# measure time for calculationg alignments - start timer
start_time <- Sys.time()

while(count < length(hypotext_ngrams)) {
  token <- hypotext_ngrams[count]
  #"try" to catch errors
  try(result <- align_local(token, hypertext))
  #In case an error occurs, only the counter is incremented
  if (class(result) == "try-error") {
    count <- count + 3
  } else {
    
      if (result$score >= 7) {
        previouscount <- count - 3
        previoustoken <- hypotext_ngrams[previouscount]
        previousresult <- (align_local(previoustoken, hypertext))
        
        if (result$a_edits != previousresult$a_edits) {
          print(paste("Counter: ", count, "of", length(hypotext_ngrams)))
          print(paste("Alignment score: ", result$score))
          print(paste("Ngram: ", token))
          print(paste("Text A: ", result$a_edits))
          print(paste("Text B: ", result$b_edits))
          cat("\n")
          
            #write the name of the play to CSV depending on the current ngram counter
            if (count >= 1 & count <= 30676) {play <- "Hamlet"}
            else if (count >= 30677 & count <= 47878) {play <- "Macbeth"}
            else if (count >= 47879 & count <= 69245) {play <- "Merchant of Venice"}
            else if (count >= 69246 & count <= 85811) {play <- "Midsummernight's Dream"}
            else if (count >= 85812 & count <= 107042) {play <- "Much Ado about Nothing"}
            else if (count >= 107043 & count <= 126828) {play <- "Julius Caesar"}      
            else if (count >= 126829 & count <= 143534) {play <- "The Tempest"}
            else if (count >= 143535 & count <= 168175) {play <- "Romeo and Juliet"}
            else if (count >= 168176 & count <= 194448) {play <- "King Lear"}
            else if (count >= 194449 & count <= 220851) {play <- "Othello"}
            else if (count >= 220852 & count <= 238443) {play <- "Sonnets (154)"} else {
              play <- "unknown"}
            
          m = data.frame(play, count, result$score, token, result$a_edits, result$b_edits)
          write.table(m, file="../csv/1_The_Pickwick_Papers.txt.csv", 
                      append=TRUE, col.names = FALSE, sep = ',')
        }
      } 
  }
  # 9grams overlap in 3-word-steps
  count <- count + 3 
}

# measure time for calculationg alignments - end timer
end_time <- Sys.time()
end_time - start_time

