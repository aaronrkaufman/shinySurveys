setwd("D:\\Dropbox\\Media Bias Project\\MTurk Validation\\Survey Framework v2")
library(curl)
library(shiny)
library(shinyapps)
library(digest)
library(shinyBS)
library(blockTools)
library(rdrop2)
library(shinyjs)
require(tm)
require(NLP)
require(openNLP)


options(stringsAsFactors = FALSE)

matches = read.csv("../final_sample_n200_102117.csv")

convert_text_to_sentences <- function(text, lang = "en") {
  # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'.
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
  # Convert text to class String from package NLP
  text <- as.String(text)
  # Sentence boundaries in text
  sentence.boundaries <- annotate(text, sentence_token_annotator)
  # Extract sentences
  sentences <- text[sentence.boundaries]
  # return sentences
  return(sentences)
}


dat = matches
dat$Fox.Title = as.character(dat$Fox.Title)
dat$Fox.Text = as.character(dat$Fox.Text)
dat$CNN.Title = as.character(dat$CNN.Title)
dat$CNN.Text = as.character(dat$CNN.Text)
dat$Fox.Title = paste("HEADLINE:", dat$Fox.Title)
dat$CNN.Title = paste("HEADLINE:", dat$CNN.Title)
dat$Fox.Text = gsub("Story highlights", "", dat$Fox.Text)
dat$CNN.Text = gsub("Story highlights", "", dat$CNN.Text)
dat$Fox.Text = gsub("EXPAND GALLERY", "", dat$Fox.Text)
dat$CNN.Text = gsub("EXPAND GALLERY", "", dat$CNN.Text)
dat$Fox.Text = gsub("Never autoplay videos", "", dat$Fox.Text)
dat$CNN.Text = gsub("Never autoplay videos", "", dat$CNN.Text)
dat$Fox.Text = gsub("NOW PLAYING", "", dat$Fox.Text)
dat$CNN.Text = gsub("NOW PLAYING", "", dat$CNN.Text)
dat$Fox.Text = gsub("ADVERTISEMENT", "", dat$Fox.Text)
dat$CNN.Text = gsub("ADVERTISEMENT", "", dat$CNN.Text)
dat$Fox.Text = gsub("\\(CNN\\)", "", dat$Fox.Text)
dat$CNN.Text = gsub("\\(CNN\\)", "", dat$CNN.Text)
dat$Fox.Text = gsub("([a-z])([A-Z])", "\\1 \\2", dat$Fox.Text)
dat$CNN.Text = gsub("([a-z])([A-Z])", "\\1 \\2", dat$CNN.Text)
dat$Fox.Text = gsub("([[:punct:]])([A-Z])", "\\1 \\2", dat$Fox.Text)
dat$CNN.Text = gsub("([[:punct:]])([A-Z])", "\\1 \\2", dat$CNN.Text)

dat$Fox.Text = sapply(dat$Fox.Text, FUN=function(x) paste(convert_text_to_sentences(x)[1:3], collapse="  "))
dat$CNN.Text = sapply(dat$CNN.Text, FUN=function(x) paste(convert_text_to_sentences(x)[1:3], collapse="  "))
dat$Fox.Text = paste(dat$Fox.Text, " [continued]...", sep="")
dat$CNN.Text = paste(dat$CNN.Text, " [continued]...", sep="")

# eq1 = function(x){ # x indexes a row of dat
#   a = nchar(dat$Fox.Text[x])
#   b = nchar(dat$CNN.Text[x])
#   pad = paste("\n", rep("=", round(abs(a-b)*.6)), collapse="")
#   if(a > b){
#     dat$CNN.Text[x] <<- paste0(dat$CNN.Text[x], pad, ".")
#   } else if(b > a){
#     dat$Fox.Text[x] <<- paste0(dat$Fox.Text[x], pad, ".")
#   } else{
#     return()
#   }
# }
# 
# for(i in 1:180){
#   eq1(i)
#   print(i)
# }

htmls = list()
for(i in 1:nrow(dat)){
  #assign(n,cbind(`Article 1` = c(dat$Fox.Title[i], dat$Fox.Text[i]), `Article 2`=c(dat$CNN.Title[i], dat$CNN.Text[i])))
  ht = paste("<table class='fixed'> <col width='500px' /><col width='25px' /><col width='500px' /> <tr style='border-bottom: 1px solid #000;'><td>",
             dat$Fox.Title[i],
             '</td><td></td><td>',
             dat$CNN.Title[i],
             "</td></tr><hr><tr style='border-bottom: 1px solid #000;'><td>",
             dat$Fox.Text[i],
             '</td><td></td><td>',
             dat$CNN.Text[i],
             '</td></tr></table>')
  htmls[[i]] = ht
}


save(htmls, file="../qualtrics_htmls.RData")
save(dat, file="dat.RData")
