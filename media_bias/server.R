#setwd("D:\\Dropbox\\Media Bias Project\\MTurk Validation\\Survey Framework v2")
library(curl)
library(shiny)
library(rsconnect)
library(digest)
library(shinyBS)
library(rdrop2)
library(shinyjs)
require(tm)
require(NLP)
require(openNLP)

# Read the survey questions
Qlist <- read.csv("Qlist.csv", sep="\t")

# set up authentication
token = readRDS("droptoken.rds")

# Set up the response code
randstring <- function(n=1, length=12){
  randomString <- c(1:n)
  for (i in 1:n){
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),length, replace=TRUE), collapse="")
  }
  return(randomString)
}

#BinToDec <- function(x) 
#  sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))
#html = readLines("https://qrng.anu.edu.au/RawBin.php")[93]
#getseed = BinToDec(as.numeric(substr(html, 1, 20)))
#print(getseed)
#set.seed(getseed)


load("dat.RData") # dat has 3 objects: dat, which we sample from; anchor, which is a row; and attn_check, which is a row
# attn check is a completely unrelated pair

shinyServer(function(input, output) {
  drop_auth(rdstoken="droptoken.rds")
  
  dat2 = dplyr::sample_n(dat[1:599,], size=8)
  dat = rbind(dat2, dat[600:601,])
  dat = dat[c(9,1,2,3,10,4,5,6,7,8),] # anchor first, then 3, then attn, then 5
  
  for(i in 1:10){
    n = paste("table", i, sep="")
    #assign(n,cbind(`Article 1` = c(dat$Fox.Title[i], dat$Fox.Text[i]), `Article 2`=c(dat$CNN.Title[i], dat$CNN.Text[i])))
    ht = paste('<table class="fixed"> <col width="500px" /><col width="25px" /><col width="500px" /> <tr style="border-bottom: 1px solid #000;"><td>',
dat$Fox.Title[i],
'</td><td></td><td>',
dat$CNN.Title[i],
'</td></tr><hr><tr style="border-bottom: 1px solid #000;"><td>',
dat$Fox.Text[i],
'</td><td></td><td>',
dat$CNN.Text[i],
'</td></tr></table>')
    assign(n, ht)
  }
  
  
  
  # Include them in the results
  results <<- c(rep(NA, 2), # COVARIATES
                rep(NA, 10), # responses
                dat$pairID) # match IDs

  
  option.list <- reactive({
    qlist <- Qlist[input$Click.Counter,3:ncol(Qlist)]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix. 
    as.matrix(qlist[qlist!=""])
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  output$question <- renderText({
    paste0(
      "Q", input$Click.Counter,":", 
      Qlist[input$Click.Counter,2]
    )
  })
  
  
  
#  output$save.results <- renderText({
#    # After each click, save the results of the radio buttons.
#    #if ((input$Click.Counter>0)&(input$Click.Counter>!nrow(Qlist)))
#    #print(input$Click.Counter)
#    if ((input$Click.Counter>0)){
#      try(results[input$Click.Counter[1]] <<- input$survey)
#    }
#   # try is used because there is a brief moment in which
#    # the if condition is true but input$survey = NULL
#    
#    #if (input$Click.Counter==nrow(Qlist)+2) {
#    #
#    #   }
#    
#    # Because there has to be a UI object to call this
#    # function I set up render text that distplays the content
#    # of this funciton.
#    
#  })
  
  observeEvent(input$Click.Counter, { 
    #print(input$Click.Counter[1])
    shinyjs::disable("Click.Counter")
    if(input$Click.Counter > 1 & input$Click.Counter <= nrow(Qlist)+12){ # changed 11 to 12
      
      results[input$Click.Counter-1] <<- input$survey
      print(results)
      print(input$Click.Counter[1])
      
    } else if(input$Click.Counter > nrow(Qlist) + 11){
      print("Finished with covariates")
    } else {
      print("Initializing...")
    }
  })
  
  observeEvent(input$survey,{
    shinyjs::enable("Click.Counter")
    # Fix this such that it only enables it if it changes to something other than 5?
  })
  
  
  
  output$MainAction <- renderUI( {
    dynamicUi()
  })
  

  # Dynamic UI is the interface which changes as the survey
  # progresses.  
  dynamicUi <- reactive({
    # Initially it shows a welcome message. 
    if (input$Click.Counter==0)
      return(
        list(
          h5("Thank you for beginning our survey! Please answer every question. If you skip questions, you may not receive payment."),
          h5(tags$b("Read each question carefully. Some of them are an attention checks."))
        )
      )
    
    # Once the next button has been clicked once we see each question
    # of the survey.
    if (input$Click.Counter>0 & input$Click.Counter<=nrow(Qlist)){
      #shinyjs::disable("Click.Counter")
      return(
        list(
          h5(textOutput("question")),
          radioButtons("survey", "Please Select:", 
                       c(option.list()), # No "Prefer Not To Answer" 
                       selected=character(0))
        )
      )
    }
    
    
    
    # THIS IS THE PART WHERE I WRITE THE VIGNETTE COMPARISON QUESTIONS
    if (input$Click.Counter==(nrow(Qlist)+1)){
      return(
        list(
          HTML(table1),
          sliderInput("survey", "How similar, where 0 is no similarity and 10 is identical, are these two articles?",
                       value=5, min=0, max=10, width="400px")
        )
      )
    }
    if (input$Click.Counter==(nrow(Qlist)+2)){ 
      return(
        list(
          HTML(table2),
          sliderInput("survey", "How similar, where 0 is no similarity and 10 is identical, are these two articles?",
                       value=5, min=0, max=10, width="400px")
        )
      )
    }
    if (input$Click.Counter==(nrow(Qlist)+3)){ 
      return(
        list(
          HTML(table3),
          sliderInput("survey", "How similar, where 0 is no similarity and 10 is identical, are these two articles?",
                       value=5, min=0, max=10, width="400px")
        )
      )
    }
    

    if (input$Click.Counter==(nrow(Qlist)+4)){ 
      return(
        list(
          HTML(table4),
          sliderInput("survey", "How similar, where 0 is no similarity and 10 is identical, are these two articles?",
                       value=5, min=0, max=10, width="400px")
        )
      )
    }
    
    if (input$Click.Counter==(nrow(Qlist)+5)){
      return(
        list(
          HTML(table5),
          sliderInput("survey", "How similar, where 0 is no similarity and 10 is identical, are these two articles?",
                       value=5, min=0, max=10, width="400px")
        )
      )
    }
    if (input$Click.Counter==(nrow(Qlist)+6)){
      return(
        list(
          HTML(table6),
          sliderInput("survey", "How similar, where 0 is no similarity and 10 is identical, are these two articles?",
                       value=5, min=0, max=10, width="400px")
        )
      )
    }
    if (input$Click.Counter==(nrow(Qlist)+7)){
      return(
        list(
          HTML(table7),
          sliderInput("survey", "How similar, where 0 is no similarity and 10 is identical, are these two articles?",
                      value=5, min=0, max=10, width="400px")
        )
      )
    }
    if (input$Click.Counter==(nrow(Qlist)+8)){
      return(
        list(
          HTML(table8),
          sliderInput("survey", "How similar, where 0 is no similarity and 10 is identical, are these two articles?",
                      value=5, min=0, max=10, width="400px")
        )
      )
    }
    if (input$Click.Counter==(nrow(Qlist)+9)){
      return(
        list(
          HTML(table9),
          sliderInput("survey", "How similar, where 0 is no similarity and 10 is identical, are these two articles?",
                      value=5, min=0, max=10, width="400px")
        )
      )
    }
    if (input$Click.Counter==(nrow(Qlist)+10)){
      return(
        list(
          HTML(table10),
          sliderInput("survey", "How similar, where 0 is no similarity and 10 is identical, are these two articles?",
                      value=5, min=0, max=10, width="400px")
        )
      )
    }
    
    
    if (input$Click.Counter>=nrow(Qlist)+11){
      Sys.sleep(.5)
      shinyjs::disable("Click.Counter")
      uniqueID = randstring()
      fn = paste(uniqueID, ".RData", sep="")
      save(results, file=fn)
      drop_auth()
      drop_upload(fn, path="Media Bias Project/MTurk Validation/results2", mode="add", dtoken=token)
      #print(result)
      #uniqueID = randstring()
      return(list(
        h4(paste("You are all finished.  Thank you for completing this survey! Your ID is:", uniqueID)),
        h4("Please copy your ID to supply to MTurk, then close this window."),
        h4("If you have any questions or concerns about this survey, please contact aaronkaufman@fas.harvard.edu")))
    }
    
  })    
})

#rsconnect::deployApp(account = "hmdc", appName = "MediaStudy")
#rsconnect::setAccountInfo(name='hmdc',
#                           token='42FD84AE4E787FCF8EFAECE236270A31',
#                          secret='q8WLRkm/Hkt/XQU7hhHhEgWVfOhvyLWZltacfKNM')