#setwd("D:\\Dropbox\\shiny surveys\\replication code\\info_battery")
library(curl)
library(shiny)
library(rsconnect)
library(digest)
library(shinyBS)
library(rdrop2)
library(shinyjs)
library(catSurv)

load("premade_cat_npi.RData")
load("npi_battery.rda") # this will eventually be in the main package

# Read the survey questions
Qlist <- read.csv("Qlist.csv", sep="\t")

# Generate a random response code
randstring <- function(n=1, length=12){
  randomString <- c(1:n)
  for (i in 1:n){
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),length, replace=TRUE), collapse="")
  }
  return(randomString)
}

# Set up the results object
results <<- rep("", nrow(Qlist)+6)
names(results)  <<- c("pid", "vote2", "npi1", "npi2", "npi3", "npi4", "npi5", "score")


shinyServer(function(input, output) {
  

  ## If we're still in the Qlist, use the radio buttom options from there
    option.list <- reactive({
    if(input$Click.Counter >= 1 & input$Click.Counter <= nrow(Qlist)){
      qlist <- Qlist[input$Click.Counter,3:ncol(Qlist)]
      # Also, convert the option list to matrix. 
      as.matrix(qlist[qlist!=""])
    } else{ # if we're in the NPI battery, show the items from there
      qid = selectItem(cat)$next_item
      #print(npi_battery[[qid]][2:3])
      npi_battery[[qid]][2:3]
    }
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  # Again, produce Qlist questions if we're there
  # If we're at the NPI battery, print those Qs instead
    
  output$question <- renderText({
    if(input$Click.Counter >= 1 & input$Click.Counter <= nrow(Qlist)){
      paste0(
        "Q", input$Click.Counter,":", 
        Qlist[input$Click.Counter,2]
      )
    } else {
      qid = selectItem(cat)$next_item
      #print(npi_battery[[qid]][1])
      
      paste0(
        "Q", input$Click.Counter,":", 
        npi_battery[[qid]][1]
      )
    }
  })
  
  
  ## This part does a lot of the work. 
  ## It determines what the server does after the Next button is clicked:
  ## Either it stores Qlist items
  ## Or it updates the cat object then stores the results
  
  observeEvent(input$Click.Counter, { 
    print(input$Click.Counter[1])
    #shinyjs::disable("Click.Counter")
    if(input$Click.Counter == 1){
      print("Initializing...")
    }
    
    if(input$Click.Counter > 1 & input$Click.Counter <= (nrow(Qlist)+1)){
      
      results[input$Click.Counter-1] <<- input$survey
      print(results)
      #print(input$Click.Counter[1])
      
    } 
    
    if(input$Click.Counter > (nrow(Qlist) +1) & input$Click.Counter <= nrow(Qlist) + 5){
      cat <<- storeAnswer(cat, item = selectItem(cat)$next_item, answer = which(npi_battery[[selectItem(cat)$next_item]] == input$survey) - 2)
      results[input$Click.Counter-1] <<- input$survey
      print(results)
      #print(input$Click.Counter[1])
    }
    
    if(input$Click.Counter > nrow(Qlist) + 5){
      print("Finalizing...")
      cat <<- storeAnswer(cat, item = selectItem(cat)$next_item, answer = which(npi_battery[[selectItem(cat)$next_item]] == input$survey) - 2)
      results[input$Click.Counter-1] <<- input$survey
      score = estimateTheta(cat)
      results$score = score
      print(results)
    }
    
  })
  
  #observeEvent(input$survey,{
  #  shinyjs::enable("Click.Counter")
  #})
  
  
  
  output$MainAction <- renderUI( {
    dynamicUi()
  })
  

  # Dynamic UI is the interface which changes as the survey
  # progresses.  
  dynamicUi <- reactive({
    # Initially it shows a welcome message. 
    if (input$Click.Counter==0){
      return(
        list(
          h5("Thank you for beginning our survey! Please answer every question. If you skip questions, you may not receive payment.")
        )
      )
    }
    
    # Once the next button has been clicked once we see each question
    # of the survey.
    # This shouldn't be modified except for the +5 at the end
    
    if (input$Click.Counter>0 & input$Click.Counter<=nrow(Qlist)+5){
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
    
    ### This below is all the finishing behavior
    ## Modify the +6 as necessary
    ## Uncomment the save and upload lines for launch
    
    if (input$Click.Counter>=nrow(Qlist)+6){
      shinyjs::disable("Click.Counter")
      uniqueID = randstring()
      fn = paste(uniqueID, ".RData", sep="")
      #save(results, file=fn)
      #drop_upload(fn, dest="your chosen destination", overwrite=FALSE)
      #print(result)
      #uniqueID = randstring()
      return(list(
        h4(paste("All finished. Thanks for completing this survey! Your ID is:", uniqueID)),
        h4("Please copy your ID to supply to MTurk, then close this window.")))
    }
    
  })    
})

