#setwd("D:\\Dropbox\\seqblock\\Survey Framework")
library(curl)
library(shiny)
library(rsconnect)
library(digest)
library(shinyBS)
library(blockTools)
library(rdrop2)
library(shinyjs)

# Read the survey questions
Qlist <- read.csv("Qlist.csv", sep="\t")


control_prompt = "The U.S. president said the United States would stay out of the conflict. The attacking country continued to invade. In the end, the U.S. president did not send troops, and the attacking country took over its neighbor."
treatment_prompt = "The U.S. president said that if the attack continued, the U.S. military would push out the invaders. The attacking country continued to invade. In the end, the U.S. president did not send troops, and the attacking country took over its neighbor."

regime = c(" a dictator ", "a democratically-elected government ")
motive = c("In declaring war, they have cited a need to get more power and resources.", "In declaring war, they have cited  a long-standing historical feud.")
power = c("The invading country had a strong military such that it would have required a major effort for the United States to help push them out.","The invading country had a weak military, which the United States could have repelled without major effort.")
result = c("Top advisors believed that a victory by the invading country would hurt the safety and economic security of the United States.", "Top advisors believed that a victory by the invading country would not affect the safety and economic security of the United States.")

randstring <- function(n=1, length=12){
  randomString <- c(1:n)
  for (i in 1:n){
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),length, replace=TRUE), collapse="")
  }
  return(randomString)
}

results <<- rep("", nrow(Qlist)+6)
names(results)  <<- c("pid", "vote2", "reg", "mot", "pow", "res", "treat", "outcome")

shinyServer(function(input, output) {
  
  
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
    #shinyjs::disable("Click.Counter")
    if(input$Click.Counter > 1 & input$Click.Counter <= nrow(Qlist)+1){
      
      results[input$Click.Counter-1] <<- input$survey
      print(results)
      print(input$Click.Counter[1])
      
    } else if(input$Click.Counter > nrow(Qlist) + 1){
      print("Finished with covariates")
    } else {
      print("Initializing...")
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
    if (input$Click.Counter==0)
      return(
        list(
          h5("Thank you for beginning our survey! Please answer every question. If you skip questions, you may not receive payment.")
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
    
    
    
    # Finally we see results of the survey as well as a
    # download button.
    if (input$Click.Counter==(nrow(Qlist)+1)){ #is +1 what I want?
      shinyjs::disable("Click.Counter")
      token <- readRDS("droptoken.rds")

      # generate conditions to block on 
      reg = regime[rbinom(1,1,0.5)+1]
      mot = motive[rbinom(1,1,0.5)+1]
      pow = power[rbinom(1,1,0.5)+1]
      res = result[rbinom(1,1,0.5)+1]
      
      results[3:6] <<- c(reg, mot, pow, res) 
      intro = paste("A country led by", reg, "sent its military to take over a neighboring country. ", mot, pow, res, sep=" ")
      drop_get("seqblock/sdata.RData", overwrite=T) 
      ## next: determine the blocktools update
      load("sdata.RData")
      last = max(bdata[[1]]$ID)
      results[duplicated(results)] = "NA"
      ##print(results)
      bdata = seqblock(query = FALSE, object = "sdata.RData", id.vars = "ID", id.vals =last+1,
                       exact.vars=c("pid",  "vote2",  "regime", "motive", "power", "result"),
                       exact.vals = c(results["pid"],  results["vote2"], reg, mot, pow, res), 
                       file.name = "sdata.RData")
      #save(bdata, file="sdata.RData") # unnecessary
      treat = bdata$x$Tr[nrow(bdata$x)]
      results[15] <<- treat
      #print(results)
      drop_upload("sdata.RData", dest="seqblock", overwrite=T)
      
      ## 
      if(treat=="Treatment 1"){
        tr = 1
        shinyjs::enable("Click.Counter")
        #print("Treatment 1")
        return(
          list(
            h4("You will read about a situation our country has faced many times in the past and will probably face again. Different leaders have handled the situation in different ways.  We will describe one approach U.S. leaders have taken, and ask whether you approve or disapprove."),
            h4(intro),
            h4(control_prompt),
            radioButtons("survey", label="Do you approve, disapprove, or neither approve nor disapprove of the way the U.S. president handled the situation?",
                         choices = c("Strongly Approve", "Somewhat Approve", "Somewhat Disapprove", "Strongly Disapprove", "Neither approve nor disapprove"),
                         selected=character(0))#,
            #actionButton("submit2", "Next")
          )
        )
        
        
        
        
        
      } else if(treat == "Treatment 2"){
        tr = 2
        shinyjs::enable("Click.Counter")
        #print("Treatment 2")
        return(
          list(
            h4("You will read about a situation our country has faced many times in the past and will probably face again. Different leaders have handled the situation in different ways.  We will describe one approach U.S. leaders have taken, and ask whether you approve or disapprove."),
            h4(intro),
            h4(treatment_prompt),
            radioButtons("survey", label="Do you approve, disapprove, or neither approve nor disapprove of the way the U.S. president handled the situation?",
                         choices = c("Strongly Approve", "Somewhat Approve", "Somewhat Disapprove", "Strongly Disapprove", "Neither approve nor disapprove"),
                         selected=character(0))#,
            #actionButton("submit2", "Next")
          )
          # weirdness: why aren't the prompts or radio buttons showing up?
          
        )
      } else {
        #print ("Error somewhere")
        tr = 3
        output$things1 = renderText({
          print("There has been a weird error. Please email me at aaronkaufman@fas.harvard.edu and let me know!")}
        )
      }
    }
    
    
    if (input$Click.Counter>=nrow(Qlist)+2){
      results[16] <<- input$survey
      shinyjs::disable("Click.Counter")
      #print(input$survey)
      uniqueID = randstring()
      fn = paste(uniqueID, ".RData", sep="")
      save(results, file=fn)
      drop_upload(fn, dest="seqblock/results_3_17", overwrite=FALSE)
      #print(result)
      uniqueID = randstring()
      return(list(
        h4(paste("All finished. Thanks for completing this survey! Your ID is:", uniqueID)),
        h4("Please copy your ID to supply to MTurk, then close this window.")))
    }
    
  })    
})


### For testing this, look at the sdata.RData object in the seqblock main directory, not the one in Survey Framework