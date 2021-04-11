library(rsconnect)
library(shiny)
library(ggmap)
library(sp)

## Code to match respondents to their Reps
load("rep_data.RData")
lookup_rep = function(address){
  g = ggmap::geocode(address)
  coordinates(g) <- ~ lon + lat
  proj4string(sp)<-proj4string(districts)
  temp = function(x){
    v1 = SpatialPolygons(list(districts@polygons[[x]]))
    proj4string(v1) = proj4string(districts)
    !is.na(sp::over(sp, v1))
  }
  out = which(sapply(1:437, temp))
  did = d2$dist_id[out]
  rep = d3$rep[d3$dist_id==did][1]
  return(rep)
}

# Covariate question list
Qlist <- read.csv("Qlist.csv", sep="\t")

# set up authentication and uncomment the below line with your own
#token = readRDS("droptoken.rds")

# Set up the response code
randstring <- function(n=1, length=12){
  randomString <- c(1:n)
  for (i in 1:n){
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),length, replace=TRUE), collapse="")
  }
  return(randomString)
}


shinyServer(function(input, output) {
  #drop_auth(rdstoken="droptoken.rds")
  

  # Include them in the results
  results <<- c(rep(NA, 20)) # excessively long just to be safe
  
  
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
    if(input$Click.Counter > 1 & input$Click.Counter <= nrow(Qlist)){ # changed 11 to 12
      
      results[input$Click.Counter-1] <<- input$survey
      print(results)
      print(input$Click.Counter[1])
      
    } else if(input$Click.Counter > nrow(Qlist)){
      print("Finished with covariates")
      results[input$Click.Counter-1] <<- input$survey
      rep <<- lookup_rep(input$survey)
      print(rep)
      vign <<- paste(rep, " is your Member of Congress. How well would you say ", rep, " has done at representing your interests?", sep="")
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
    
    
    
    # If the basic covariates are done, collect the address
    if (input$Click.Counter==(nrow(Qlist)+1)){
      return(
        list(
          textInput("survey", "Please enter your full address, including city and ZIP code. We will only use this to identify your Member of Congress and will not store this information.",
                      width="400px", placeholder = "123 Main Street, New York, NY")
        )
      )
    }
    # After the address is collected, gauge representative support
    # Alternately, add a randomizer to add vignette information (recording the treatment)
    # Then ask the same question
    if (input$Click.Counter==(nrow(Qlist)+2)){
      return(
        list(
          h5(textOutput(vign)),
          radioButtons("survey", "Please Select:", 
                       c("Strongly support", "Somewhat support", "Neither support nor oppose",
                         "Somewhat oppose", "Strongly oppose"), # No "Prefer Not To Answer" 
                       selected=character(0))
        )
      )
    }
    
    

    # Uncomment the drop_auth and drop_upload lines before the survey is live
    if (input$Click.Counter>=nrow(Qlist)+3){
      Sys.sleep(.5)
      shinyjs::disable("Click.Counter")
      uniqueID = randstring()
      fn = paste(uniqueID, ".RData", sep="")
      save(results, file=fn)
      #drop_auth()
      #drop_upload(fn, path="Media Bias Project/MTurk Validation/results2", mode="add", dtoken=token)
      return(list(
        h4(paste("You are all finished.  Thank you for completing this survey! Your ID is:", uniqueID)),
        h4("Please copy your ID to supply to MTurk, then close this window."),
        h4("If you have any questions or concerns about this survey, please contact aaronkaufman@fas.harvard.edu")))
    }
    
  })    
})
