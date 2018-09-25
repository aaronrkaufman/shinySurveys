#setwd("D:/Dropbox/Compactness Shared/Code/shiny_dragndrop")

library(curl)
library(shiny)
library(rsconnect)
library(digest)
library(shinyBS)
library(rdrop2)
library(shinyjs)
library(shinyjqui)

starttime = Sys.time()


server <- function(input, output, session) {
  shinyjs::disable("Click.Counter")
  
 
  v <- reactiveValues(counter = 1L)
  observeEvent(input$dest1_order, {
      v$counter <- v$counter + 1L
      print(v$counter)
      print(input$dest1_order)
      if(v$counter >= 11){
        shinyjs::enable("Click.Counter")
      }
  })


  observeEvent(input$Click.Counter, {
    if(input$Click.Counter==2){
      hide("Click.Counter")
    }
  })
  
  output$MainAction = renderUI({
    dynamicUI()
  })
  
  dynamicUI = reactive({
    ## set up responseID
    uniqueID <- parseQueryString(session$clientData$url_search)$id
    print(uniqueID)
    
    if(input$Click.Counter == 0){
      return(
        list(
          tags$br(),
          tags$div(id = "toptext",
            tags$b("The law requires that legislative districts for the US congress and many state legislatures be “compact”. The law does not say exactly what district compactness is, but generally, people think they know it when they see it. One dictionary definition of compactness is “joined or packed together closely and firmly united; dense; arranged efficiently within a relatively small space.”"),
            tags$br(), tags$br(),
            tags$b("Here’s your task: Below is a group of legislative districts, randomly ordered. Order the districts from most compact (at the top left) to least compact (at the bottom right) according to your own best judgement, by dragging and dropping. We have many individuals performing this task, and the more your ranks are similar to others like yourself, the better you will have done.")
            ),
          tags$br(),
          tags$br(),
          orderInput('dest1', label ='MOST Compact Here', width="620px", items = sample(1:20), as_souruce=TRUE,
                     placeholder = 'Drag districts here. You can reorder them at any time.'),
          tags$div(id = "bottomtext",
                   tags$b("...........................................................................................................", style="color:white"),
                   tags$b("LEAST Compact Here", align="right")
                   )
          #verbatimTextOutput('order')
        )
      )
    }
    if(input$Click.Counter == 1){
      a = input$dest1_order
      return(
        list(
          tags$br(),
          tags$div(id = "toptext2",
                   tags$b("Last step: Verify your decisions. Here is a new view of the same districts, in the same order. Carefully consider the position of each district, from top to bottom. Do one final set of adjustments, by dragging and dropping, until every district is more compact than the one below it.")
          ),
          tags$br(),
          tags$br(),
          orderInput('dest2', label ='MOST Compact Here', width="130px", items = a, as_souruce=TRUE,
                     placeholder = 'Drag districts here. You can reorder them at any time.'),
          tags$div(id = "bottomtext",
                   #tags$b(".................................................................................................", style="color:white"),
                   tags$b("LEAST Compact Here", align="right")
          )
          #verbatimTextOutput('order')
        )
      )
    }
    
    
    
    if(input$Click.Counter == 2){
      print("hello")
      hide("Click.Counter")
      print(input$dest_order)
      #uniqueID = randstring()
      stoptime = Sys.time()
      fn = paste(uniqueID, ".RData", sep="")
      a = input$dest2_order
      a = c(a, v$counter, starttime, stoptime)
      save(a, file=fn)
      drop_upload(fn, dest="Compactness Shared/Data/shiny_results4", overwrite=FALSE, autorename=TRUE)
      return(
        list(
          h4("Thank you for taking our survey!"),
          h4("Your identity, and all your responses, will remain confidential."),
          h4("If you have any questions or comments, please email: aaronkaufman@fas.harvard.edu")
        )
      )
    }
    
  })

    
}



#rsconnect::deployApp( appName = "CompactnessStudy", account="hmdc")



