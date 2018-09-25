library(shinyjqui)
library(shinyjs)


ui <-shinyUI(fluidPage(theme = "test.css",
  shinyjs::useShinyjs(),
  includeJqueryUI(),
  #  Application title
  headerPanel("Legislative District Compactness Study"),
  
  #sidebarPanel(
  #  # This is intentionally an empty object.
  #  h5("Created by:"),
  #  tags$a("The Institute for Quantitative Social Science", 
  #         href="http://www.iq.harvard.edu"),
  #  h5("Please do not close this window until you have received your Response Code!"),
  #  h5(tags$b("The law requires that US legislative districts be compact.")),
  #  h5(tags$b("Please rank these districts from left to right, where left is the MOST COMPACT and right is the LEAST COMPACT."))
  #),
  
  
  # Show a table summarizing the values entered
  mainPanel(
    #orderInput('source', 'Districts', items = 1:20,
    #           as_source = FALSE, connect = 'dest'),
    #orderInput('dest', 'Order them here! Left is most compact.', items = NULL, placeholder = 'Drag items here...'),
    #verbatimTextOutput('order'),
    #actionButton("submit", "Submit!")
    
    
    # Main Action is where most everything is happenning in the
    # object (where the welcome message, survey, and results appear)
    tags$div(
      list(
        uiOutput("MainAction"),
        # This displays the action putton Next.
        actionButton("Click.Counter", "Submit!", title="If this is grey, move the remaining districts from the black box to the red box.")#,
        #tags$b("..............................................................", style="color:white")#,
        #tags$b("Least Compact: Bottom Right")
      )
    )
  )
))
