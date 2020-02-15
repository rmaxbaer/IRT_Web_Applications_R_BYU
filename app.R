# Packages ----------------------------------------------------------------
  if (tryCatch(require("eRm")) == FALSE){
    install.packages("eRm")
  }
  library(eRm)
  
  if (tryCatch(require("ltm")) == FALSE){ #Where do we need this?
    install.packages("ltm")
  }
  library(ltm)
  
  if (tryCatch(require("difR")) == FALSE){ #Where do we need this?
    install.packages("difR")
  }
  library(difR)
  
  if (tryCatch(require("WrightMap")) == FALSE){
    install.packages("WrightMap")
  }
  library(WrightMap)
  
  if (tryCatch(require("CTT")) == FALSE){
    install.packages("CTT")
  }
  library(CTT)
library(ggplot2)

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "Item Analysis Dashboard",
  

  
# PAGE 1 -----------------------------------------------------------------------------

  tabPanel("Rasch",
           # Graphics on this page
           sidebarPanel(
             
             #sliderInput("bins",
              #           "Number of bins:",
               #          min = 1, 
                #         max = 50,
                 #        value = 30),
             fileInput("file1", "Choose proficiency file", multiple = FALSE,
                       accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv"
                                  )# Should text/plain be separated from the one before it?
              )
           ),
           mainPanel(
             plotOutput("histProficiency")
           )
           
  )
# PAGE 2 -----------------------------------------------------------------------------

   

)
# OUTPUT -----------------------------------------------------------------------------
server <- function(input, output) {
  # OUTPUT PAGE 1 -----------------------------------------------------------------------------
   output$histProficiency <- renderPlot({

       read.csv(input$file1$datapath)
       
       
       
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2] 
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
        #ggplot(data = datProficiency, aes(x = datProficiency$floor), stat= "count") + geom_bar() + geom_text(stat = "count", aes(label=..count..), vjust = -.5)
       
     
   })
   # OUTPUT PAGE 2 -----------------------------------------------------------------------------

   


      
} #end output

# Run the application 
shinyApp(ui = ui, server = server)

