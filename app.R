# ==== Shiny app Example ========================================================
# ==== global.R START ===========================================================
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(shiny)
library(ggplot2)
library(DT)
library(shinythemes)
library(shinydashboard)
library(GeneBook)
gn <- read.csv('inc/F500_names.csv')
g1 <- read.csv("inc/g1.csv")
g2 <- read.csv("inc/g2.csv")

# ===================================================== global.R END ============

# ==== ui.R START ===============================================================

# ==== NEW UI using shinydashboard ==============================================
ui <- dashboardPage(
    dashboardHeader(
        title = "Fortune 30 Companies"
    ),
    
    dashboardSidebar(
        selectInput(
            inputId = "G_groups",
            label = "A- Choose Group to plot:",
            choices = c(
                "Fortune 1-15 companies" = "g1",
                "Fortune 16-30 companies" = "g2"
               
            )
        )
        ,
        # We need comma between each input
        
        selectInput(
            inputId = "My_dataset",
            label = "B- Choose NYSE name to show it's full name:",
            choices = levels(gn$GeneID)
        ),
        
        selectInput(
            inputId = "More_info",
            label = "C- Documentation:",
            choices = c(
                'Introduction',
                'Table-1',
                'Table-2'
              
            ),
            selected = "Introduction"
        )
        
    ),
    
    dashboardBody(
        downloadButton(outputId = "downloadData",
                       label = "Download Data"),
        
        plotOutput(
            outputId = "myplot",
            width = "100%",
            height = "400px"
        ),
        
        verbatimTextOutput(outputId = "odataset"),
        
        uiOutput(outputId = "odataset_link"),
        
        uiOutput(outputId = "text1")
        
    )
)

# ===================================================== NEW UI END ==============
# ===================================================== ui.R END ================

# ==== server.R START ===========================================================
# Define server logic
# To access any input use input$[inputId]
#                     ex. input$G_groups (the first select input value)
# To assign any output use output$[outputId] output$
#                      ex. output$myplot (assign the plot output)
server <- function(input, output) {
    output$odataset <- renderPrint({
        paste(input$My_dataset," = ", gn$Gene[gn$GeneID==input$My_dataset])
    })
    
    # using GeneBook library to construct a link to the gene database
    abbreviation <- reactive((GeneCard_ID_Convert(input$My_dataset)))
    
    # output for the odataset_link
    output$odataset_link <- renderPrint({
        tags$a(
            href = paste(
                "https://finance.yahoo.com/quote/",
                as.character(abbreviation()[1]),
                sep = ''
            ),
            as.character(abbreviation()[1])
        )
    })
    
    
    full_file_name <-reactive(paste("./inc/", input$G_groups, ".csv", sep = ""))
    
    output$downloadData <- downloadHandler(
        
        filename = full_file_name,
        
        content = function(file){
            write.csv(read.csv(full_file_name()), quote = FALSE,file)
        } )
    
    output$myplot = renderPlot({
        g_x <- read.csv(full_file_name())
        
        p <- ggplot(g_x, aes(x=NYSE, y=USD,
                             fill=Metric)) +
            
            geom_bar(stat="identity", position=position_dodge()) +
            geom_errorbar(aes(ymin=USD,
                              ymax=USD),width=.3,
                          position=position_dodge(.9))
        p + scale_fill_brewer(palette="Paired")+
            ggtitle(paste("Relative expression levels of candidate gene list","\n",
                          "expressed as mean fold difference between pre- and",
                          "\n", "post-infection ± standard deviation (SD) ")) +
            guides(fill=guide_legend(title=NULL))
        
        p$theme <- theme(axis.text.x = element_text(angle = 90, hjust = 1))
        p$labels$x <- "NYSE"
        p$labels$y <- "Revenue"
        p$labels$fill <- NULL
        
        return(p)
        
    })
    
    
    # renderDT() from DT library is a replacement for Shiny renderDataTable()
    output$datatable1 <- renderDT(datatable(g1))
    output$datatable2 <- renderDT(datatable(g2))
    output$datatable3 <- renderDT(datatable(g3))
    
    output$text1 <- renderUI({
        if(input$More_info=="Introduction"){
            includeHTML("inc/introduction.html")
        } else if(input$More_info=="Information"){
            includeHTML("inc/information.html")
        } else if(input$More_info=="Help"){
            includeHTML("inc/help.html")
        } else if(input$More_info=="Table-1"){
            DTOutput('datatable1')
        } else if(input$More_info=="Table-2"){
            DTOutput('datatable2')
        } else if(input$More_info=="Table-3"){
            DTOutput('datatable3')
        } else if(input$More_info=="References"){
            includeHTML("inc/references.html")
        }
    })
}
# ===================================================== server.R END ============

# Run the application
shinyApp(ui = ui, server = server)
