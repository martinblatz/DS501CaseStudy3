#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


#source 
source("./global.R")

vars = names(dataIBM)
numVars = length(cov$Variance)


# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("IBM Attrition Data - Principal Components Analysis"),
    
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            uiOutput("dynSlider"),
            uiOutput("xSelect"),
            uiOutput("ySelect"),
            
            selectInput("variables",
                        "Select Included Variables",
                        IBMQuant,
                        selected = IBMDef,#"JobSatisfaction",
                        multiple = TRUE
                        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Pareto Plot", 
                                 plotOutput("paretoPlot"),
                                 textOutput("VarSum")),
                        tabPanel("Principal Component Relationships by Attrition",
                                 plotOutput("RelPlots")
                                 ),
                        tabPanel("PC Loadings",
                                 tableOutput("PcLoadings")
                                 
                        )
                        )
        )
    )
)



#server function required for all Shiny applications
server <- function(input, output) {
    
    # renderUI used to create a dynamic slider range to select the number 
    # of PCs used to calculate the cumulative percentage of variance represented
    # The slider will not allow the user to select more than are available
    output$dynSlider <- renderUI({
        sliderInput("NumPCs",
                    "Sum how many PCs?",
                    min = 1,
                    step = 1,
                    max = (length(input$variables)),
                    value = 1)
    })
    
    # renderUI used to create a dynamic 
    output$xSelect <- renderUI({
        selectInput("xval",
                    "Select X axis PC",
                    names(pcs)[1:length(input$variables)]
        )
        
    })
    output$ySelect <- renderUI({
        selectInput("yval",
                    "Select y axis PC",
                    names(pcs)[1:length(input$variables)]
                )
        
    })
    

    output$paretoPlot <- renderPlot({
        
        
        pca = prcomp(dataIBM[,input$variables], center=T, scale.=T)
        QuantLen = length(input$variables)
        
        cov <<- round(pca$sdev^2/sum(pca$sdev^2)*100, 2)
        cov <<- data.frame(c(1:QuantLen),cov)
        names(cov)[1] = 'PCs'
        names(cov)[2] = 'Variance'
        
        PCA = pca$sdev^2
        names(PCA) = paste0('PC', cov$PCs)
        qcc::pareto.chart(PCA)
    })
    output$RelPlots <- renderPlot({
        pca = prcomp(dataIBM[,input$variables], center=T, scale.=T)
        pcs = data.frame(pca$x)
        pcs$Label = dataIBM$Attrition
        
        ggplot(pcs, aes_string(x = input$xval, y = input$yval, color='Label'))+ geom_point() +
            scale_colour_manual(values=c('green','red'))
    })
    output$VarSum <- renderText({
        pca = prcomp(dataIBM[,input$variables], center=T, scale.=T)
        QuantLen = length(input$variables)
        
        cov <<- round(pca$sdev^2/sum(pca$sdev^2)*100, 2)
        cov <<- data.frame(c(1:QuantLen),cov)
        names(cov)[1] = 'PCs'
        names(cov)[2] = 'Variance'
        
        paste("Cumulative Percentage of", input$NumPCs, 
              " Principal Components: ", sum(cov$Variance[1:input$NumPCs]), "%")
    })

    output$PcLoadings <- renderTable({
        pca = prcomp(dataIBM[,input$variables], center=T, scale.=T)
        table = data.frame(pca$rotation)
        # table = cbind(names = names(table),data)
        table = cbind(rownames(table),table)
        table
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
