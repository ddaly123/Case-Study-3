#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(caret)
library(caTools)
source("global.R")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Predicting the Reoccurance of Tumors", img(src = "wpi.png", height = 100, width = 300)),

    # Select the Category to explore
    sidebarLayout(
        sidebarPanel(
            selectInput("data", "Visualizing the Raw Data Set", choices = vis_choices, selected = "Age"),
            selectInput("xcol_data", "X for Cluster", vis_choices),
            helpText("The top graph depicts the total data points in the data set for selected category. The bottoms graph shows how many within that category are reoccurance and nonreoccurance. Data labels are too large to fit thus is the bars are group the same as the graph above yet split in two with the left representing the non and right as the reoccurance")
        ),

        # Show a plot of the generated distribution
        mainPanel(
        plotOutput("Selected_Data"), plotOutput('freqPlot'), img(src="smiley.png", height = 100, width = 300)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Selected_Data <- renderPlot ({
        chosen <- switch(input$data, "Age" = cancer$Age,
                         "Menopause" = cancer$Menopause, 
                         "Tumor_Size" = cancer$Tumor_Size,
                         "Inv_Nodes" = cancer$Inv_Nodes,
                         "Node_Caps" = cancer$Inv_Nodes, 
                         "Deg-Malig" = cancer$`Deg-Malig`, 
                         "Breast" = cancer$Breast,
                         "Breast_Quad" = cancer$Breast_Quad, 
                         "Irradiat" = cancer$Irradiat)
        counts <- table(as.factor(chosen))
        name <- names(chosen)
        barplot(counts, main= name)
    })

    output$freqPlot <- renderPlot({
        cancer_new <- data.frame(cancer)
        cat <- switch(input$xcol_data, 
                    "Age" = cancer_new$Age,
                    "Menopause" = cancer_new$Menopause, 
                    "Tumor_Size" = cancer_new$Tumor_Size,
                    "Inv_Nodes" = cancer_new$Inv_Nodes,
                    "Node_Caps" = cancer_new$Inv_Nodes, 
                    "Deg-Malig" = cancer_new$`Deg-Malig`, 
                    "Breast" = cancer_new$Breast,
                    "Breast_Quad" = cancer_new$Breast_Quad, 
                    "Irradiat" = cancer_new$Irradiat)
        library(dplyr)
        cancer_new$x <- paste(cat, cancer_new$Class)
        df <- cancer_new %>%
            group_by(x)%>%
            summarise(freq = n())
        name = names(df$x)
        barplot(df$freq, las=2, cex.names = .5, main = "Non-Re = Left Half, Re = Right Half")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
