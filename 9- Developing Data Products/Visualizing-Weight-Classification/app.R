library(shiny) 
library(ggplot2)
library(plyr)
library(caret)

# SCRIPT ------------------------------------------------------------------

dat <- read.csv("./pml-training.csv")

# CONVERT OUTCOME TO FACTOR
dat$classe <- plyr::mapvalues(dat$classe, c("A", "B", "C", "D", "E"), 0:4)
dat$classe <- as.factor(dat$classe)

# REMOVE NA COLUMNS 
NAs <- unlist(which(colSums(is.na(dat)) > 0))
dat <- dat[-NAs]

# REMOVE CHAR NA COLUMNS 
char_NAs <- unlist(which(sapply(dat, is.character)))
dat <- dat[-char_NAs]

# REMOVE UNNECESSARY COLUMNS
dat <- dat[,-c(1:4)]

# MAKE ALL COLUMNS NUMERIC
dat <- apply(dat, 2, as.numeric)
dat <- as.data.frame(dat)

# -------------------------------------------------------------------------

ui <- fluidPage(

    titlePanel("Visualizing Weight Classification Data"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "plotType", 
                        label = "Plot", 
                        choices = list("Scatter", "Boxplot", "Histogram")),
            selectInput(inputId = "var1", 
                        label = "X Variable", 
                        choices = as.list(colnames(dat))),
            selectInput(inputId = "var2", 
                        label = "Y Variable", 
                        choices = as.list(colnames(dat)),
                        selected = as.list(colnames(dat)[2])),
            selectInput(inputId = "scale", 
                        label = "Scale  Function", 
                        choices = list("scale()", "log10()", "No scale")),
            checkboxInput(inputId = "group", label = "Grouping", T),
            # checkboxInput(inputId = "kmeans", label = "Kmeans", T),
            submitButton("Submit")
        ),

        mainPanel(
           plotOutput("mainPlot"),
           # h3("K-means Classification"),
           # verbatimTextOutput("kmeans"),
           h3("Using this App"),
           h5("The purpose of this shiny app is to assist with visualizing a
              weight lifting classification dataset. I used the dataset for
              a Coursera project for a course called Practical Machine Learning,
              by Johns Hopkins University. In this plot, you can make a
              histogram, boxplot, or scatterplot of the data. When making a
              histogram, only the X variable will be used. When making a
              boxplot, only the Y variable will be used. The data 
              can be scaled with the scale() and log10() functions, and the data 
              can be  can grouped and ungrouped with the Grouping checkbox 
              (ungrouping can only be done with histrogram and scatterplot).")
    )
),
)


server <- function(input, output) {
    
    df <- reactive({
        
        df <- data.frame(dat[ ,c(input$var1, input$var2, "classe")])
        colnames(df) <- c("var1", "var2", "classe")
        df$classe <- plyr::mapvalues(df$classe, 0:4, c("A","B","C","D","E"))
        
        if (input$scale == "log10()") {
            df$var1 <- log10(df$var1)
            df$var2 <- log10(df$var2)
        }
        
        if (input$scale == "scale()") {
            df$var1 <- scale(df$var1)
            df$var2 <- scale(df$var2)
        }
        
        return(df)
        
    })
    
    output$mainPlot <- renderPlot({
        
        if (input$plotType == "Histogram") {
            
            p <- ggplot(df(), aes(x = var1, 
                                  fill = if (input$group == T) {classe})) + 
                 geom_density(alpha = 0.2) +
                 theme_bw() +
                 xlab("Conditions") + 
                 ylab(input$var1)
        
        } else if (input$plotType == "Boxplot") {
            
            p <- ggplot(df(), aes(x = classe, y = var2, fill = classe)) +
                 geom_boxplot() +
                 geom_jitter(alpha = 0.1) +
                 theme_bw() +
                 xlab("Conditions") + 
                 ylab(input$var2)
            
        } else if (input$plotType == "Scatter") {
        
            p <- ggplot(df(), aes(x = var1, 
                                  y = var2, 
                                  col = if (input$group == T) {classe})) +
                 geom_point() +
                 theme_bw() +
                 xlab(input$var1) + 
                 ylab(input$var2)
        }
        
        return(p)
        
    })
    
    # output$kmeans <- reactive({
    # 
    #     if (input$kmeans) {
    #         kclust <- kmeans(df()[1:2], centers = 5, iter.max = 50)
    #         classe <- unlist(df()[3])
    #         classe <- factor(classe, labels = 1:5)
    #         classe <- as.factor(classe)
    #         cm <- confusionMatrix(as.factor(kclust$cluster), classe)
    #         return(paste("Accuracy: ", round(cm$overall[[1]], 2)))
    #     }
    # 
    # })
    
}

shinyApp(ui = ui, server = server)