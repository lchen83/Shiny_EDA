library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)  #data manipulation
library(imputeTS) # data imputation
library(ggplot2)
library(scales)
library(factoextra) # clustering algorithms & visualization
library(ggdendro)
library(plotly)
library(psych)
library(caret)

ui <- navbarPage(theme = shinytheme("cerulean"),
                 title = "EDA",
                 tabPanel("Data",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                              conditionalPanel(condition = "input.tabselected==1",
                                               radioButtons(inputId = "upload", " ", choices = c("Upload Data", "Use sample data"),
                                                            selected = "Use sample data"),
                                               conditionalPanel(condition = "input.upload == 'Upload Data'",
                                                                fileInput(inputId = "file", label = "Choose CSV File",
                                                                          multiple = FALSE,
                                                                          accept = ".csv"),
                                                                checkboxInput(inputId = "header", label = "Header", TRUE),
                                                                checkboxInput(inputId = "check", label = "Set the first column as rownames")

                                               ),
                                               conditionalPanel(condition = "input.upload == 'Use sample data'",
                                                                h5("Sample data:"),
                                                                h5("The Lactose Intolerant data has 970 SNPs data from
                                                                   428 users with two different phenotype: intolerant and tolerant"),
                                                                downloadButton(outputId = "download", label = "Download sample data")
                                               )

                              ),
                              conditionalPanel(condition = "input.tabselected==2",
                                               checkboxInput(inputId = "miss_sort",
                                                             label = "Sort by descending order",
                                                             value = TRUE)),
                              conditionalPanel(condition = "input.tabselected==3",
                                               htmlOutput(outputId = "dim"),
                                               br(),
                                               numericInput(inputId = "na_col",
                                                            label = "Remove columns with more than __% missing",
                                                            min = 0,
                                                            max = 100,
                                                            value = 40),
                                               numericInput(inputId = "na_row",
                                                            label = "Remove rows with more than __% missing",
                                                            min = 0,
                                                            max = 100,
                                                            value = 90),
                                               actionButton(inputId = "remove", label = "Remove"),
                                               actionButton(inputId = "remove_reset", label = "Reset"),
                                               br(),
                                               htmlOutput(outputId = "dim_remove"),
                                               br(),
                                               htmlOutput(outputId = "dim_after_remove")),
                              conditionalPanel(condition = "input.tabselected==4",
                                               radioButtons(inputId = "impute",
                                                            label = "Impute missing value with:",
                                                            choices = c("mean", "median", "mode", "Do not apply"),
                                                            selected = "mode"),
                                               fluidRow(
                                                 actionButton(inputId = "action",
                                                              label = "Impute"),
                                                 actionButton(inputId = "reset",
                                                              label = "Reset")
                                                 )),
                              conditionalPanel(condition = "input.tabselected==5",
                                               numericInput(inputId = "from",
                                                            label = "Show data summary from column",
                                                            value = 1),
                                               numericInput(inputId = "to",
                                                            label = "to column",
                                                            value = 50)),
                              conditionalPanel(condition = "input.tabselected==6",
                                               numericInput(inputId = "start",
                                                            label = "Show data structure from column",
                                                            value = 1),
                                               numericInput(inputId = "end",
                                                            label = "to column",
                                                            value = 50)),
                              conditionalPanel(condition = "input.tabselected==7",
                                               selectizeInput(inputId = "x",
                                                              label = "Select the variable:",
                                                              choices = c(""),
                                                              options = list(maxItems = 1)),
                                               checkboxInput(inputId = "color",
                                                             label = "color by phenotype", TRUE),
                                               actionButton(inputId = "act_bar",
                                                            label = "Plot"))
                            ),
                            mainPanel(
                              tabsetPanel(id = "tabselected",
                                tabPanel("Data Source", value = 1,
                                         dataTableOutput(outputId = "df")),
                                tabPanel("Missing Value", value = 2,
                                         verbatimTextOutput(outputId = "miss")),
                                tabPanel("Data Preprocess", value = 3,
                                         dataTableOutput(outputId = "table1")),
                                tabPanel("Imputation", value = 4,
                                         htmlOutput(outputId = "DT_text"),
                                         dataTableOutput(outputId = "table")),
                                tabPanel("Summary", value = 5,
                                         verbatimTextOutput(outputId = "summary")),
                                tabPanel("Structure", value = 6,
                                         verbatimTextOutput(outputId = "structure")),
                                tabPanel("Plot", value = 7,
                                         htmlOutput(outputId = "barplot_text"),
                                         plotOutput(outputId = "bar"))
                              )
                            )
                          )
                          ),
                 tabPanel("PCA",
                          tabsetPanel(
                            tabPanel("PCA plot",
                                     sidebarLayout(
                                       sidebarPanel(width = 2,
                                                    h5("Plot Options"),
                                                    selectInput(inputId = "pca_x", label = "X Variable",
                                                                choices = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10"),
                                                                selected = "PC1"),
                                                    selectInput(inputId = "pca_y", label = "Y Variable",
                                                                choices = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10"),
                                                                selected = "PC2"),
                                                    checkboxInput(inputId = "pca_col", label = "color by phenotype", value = TRUE),
                                                    checkboxInput(inputId = "pca_ellipse", label = "add 95% confidence ellipse"),
                                                    h5("Confusion Matrix Option"),
                                                    numericInput(inputId = "pc_threshold", label = "Threshold", value = 0),
                                                    actionButton(inputId = "act_pca", label = "Run")
                                       ),
                                       mainPanel(
                                         htmlOutput(outputId = "pca_text"),
                                         fluidRow(
                                           column(width = 6, verbatimTextOutput("pca_cm_x")),
                                           column(width = 6, verbatimTextOutput("pca_cm_y"))
                                         ),
                                         plotlyOutput("pca_plot"))
                                     )),
                            tabPanel("3D plot",
                                     sidebarLayout(
                                       sidebarPanel(width = 3,
                                                    selectizeInput(inputId = "d_x", label = "Select X, Y and Z",
                                                                   choices = c("Choose" = "", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10"),
                                                                   options = list(placeholder = "Select three variables", maxItems = 3))
                                       ),
                                       mainPanel(plotlyOutput("thrd_plot"))
                                     )),
                            tabPanel("Scree plot",
                                     htmlOutput(outputId = "scree_text"),
                                     plotOutput("scree"),
                                     htmlOutput(outputId = "scree_title"),
                                     verbatimTextOutput("pca_var")),
                            tabPanel("Cumulative plot",
                                     htmlOutput(outputId = "cumu_text"),
                                     plotOutput("cumu"),
                                     htmlOutput(outputId = "cumu_title"),
                                     verbatimTextOutput("pca_cumu_var"))
                          )
                 ),
                 tabPanel("Varimax-rotated PCA",
                          tabsetPanel(
                            tabPanel("Plot",
                                     sidebarLayout(
                                       sidebarPanel(width = 2,
                                                    numericInput(inputId = "rc_num", label = "No. of rotated components",
                                                                 value = 2, min = 2, max = 5),
                                                    selectInput(inputId = "rc_x", label = "X Variable",
                                                                choices = c("RC1"),
                                                                selected = "RC1"),
                                                    selectInput(inputId = "rc_y", label = "Y Variable",
                                                                choices = c("RC2"),
                                                                selected = "RC2"),
                                                    checkboxInput(inputId = "rc_col", label = "color by phenotype",
                                                                  value = TRUE),
                                                    actionButton(inputId = "act_rc", label = "Run")
                                       ),
                                       mainPanel(
                                         htmlOutput(outputId = "rc_text"),
                                         plotlyOutput("rc_plot"))
                                     )),
                            tabPanel("3D plot",
                                     sidebarLayout(
                                       sidebarPanel(width = 3,
                                                    selectizeInput(inputId = "d_rc_x", label = "Select X, Y and Z",
                                                                   choices = c("Choose" = "", "RC1", "RC2", "RC3"),
                                                                   options = list(placeholder = "Select three variables", maxItems = 3))
                                       ),
                                       mainPanel(plotlyOutput("thrd_rc_plot"))
                                     ))
                          )
                 ),
                 tabPanel("K means",
                          sidebarLayout(
                            sidebarPanel(width = 2,
                                         selectizeInput(inputId = "km_x",
                                                        label = "X Variable",
                                                        choices = c(""),
                                                        options = list(maxItems = 1)),
                                         selectizeInput(inputId = "km_y",
                                                        label = "Y Variable",
                                                        choices = c(""),
                                                        options = list(maxItems = 1)),
                                         numericInput(inputId = "km_k",
                                                      label = "Cluster count",
                                                      value = 2,
                                                      min = 1,
                                                      max = 5),
                                         selectInput(inputId = "km_col",
                                                     label = "Color points by",
                                                     choices = c("phenotype", "cluster"),
                                                     selected = "phenotype"),
                                         checkboxInput(inputId = "jitter",
                                                       label = "Add jitter", TRUE),
                                         actionButton(inputId = "km_action", label = "Run")
                            ),
                            mainPanel(
                              htmlOutput(outputId = "km_snp_text"),
                              verbatimTextOutput(outputId = "km_table"),
                              plotlyOutput(outputId = "km_snp_plot")
                            )
                          )
                 ),
                 tabPanel("Hierarchical",
                          sidebarLayout(
                            sidebarPanel(width = 2,
                                         h5("Clustering Options"),
                                         selectInput(inputId = "dist",
                                                     label = "Distance",
                                                     choices = c("euclidean", "maximum", "manhattan",
                                                                 "canberra", "binary", "minkowski"),
                                                     selected = "euclidean"),
                                         selectInput(inputId = "link",
                                                     label = "Linkage",
                                                     choices = c("ward.D", "ward.D2", "single", "complete",
                                                                 "average", "mcquitty", "median", "centroid"),
                                                     selected = "ward.D2"),
                                         numericInput(inputId = "hc_k",
                                                      label = "Cluster count",
                                                      min = 2,
                                                      max = 10,
                                                      value = 2),
                                         selectInput(inputId = "hc_col",
                                                     label = "Color labels by",
                                                     choices = c("phenotype", "cluster"),
                                                     selected = "phenotype"),
                                         actionButton(inputId = "update",
                                                      label = "Run")

                            ),
                            mainPanel(
                              htmlOutput(outputId = "hc_text"),
                              verbatimTextOutput(outputId = "hc_table"),
                              plotOutput(outputId = "dend",
                                         width = "125%",
                                         dblclick = "plot_dblclick",
                                         brush = brushOpts(id = "plot_brush",
                                                           resetOnNew = TRUE)))
                          ))

)



server <- function(input, output, session){

  #-------------------------------------------data upload-----------------------------------

  data <- reactive({
    if (input$upload == "Use sample data") {
      read.csv("Lactose_intolerant_data.csv", row.names = 1)
    } else {
      if (is.null(input$file)) {
        return(NULL)
      } else if (input$check == FALSE) {
        read.csv(input$file$datapath,
                 header = input$header,
                 sep = ",")
      } else if (input$check == TRUE) {
        read.csv(input$file$datapath,
                 header = input$header,
                 sep = ",",
                 row.names = 1)
      }
    }


  })

  output$df <- renderDataTable({
    if (is.null(data())) {
      return()
    } else {
      datatable(data(), options = list(searching = FALSE))
    }
  })

  output$download <- downloadHandler(
    filename = function() {
      paste("Lactose_intolerant_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file)
    }
  )

  #--------------------------------remove columns & rows with high % of NAs-----------------

  # Defining & initializing the reactiveValues object
  counter_re <- reactiveValues(countervalue_re = 0)

  # if the action button is clicked, increment the value by 1 and update it
  observeEvent(input$remove, {counter_re$countervalue_re <- counter_re$countervalue_re + 1})

  observeEvent(input$remove_reset, {counter_re$countervalue_re <- 0
  counter$countervalue <- 0})

  # Remove columns with x% of NAs
  data_re_col <- eventReactive(input$remove, {
    if (length(which(colMeans(is.na(data())) >= input$na_col / 100)) == 0) {
      data()
    } else {
      data()[, -which(colMeans(is.na(data())) >= input$na_col / 100)]
    }
  })

  # Remove rows with x% of NAs

  data_re_row <- eventReactive(input$remove, {
    if (length(which(rowMeans(is.na(data_re_col())) >= input$na_col / 100)) == 0) {
      data_re_col()
    } else {
      data_re_col()[-which(rowMeans(is.na(data_re_col())) >= input$na_row / 100),]
    }
  })


  output$dim_remove <- renderText({
    if (counter_re$countervalue_re != 0) {
      paste(dim(data())[2] - dim(data_re_row())[2], "<b>columns</b> and",
            dim(data())[1] - dim(data_re_row())[1], "<b>rows</b> have been removed")
    } else {
      return()
    }
  })



  output$dim_after_remove <- renderText({
    if (counter_re$countervalue_re != 0) {
      paste("<b>The data currently has:</b>", "<br>", dim(data_re_row())[1],
            "rows", "<br>", dim(data_re_row())[2], "columns")
    } else {
      return()
    }
  })


  # Generate a table view of the data
  output$table1 <- renderDataTable({
    if (counter_re$countervalue_re != 0) {
      datatable(data_re_row(), options = list(searching = FALSE))
    } else {
      datatable(data(), options = list(searching = FALSE))
    }

  })

  #---------------------------------------Load data----------------------------------

  # Make the data reactive & Imputation
  data_impute <- eventReactive(input$action, {

    if (input$impute == "Do not apply") {
      data_re_row()
    } else {
      na_mean(data_re_row(), option = input$impute)}
    })

  #extract phenotype
  phenotype <- reactive({
    if (class(data_re_row()[, length(data_re_row())]) == "factor") {
      data_re_row()[, length(data_re_row())]
    } else {
      as.factor(data_re_row()[, length(data_re_row())])
    }
    })

  data_without_labs <- reactive(data_impute()[, 1:length(data_re_row()) - 1])

  #----------------------------------------Data---------------------------------------

  # The dimension of the data
  output$dim <- renderText({paste("<b>This data has:</b>", "<br>", dim(data)[1],
                                  "rows", "<br>", dim(data())[2], "columns")})

  # Defining & initializing the reactiveValues object
  counter <- reactiveValues(countervalue = 0)

  # if the action button is clicked, increment the value by 1 and update it
  observeEvent(input$action, {counter$countervalue <- counter$countervalue + 1})

  observeEvent(input$reset, {counter$countervalue <- 0})

  # Warning if user didn't preprocess data
  output$DT_text <- renderText({

    if (counter_re$countervalue_re == 0)

      paste(h4("Please preprocess the data first"))

  })

  # Generate a table view of the data
  output$table <- renderDataTable({
    if (counter_re$countervalue_re != 0 && counter$countervalue == 0) {

      datatable(data_re_row(), options = list(searching = FALSE))

    } else if (counter_re$countervalue_re != 0 && counter$countervalue != 0) {
      datatable(data_impute(), options = list(searching = FALSE))
    } else {
      return()
    }
  })

  #-----------------------------------------Missing Variables-------------------------------

  output$miss <- renderPrint({
    if (is.null(data())) {
      return()
    } else if (counter_re$countervalue_re == 0 && counter$countervalue == 0) {
      sort(colMeans(is.na(data())), decreasing = input$miss_sort)

    } else if (counter_re$countervalue_re != 0 && counter$countervalue == 0) {
      sort(colMeans(is.na(data_re_row())), decreasing = input$miss_sort)

    } else if (counter_re$countervalue_re != 0 && counter$countervalue != 0) {
      sort(colMeans(is.na(data_impute())), decreasing = input$miss_sort)
    }
  })

  #---------------------------------------Summary-----------------------------------

  # Generate a summary of the data
  output$summary <- renderPrint({
    if (is.null(data())) {
      return()
    } else if (counter_re$countervalue_re == 0 && counter$countervalue == 0) {
      summary(data()[, input$from:input$to])

    } else if (counter_re$countervalue_re != 0 && counter$countervalue == 0) {

      summary(data_re_row()[, input$from:input$to])

    } else if (counter_re$countervalue_re != 0 && counter$countervalue != 0) {
      summary(data_impute()[, input$from:input$to])
    }

  })

  #----------------------------------------Structure--------------------------------

  # Generate a structure of the data
  output$structure <- renderPrint({
    if (is.null(data())) {
      return()
    } else if (counter_re$countervalue_re == 0 && counter$countervalue == 0) {
      str(data()[, input$start:input$end])

    } else if (counter_re$countervalue_re != 0 && counter$countervalue == 0) {

      str(data_re_row()[, input$start:input$end])

    } else if (counter_re$countervalue_re != 0 && counter$countervalue != 0) {
      str(data_impute()[, input$start:input$end])
    }

  })

  #-----------------------------------------Barplot----------------------------------

  output$barplot_text <- renderText({

    if (counter_re$countervalue_re == 0)

      paste(h4("Please preprocess the data first"))

  })

  #updata selectizeInput
  observe({
    x <- names(data_re_row()[, 1:length(data_re_row()) - 1])

    # Can also set the label and select items
    updateSelectizeInput(session, "x",
                         choices = x,
                         options = list(maxItems = 1)
    )
  })

  #convert imputed data to factor
  data_impute_fc <- reactive(as.data.frame(lapply(data_impute(), as.factor)))

  #convert data to factor
  data_fc <- reactive(as.data.frame(lapply(data_re_row(), as.factor)))

  barplot <- eventReactive(input$act_bar, {

    if (counter$countervalue > 0) {
      #data: remove and impute
      g <- ggplot(data = data_impute_fc(), aes(y = (..count..)/sum(..count..))) +
        scale_y_continuous(labels = percent)
      p <- g + geom_bar(aes_string(x = input$x), fill = "steelblue",
                        width = 0.5, alpha = 0.8) + ylab("Percentage")

      if (input$color)
        p <- g + geom_bar(aes_string(x = input$x, fill = phenotype()),
                          width = 0.5, alpha = 0.8, position = "dodge") +
        labs(fill = "Class") + ylab("Percentage")
      p

    } else {
      #data: remove
      g <- ggplot(data = data_fc(), aes(y = (..count..)/sum(..count..))) +
        scale_y_continuous(labels = percent)
      p <- g + geom_bar(aes_string(x = input$x), fill = "steelblue",
                        width = 0.5, alpha = 0.8) + ylab("Percentage")

      if (input$color)
        p <- g + geom_bar(aes_string(x = input$x, fill = phenotype()),
                          width = 0.5, alpha = 0.8, position = "dodge") +
        labs(fill = "Class") + ylab("Percentage")
      p

    }

  })

  output$bar <- renderPlot({

    barplot()

  })


  #------------------------------------------------PCA-----------------------------------------------------

  pr.out <- reactive({
    prcomp(data_without_labs(), center = TRUE, scale = TRUE)
  })

  pca.summary <- reactive({summary(pr.out())})

  pca.x <- reactive({as.data.frame(pr.out()$x[, 1:10])})

  #------------------------------------------------PCA plot-------------------------------------------------

  output$pca_text <- renderText({

    if (counter$countervalue == 0 || counter_re$countervalue_re == 0)

      paste(h4("Please preprocess and impute the data first"))

  })

  pca_plot <- eventReactive(input$act_pca, {
    if (input$pca_col == FALSE && input$pca_ellipse == FALSE) {
      ggplot(pca.x(), aes_string(x = input$pca_x, y = input$pca_y)) +
        geom_point(aes(text = paste("User ID: ",rownames(data_without_labs()))), size = 3, alpha = 0.5)

    } else if (input$pca_col == FALSE && input$pca_ellipse == TRUE) {
      ggplot(pca.x(),aes_string(x = input$pca_x, y = input$pca_y)) +
        geom_point(aes(text = paste("User ID: ",rownames(data_without_labs()))), size = 3, alpha = 0.5) +
        stat_ellipse(geom = "polygon", alpha = 0.3)

    } else if (input$pca_col == TRUE && input$pca_ellipse == FALSE) {
      ggplot(pca.x(),aes_string(x = input$pca_x, y = input$pca_y,
                                color = phenotype(), fill = phenotype())) +
        geom_point(aes(text = paste("User ID: ",rownames(data_without_labs()))), size = 3, alpha = 0.5)

    } else {
      ggplot(pca.x(),aes_string(x = input$pca_x, y = input$pca_y,
                                color = phenotype(), fill = phenotype())) +
        geom_point(aes(text = paste("User ID: ",rownames(data_without_labs()))), size = 3, alpha = 0.5) +
        stat_ellipse(geom = "polygon", alpha = 0.2)
    }
  })

  output$pca_plot <- renderPlotly({
    pca_plot() + theme_light()
  })

  #------------------------------------------------PCA CM---------------------------------------------------

  pc_cm <- reactive({
    pca.x()[, c(input$pca_x, input$pca_y)]
  })

  cm_x <- eventReactive(input$act_pca, {
    x_var <- ifelse(pc_cm()[,1] > input$pc_threshold, "Above threshold", "Under threshold")
    table(x_var, phenotype())
  })

  cm_y <- eventReactive(input$act_pca, {
    y_var <- ifelse(pc_cm()[,2] > input$pc_threshold, "Above threshold", "Under threshold")
    table(y_var, phenotype())
  })

  output$pca_cm_x <- renderPrint({
    cm_x()
  })

  output$pca_cm_y <- renderPrint({
    cm_y()
  })

  #------------------------------------------------3D Plot--------------------------------------------------

  threeD_data <- reactive({
    pca.x()[, c(input$d_x)]
  })


  output$thrd_plot <- renderPlotly({
    if (length(colnames(threeD_data())) < 3) {
      return()
    } else {
      fig <- plot_ly(threeD_data(), x = ~threeD_data()[,1], y = ~threeD_data()[,2],
                     z = ~threeD_data()[,3], color = ~phenotype(),
                     colors = c("#F8766D", "#7CAE00", "#C77CFF", "#00BFC4"),
                     alpha = 0.9, opacity = 0.6,
                     type = "scatter3d", mode = "markers",
                     text = ~rownames(threeD_data()))
      fig <- fig %>% layout(scene = list(xaxis = list(title = input$d_x[1]),
                                         yaxis = list(title = input$d_x[2]),
                                         zaxis = list(title = input$d_x[3])),
                            legend = list(orientation = 'h'))
      fig
    }
  })


  #------------------------------------------------scree plot-----------------------------------------------

  output$scree_text <- renderText({

    if (counter$countervalue == 0 || counter_re$countervalue_re == 0)

      paste(h4("Please preprocess and impute the data first"))

  })

  # Variability of each principal component: pr.var
  pr.var <- reactive({pr.out()$sdev^2})
  # Variance explained by each principal component: pve
  pve <- reactive({pr.var()/sum(pr.var())})

  output$scree <- renderPlot({
    if (counter$countervalue == 0 || counter_re$countervalue_re == 0) {
      return()
    } else {
      plot(pve(), xlab = "Principal Component",
           ylab = "Proportion of Variance Explained",
           main = "PCA Scree Plot",
           ylim = c(0, 1), type = "b")
    }
  })

  output$scree_title <- renderText({

    if (counter$countervalue != 0 && counter_re$countervalue_re != 0)

      paste(h5("The proportion of variance explained by each principal component"))

  })

  output$pca_var <- renderPrint({
    pca.summary()$importance[2, ]
  })

  #--------------------------------------------Cumulative plot-------------------------------------------------

  output$cumu_text <- renderText({

    if (counter$countervalue == 0 || counter_re$countervalue_re == 0)

      paste(h4("Please preprocess and impute the data first"))

  })

  output$cumu <- renderPlot({
    if (counter$countervalue == 0 || counter_re$countervalue_re == 0) {
      return()
    } else {
      plot(cumsum(pve()), xlab = "Principal Component",
           ylab = "Cumulative Proportion of Variance Explained",
           main = "PCA Cumulative Plot",
           ylim = c(0, 1), type = "l")
    }
  })

  output$cumu_title <- renderText({

    if (counter$countervalue != 0 && counter_re$countervalue_re != 0)

      paste(h5("The cumulative proportion of variance explained"))

  })

  output$pca_cumu_var <- renderPrint({
    pca.summary()$importance[3, ]
  })


  #---------------------------------------------PCA with RC-----------------------------------------------------

  #remove columns that are highly correlated

  corr <- reactive({cor(data_impute()[,1:length(data_impute()) - 1])})

  highCorr <- reactive({findCorrelation(corr(), cutoff = 0.99, names = T)})

  clean_data <- reactive({data_impute()[, !names(data_impute()) %in% highCorr()]})


  pca_varimax <- reactive({principal(clean_data()[, 1:length(clean_data()) - 1],
                                     nfactors = input$rc_num, rotate = "varimax")})

  pca_varimax_score <- reactive({as.data.frame(pca_varimax()$score)})


  #------------------------------------------------RC plot-------------------------------------------------

  output$rc_text <- renderText({

    if (counter$countervalue == 0 || counter_re$countervalue_re == 0)

      paste(h4("Please preprocess and impute the data first"))

  })

  #updata selectInput
  observe({
    rc_x <- names(pca_varimax_score())

    # Can also set the label and select items
    updateSelectInput(session, "rc_x",
                      choices = rc_x,
                      selected = "RC1"
    )
  })

  observe({
    rc_y <- names(pca_varimax_score())

    # Can also set the label and select items
    updateSelectInput(session, "rc_y",
                      choices = rc_y,
                      selected = "RC2"
    )
  })

  rc_plot <- eventReactive(input$act_rc, {
    if (input$rc_col == FALSE ) {
      ggplot(pca_varimax_score(), aes_string(x = input$rc_x, y = input$rc_y))+
        geom_point(size = 2, alpha = 0.5)

    } else  {
      ggplot(pca_varimax_score(),aes_string(x = input$rc_x, y = input$rc_y,
                                            color = clean_data()$pheno, fill = clean_data()$pheno))+
        geom_point(size = 2, alpha = 0.5)

    }
  })

  output$rc_plot <- renderPlotly({
    rc_plot() + theme_light()
  })

  #------------------------------------------------3D RC plot--------------------------------------------------

  #updata selectizeInput
  observe({
    d_rc_x <- names(pca_varimax_score())

    # Can also set the label and select items
    updateSelectizeInput(session, "d_rc_x",
                         choices = d_rc_x,
                         options = list(placeholder = "Select three variables",
                                        maxItems = 3))
  })

  threeD_data_vm <- reactive({
    pca_varimax_score()[, c(input$d_rc_x)]
  })

  output$thrd_rc_plot <- renderPlotly({
    if (length(colnames(threeD_data_vm())) < 3) {
      return()
    } else {
      p <- plot_ly(threeD_data_vm(), x = ~threeD_data_vm()[,1],
                   y = ~threeD_data_vm()[,2],
                   z = ~threeD_data_vm()[,3],
                   color = ~clean_data()$pheno,
                   colors = c("#F8766D", "#7CAE00", "#C77CFF", "#00BFC4"),
                   alpha = 0.9, type = "scatter3d", mode = "markers",
                   text = ~rownames(pca_varimax_score()), opacity = 0.6)
      p <- p %>% layout(scene = list(xaxis = list(title = 'RC1'),
                                     yaxis = list(title = 'RC2'),
                                     zaxis = list(title = 'RC3')))

      p}
  })



  #----------------------------------k means clustering SNPs--------------------------

  #Waring: need preprocess and impute data first
  output$km_snp_text <- renderText({

    if (counter$countervalue == 0 || counter_re$countervalue_re == 0)

      paste(h4("Please preprocess and impute the data first"))

  })

  #updata selectizeInput
  observe({
    km_x <- names(data_re_row()[, 1:length(data_re_row()) - 1])

    # Can also set the label and select items
    updateSelectizeInput(session, "km_x",
                         choices = km_x,
                         selected = "rs4988235",
                         options = list(maxItems = 1)
    )
  })

  observe({
    km_y <- names(data_re_row()[, 1:length(data_re_row()) - 1])

    # Can also set the label and select items
    updateSelectizeInput(session, "km_y",
                         choices = km_y,
                         options = list(maxItems = 1),
                         selected = "rs182549"
    )
  })

  km.out <- reactive({
    set.seed(2)
    kmeans(data_without_labs(), input$km_k)
  })

  km_table <- eventReactive(input$km_action, {table(km.out()$cluster, phenotype())})

  output$km_table <- renderPrint({
    km_table()
  })

  km_cluster <- reactive(as.factor(km.out()$cluster))

  km_center <- reactive({as.data.frame(km.out()$centers)})

  km_plot <- eventReactive(input$km_action, {

    set.seed(1)
    if (input$km_col == "cluster" && input$jitter == TRUE) {
      ggplot(data = data_without_labs(),
             aes_string(x = input$km_x, y = input$km_y, colour = km_cluster())) +
        geom_jitter(aes(text = paste("User ID: ",rownames(data_without_labs()), "\n",
                                     "Phenotype: ", as.character(phenotype()), sep = "")),
                    size = 3, alpha = 0.5) +
        geom_point(data = km_center(), aes_string(x = input$km_x,
                                                  y = input$km_y),
                   color = "black", size = 5, alpha = 0.5)

    } else if (input$km_col == "phenotype" && input$jitter == TRUE) {
      ggplot(data = data_without_labs(),
             aes_string(x = input$km_x, y = input$km_y, colour = phenotype())) +
        geom_jitter(aes(text = paste("User ID: ",rownames(data_without_labs()), "\n",
                                     "Cluster: ", as.character(km_cluster()), sep = "")),
                    size = 3, alpha = 0.5) +
        geom_point(data = km_center(), aes_string(x = input$km_x,
                                                  y = input$km_y),
                   color = "black", size = 5, alpha = 0.5)


    } else if (input$km_col == "cluster" && input$jitter == FALSE) {
      ggplot() +
        geom_point(data = data_without_labs(),
                   mapping = aes_string(x = input$km_x,y = input$km_y, colour = km_cluster()),
                   size = 3, alpha = 0.5) +
        geom_point(data = km_center(), aes_string(x = input$km_x,
                                                  y = input$km_y),
                   color = "black", size = 5, alpha = 0.5)
    } else {
      ggplot() +
        geom_point(data = data_without_labs(),
                   mapping = aes_string(x = input$km_x,y = input$km_y, colour = phenotype()),
                   size = 3, alpha = 0.5) +
        geom_point(data = km_center(), aes_string(x = input$km_x,
                                                  y = input$km_y),
                   color = "black", size = 5, alpha = 0.5)
    }
  })

  output$km_snp_plot <- renderPlotly({
    km_plot() +
      theme_light() + ggtitle("K means clustering")

  })

  #---------------------------------Hierarchical Clustering---------------------------

  output$hc_text <- renderText({

    if (counter$countervalue == 0 || counter_re$countervalue_re == 0)

      paste(h4("Please preprocess and impute the data first"))

  })

  hc <- reactive(hclust(dist(scale(data_without_labs()), method = input$dist),
                        method = input$link))


  clust <- eventReactive(input$update, {

    cutree(hc(), k = input$hc_k)
  })

  output$hc_table <- renderPrint({
    table(clust(), phenotype())
  })

  ranges <- reactiveValues(x = NULL, y = NULL)

  hc_plot <- eventReactive(input$update, {

    dendr <- dendro_data(hc(), type = "rectangle")
    clust <- cutree(hc(),k = input$hc_k)
    clust.df <- data.frame(label = names(clust), cluster = factor(clust))

    dendr[["labels"]] <- merge(dendr[["labels"]],clust.df, by = "label")

    ggplot() +
      geom_segment(data = segment(dendr), aes(x = x, y = y, xend = xend, yend = yend)) +

      if (input$hc_col == "phenotype") {

        geom_text(data = label(dendr), aes_string("x", "y", label = "label", hjust = "1",
                                                  angle = "90", color = phenotype()), size = 3)

      } else {

        geom_text(data = label(dendr), aes_string("x", "y", label = "label", hjust = "1",
                                                  angle = "90", color = "cluster"), size = 3)
      }

  })

  output$dend <- renderPlot({

    hc_plot() +
      ggtitle("Hierarchical Clustering - dendrogram", "Brush and double-click to zoom") +
      theme(legend.position = "bottom", plot.title = element_text(size = 16, colour = "black"),
            plot.subtitle = element_text(size = 14, colour = "grey")) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y)

  })

  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  #ref: https://shiny.rstudio.com/gallery/plot-interaction-zoom.html
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })

}

shinyApp(ui = ui, server = server)
