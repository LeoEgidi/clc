#'  CLC
#'
#'  @description Congeneric Latent Construct Estimator
#'
#'  @import shiny
#'  @import shinyWidgets
#'  @import shinyjs
#'  @import dplyr
#'  @import moments
#'  @import GPArotation
#'  @import shinyFiles
#'  @export

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(GPArotation)
library(shinyFiles)
library(moments)




clc <- function(...){
ui <- fluidPage(
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  tags$head(
    tags$style(HTML("
      /* this will affect all the pre elements */
      pre {
        color: red;
        background-color: white;
      }
      /* this will affect only the pre elements under the class myclass */
      .myclass pre {
        color: black;
        background-color: #d6860f;
        font-weight: bolder;
      }"))
  ),
  tags$style(HTML("h2 { color: blue;
                        font-size:18px;
                  } ")),
  shinyjs::useShinyjs(),
  titlePanel(  imageOutput("logo"),
               tags$head(tags$style(('text{width: 10px; font-size:35px; text-align:center; padding:-250px;}')))
               ),
  div(style = "font-size: 14px;
                   padding: 0px 0px;
                   margin-top:-25em",
  sidebarLayout(
    sidebarPanel(
      h2("File upload"),
       tags$head(tags$style(
         "#sidebarItemExpanded {
            overflow: auto;
            max-height: 100vh;
        }"
       )),
      style = paste0("height: 100vh; overflow-y: auto;"),
      radioButtons("sep", ".csv file separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      fileInput("file1", "Choose CSV file",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      #tags$hr(),
      h2("Estimation options"),
      #div(style = "padding: 8px 8px"),
      selectizeInput("new.var", "Select the items",
                  choices = colnames(data()),
                  multiple = TRUE,
                  options = list(
                    placeholder = 'Please select an item',
                    onInitialize = I('function() { this.setValue(""); }')
                  )),
      textInput("name.var", "Name of the latent construct", value = "", placeholder = 'Please provide a valid name'
                ),
      prettyRadioButtons("method","Select the model estimation method: EFA (Exploratory Factor Analysis) or IRT (Item Response Theory)", icon = icon("check"),
                         choiceValues=c("ml", "ols", "wls", "gls", "EM"
                                        #, "MCEM","QMCEM"
                                        ),
                         inline =FALSE, width ='600px',
                         choiceNames = c("EFA maximum likelihood", "EFA ordinary least squares", "EFA weighted least squares", "EFA generalized least squares", "IRT expectation-maximization"
                                         #, "IRT MCEM", "IRT QMCEM"
                                         #, "Expected a-posteriori each", "Plausible", "Classify"
                                         ),
                         animation = "pulse", status = "danger", shape ="round", thick =TRUE),
      prettyRadioButtons("comp","Select the score estimation method", icon = icon("check"),
                         choiceValues=c("Weighted average", "Weighted sum", "Bartlett", "regression", "EAP", "MAP", "ML"),
                         choiceNames=c("Weighted average", "Weighted sum", "Bartlett", "Regression", "Expected a-posteriori", "Maximum a-posteriori", "Maximum likelihood"),
                         inline =FALSE, width ='600px',
                         animation = "pulse", status = "danger", shape ="round", thick =TRUE),
      # div(style = "font-size: 14px;
      #              padding: 0px 0px;
      #              margin-top:-2.1em"),
      #  prettyRadioButtons("comp2","", icon = icon("check"),
      #                     choiceValues=c("EAP", "MAP", "ML"),
      #                     choiceNames = c("Expected a-posteriori", "Maximum a-posteriori", "Maximum likelihood"),
      #                     inline =FALSE, width ='600px',
      #                     animation = "pulse", status = "danger", shape ="round", thick =TRUE),
      #tags$hr(),
      column(width = 3, offset =1, actionButton("do", "Calculate",
                   style="position: relative;height: 70px;width: 210%;text-align:center;color:black;font-weight: bold;background-color:#98E0B5;border-radius: 6px;border-color:gray;border-width:2px;text-decoration:none"),
             div(style="margin-bottom:40px")
             ),
      column(width = 3, offset =2, actionButton("reset", "Clean all tabs",
                                        style="white-space:normal; position: relative;height: 70px;width: 210%;text-align:center;color:black;font-weight: bold;background-color:yellow;border-radius: 6px;border-color:gray;border-width:2px;text-decoration:none"),
             div(style="margin-bottom:40px")
             ),
      div(style = "font-size: 14px;
                   padding: 0px 0px;
                   margin-top:9.1em"),
      h2("Download"),
      column(5, offset = 1,
            downloadButton("downloadData", "Download computed .csv",
                           style="white-space:normal; position: relative;height: 70px;width: 110%;text-align:center;color:black;font-weight: bold;background-color:grey;border-radius: 6px;border-color:gray;border-width:2px;text-decoration:none")),

       column(width =3, offset = 1 ,
             radioButtons("sep.down", ".csv file separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ",")

      )),
    mainPanel(
      fluidRow(
        tabsetPanel(id="panel",
          tabPanel("Loaded data" , dataTableOutput("contents"),
                   tags$head(tags$style("#text{color: black;
                          font-size: 40px;
                          font-style: italic;
                          font-family: 'NSimSun';
                          }"
                   ))),
          tabPanel("Estimated scores" , tableOutput("contents2"),
                   tags$head(tags$style("#text{color: black;
                          font-size: 40px;
                          font-style: italic;
                          font-family: 'NSimSun';
                          }"
                   ))),
          tabPanel("Estimated loadings", tableOutput("fl"),
                        tags$head(tags$style("#text{color: black;
                          font-size: 40px;
                          font-style: italic;
                          font-family: 'NSimSun';
                          }"
                        ))),
          tabPanel("Construct statistics", tableOutput("stat"),
               tags$head(tags$style("#text{color: black;
                          font-size: 40px;
                          font-style: italic;
                          font-family: 'NSimSun';
                          }"
               ))),
          tabPanel("Fit indices", tableOutput("sum"),
                   tags$head(tags$style("#text{color: black;
                          font-size: 40px;
                          font-style: italic;
                          font-family: 'NSimSun';
                          }"
                   ))),
          tabPanel("Help", uiOutput("info"),
                   tags$head(tags$style("#text{color: black;
                          font-size: 40px;
                          font-style: italic;
                          font-family: 'NSimSun';
                          }"
                   )))
          ))

    )
  )
  )
)




server <- function(input, output, session) {



 # logo image
  output$logo <- renderImage({
    return(list(
      src =  "inst/shiny/www/logo_iper_small.png",
      height = 60,
      contentType = "image/png",
      align ="center"
    ))
  }, deleteFile = FALSE)

  url <- a("Github page", href="https://github.com/LeoEgidi/clc")
  url2 <- a("Youtube tutorial", href="https://github.com/LeoEgidi")

  # info page
  output$info <- renderUI({
    tagList(
            div(HTML("<b>Software version</b>: v1.1")),
            div(style = "padding: 5px 5px"),
            div(HTML("<b>To cite CLC Estimator (APA 7th)</b>: Marzi, G., Balzano, M., Egidi, L., Magrini, A. (2023). Concegeric Latent Contruct Estimator - CLC Estimator [Computer software].")),
            a("https://plsdeams.shinyapps.io/CLC_Estimator/", href ="https://plsdeams.shinyapps.io/CLC_Estimator/"),
            div(style = "padding: 5px 5px"),
            div(HTML("<b>Code</b>:")),
            a("Github page", href= "https://github.com/LeoEgidi/clc"),
            div(style = "padding: 5px 5px"),
            div(HTML("<b>Sample Dataset (comma separated)</b>:")),
            a("https://dx.doi.org/10.6084/m9.figshare.21786335", href = "https://dx.doi.org/10.6084/m9.figshare.21786335"),
            div(style = "padding: 5px 5px"),
            div(HTML("<b> Data policy</b>: Uploaded files will not be saved locally by CLC estimator. All data will be deleted after when closing CLC Estimator session.")),
            div(style = "padding: 5px 5px"),
            div(HTML("<b>Usage notes</b>")),
            div(),
            div(HTML("<em>What can CLC estimator do?</em>")),
            div(),
           "The code only estimates unidimensional latent constructs based on congeneric approaches. CLC estimator is not intended for statistical procedures commonly available in already available statistical packages such as exploratory factor analysis, principal component analysis, or confirmatory factor analysis.",
            div(style = "padding: 5px 5px"),
            div(HTML("<em>When should CLC estimator be used? </em>")),
            div(),
            "CLC estimator performs latent construct estimation via congeneric approaches when the available statistical packages do not embed this function. For example, scholars can use CLC estimator when they perform analysis with PLS-SEM and want to integrate such analysis with a QCA analysis.",
            div(style = "padding: 5px 5px"),
            div(HTML("<em>At what point in the data analysis process the CLC estimator should be used?  </em>")),
            div(),
            "CLC estimator should be integrated into the data analysis process after having checked the items (including reliability analysis) to be retained in the estimation of the latent construct.
            When the CLC estimator should not be used? CLC estimator assumes a common factor model, i.e., a reflective latent construct. Therefore, it is not adequate when the latent construct is a composite (Rhemtulla et al., 2020).",
            div(style = "padding: 5px 5px"),
            div(HTML("<em>How to deal with missing data, outliers, and other data issues in CLC estimator? </em>")),
            div(),
            "CLC estimator does not treat missing data, outliers or incomplete data. The users are supposed to cope with these issues before using CLC estimator. Specifically for missing data,  note that CLC Estimator applies the default missing data treatment procedure embedded in 'psych' and 'mirt' R packages. For EFA, the correlation matrix is computed with 'pairwise complete observations'. For IRT, 'full information maximum likelihood' is employed. If the database has missing data, CLC estimator promptly warns users about this issue.",
            div(style = "padding: 5px 5px"),
            div(HTML("<em>How to create a usable .csv file for CLC estimator?</em>")),
            div(),
            div(HTML("CLC estimator supports standard .csv files generated with commonly used statistical packages. The first row of the .csv file should contain the variables’ labels (i.e., the header), while the other rows should contain the data. The .csv file loading interface allows the user to select the delimiter between values (comma, semicolon, tab). The .csv file should be formatted in UNICODE with decimal symbols expressed in periods following the international scientific conventions.")),
            div(style = "padding: 5px 5px"),
            div(HTML("<em>What type of model estimation method should be used to estimate the latent constructs? </em>")),
            div(),
            "If social scientists are adopting previously validated scales, the same estimation method used for those scales should be used. If neither the estimation method is specified by the first developers of the scales, nor such scales are previously validated, maximum likelihood estimation (MLE) is recommended (for a detailed discussion on the topic, see Thompson, 2004).",
            div(style = "padding: 5px 5px"),
            div(HTML("<em>What type of output does CLC estimator produce?  </em>")),
            "CLC estimator generates a standard .csv file with variable names in the first row and values in the other rows. The generated .csv file output can then be easily imported into commonly used statistical packages.",
            div(style = "padding: 5px 5px"),
            div(HTML("<b>Install and use in R</b>")),
            div(style = "padding: 5px 5px"),
           div(HTML("<code> install.packages('devtools')  # install devtools package</code>")),
            div(style = "padding: 1px 1px"),
            div(HTML("<code> library('devtools')  # load package</code>")),
            div(style = "padding: 1px 1px"),
           div(HTML("<code> install_github('leoegidi/clc') # install clc package from Github directly </code>")),
            div(style = "padding: 1px 1px"),
           div(HTML("<code> library('clc')  # load clc package</code>")),
            div(style = "padding: 1px 1px"),
           div(HTML("<code> clc()   # load clc estimator</code>")),
            div(style = "padding: 5px 5px"),
            div(HTML("<b>References</b>")),
            div(style = "padding: 2px 2px"),
            div(HTML("Rhemtulla, M., van Bork, R., & Borsboom, D. (2020). Worse than measurement error: Consequences of inappropriate latent variable measurement models. <em>Psychological Methods</em>, 25(1), 30–45.")),
            a("https://doi.org/10.1037/met0000220", href = "https://doi.org/10.1037/met0000220"),
            div(style = "padding: 2px 2px"),
            div(HTML("Thompson, B. (2004).<em> Exploratory and confirmatory factor analysis: Understanding concepts and applications.</em> American Psychological Association.")),
            a("https://doi.org/10.1037/10694-000", href ="https://doi.org/10.1037/10694-000"),
            div(style = "padding: 5px 5px"))
  })


  #Reactive to store loaded data
  reactives <- reactiveValues(
    mydata = NULL,
    new.dataset = NULL,
    new = NULL,
    name_vectors = rep("",10^3),
    volumes = c("UserFolder"="C:/")

  )

    # observeEvent(input$new.var,{
    #   updateSelectizeInput(session,"new.var",choices=colnames(reactives$mydata))
    # })
    #Observe file being selected: input file
    observeEvent(input$file1, {
      updateTabsetPanel(session, "panel", selected = "Loaded data")

    #Store loaded data in reactive
    reactives$mydata <- read.csv(file = input$file1$datapath, sep = input$sep, header = TRUE)

    # Handle non-numeric columns
    # if (is.vector(my_update_function(reactives$mydata))==FALSE){
    # showModal(modalDialog(
    #   title= "CLC Estimator found non-numeric values in your loaded dataset. Please note that only numeric values can be used to estimate latent constructs.",
    #   footer = modalButton("Continue"),
    #   easyClose = TRUE))
    # }


    updateSelectInput(session,"new.var",choices=colnames(reactives$mydata))
    #updateSelectInput(session,"clean.var",choices=colnames(reactives$mydata))

    reactives$new.dataset <- reactives$mydata
    reactives$new <- matrix(NA, dim(reactives$new.dataset)[1],10^3)

    #Update select input
    updateSelectInput(session, inputId = 'new.var', label = 'Select items for the latent construct',
                      choices  = colnames(reactives$mydata))

    shinyDirChoose(input, "folder", roots=reactives$volumes, session=session)

  })


  output$contents <- renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1


    if (is.null(inFile))
      return(NULL)

    read.csv(inFile$datapath, sep = input$sep, header = TRUE)

    })

    # observeEvent(input$clean.var > 0,{
    #     reactives$mydata <- subset(reactives$mydata, select = - c(get(input$clean.var)))
    # })

    # check if selected items are non numeric
    observeEvent(input$new.var,{
      data <- reactives$mydata[, input$new.var]
      #if (length(input$new.var)>0){
      if (is.vector(my_update_function2(data))==FALSE){
        showModal(modalDialog(
          title= "Input data must be integer or numeric values only.
                  Please, remove non-numeric columns to estimate latent constructs.",
          footer = modalButton("Continue"),
          easyClose = TRUE))
        }
      #}
      })

    # calculation step
    observeEvent(input$do,{
      # if (!is.null(input$clean.var)){
      #   reactives$mydata <- subset(reactives$mydata, select = - c(get(input$clean.var)))
      # }else{
      #   reactives$mydata <- reactives$mydata
      # }
      data <- reactives$mydata[, input$new.var]
      # Handle non-numeric columns


        if (sum(is.na(data))>0){
        showModal(modalDialog(
          title= "CLC Estimator found missing data. Please note that CLC Estimator applies the default missing data treatment procedure embedded in “psych” and “mirt” R packages. For EFA, the correlation matrix is computed with “pairwise complete observations”. For IRT, “full information maximum likelihood” is employed.",
          easyClose = TRUE))
        }


      if (input$name.var==""){
        showModal(modalDialog(
          title= "Please, provide a valid name for your new latent construct!",
          easyClose = TRUE))

      }


      defaultW <- getOption("warn")
      options(warn = -1)
      if( input$method == "ml"| input$method == "ols" | input$method == "wls" | input$method == "gls"){
      # factor analysis with psych::fa
        if (input$comp == "regression" | input$comp == "Bartlett"){

      fit <- psych::fa(r = data, rotate = "none",
                 nfactors = 1,
                 fm = input$method,
                 scores = input$comp,
                 missing = FALSE)
      # new latent construct
      var.new <- fit$scores*sd(unlist(data), na.rm = TRUE)+mean(unlist(data), na.rm = TRUE)



       ## compute the new latent cosntruct as a weighted sum or average
       }else if (input$comp == "Weighted average"){
         fit <- psych::fa(r = data, rotate = "none",
                          nfactors = 1,
                          fm = input$method,
                          scores = "regression",
                          missing = TRUE,
                          impute = "median")
          n_items <- length(input$new.var)
          mat <- as.data.frame(as.table(fit$loadings))
          tib <- as_tibble(mat)
          factor_loading <- c()
          factor_prod_vec <- matrix(NA, n_items, dim(data)[1])

            for (i in 1:n_items){
              new.tib <- tib %>% filter(Var1 == input$new.var[i])
              factor_loading[i] <- round(as.numeric(new.tib[3]),3)
              factor_prod_vec[i,] <- factor_loading[i]*data[, input$new.var[i]]
              }
          # new latent construct
          var.new <- apply(factor_prod_vec,2, sum, na.rm = TRUE)/sum(factor_loading, na.rm = TRUE)

          }else if (input$comp == "Weighted sum") {
            fit <- psych::fa(r = data, rotate = "none",
                             nfactors = 1,
                             fm = input$method,
                             scores = "regression",
                             missing = TRUE,
                             impute = "median")
           n_items <- length(input$new.var)
           mat <- as.data.frame(as.table(fit$loadings))
           tib <- as_tibble(mat)
           factor_loading <- c()
           factor_prod_vec <- matrix(NA, n_items, dim(data)[1])

            for (i in 1:n_items){
              new.tib <- tib %>% filter(Var1 == input$new.var[i])
              factor_loading[i] <- round(as.numeric(new.tib[3]),3)
              factor_prod_vec[i,] <- factor_loading[i]*data[, input$new.var[i]]
            }
          # new latent construct
          var.new <- apply(factor_prod_vec,2, sum, na.rm = TRUE)
        }
      }else if (input$method=="EM" | input$method == "MCEM" | input$method =="QMCEM"){
        # irt with mirt
        if  (input$comp == "EAP" |input$comp == "MAP"| input$comp == "ML" ){
        fit <- gradedIRT_1(data, method = input$method, extraction = input$comp, n.input = length(input$new.var))
        # new latent construct
        var.new <- fit$scores
        }else if (input$comp =="Weighted sum"){
          fit <- gradedIRT_1(data, method = input$method, extraction = "EAP", n.input = length(input$new.var))
          n_items <- length(input$new.var)
          mat <- as.data.frame(as.table(fit$loadings))
          tib <- as_tibble(mat)
          factor_loading <- c()
          factor_prod_vec <- matrix(NA, n_items, dim(data)[1])

          for (i in 1:n_items){
            new.tib <- tib %>% filter(Var1 == input$new.var[i])
            factor_loading[i] <- round(as.numeric(new.tib[3]),3)
            factor_prod_vec[i,] <- factor_loading[i]*data[, input$new.var[i]]
          }
          # new latent construct
          var.new <- apply(factor_prod_vec,2, sum, na.rm = TRUE)
        }else if(input$comp=="Weighted average"){
          fit <- gradedIRT_1(data, method = input$method, extraction = "EAP", n.input = length(input$new.var))
          n_items <- length(input$new.var)
          mat <- as.data.frame(as.table(fit$loadings))
          tib <- as_tibble(mat)
          factor_loading <- c()
          factor_prod_vec <- matrix(NA, n_items, dim(data)[1])

          for (i in 1:n_items){
            new.tib <- tib %>% filter(Var1 == input$new.var[i])
            factor_loading[i] <- round(as.numeric(new.tib[3]),3)
            factor_prod_vec[i,] <- factor_loading[i]*data[, input$new.var[i]]
          }
          var.new <- apply(factor_prod_vec,2, sum, na.rm = TRUE)/sum(factor_loading, na.rm = TRUE)
        }
      }
      options(warn = defaultW)

      # to avoid +/-Inf scores
      range.scores <- range(var.new[var.new!=Inf & var.new != -Inf])
      var.new[var.new==Inf] <- range.scores[2]
      var.new[var.new==-Inf] <- range.scores[1]



      # computed statistics
      output$stat <- renderTable({
        var <- as.numeric(as.vector(var.new))
        mode <- function(x){f <- table(x); as.numeric(names(which.max(f)))}
        stat_table <- matrix(NA, 13, 2)
        stat_table[, 2] <- c(
          round(mean(var, na.rm = TRUE),2),   # mean
          round(mode(var),2),   # mode
          round(median(var, na.rm = TRUE),2), # median
          round(sd(var, na.rm = TRUE),2),     # sd
          round(quantile(var, 0.25, na.rm = TRUE),2), # 1st quartile
          round(quantile(var, 0.75, na.rm = TRUE),2), # 3rd quartile
          round(min(var, na.rm = TRUE),2),   # min
          round(max(var, na.rm = TRUE),2),   # max
          round(max(var, na.rm = TRUE)- min(var, na.rm = TRUE),2), # range
          round(length(var),2), # n
          round(sum(var, na.rm = TRUE),2), # sum
          round(kurtosis(var, na.rm = TRUE),2), # kurtosis
          round(skewness(var, na.rm = TRUE),2)  # skewness
        )
        colnames(stat_table) <- c("Statistics", input$name.var)
        stat_table[,1] <- c("Mean", "Mode", "Median", "SD",
                            "1st Quartile", "3rd Quartile", "Min", "Max",
                            "Range", "N", "Sum", "Kurtosis", "Skewness")
        stat_table

      })


      output$fl <- renderTable({
        a <- round(as.table(fit$loadings),3)
        b <- cbind(names(a[,1]), a[,1])
        colnames(b) <- c("Item", "Factor loadings")
        b
      })


      if (length(input$new.var)<=3){
               output$sum <- renderText({

          print("Statistics cannot be calculated due to too few degrees of freedom.")
               })
        }else{
              output$sum <- renderTable({
        if (input$method != "EM"){  # psych method
          fitIndices(fit)
          }else{
          fit$indices               # mirt method
          }
          })
        }



      output$contents2 <- renderTable({


        print("Your construct has been successfully estimated")

        reactives$new[,input$do] <- round(as.numeric(as.vector(var.new)),3)
        reactives$name_vectors[input$do] <- input$name.var
        colnames(reactives$new) <- reactives$name_vectors
        scores <- as.matrix(reactives$new[, (1:input$do)])
        colnames(scores)[1] <- reactives$name_vectors[1]
        scores
      })

      updateTabsetPanel(session, "panel", selected = "Estimated scores")

      reactives$new.dataset$daje <- round(as.numeric(as.vector(var.new)),2)
      colnames(reactives$new.dataset)[dim(reactives$new.dataset)[2]] <- input$name.var

      # Downloadable csv of selected dataset ----
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("new_dataset", ".csv", sep = "")
        },
        content = function(file) {
          write.table(as.data.frame(reactives$new.dataset), file, row.names = FALSE, sep = input$sep.down)
        }
      )
      })

    observeEvent(input$reset,{

      updateSelectInput(session, inputId = 'new.var', label = 'Select items for the latent construct',
                        choices  = colnames(reactives$mydata),
                        selected = character(0))
      updateSelectInput(session, inputId = 'name.var', label = 'Name of the new latent construct',
                        choices  = colnames(reactives$mydata),
                        selected = character(0))
      updateTabsetPanel(session, "panel", selected = "Statistics")
      updateTabsetPanel(session, "panel", selected = "Constructed output")
      updateTabsetPanel(session, "panel", selected = "Summary fit")
      updateTabsetPanel(session, "panel", selected = "Loaded data")


      output$fl <- renderTable({
        NULL
      })

      output$stat <- renderTable({
        NULL
      })

      output$contents2 <- renderTable({
        NULL
      })

      output$sum <- renderTable({
        NULL
      })

    })

     observe({
       shinyjs::toggleState(id ="do", condition = !is.null(colnames(input$file1)) &&
                              !is.null(input$new.var) &&  input$name.var!="" && length(input$new.var)>1
                            #&& is.vector(my_update_function(reactives$mydata ))
                            )
     })


     observe({
       shinyjs::toggleState(id ="reset", condition = input$do > input$reset )
     })

     observe({
       shinyjs::toggleState(id ="downloadData", condition = input$do > 0 )
     })

     observeEvent(input$method,{
       if (input$method == "ml"| input$method == "ols" | input$method == "wls" | input$method =="gls"){

         # psych::fa methods
          #shinyjs::enable("comp")
         #shinyjs::disable("comp2")
         mychoices <- c("Weighted average", "Weighted sum", "Bartlett", "regression" )
         updatePrettyRadioButtons(session, "comp",
                                  choiceNames = c("Weighted average", "Weighted sum", "Bartlett", "Regression"),
                                  choiceValues = mychoices, selected = NULL,
                                  prettyOptions = list(inline =FALSE,
                                                       icon = icon("check"),
                                                       width ='600px',
                                                       animation = "pulse",
                                                       status = "danger",
                                                       shape ="round",
                                                       thick =TRUE))

       }else{

         # mirt::mirty methods
         #shinyjs::enable("comp2")
        # shinyjs::disable("comp")
        # shinyjs::disable("comp", selector = "regression")
        # runjs('$("#comp button:eq(1)").attr("disabled", true);')
        # runjs("$('input[value=regression]').parent().attr('disabled', true);")
         mychoices <- c("Weighted average", "Weighted sum", "EAP", "MAP", "ML")
         updatePrettyRadioButtons(session, "comp",
                                  choiceNames = c("Weighted average", "Weighted sum", "Expected a-posteriori", "Maximum a-posteriori", "Maximum likelihood"),
                                  choiceValues = mychoices, selected = NULL,
                                  prettyOptions = list(inline =FALSE,
                                  icon = icon("check"),
                                  width ='600px',
                                  animation = "pulse",
                                  status = "danger",
                                  shape ="round",
                                  thick =TRUE
                                  ))
       }
     })



}

shinyApp(ui, server)
}






##  Here there are all the functions from the package 'psych' required to launch the 'fa' function.


#a function to do principal axis, minres,  weighted least squares and maximimum likelihood factor analysis
#basically, just combining the three separate functions
#the code for wls and minres is adapted from the factanal function
#the optimization function in ml is taken almost directly from the factanal function
#created May 28, 2009
#modified June 7, 2009 to add gls fitting
#modified June 24, 2009 to add ml fitting
#modified March 4, 2010 to allow for factoring of covariance matrices rather than correlation matrices
#this itself is straight foward, but the summary stats need to be worked on
#modified April 4, 2011 to allow for factor scores of oblique or orthogonal solution
#In May, 2011, fa was added as a wrapper to do iterations, and the original fa function was changed to fac.  The functionality of fa has not changed.
#Revised November, 2012 to add the minchi option for factoring.  This minimizes the sample size weighted residual matrix
#Revised 1/2/14 to add mclapply (multicore) feature.  Increase in speed is 50\% for two cores, but only 63\% for 4 cores or 76\% for 8 cores
#dropped the fisherz transform on loadings and phis
#6/12/14  Added the ability to find tetrachorics, polychorics, or mixed cors.
#15/1/15  Fixed the way we handle missing and imputation to actually work.
#19/1/15 modified calls to rotation functions to meet CRAN specs using nameSpace

# "fa" <-
#   function(r,nfactors=1,n.obs = NA,n.iter=1,rotate="oblimin",scores="regression", residuals=FALSE,SMC=TRUE,covar=FALSE,missing=FALSE,impute="median", min.err = .001,max.iter=50,symmetric=TRUE,warnings=TRUE,fm="minres",alpha=.1, p =.05,oblique.scores=FALSE,np.obs=NULL,use="pairwise",cor="cor",correct=.5,weight=NULL,...) {
#     cl <- match.call()
#     if(isCorrelation(r)) {if(is.na(n.obs) && (n.iter >1)) stop("You must specify the number of subjects if giving a correlation matrix and doing confidence intervals")
#       if(length(class(r)) > 1)  { if( inherits(r,"partial.r")) class(r) <- c("matrix","array") }
#       #in case we are using partial.r of class psych and partial.r   added 4/10/21
#     }
#
#     f <- fac(r=r,nfactors=nfactors,n.obs=n.obs,rotate=rotate,scores=scores,residuals=residuals,SMC = SMC,covar=covar,missing=missing,impute=impute,min.err=min.err,max.iter=max.iter,symmetric=symmetric,warnings=warnings,fm=fm,alpha=alpha,oblique.scores=oblique.scores,np.obs=np.obs,use=use,cor=cor, correct=correct,weight=weight,...=...) #call fa with the appropriate parameters
#     fl <- f$loadings  #this is the original
#
#
#     nvar <- dim(fl)[1]
#
#     if(n.iter > 1) {
#       if(is.na(n.obs) ) {n.obs <- f$n.obs}
#       replicates <- list()
#       rep.rots <- list()
#
#       replicateslist <- parallel::mclapply(1:n.iter,function(x) {
#         #replicateslist <- lapply(1:n.iter,function(x) {
#         if(isCorrelation(r)) {#create data sampled from multivariate normal with observed correlation
#           mu <- rep(0, nvar)
#           #X <- mvrnorm(n = n.obs, mu, Sigma = r, tol = 1e-06, empirical = FALSE)
#           #the next 3 lines replaces mvrnorm (taken from mvrnorm, but without the checks)
#           eX <- eigen(r)
#           X <- matrix(rnorm(nvar * n.obs),n.obs)
#           X <-  t(eX$vectors %*% diag(sqrt(pmax(eX$values, 0)), nvar) %*%  t(X))
#         } else {X <- r[sample(n.obs,n.obs,replace=TRUE),]}
#         fs <- fac(X,nfactors=nfactors,rotate=rotate,scores="none",SMC = SMC,missing=missing,impute=impute,min.err=min.err,max.iter=max.iter,symmetric=symmetric,warnings=warnings,fm=fm,alpha=alpha,oblique.scores=oblique.scores,np.obs=np.obs,use=use,cor=cor,correct=correct,...=...) #call fa with the appropriate parameters
#         if(nfactors == 1) {replicates <- list(loadings=fs$loadings)} else  {
#           t.rot <- target.rot(fs$loadings,fl)
#
#           if(!is.null(fs$Phi)) {  phis <- fs$Phi  # should we rotate the simulated factor  correlations?
#           #we should report the target rotated phis, not the untarget rotated phis
#           replicates <- list(loadings=t.rot$loadings,phis=phis[lower.tri(t.rot$Phi)])   #corrected 6/10/15
#           #replicates <- list(loadings=t.rot$loadings,phis=phis[lower.tri(phis)])
#           }  else
#           {replicates <- list(loadings=t.rot$loadings)}
#         }})
#
#
#       replicates <- matrix(unlist(replicateslist),nrow=n.iter,byrow=TRUE)
#
#       means <- colMeans(replicates,na.rm=TRUE)
#       sds <- apply(replicates,2,sd,na.rm=TRUE)
#
#       if(length(means) > (nvar * nfactors) ) {
#         means.rot <- means[(nvar*nfactors +1):length(means)]
#         sds.rot <-      sds[(nvar*nfactors +1):length(means)]
#         ci.rot.lower <- means.rot + qnorm(p/2) * sds.rot
#         ci.rot.upper <- means.rot + qnorm(1-p/2) * sds.rot
#         ci.rot <- data.frame(lower=ci.rot.lower,upper=ci.rot.upper)    } else  {
#           rep.rots <- NULL
#           means.rot <- NULL
#           sds.rot <- NULL
#           z.rot <- NULL
#           ci.rot <- NULL }
#
#       means <- matrix(means[1:(nvar*nfactors)],ncol=nfactors)
#       sds <- matrix(sds[1:(nvar*nfactors)],ncol=nfactors)
#       tci <- abs(means)/sds
#       ptci <- 1-pnorm(tci)
#       if(!is.null(rep.rots)) {
#         tcirot <- abs(means.rot)/sds.rot
#         ptcirot <- 1- pnorm(tcirot)} else  {tcirot <- NULL
#         ptcirot <- NULL}
#       ci.lower <-  means + qnorm(p/2) * sds
#       ci.upper <- means + qnorm(1-p/2) * sds
#
#       ci <- data.frame(lower = ci.lower,upper=ci.upper)
#       class(means) <- "loadings"
#
#       colnames(means) <- colnames(sds) <- colnames(fl)
#       rownames(means) <- rownames(sds) <- rownames(fl)
#       f$cis <- list(means = means,sds = sds,ci = ci,p =2*ptci, means.rot=means.rot,sds.rot=sds.rot,ci.rot=ci.rot,p.rot = ptcirot,Call= cl,replicates=replicates,rep.rots=rep.rots)
#       results <- f
#       results$Call <- cl
#       class(results) <- c("psych","fa.ci")
#     } else {results <- f
#     results$Call <- cl
#     class(results) <- c("psych","fa")
#     }
#     return(results)
#
#   }
# #written May 1 2011
# #modified May 8, 2014 to make cis an object in f to make sorting easier
#
# ########################################################
# #the main function
#
# "fac" <-
#   function(r,nfactors=1,n.obs = NA,rotate="oblimin",scores="tenBerge",residuals=FALSE,SMC=TRUE,covar=FALSE,missing=FALSE,impute="median", min.err = .001,max.iter=50,symmetric=TRUE,warnings=TRUE,fm="minres",alpha=.1,oblique.scores=FALSE,np.obs=NULL,use="pairwise",cor="cor",correct=.5,weight=NULL,...) {
#     cl <- match.call()
#     control <- NULL   #if you want all the options of mle, then use factanal
#
#     ##first some functions that are internal to fa
#     #this does the WLS or ULS fitting  depending upon fm
#     "fit.residuals" <- function(Psi,S,nf,S.inv=NULL,fm) {
#       diag(S) <- 1- Psi
#       if(!is.null(S.inv)) sd.inv <- diag(1/diag(S.inv))
#       eigens <- eigen(S)
#       eigens$values[eigens$values  < .Machine$double.eps] <- 100 * .Machine$double.eps
#
#       if(nf >1 ) {loadings <- eigens$vectors[,1:nf] %*% diag(sqrt(eigens$values[1:nf])) } else {loadings <- eigens$vectors[,1] * sqrt(eigens$values[1] ) }
#       model <- loadings %*% t(loadings)
#       #use switch to clean up the code
#       switch(fm,
#              wls = {residual <- sd.inv %*% (S- model)^2 %*% sd.inv},
#              gls = {residual <- (S.inv %*%(S - model))^2 } ,
#              uls = {residual <- (S - model)^2},
#              #  minres = {residual <- (S - model)^2
#              #             diag(residual) <- 0},
#              ols = {residual <- (S-model)
#              residual <- residual[lower.tri(residual)]
#              residual <- residual^2},
#              minres = {residual <- (S-model)
#              residual <- residual[lower.tri(residual)]
#              residual <- residual^2},
#              old.min = {residual <- (S-model)
#              residual <- residual[lower.tri(residual)]
#              residual <- residual^2},
#              minchi = {residual <- (S - model)^2   #min chi does a minimum residual analysis, but weights the residuals by their pairwise sample size
#              residual <- residual * np.obs
#              diag(residual) <- 0
#              })
#
#       #     #weighted least squares weights by the importance of each variable
#       #        if(fm == "wls" ) {residual <- sd.inv %*% (S- model)^2 %*% sd.inv} else {if (fm=="gls") {residual <- (S.inv %*%(S - model))^2 } else {residual <- (S - model)^2 #this last is the uls case
#       #        if(fm == "minres") {diag(residual) <- 0}   #this is minimum residual factor analysis, ignore the diagonal
#       #        if(fm=="minchi") {residual <- residual * np.obs
#       #                          diag(residual) <- 0 }   #min chi does a minimum residual analysis, but weights the residuals by their pairwise sample size
#       #        }}  # the uls solution usually seems better than wls or gls?
#       #         #
#       error <- sum(residual)
#     }
#
#     #this next section is taken (with minor modification to make ULS, WLS or GLS) from factanal
#     #it has been further modified with suggestions by Hao Wu to improve the ols/minres solution (Apri, 2017)
#     #it does the iterative calls to fit.residuals
#     #modified June 7, 2009 to add gls fits
#     #Modified December 11, 2009 to use first derivatives from formula rather than empirical.  This seriously improves the speed.
#     #but does not seem to improve the accuracy of the minres/ols solution (note added April, 2017)
#     "fit" <- function(S,nf,fm,covar) {
#       if(is.logical(SMC)) {S.smc <- smc(S,covar)} else{ S.smc <- SMC }  #added this option, August 31, 2017
#       upper <-  max(S.smc,1)  #Added Sept 11,2018  to handle case of covar , adjusted October 24 by adding 1
#       if((fm=="wls") | (fm =="gls") ) {S.inv <- solve(S)} else {S.inv <- NULL}
#       if(!covar &&(sum(S.smc) == nf) && (nf > 1)) {start <- rep(.5,nf)}  else {start <- diag(S)- S.smc}
#       #initial communality estimates are variance - smc  unless smc = 1
#
#       if(fm=="ml" || fm=="mle" )  {res <- optim(start, FAfn, FAgr, method = "L-BFGS-B",
#                                                 lower = .005, upper = upper,
#                                                 control = c(list(fnscale=1,
#                                                                  parscale = rep(0.01, length(start))), control),
#                                                 nf = nf, S = S)
#       } else {
#         if(fm=="ols" ) { #don't use a gradient
#           if(is.logical(SMC)) {start <- diag(S)- smc(S,covar)} else {start <- SMC}   #added covar  9/11/18
#           res <- optim(start, FA.OLS, method = "L-BFGS-B", lower = .005,
#                        upper = upper, control = c(list(fnscale = 1, parscale = rep(0.01,
#                                                                                    length(start)))), nf= nf, S=S )
#
#         } else {
#           if((fm=="minres")| (fm=="uls")) {  #which is actually the same as OLS but we use the gradient
#             start <- diag(S)- smc(S,covar)  #added 9/11/18    ## is this correct, or backward?
#
#             res <- optim(start, fit.residuals,gr=FAgr.minres, method = "L-BFGS-B", lower = .005,
#                          upper = upper, control = c(list(fnscale = 1, parscale = rep(0.01,
#                                                                                      length(start)))), nf= nf, S=S,fm=fm)
#
#
#
#           } else   {   #this is the case of old.min
#             start <- smc(S,covar)  #added 9/11/18 ##but why is this not diag(S)-smc(S,covar)
#             res <- optim(start, fit.residuals,gr=FAgr.minres2, method = "L-BFGS-B", lower = .005,
#                          upper = upper, control = c(list(fnscale = 1, parscale = rep(0.01,
#                                                                                      length(start)))), nf= nf, S=S, S.inv=S.inv,fm=fm )
#
#           }
#         }
#       }
#
#       if((fm=="wls") | (fm=="gls") | (fm =="ols") | (fm =="uls")| (fm=="minres") |  (fm=="old.min")) {Lambda <- FAout.wls(res$par, S, nf)} else { Lambda <- FAout(res$par, S, nf)}
#       result <- list(loadings=Lambda,res=res,S=S)
#     }
#
#     ## the next two functions are taken directly from the factanal function in order to include maximum likelihood as one of the estimation procedures
#
#     FAfn <- function(Psi, S, nf)
#     {
#       sc <- diag(1/sqrt(Psi))
#       Sstar <- sc %*% S %*% sc
#       E <- eigen(Sstar, symmetric = TRUE, only.values = TRUE)
#       e <- E$values[-(1:nf)]
#       e <- sum(log(e) - e) - nf + nrow(S)
#       -e
#     }
#     FAgr <- function(Psi, S, nf)  #the first derivatives
#     {
#       sc <- diag(1/sqrt(Psi))
#       Sstar <- sc %*% S %*% sc
#       E <- eigen(Sstar, symmetric = TRUE)
#       L <- E$vectors[, 1:nf, drop = FALSE]
#       load <- L %*% diag(sqrt(pmax(E$values[1:nf] - 1, 0)), nf)
#       load <- diag(sqrt(Psi)) %*% load
#       g <- load %*% t(load) + diag(Psi) - S     # g <- model - data
#       diag(g)/Psi^2                             #normalized
#     }
#
#     #      FAgr.minres.old <- function(Psi, S, nf,S.inv,fm)  #the first derivatives -- no longer used
#     #     {  sc <- diag(1/sqrt(Psi))
#     #         Sstar <- sc %*% S %*% sc
#     #         E <- eigen(Sstar, symmetric = TRUE)
#     #         L <- E$vectors[, 1:nf, drop = FALSE]
#     #         load <- L %*% diag(sqrt(pmax(E$values[1:nf] - 1, 0)), nf)
#     #         load <- diag(sqrt(Psi)) %*% load
#     #         model <- load %*% t(load)
#     #         g <- diag(Psi) -  diag(S -model) # g <- model - data
#     #         if(fm=="minchi") {g <- g*np.obs}
#     #         diag(g)/Psi^2                             #normalized
#     #     }
#
#     FAgr.minres2 <- function(Psi, S, nf,S.inv,fm)  #the first derivatives used by old.min
#     {
#       sc <- diag(1/sqrt(Psi))
#       Sstar <- sc %*% S %*% sc
#       E <- eigen(Sstar, symmetric = TRUE)
#       L <- E$vectors[, 1:nf, drop = FALSE]
#       load <- L %*% diag(sqrt(pmax(E$values[1:nf]-1 , 0)), nf)
#       load <- diag(sqrt(Psi)) %*% load
#       g <- load %*% t(load) + diag(Psi) - S     # g <- model - data
#       if(fm=="minchi") {g <- g*np.obs}
#       #normalized
#       diag(g)/Psi^2
#     }
#
#     FAgr.minres <- function(Psi, S, nf,fm)  #the first derivatives used by minres
#     { Sstar <- S - diag(Psi)
#     E <- eigen(Sstar, symmetric = TRUE)
#     L <- E$vectors[, 1:nf, drop = FALSE]
#     load <- L %*% diag(sqrt(pmax(E$values[1:nf] , 0)), nf)
#     # load <- diag(sqrt(Psi)) %*% load
#     g <- load %*% t(load) + diag(Psi) - S     # g <- model - data
#     #if(fm=="minchi") {g <- g*np.obs}
#     #normalized
#     diag(g)
#     }
#
#     #this was also taken from factanal
#     FAout <- function(Psi, S, q) {
#       sc <- diag(1/sqrt(Psi))
#       Sstar <- sc %*% S %*% sc
#       E <- eigen(Sstar, symmetric = TRUE)
#       L <- E$vectors[, 1L:q, drop = FALSE]
#       load <- L %*% diag(sqrt(pmax(E$values[1L:q] - 1, 0)),
#                          q)
#       diag(sqrt(Psi)) %*% load
#     }
#     #This is modified from factanal -- the difference in the loadings is that these produce orthogonal loadings, but slightly worse fit
#     FAout.wls <-  function(Psi, S, q) {
#       diag(S) <- diag(S)- Psi  # added diag(S) - Psi instead of 1- Psi to handle covar=TRUE  9/11/18
#       E <- eigen(S,symmetric = TRUE)
#       #    L <- E$vectors[,1L:q,drop=FALSE] %*%  diag(sqrt(E$values[1L:q,drop=FALSE]),q)
#       L <- E$vectors[,1L:q,drop=FALSE] %*%  diag(sqrt(pmax(E$values[1L:q,drop=FALSE],0)),q)  #added the > 0 test August 30, 2017
#       return(L)
#     }
#
#     #this takes advantage of the glb.algebraic function to do min.rank factor analysis
#     "MRFA" <- function(S,nf) {
#       com.glb <- glb.algebraic(S)
#       L <- FAout.wls(1-com.glb$solution,S,nf)
#       h2 <- com.glb$solution
#       result <- list(loadings =L, communality = h2)
#     }
#
#
#     #The next  function was adapted by WR from a suggestion by Hao Wu (April 12, 2017)
#
#     FA.OLS <- function(Psi,S,nf) {
#
#       E <- eigen(S-diag(Psi),symmetric=T)
#       U <-E$vectors[,1:nf,drop=FALSE]
#       D <- E$values[1:nf,drop=FALSE]
#       D [D < 0] <- 0
#       if(length(D) < 2) {L <- U * sqrt(D)} else { L <- U %*% diag(sqrt(D))} #gets around a weird problem for nf=1
#       model <- L %*% t(L)
#       diag(model) <- diag(S)
#       return(sum((S-model)^2)/2)
#     }
#     ##The gradient function  speeds up the function drastically but is incorrect and is not used
#     FAgr.OLS <- function(Psi, S, nf)  #the first derivatives -- seems bad
#     {   E <- eigen(S-diag(Psi), symmetric = TRUE)
#     U <- E$vectors[, 1:nf, drop = FALSE]
#     D <- E$values[1:nf]
#     D [D < 0] <-0
#     L <- U %*% diag(sqrt(D))
#     model <- L %*% t(L)
#     g <- diag(Psi) -  diag(S -model) # g <- model - data
#     diag(g)/Psi^2
#     #(diag(g) - Psi)/Psi
#     }
#
#     ###############################
#     ##############################
#     # These functions are now commented out, used to test fa
#     # #now test this
#     # test.ols <- function(R,nf) {   #this does not agree with Hao Wu -- something is wrong with the gradient
#     # start <- diag(R)- smc(R)
#     #   	res <- optim(start, FA.OLS,gr=FAgr.OLS, method = "L-BFGS-B", lower = .005,
#     #                   			upper = 1, control = c(list(fnscale = 1, parscale = rep(0.01,
#     #                   			length(start)))), nf= nf, S=R)
#     #     Lambda <- FAout.wls(res$par, R, nf)
#     # }
#     #
#     # test.ols <- function(R,nf) {   #this agrees with the Hao solution-- not using a gradient
#     # start <- diag(R)- smc(R)
#     #   	res <- optim(start, FA.OLS, method = "L-BFGS-B", lower = .005,
#     #                   			upper = 1, control = c(list(fnscale = 1, parscale = rep(0.01,
#     #                   			length(start)))), nf= nf, S=R )
#     #     Lambda1 <- FAout.wls(res$par, R, nf)
#     #
#     # }
#
#     ##########
#     #the following two functions, sent by Hao Wu have been used for benchmarking, but are not used in fa.
#     ##Now I define a function to minimize the FOLS function above w.r.t the unique variances. It returns the standardized loadings, raw loadings, unique variances, OLS function value and convergence status (0= convergence).
#     #the Hao Wu functions (although not used, they are included here for completeness
#     # FOLS<-function(Psi,S,fac){
#     #      eig<-eigen(S-diag(Psi),symmetric=T);
#     #      U<-eig$vectors[,1:fac];
#     #      D<-eig$values[1:fac];
#     #      D[D<0]<-0;
#     #      L<-U%*%diag(sqrt(D),fac,fac);
#     #      Omega<-L%*%t(L);
#     #      diag(Omega)<-diag(S);
#     #      return(sum((S-Omega)^2)/2);
#     #  }
#     #
#     #  EFAOLS2 <- function(S,Psi0=1/diag(chol2inv(chol(S))),fac) {
#     #      efa <- nlminb(Psi0,FOLS,lower=0,S=S,fac=fac)
#     #      fit.OLS<-efa$objective
#     #      fit.Psi<-efa$par
#     #      eig<-eigen(S-diag(fit.Psi),symmetric=T)
#     #      U<-eig$vectors[,1:fac]
#     #      D<-eig$values[1:fac]
#     #      D [D<0] <-0
#     #      fit.L<-U%*%diag(sqrt(D),fac,fac)
#     #      return(list(st.L=diag(1/diag(S))%*%fit.L,L=fit.L,Psi=fit.Psi,F=fit.OLS,convergence=efa$convergence))
#     #      }
#     #
#     ##############################
#     ## now start the main function
#     #np.obs <- NULL   #only returned with a value in case of fm="minchi"
#     if (fm == "mle" || fm =="MLE" || fm == "ML" ) fm <- "ml"  #to correct any confusion
#     if (!any(fm %in%(c("pa","alpha", "minrank","wls","gls","minres","minchi", "uls","ml","mle","ols" ,"old.min") ))) {message("factor method not specified correctly, minimum residual (unweighted least squares  used")
#       fm <- "minres" }
#
#     x.matrix <- r
#     n <- dim(r)[2]
#     if (!isCorrelation(r)  & !isCovariance(r)) {  matrix.input <- FALSE  #return the correlation matrix in this case
#     n.obs <- dim(r)[1]       #Added the test for nono-symmetric in case we have a covariance matrix 4/10/19
#
#     if(missing) { #impute values
#       x.matrix <- as.matrix(x.matrix)  #the trick for replacing missing works only on matrices
#       miss <- which(is.na(x.matrix),arr.ind=TRUE)
#       if(impute=="mean") {
#         item.means <- colMeans(x.matrix,na.rm=TRUE)   #replace missing values with means
#         x.matrix[miss]<- item.means[miss[,2]]} else {
#           item.med   <- apply(x.matrix,2,median,na.rm=TRUE) #replace missing with medians
#           x.matrix[miss]<- item.med[miss[,2]]}
#     }
#     #if(fm=="minchi")
#     np.obs <- pairwiseCount(r)    #used if we want to do sample size weighting
#     if(covar) {cor <- "cov"}
#
#     # if given a rectangular matrix, then find the correlation or covariance
#     #multiple ways of find correlations or covariances
#     #added the weights option to tet, poly, tetrachoric, and polychoric  June 27, 2018
#     switch(cor,
#            cor = {if(!is.null(weight))  {r <- cor.wt(r,w=weight)$r} else  {
#              r <- cor(r,use=use)}
#            },
#            cov = {r <- cov(r,use=use)
#            covar <- TRUE},
#            wtd = { r <- cor.wt(r,w=weight)$r},
#            spearman = {r <- cor(r,use=use,method="spearman")},
#            kendall = {r <- cor(r,use=use,method="kendall")},
#            tet = {r <- tetrachoric(r,correct=correct,weight=weight)$rho},
#            poly = {r <- polychoric(r,correct=correct,weight=weight)$rho},
#            tetrachoric = {r <- tetrachoric(r,correct=correct,weight=weight)$rho},
#            polychoric = {r <- polychoric(r,correct=correct,weight=weight)$rho},
#            mixed = {r <- mixedCor(r,use=use,correct=correct)$rho},
#            Yuleb = {r <- YuleCor(r,,bonett=TRUE)$rho},
#            YuleQ = {r <- YuleCor(r,1)$rho},
#            YuleY = {r <- YuleCor(r,.5)$rho }
#     )
#
#
#
#
#     } else { matrix.input <- TRUE #don't return the correlation matrix
#     if(fm=="minchi") {
#       if(is.null(np.obs)) {fm <- "minres"
#       message("factor method minchi does not make sense unless we know the sample size, minres used instead")
#       }
#     }
#     if(is.na(n.obs) && !is.null(np.obs))         n.obs <- max(as.vector(np.obs))
#     if(!is.matrix(r)) {  r <- as.matrix(r)}
#     if(!covar) {
#       r <- cov2cor(r)  #probably better to do it this way (11/22/2010)
#       #sds <- sqrt(diag(r))    #convert covariance matrices to correlation matrices
#       # r <- r/(sds %o% sds) #if we remove this, then we need to fix the communality estimates
#     }
#     } #added June 9, 2008
#     #does this next line actually do anything?
#     if (!residuals) { result <- list(values=c(rep(0,n)),rotation=rotate,n.obs=n.obs,np.obs=np.obs,communality=c(rep(0,n)),loadings=matrix(rep(0,n*n),ncol=n),fit=0)} else { result <- list(values=c(rep(0,n)),rotation=rotate,n.obs=n.obs,np.obs=np.obs,communality=c(rep(0,n)),loadings=matrix(rep(0,n*n),ncol=n),residual=matrix(rep(0,n*n),ncol=n),fit=0,r=r)}
#
#
#     if(is.null(SMC)) SMC=TRUE   #if we don't specify it, make it true
#     r.mat <- r
#     Phi <- NULL
#     colnames(r.mat) <- rownames(r.mat) <- colnames(r)
#     if(any(is.na(r))) {
#       bad <- TRUE
#       tempr <-r
#       wcl <-NULL
#       while(bad) {
#         wc <- table(which(is.na(tempr), arr.ind=TRUE))  #find the correlations that are NA
#         wcl <- c(wcl,as.numeric(names(which(wc==max(wc)))))
#         tempr <- r[-wcl,-wcl]
#         if(any(is.na(tempr))) {bad <- TRUE} else {bad <- FALSE}
#       }
#
#       cat('\nLikely variables with missing values are ',colnames(r)[wcl],' \n')
#       stop("I am sorry: missing values (NAs) in the correlation matrix do not allow me to continue.\nPlease drop those variables and try again." )
#     }
#     if(is.logical(SMC) )  {
#       if(SMC) {if(nfactors <= n)   {#changed to <= n instead of < n/2 This warning seems to confuse people unnecessarily
#         diag(r.mat) <- smc(r,covar=covar)
#       }  else {if (warnings) {
#         message("In fa, too many factors requested for this number of variables to use SMC for communality estimates, 1s are used instead")}
#       }   } else { diag(r.mat) <- 1
#       }
#     } else { diag(r.mat) <- SMC}
#     orig <- diag(r)
#
#     comm <- sum(diag(r.mat))
#     err <- comm
#     i <- 1
#     comm.list <- list()
#
#     #principal axis is an iterative eigen value fitting
#
#     if(fm =="alpha") { #alpha factor analysis iteratively replaces the diagonal with revised communalities, and then rescales the matrix
#       i <- 1   #count the iterations
#       e.values <- eigen(r,symmetric=symmetric)$values   #store the original solution
#       H2 <- diag(r.mat)  #the original communality estimate
#       while(err > min.err) {    #iteratively replace the diagonal with our revised communality estimate
#         r.mat <- cov2cor(r.mat)  #this has rescaled the correlations based upon the communalities
#         eigens <- eigen(r.mat,symmetric=symmetric)
#         loadings <- eigens$vectors[,1:nfactors,drop=FALSE] %*% diag(sqrt(eigens$values[1:nfactors,drop=FALSE]))
#
#
#         model <- loadings %*% t(loadings)
#         newH2 <- H2 * diag(model)
#
#         err <- sum(abs(H2 - newH2))
#         r.mat <- r
#         diag(r.mat) <- newH2
#         H2 <- newH2
#         i <- i + 1
#         if(i > max.iter) {
#           if(warnings)  {message("maximum iteration exceeded")}
#           err <-0 }
#       } #end of while loop
#       loadings <- sqrt(H2) * loadings
#       eigens <- sqrt(H2) *  eigens$vaues
#       comm1 <- sum(H2)
#     }  #end alpha factor analysis
#
#     if(fm=="pa") {
#       e.values <- eigen(r,symmetric=symmetric)$values   #store the original solution
#       while(err > min.err)    #iteratively replace the diagonal with our revised communality estimate
#       {
#         eigens <- eigen(r.mat,symmetric=symmetric)
#
#         if(nfactors >1 ) {loadings <- eigens$vectors[,1:nfactors] %*% diag(sqrt(eigens$values[1:nfactors])) } else {loadings <- eigens$vectors[,1] * sqrt(eigens$values[1] ) }
#         model <- loadings %*% t(loadings)
#         new <- diag(model)
#         comm1 <- sum(new)
#         diag(r.mat) <- new
#         err <- abs(comm-comm1)
#         if(is.na(err)) {warning("imaginary eigen value condition encountered in fa\n Try again with SMC=FALSE \n exiting fa")
#           break}
#         comm <- comm1
#         comm.list[[i]] <- comm1
#         i <- i + 1
#         if(i > max.iter) {
#           if(warnings)  {message("maximum iteration exceeded")}
#           err <-0 }
#       }  #end of while loop
#       eigens <- eigens$values
#     }
#
#     if (fm=="minrank")  {mrfa <- MRFA(r,nfactors)
#     loadings <- mrfa$loadings
#     model <- loadings %*% t(loadings)
#     e.values <- eigen(r)$values
#     S <- r
#     diag(S) <- diag(model)
#     eigens <- eigen(S)$values
#     }
#
#     if((fm == "wls") | (fm=="minres") |(fm=="minchi") | (fm=="gls") | (fm=="uls")|(fm== "ml")|(fm== "mle") | (fm=="ols") | (fm=="old.min")) {
#       uls <- fit(r,nfactors,fm,covar=covar)
#
#       e.values <- eigen(r)$values  #eigen values of pc: used for the summary stats --
#       result.res  <- uls$res
#
#       loadings <- uls$loadings
#       model <- loadings %*% t(loadings)
#       S <- r
#       diag(S) <- diag(model)   #communalities from the factor model
#       eigens <- eigen(S)$values
#
#     }
#
#     # a weird condition that happens with poor data
#     #making the matrix symmetric solves this problem
#     if(!is.double(loadings)) {warning('the matrix has produced imaginary results -- proceed with caution')
#       loadings <- matrix(as.double(loadings),ncol=nfactors) }
#     #make each vector signed so that the maximum loading is positive  -  should do after rotation
#     #Alternatively, flip to make the colSums of loading positive
#
#
#     if (nfactors >1) {sign.tot <- vector(mode="numeric",length=nfactors)
#     sign.tot <- sign(colSums(loadings))
#     sign.tot[sign.tot==0] <- 1
#     loadings <- loadings %*% diag(sign.tot)
#     } else { if (sum(loadings) < 0) {loadings <- -as.matrix(loadings)} else {loadings <- as.matrix(loadings)}
#       colnames(loadings) <- "MR1" }
#
#
#     switch(fm,
#            alpha = {colnames(loadings) <- paste("alpha",1:nfactors,sep='')},
#            wls={colnames(loadings) <- paste("WLS",1:nfactors,sep='')	},
#            pa= {colnames(loadings) <- paste("PA",1:nfactors,sep='')} ,
#            gls = {colnames(loadings) <- paste("GLS",1:nfactors,sep='')},
#            ml = {colnames(loadings) <- paste("ML",1:nfactors,sep='')},
#            minres = {colnames(loadings) <- paste("MR",1:nfactors,sep='')},
#            minrank = {colnames(loadings) <- paste("MRFA",1:nfactors,sep='')},
#            minchi = {colnames(loadings) <- paste("MC",1:nfactors,sep='')}
#     )
#
#     rownames(loadings) <- rownames(r)
#     loadings[loadings==0.0] <- 10^-15    #added to stop a problem with varimax if loadings are exactly 0
#
#     model <- loadings %*% t(loadings)
#
#     f.loadings <- loadings #used to pass them to factor.stats
#
#     rot.mat <- NULL
#     if(rotate != "none") {if (nfactors > 1) {
#
#       if (rotate=="varimax" |rotate=="Varimax" | rotate=="quartimax" | rotate =="bentlerT" | rotate =="geominT" | rotate =="targetT" | rotate =="bifactor"   | rotate =="TargetT"|
#           rotate =="equamax"| rotate =="varimin"|rotate =="specialT" | rotate =="Promax"  | rotate =="promax"| rotate =="cluster" |rotate == "biquartimin" |rotate == "TargetQ"  |rotate =="specialQ" ) {
#         Phi <- NULL
#         switch(rotate,  #The orthogonal cases  for GPArotation + ones developed for psych
#                varimax = {rotated <- stats::varimax(loadings)  #varimax is from stats, the others are from GPArotation
#                loadings <- rotated$loadings
#                rot.mat <- rotated$rotmat},
#                Varimax = {if (!requireNamespace('GPArotation')) {stop("I am sorry, to do this rotation requires the GPArotation package to be installed")}
#                  #varimax is from the stats package, Varimax is from GPArotations
#                  #rotated <- do.call(rotate,list(loadings,...))
#                  #rotated <- do.call(getFromNamespace(rotate,'GPArotation'),list(loadings,...))
#                  rotated <- GPArotation::Varimax(loadings,...)
#                  loadings <- rotated$loadings
#                  rot.mat <- t(solve(rotated$Th))} ,
#                quartimax = {if (!requireNamespace('GPArotation')) {stop("I am sorry, to do this rotation requires the GPArotation package to be installed")}
#
#                  #rotated <- do.call(rotate,list(loadings))
#                  rotated <- GPArotation::quartimax(loadings,...)
#                  loadings <- rotated$loadings
#                  rot.mat <- t(solve(rotated$Th))} ,
#                bentlerT =  {if (!requireNamespace('GPArotation')) {stop("I am sorry, to do this rotation requires the GPArotation package to be installed")}
#
#                  #rotated <- do.call(rotate,list(loadings,...))
#                  rotated <- GPArotation::bentlerT(loadings,...)
#                  loadings <- rotated$loadings
#                  rot.mat <- t(solve(rotated$Th))} ,
#                geominT	= {if (!requireNamespace('GPArotation')) {stop("I am sorry, to do this rotation requires the GPArotation package to be installed")}
#
#                  #rotated <- do.call(rotate,list(loadings,...))
#                  rotated <- GPArotation::geominT(loadings,...)
#                  loadings <- rotated$loadings
#                  rot.mat <- t(solve(rotated$Th))} ,
#                targetT = {if (!requireNamespace('GPArotation')) {stop("I am sorry, to do this rotation requires the GPArotation package to be installed")}
#                  rotated <- GPArotation::targetT(loadings,Tmat=diag(ncol(loadings)),...)
#                  loadings <- rotated$loadings
#                  rot.mat <- t(solve(rotated$Th))} ,
#
#                bifactor = {rot <- bifactor(loadings,...)
#                loadings <- rot$loadings
#                rot.mat <- t(solve(rot$Th))},
#                TargetT =  {if (!requireNamespace('GPArotation')) {stop("I am sorry, to do this rotation requires the GPArotation package to be installed")}
#                  rot <- GPArotation::targetT(loadings,Tmat=diag(ncol(loadings)),...)
#                  loadings <- rot$loadings
#                  rot.mat <- t(solve(rot$Th))},
#                equamax =  {rot <- equamax(loadings,...)
#                loadings <- rot$loadings
#                rot.mat <- t(solve(rot$Th))},
#                varimin = {rot <- varimin(loadings,...)
#                loadings <- rot$loadings
#                rot.mat <- t(solve(rot$Th))},
#                specialT =  {rot <- specialT(loadings,...)
#                loadings <- rot$loadings
#                rot.mat <- t(solve(rot$Th))},
#                Promax =   {pro <- Promax(loadings,...)  #Promax without Kaiser normalization
#                loadings <- pro$loadings
#                Phi <- pro$Phi
#                rot.mat <- pro$rotmat},
#                promax =   {#pro <- stats::promax(loadings,...)   #from stats
#                  pro <- kaiser(loadings,rotate="Promax",...)   #calling promax will now do the Kaiser normalization before doing Promax rotation
#                  loadings <- pro$loadings
#                  rot.mat <- pro$rotmat
#                  # ui <- solve(rot.mat)
#                  # Phi <-  cov2cor(ui %*% t(ui))
#                  Phi <- pro$Phi
#                },
#                cluster = 	 {loadings <- varimax(loadings,...)$loadings
#                pro <- target.rot(loadings)
#                loadings <- pro$loadings
#                Phi <- pro$Phi
#                rot.mat <- pro$rotmat},
#                biquartimin =    {ob <- biquartimin(loadings,...)
#                loadings <- ob$loadings
#                Phi <- ob$Phi
#                rot.mat <- t(solve(ob$Th))},
#                TargetQ  =  {ob <- TargetQ(loadings,...)
#                loadings <- ob$loadings
#                Phi <- ob$Phi
#                rot.mat <- t(solve(ob$Th))},
#                specialQ = {ob <- specialQ(loadings,...)
#                loadings <- ob$loadings
#                Phi <- ob$Phi
#                rot.mat <- t(solve(pro$Th))})
#       } else {
#         #The following oblique cases all use GPArotation
#         if (rotate =="oblimin"| rotate=="quartimin" | rotate== "simplimax" | rotate =="geominQ"  | rotate =="bentlerQ"  |rotate == "targetQ"  ) {
#           if (!requireNamespace('GPArotation')) {warning("I am sorry, to do these rotations requires the GPArotation package to be installed")
#             Phi <- NULL} else {
#
#               ob <- try(do.call(getFromNamespace(rotate,'GPArotation'),list(loadings,...)))
#               if(inherits(ob,as.character("try-error")))  {warning("The requested transformaton failed, Promax was used instead as an oblique transformation")
#                 ob <- Promax(loadings)}
#
#               loadings <- ob$loadings
#               Phi <- ob$Phi
#               rot.mat <- t(solve(ob$Th))}
#         } else {message("Specified rotation not found, rotate='none' used")}
#       }
#     }
#     }
#
#     signed <- sign(colSums(loadings))
#     signed[signed==0] <- 1
#     loadings <- loadings %*% diag(signed)  #flips factors to be in positive direction but loses the colnames
#     if(!is.null(Phi)) {Phi <- diag(signed) %*% Phi %*% diag(signed) }  #added October 20, 2009 to correct bug found by Erich Studerus
#
#     switch(fm,
#            alpha = {colnames(loadings) <- paste("alpha",1:nfactors,sep='')},
#            wls={colnames(loadings) <- paste("WLS",1:nfactors,sep='')	},
#            pa= {colnames(loadings) <- paste("PA",1:nfactors,sep='')} ,
#            gls = {colnames(loadings) <- paste("GLS",1:nfactors,sep='')},
#            ml = {colnames(loadings) <- paste("ML",1:nfactors,sep='')},
#            minres = {colnames(loadings) <- paste("MR",1:nfactors,sep='')},
#            minrank = {colnames(loadings) <- paste("MRFA",1:nfactors,sep='')},
#            uls =  {colnames(loadings) <- paste("ULS",1:nfactors,sep='')},
#            old.min = {colnames(loadings) <- paste0("oldmin",1:nfactors)},
#            minchi = {colnames(loadings) <- paste("MC",1:nfactors,sep='')})
#     #just in case the rotation changes the order of the factors, sort them
#     #added October 30, 2008
#
#     if(nfactors >1) {
#       ev.rotated <- diag(t(loadings) %*% loadings)
#       ev.order <- order(ev.rotated,decreasing=TRUE)
#       loadings <- loadings[,ev.order]}
#     rownames(loadings) <- colnames(r)
#     if(!is.null(Phi)) {Phi <- Phi[ev.order,ev.order] } #January 20, 2009 but, then, we also need to change the order of the rotation matrix!
#     class(loadings) <- "loadings"
#     if(nfactors < 1) nfactors <- n
#     # if(max(abs(loadings) > 1.0) && !covar) warning(' A loading greater than abs(1) was detected.  Examine the loadings carefully.')
#     result <- factor.stats(r,loadings,Phi,n.obs=n.obs,np.obs=np.obs,alpha=alpha)   #do stats as a subroutine common to several functions
#     result$rotation <- rotate
#     result$communality <- diag(model)
#     if(max(result$communality > 1.0) && !covar) warning("An ultra-Heywood case was detected.  Examine the results carefully")
#     if(fm == "minrank") {result$communalities <- mrfa$communality} else {if(fm=="pa" | fm == "alpha") {result$communalities <- comm1} else {result$communalities <- 1- result.res$par}}
#
#
#     result$uniquenesses <- diag(r-model)
#     result$values <-  eigens
#     result$e.values <- e.values
#     result$loadings <- loadings
#     result$model <- model
#     #diag(result$model) <- diag(r)
#     result$fm <- fm  #remember what kind of analysis we did
#     result$rot.mat <- rot.mat
#     if(!is.null(Phi) ) {colnames(Phi)  <- rownames(Phi) <- colnames(loadings) #added 2/14/16 to help with fa.multi
#     result$Phi <- Phi      #the if statement was incorrectly including oblique.scores.  Fixed Feb, 2012 following a report by Jessica Jaynes
#     Structure <- loadings %*% Phi} else {Structure <- loadings}
#     class(Structure) <- "loadings"
#     result$Structure <- Structure #added December 12, 2011
#
#     if(fm == "pa") result$communality.iterations <- unlist(comm.list)
#     #Some of the Grice equations use the pattern matrix, but some use the structure matrix
#     #we are now dropping this oblique score option  (9/2/17)
#     result$method=scores  #this is the chosen method for factor scores
#     if(oblique.scores) {result$scores <- factor.scores(x.matrix,f=loadings,Phi=NULL,method=scores) } else {result$scores <- factor.scores(x.matrix,f=loadings,Phi=Phi,method=scores)}
#     if(is.null( result$scores$R2))  result$scores$R2 <- NA
#     result$R2.scores <- result$scores$R2
#
#     result$weights <- result$scores$weights    #these are the weights found in factor scores and will be different from the ones reported by factor.stats
#     result$scores <- result$scores$scores
#     if(!is.null(result$scores)) colnames(result$scores) <- colnames(loadings) #added Sept 27, 2013
#     result$factors <- nfactors
#     result$r <- r   #save the correlation matrix
#     result$np.obs <- np.obs
#     result$fn <- "fa"
#     result$fm <- fm
#
#
#     #Find the summary statistics of Variance accounted for
#     #normally just found in the print function  (added 4/22/17)
#     #from  the print function
#     if(is.null(Phi)) {if(nfactors > 1)  {vx <- colSums(loadings^2) } else {vx <- sum(loadings^2)
#     }} else {vx <- diag(Phi %*% t(loadings) %*% loadings)
#     }
#
#     vtotal <- sum(result$communality + result$uniquenesses)
#     names(vx) <- colnames(loadings)
#     varex <- rbind("SS loadings" =   vx)
#     varex <- rbind(varex, "Proportion Var" =  vx/vtotal)
#     if (nfactors > 1) {
#       varex <- rbind(varex, "Cumulative Var"=  cumsum(vx/vtotal))
#       varex <- rbind(varex, "Proportion Explained"=  vx/sum(vx))
#       varex <- rbind(varex, "Cumulative Proportion"=  cumsum(vx/sum(vx)))
#     }
#
#     result$Vaccounted <- varex
#     result$Call <- cl
#     class(result) <- c("psych", "fa")
#     return(result) }
#
# #modified October 30, 2008 to sort the rotated loadings matrix by the eigen values.
# #modified Spring, 2009 to add multiple ways of doing factor analysis
# #corrected, August, 2009 to count the diagonal when doing GLS or WLS - this mainly affects (improves) the chi square
# #modified April 4, 2011 to find the factor scores of the oblique factors
# #modified December 12, 2011 to report structure coefficients as well as pattern (loadings)
# #modified February 11, 2013 to correctly treat SMC=FALSE as 1s instead of 0s.
# #modified spring, 2015 to use switch in the rotation options
# #modified August 25, 2015 to add rot.mat as output
# #modified February 22, 2016 to keep the diagonal of the model as it should be -- e.g., the communalities
# #December 23, 2019  changed the call to mixed.cor to be mixedCor.
#
#
# #Pearson or polychoric correlations with confidence intervals
#
# "cor.ci" <-
#   function(x, keys = NULL, n.iter = 100, p = 0.05, overlap=FALSE, poly = FALSE, method = "pearson",plot=TRUE,minlength=5,n=NULL,...) {
#     corCi(x=x, keys = keys, n.iter = n.iter, p = p, overlap=overlap, poly = poly, method = method,plot=plot,minlength=minlength,n=n,...) }
#
# "corCi" <-
#   function(x, keys = NULL, n.iter = 100, p = 0.05, overlap=FALSE, poly = FALSE, method = "pearson",plot=TRUE,minlength=5,n = NULL,...) {
#
#     cl <- match.call()
#     n.obs <- dim(x)[1]
#
#     if(!isCorrelation(x)) {#the normal case is to have data and find the correlations and then bootstrap them
#       #added the direct from correlation matrix option, August 17, 2019 since I was finding them for statsBy
#
#       if(is.null(keys) && overlap) overlap <- FALSE  #can not correct for overlap with just items
#       if(poly) {  #find polychoric or tetrachoric correlations if desired
#         ncat <- 8
#         nvar <- dim(x)[2]
#         tx <- table(as.matrix(x))
#
#         if(dim(tx)[1] ==2) {tet <- tetrachoric(x)
#         typ = "tet"} else {  #should we do mixed correlations?
#           tab <- apply(x,2,function(x) table(x))
#           if(is.list(tab)) {len <- lapply(tab,function(x) length(x))} else {len <- dim(tab)[1] }
#
#           dvars <- subset(1:nvar,len==2)   #find the dichotomous variables
#           pvars <- subset(1:nvar,((len > 2) & (len <= ncat)))  #find the polytomous variables
#           cvars <- subset(1:nvar,(len > ncat))  #find the continuous variables (more than ncat levels)
#           if(length(pvars)==ncol(x)) {tet <- polychoric(x)
#           typ = "poly"} else {tet <- mixedCor(x)
#           typ="mixed" }
#         }
#
#         Rho <- tet$rho    #Rho is the big correlation of all of items
#       } else { Rho <- cor(x,use="pairwise",method=method)    #the normal Pearson correlations
#       }
#       #now, if there are keys, find the correlations of the scales
#       if(!is.null(keys)) {bad <- FALSE
#       if(!is.matrix(keys)) keys <- make.keys(x,keys)  #handles the new normal way of just passing a keys list
#       if(any(is.na(Rho))) {warning(sum(is.na(Rho)), " of the item correlations are NA and thus finding scales that include those items will not work.\n We will try to do it for those  scales which do not include those items.
#          \n Proceed with caution. ")
#         bad <- TRUE
#         rho <- apply(keys,2,function(x) colMeans(apply(keys,2,function(x) colMeans(Rho*x,na.rm=TRUE))*x,na.rm=TRUE))  #matrix multiplication without matrices!
#         #switched to using colMeans instead of colSums, recognizing the problem of different number of items being dropped.
#       } else {
#         rho <- t(keys) %*% Rho %*% keys} }  else {rho <- Rho} #find the covariances between the scales
#
#       #
#
#       ##correct for overlap if necessary on the original data
#
#       if(overlap) { key.var <- diag(t(keys) %*% keys)
#       var <- diag(rho)    #these are the scale variances
#       n.keys <- ncol(keys)
#       key.av.r <- (var - key.var)/(key.var * (key.var-1))
#       item.cov <- t(keys) %*% Rho #this will blow up if there are bad data
#       raw.cov <- item.cov %*% keys
#       adj.cov <- raw.cov
#       for (i in 1:(n.keys)) {
#         for (j in 1:i) {
#           adj.cov[i,j] <- adj.cov[j,i]<- raw.cov[i,j] - sum(keys[,i] * keys[,j] ) + sum(keys[,i] * keys[,j] *  sqrt(key.av.r[i] * key.av.r[j]))
#         }
#       }
#       diag(adj.cov) <- diag(raw.cov)
#       rho <- cov2cor(adj.cov)
#       }
#       rho <- cov2cor(rho)     #scale covariances to correlations
#       nvar <- dim(rho)[2]
#
#
#       if(n.iter > 1) {
#         replicates <- list()
#         rep.rots <- list()
#         ##now replicate it to get confidence intervals
#         #replicates <- lapply(1:n.iter,function(XX) {
#
#         replicates <- mclapply(1:n.iter,function(XX) {
#           xs <- x[sample(n.obs,n.obs,replace=TRUE),]
#           {if(poly) {
#             if(typ!= "tet") {tets <- mixedCor(xs)} else {tets <- tetrachoric(xs)}
#
#             R <- tets$rho} else {R <- cor(xs,use="pairwise",method=method)}  #R is the big correlation matrix
#
#             if(!is.null(keys)) { if (bad) {covariances <- apply(keys,2,function(x) colMeans(apply(keys,2,function(x) colMeans(R*x,na.rm=TRUE))*x,na.rm=TRUE))  #matrix multiplication without matrices!
#             } else {
#               covariances <- t(keys) %*% R %*% keys}
#               r <- cov2cor(covariances) } else {r <- R}
#             #correct for overlap if this is requested
#             if(overlap) {
#               var <- diag(covariances)
#               item.cov <- t(keys) %*% R
#               raw.cov <- item.cov %*% keys
#               adj.cov <- raw.cov
#               key.av.r <- (var - key.var)/(key.var * (key.var-1))
#               for (i in 1:(n.keys)) {
#                 for (j in 1:i) {
#                   adj.cov[i,j] <- adj.cov[j,i]<- raw.cov[i,j] - sum(keys[,i] * keys[,j] ) + sum(keys[,i] * keys[,j] *  sqrt(key.av.r[i] * key.av.r[j]))
#                 }
#               }
#               diag(adj.cov) <- diag(raw.cov)
#               r <- cov2cor(adj.cov) #fixed 03/12/14
#             }
#             rep.rots <- r[lower.tri(r)]
#           }
#         }
#         )
#
#       }
#
#       replicates <- matrix(unlist(replicates),ncol=nvar*(nvar-1)/2,byrow=TRUE)
#       z.rot <- fisherz(replicates)
#       means.rot <- colMeans(z.rot,na.rm=TRUE)
#       sds.rot <- apply(z.rot,2,sd, na.rm=TRUE)
#       sds.rot <- fisherz2r(sds.rot)
#       ci.rot.lower <- means.rot + qnorm(p/2) * sds.rot  #This is the normal value of the observed distribution
#       ci.rot.upper <- means.rot + qnorm(1-p/2) * sds.rot
#       means.rot <- fisherz2r(means.rot)
#       ci.rot.lower <- fisherz2r(ci.rot.lower)
#       ci.rot.upper <- fisherz2r(ci.rot.upper)
#       low.e <- apply(replicates,2,quantile, p/2,na.rm=TRUE)
#       up.e  <- apply(replicates, 2, quantile, 1-p/2,na.rm=TRUE)
#       tci <- abs(means.rot)/sds.rot
#       ptci <- 1- pnorm(tci)    #subtract from 1 (added 11/14/20)
#       ci.rot <- data.frame(lower=ci.rot.lower,low.e=low.e,upper=ci.rot.upper,up.e=up.e,p =2*(ptci))   #dropped the 1-ptci 11/14/20
#       cnR <- abbreviate(colnames(rho),minlength=minlength)
#       k <- 1
#       for(i in 1:(nvar-1)) {for (j in (i+1):nvar) {
#         rownames(ci.rot)[k] <- paste(cnR[i],cnR[j],sep="-")
#         k<- k +1 }}
#
#
#
#
#       results <- list(rho=rho, means=means.rot,sds=sds.rot,tci=tci,ptci=ptci,ci=ci.rot,Call= cl,replicates=replicates)
#       #if(plot) {cor.plot.upperLowerCi(results,numbers=TRUE,cuts=c(.001,.01,.05),...) }  #automatically plot the results
#       if(plot) {cor.plot(rho,numbers=TRUE,cuts=c(.001,.01,.05),pval =  2*(1-ptci),...) }
#       class(results) <- c("psych","cor.ci")
#
#       return(results)  } else {#we have been given correlations, just find the cis.
#         if(is.null(n)) {warning("\nFinding confidence intervals from a correlation matrix, but  n is not specified, arbitrarily set to 1000")
#           n <- 1000}
#         results <- cor.Ci(x,n=n, alpha=p, minlength=minlength)
#         results$ci <- results$r.ci
#         results$r <- x
#         class(results) <- cs(psych, corCi)
#         return(results)
#       }
#
#   }
# #written Sept 20, 2013
# #adapted from fa.poly
# #modified May 1, 2014 to scale by pvals
# #modified August 24, 2017 to include Bonferoni corrections from cor.test
#
# "cor.plot.upperLowerCi" <- "corPlotUpperLowerCi" <-
#   function(R,numbers=TRUE,cuts=c(.001,.01,.05),select=NULL,main="Upper and lower confidence intervals of correlations",adjust=FALSE,...) {
#
#     if(adjust) {lower <- R$ci.adj$lower.adj
#     upper <- R$ci.adj$upper.adj} else {
#       lower <- R$ci$lower
#       upper <- R$ci$upper}
#     temp <- lower
#     if(is.null(R$r)) {cn = colnames(R$rho)
#     rl <- R$rho[lower.tri(R$rho)]} else {
#       cn = colnames(R$r)
#       rl <-  R$r[lower.tri(R$r)]} #is input from cor.ci or corr.test
#     lower[rl < 0 ] <- upper[rl < 0]
#     upper[rl < 0] <- temp[rl < 0]
#     m <- length(lower)
#     n <- floor((sqrt(1 + 8 * m) +1)/2)
#     X <- diag(n)
#     X[lower.tri(X)] <- upper
#     X <- t(X)
#     X[lower.tri(X)] <- lower
#     diag(X) <- 1
#     colnames(X) <- rownames(X) <- cn
#     if(is.null(R$ptci))  {pval <- R$p} else {pval = 2*(1-R$ptci)}
#
#     cor.plot(X,numbers=numbers,pval=pval,cuts=cuts,select=select,main=main,...)
#     class(X) <-  c("psych","cor.cip")
#     colnames(X) <- abbreviate(rownames(X,4))
#     invisible(X) }
#
#
#
# #A number of useful helper functions
# #added January, 2012
# #most are public, some are local just for me
#
# #a dummy function to allow the help to find misc
# "psych.misc" <-
#   function() {}
#
# "lowerMat" <-
#   function(R,digits=2,minlength=5) {
#     lowleft <- lower.tri(R,diag=TRUE)
#     nvar <- ncol(R)
#     nc <- digits+3
#     width <- getOption("width")
#     k1 <- width/(nc+2)
#     if(is.null(colnames(R))) {colnames(R) <- paste("C",1:nvar,sep="")}
#     if(is.null(rownames(R))) {rownames(R) <- paste("R",1:nvar,sep="")}
#     colnames(R) <- abbreviate(colnames(R),minlength=minlength)
#
#     nvar <- ncol(R)
#     nc <- digits+3
#     #k1 <- width/(nc+2)
#
#     if(k1 * nvar < width) {k1 <- nvar}
#     k1 <- floor(k1)
#     if(!is.character(R) ) {
#       fx <- format(round(R,digits=digits))} else {fx <- format(R)}
#     if(nrow(R) == ncol(R) ) {fx[!lowleft] <- ""}
#     for(k in seq(0,nvar,k1)) { if(k<nvar) {
#       print(fx[(k+1):nvar,(k+1):min((k1+k),nvar)],quote=FALSE)}
#     }
#     invisible(R[lower.tri(R,diag=FALSE)])}
#
# "lowerCor" <-
#   function(x,digits=2,use="pairwise",method="pearson",minlength=5) {
#     nvar <- NCOL(x)
#     x <- char2numeric(x)    #added 1/2/21
#     R <- cor(x,use=use,method=method)
#     lowerMat(R,digits,minlength=minlength)
#     invisible(R)
#   }
#
#
# #adapted from utils::txtProgressBar
# #modified August 10, 2012 to print just 100 times.
# "progressBar" <-
#   function(value,max,label=NULL) {
#     if(inherits(stdout()[1],"terminal")) { pc <- round(100 * value/max)  #only print to the screen, not to a file
#     if(ceiling(100 * value/max)==floor(100 * value/max)) {
#       width <- 100
#       char="."
#       nw <- nchar(char, "w")
#       nb <- round(width * value/max )
#
#       #cat(paste(c("\r  ",label," |", rep.int(" ", nw * width + 6)), collapse = ""))
#       cat(paste(c("\r  ",label," |", rep.int(char, nb), rep.int(" ", nw * (width - nb)), sprintf("| %3d%%", pc)), collapse = ""))
#     }
#     }
#     #flush.console()
#     flush(stdout())
#   }
#
#
# "reflect" <-
#   function(f,flip=NULL) {
#     if(is.null(names(f))) {temp <- f
#     f <-list()
#     f$loadings <- temp}
#     rnames <- colnames(f$loadings)
#     rnames[flip] <- paste(rnames[flip],'(R)',sep="")
#     flipper <- rep(1,ncol(f$loadings))
#     flipper[flip] <- -1
#     flip <- diag(flipper)
#     colnames(flip) <- rownames(flip) <- rnames
#     f$loadings <- f$loadings %*% flip
#     if(!is.null(f$weights)) f$weights <- f$weights %*% flip
#     if(!is.null(f$scores)) f$scores <- f$scores %*% flip
#     if(!is.null(f$Phi)) f$Phi <- flip %*% f$Phi %*% t(flip)
#     return(f)
#   }
#
#
#
#
# #and futher patched May 10, 2017 to get the right denominator (I was dividing by n-1 - lag instead of n-lag
# #patched March 1, 2017 to get the df right for lags
# #developed January 10, 2012 to find the mean square of successive differences
# #see Von Neuman et al. 1941
# #added the minus 1 to the case of a single vector.  Sept 21, 2017
# #the matrix version was correct, just not the single case version
# "mssd" <- function(x,group=NULL,lag=1,na.rm=TRUE) {
#   if(is.null(group)) {
#     if(is.vector(x)) { result <- sum(diff(x,lag=lag,na.rm=na.rm)^2,na.rm=na.rm)/(sum(!is.na(x)) -lag )} else {
#       x <- as.matrix(x)
#       if(NCOL(x) == 1) {
#         result <- sum(diff(x,lag=lag,na.rm=na.rm)^2,na.rm=na.rm)/(sum(!is.na(x))-lag)
#       } else {
#         n <- colSums(!is.na(x))  -lag
#         result <- colSums(diff(x,lag=lag,na.rm=na.rm)^2,na.rm=na.rm)/n}
#     } } else {
#       x <- as.matrix(x) #added 26/5/14
#       if(NROW(group) != NROW(x)) group <- x[,group]  #added 26/5/14
#       nvar <- ncol(x)
#       cname <- colnames(x)
#       temp <- by(x,group, mssd,na.rm=na.rm,lag=lag)
#       rownn <- lapply(temp,is.null)
#       if(sum(as.integer(rownn)) > 0) {
#         rown <-  names(temp)[-which(rownn==TRUE)] } else {rown <- names(temp) }
#
#       result <- t(matrix(unlist(temp),nrow=nvar))
#       colnames(result) <- cname
#       rownames(result) <- rown
#     }
#   return(result)}
#
#
# "rmssd" <- function(x,group=NULL,lag=1,na.rm=TRUE) {
#   return(sqrt(mssd(x,group=group,lag=lag,na.rm=na.rm))) }
#
#
#
# ##### Added March 1, 2017
# "autoR" <- function(x,group=NULL,lag=1,na.rm=TRUE,use="pairwise") {
#
#   if(is.null(group)) {
#     n.obs <- NROW(x)
#     if(is.vector(x)) {
#       x <- as.vector(scale(x,scale=FALSE))
#       mssd <- sum(diff(x,lag=lag,na.rm=na.rm)^2,na.rm=na.rm)/(sum(!is.na(x))-lag)
#       v1 <- sd(x[1:(n.obs-lag)],na.rm=na.rm)^2
#       v2 <- sd(x[(lag+1):n.obs],na.rm=na.rm)^2
#       # r <- -(mssd - v1 - v2)/(2*sqrt(v1*v2))
#       r <- cor(x[1:(n.obs-lag)],x[(lag+1):n.obs],use=use)
#       result <- list(autoR = r,rssd=sqrt(mssd))  #fixed May 10 ,2017 to correct autorR-
#     } else {
#       x <- as.matrix(x)
#       n <- colSums(!is.na(x))
#       mssd <- colSums(diff(x,lag=lag,na.rm=na.rm)^2,na.rm=na.rm)/(n-lag)
#       v1 <- apply(x[1:(n.obs-lag),],2,sd, na.rm=na.rm)^2
#       v2 <- apply(x[(lag+1):n.obs,],2, sd,na.rm=na.rm)^2
#       # r <- -(mssd - v1 - v2)/(2*sqrt(v1*v2))
#       r <- diag(cor(x[1:(n.obs-lag),],x[(lag+1):n.obs,],use=use))
#       result <- list(autoR = r,rssd=sqrt(mssd))
#     }
#   } else {
#     cl <- match.call()
#     x <- as.matrix(x) #added 26/5/14
#     if(NROW(group) != NROW(x)) group <- x[,group]  #added 26/5/14
#     nvar <- ncol(x)
#     cname <- colnames(x)
#     temp <- by(x,group, autoR,na.rm=na.rm,lag=lag)
#
#
#     rownn <- lapply(temp,is.null)
#     if(sum(as.integer(rownn)) > 0) {
#       rown <-  names(temp)[-which(rownn==TRUE)] } else {rown <- names(temp) }
#
#     tm <- t(matrix(unlist(temp),nrow=nvar*2))
#     autoR <- tm[,1:nvar]
#     rmssd  <- tm[,(nvar+1):(nvar*2)]
#
#     colnames(autoR) <- colnames(rmssd) <- cname
#     rownames(autoR) <- rownames(rmssd) <- rown
#
#     result <- list(autoR = autoR,rmssd=rmssd,Call=cl)
#   }
#   class(result) <- c("psych","autoR")
#   return(result)}
#
#
#
# #####
# sim.mssd <- function(n,r,g=.1) {
#   rw <- rnorm(n,sqrt(1/(1-r^2)))
#   x  <- xg <- rep(0,n)
#   for(i in 2:n) {x[i] <- r*x[i-1] + rw[i]
#   xg[i] <- x[i] + g*i }
#   rx <- sample(x,n,replace=FALSE)
#   x2 <- x*2
#   rx2 <- rx*2
#   x.df <- data.frame(x,rx,x2,rx2,xg)
#   return(x.df)}
#
#
#
#
# #shannon complexity index
# "shannon" <-
#   function(x,correct=FALSE,base=2) {if(is.null(dim(x))) {
#     t <- table(x)
#     s <- sum(t)
#     p <- t/s
#     H <- -sum(p * log(p,base))
#     if(correct) {
#       Hmax <- -log(1/length(p),base)
#       H <- H/Hmax}
#   } else {  H <- apply(x,2,function(x) shannon(x, correct=correct, base=base))}
#     return(H)
#   }
#
#
# test.all <- function(p) {
#   library(p,character.only=TRUE)
#   ob <- paste("package",p,sep=":")
#   ol <- objects(ob)
#   nf <- length(ol)
#   for(i in 1:nf) {
#     fn <- as.character(ol[[i]])
#     example(topic=fn,package=p,character.only=TRUE)
#   }
#   detach(ob,character.only=TRUE)
# }
#
# #lookup a set of items from a bigger set
# lookupItem <- function(x,y) {
#   n.look <- NROW(x)
#   possible <- list()
#   y <- as.character(y)
#   x <- as.character(x)
#   for(i in 1:n.look){
#     temp <- grep(x[i],y)
#     if(length(temp)>0) possible[i]<- temp
#
#   }
#   return(possible)
# }
#
#
# #lookup which x's are found in y[c1],return matches for y[]
# "lookup" <-
#   function(x,y,criteria=NULL) {
#     if (is.null(criteria)) {temp <- match(x,rownames(y))} else {
#       temp <- match(x,y[,criteria])}
#     if(any(!is.na(temp))) {
#       y <- (y[temp[!is.na(temp)],,drop=FALSE]) } else {y <- NA}
#     return(y)}
#
# #use lookup to take fa/ic output and show the results
# #modified July 4, 2017 to allow for omega output as well
# #modified June 23, 2018 to limit to top n items per factor and abs(loading) > cut
# #modified April 30, 2020 to include the ability to handle bassAckward output
# "fa.lookup"  <-
#   function(f,dictionary=NULL,digits=2,cut=.0,n=NULL,sort=TRUE) {
#     omega <- bassAck <- none <- lavaan <- NA    #weird requirement to avoid being global
#     if(length(class(f)) > 1){ obnames <- cs(omega, fa, principal, iclust,bassAck, none)
#     value <- inherits(f, obnames, which=TRUE)
#     if (any(value > 1)) {value <- obnames[which(value >0)]} else {value <- "none"}}
#     if(sort & (value !="bassAck")) {f <- fa.sort(f)}
#     switch(value,
#
#            omega = {f <- f$schmid$sl
#            h2 <- NULL},
#            fa    = {
#              h2 <- f$communality
#              com <- f$complexity
#              f <- f$loading        },
#            principal= {
#              h2 <- f$communality
#              com <- f$complexity
#              f <- f$loading},
#            iclust = {f <- f$loadings
#            h2 <- NULL},
#            bassAck = {nlevels <- length(f$bass.ack)
#            h2 <- NULL
#            ord1 <- rownames(fa.sort(f$bass.ack[[nlevels-1]]))
#            f <- f$bass.ack[[nlevels]]
#            f <- f[,ord1]
#            f <- fa.sort(f)
#            colnames(f) <- paste0("F",1:ncol(f))
#            },
#            none = {f <- f
#            h2 <- NULL})
#
#     n.fact <- NCOL(f)
#
#     ord <- rownames(f)
#     old.names <- ord
#     ord <- sub("-","",ord)
#     rownames(f) <- ord
#     if(!is.null(dictionary))  {
#       contents <- lookup(rownames(f),dictionary)} else {message("fa.lookup requires a dictionary, otherwise just use fa.sort")}
#     if(!is.null(h2)) {results <- data.frame(round(unclass(f),digits=digits),com=round(com,digits=digits),h2=round(h2,digits=digits))} else {
#       results <- data.frame(round(unclass(f),digits=digits))}
#
#     results <- merge(results,contents,by="row.names",all.x=TRUE,sort=FALSE)
#     rownames(results) <- results[,"Row.names"]
#     results <- results[ord,-1]  #now put it back into the correct order
#     rownames(results) <- old.names
#     if(!is.null(n)) {
#       rn <-rownames(results)
#       results <- cbind(results,rn)
#       f2c <- table(apply(abs(results[1:n.fact]),1,which.max))   #which column is the maximum value
#       k <- 1
#       j <- 1
#       for(i in 1:n.fact) {
#         results[k:(k+min(n,f2c[i])),] <-  results[j:(j+ min(n,f2c[i])),]
#         k <- (k+min(n,f2c[i]))
#         j <- j + f2c[i]   }
#       results <- results[1:(k-1),]
#       rownames(results) <- results[,"rn"]
#       results <- results[,-NCOL(results)]
#     }
#
#     if(cut > 0) {
#       r.max <- apply(abs(results[,1:n.fact]),1,max)
#       results <- results[abs(r.max) > cut,]
#     }
#     return(results)}
#
#
#
#
# #revised 07/07/18 to add the cluster option
# #revised 12/07/18 to allow for simple matrix input
# #read a matrix, return a matrix
# #read a list, return a list
# #modified 9/2/20 for the case of not full rank cluster scores
# "fa.organize" <-
#   function(fa.results,o=NULL,i=NULL,cn=NULL,echelon=TRUE,flip=TRUE) {
#     if(is.null(names(fa.results)) )  {temp <- fa.results   #the matrix form
#     if(flip) {
#       total.load <-colSums(temp)
#       flipper <- sign(total.load)
#       flipper[flipper==0] <-1
#       temp <- t( t(temp) * flipper ) }
#     if(!is.null(o)) {temp <- temp[,o]}
#     if(!is.null(i)) {temp <-temp[i,]}
#     fa.results <- temp
#     nf <- ncol(temp)} else { # the list form
#       nf <- ncol(fa.results$loadings)
#       if(echelon & is.null(o) ) {temp <- apply(abs(  fa.results$loadings),1,which.max)
#       nf <- ncol(fa.results$loadings)
#       nvar <- nrow(fa.results$loadings)
#       o <- rep(NA,nf)
#       k <- 1
#       o[k] <- temp[k]
#       for (ki in 2:nvar) {if (!(temp[ki] %in% o[1:k])) {o[k+1] <- temp[ki]
#       k <- k + 1}  }
#       }
#
#       #now, a kludge for the case where there are some empty columns
#
#       n.good <- sum(1:nf %in% o)
#       if (n.good < nf) { tt <- 1:nf
#       o[(n.good + 1):nf] <- tt[!1:nf %in% o]
#       }
#       if(flip) {
#         total.load <- colSums(fa.results$loadings)
#         flipper <- sign(total.load)
#         flipper[flipper==0] <-1 } else { flipper <- rep(1,NCOL(fa.results$loadings)) }
#       fa.results$loadings <- t(t(fa.results$loadings) * flipper)
#
#
#       if(!is.null(o)) {fa.results$loadings <- fa.results$loadings[,o]
#       flipper <- flipper[o]
#       fa.results$Structure <- t(t(fa.results$Structure[,o]) * flipper)
#       fa.results$valid <- t(t(fa.results$valid[o])*flipper)
#       fa.results$score.cor <- fa.results$score.cor[o,o]
#       fa.results$r.scores <- fa.results$r.scores[o,o]
#       fa.results$R2 <- fa.results$R2[o]
#       if(!is.null(cn)) {colnames(fa.results$loadings) <- cn}
#       fa.results$Phi <- fa.results$Phi[o,o]}
#       if(!is.null(i)) {fa.results$loadings <- fa.results$loadings[i,]
#       fa.results$Structure <- fa.results$Structure[i,]
#       fa.results$weights <- fa.results$weights[i,]
#       fa.results$complexity=fa.results$complexity[i]
#       fa.results$uniquenesses <- fa.results$uniquenesses[i]}
#     }
#     return(fa.results)
#   }
#
#
# #fixed 13/6/14 to solve the missing data problem
# "con2cat" <- function(old,cuts=c(0,1,2,3),where) {
#   new <- old
#   nc <- length(cuts)
#   if(missing(where)) where <- 1:ncol(old)
#   lw <- length(where)
#   if(is.matrix(cuts)) {mcuts <- cuts} else {mcuts <- matrix(rep(cuts,lw),nrow=lw,byrow=TRUE)}
#   vwhere <- as.vector(where)
#   for (w in 1:lw) {where <- vwhere[w]
#   cuts <- mcuts[w,]
#   nc <- length(cuts)
#   if(nc < 2 ) {new[(!is.na(new[,where]) & ( old[,where] <= cuts)),where] <- 0
#   new[(!is.na(new[,where]) & ( old[,where] > cuts)),where] <- 1}   else {
#
#
#
#     new[(!is.na(new[,where]) & ( old[,where] <= cuts[1])),where] <- 0
#     for(i in(2:nc)) {
#       new[(!is.na(new[,where]) & ( old[,where] > cuts[i-1] )),where] <- i-1
#       # & (new[(!is.na(new[,where]) & ( old[,where] > cuts[i-1] )),where]),where]  <- i-1
#     }
#     new[(!is.na(new[,where]) & ( old[,where] > cuts[nc])),where]  <- nc }
#   }
#   new}
#
# "keys.lookup" <- function(keys.list,dictionary) {
#   if(is.list(keys.list)) { items <-  sub("-","",unlist(keys.list))
#   f <- make.keys(items,keys.list)}
#   keys.list <- fa.sort(f)
#   contents <- lookup(rownames(f), y=dictionary)
#   rownames(contents)[rowSums(f) <0 ] <- paste0(rownames(contents)[rowSums(f)<0],"-")
#   return(contents)
# }
#
# "item.lookup" <-
#   function (f,m, dictionary,cut=.3, digits = 2) {
#     f <- fa.sort(f)
#     none<- NULL   #A strange requirement of R 4.0
#     if(length(class(f)) > 1){ obnames <- cs(omega, fa, principal, iclust, none)
#     value <- inherits(f, obnames, which=TRUE)
#     if (any(value > 1)) {value <- obnames[which(value >0)]} else {value <- "none"}} else {value <- "none"}
#     old.names <- NULL
#     switch(value,
#
#            omega = {f <- f$schmid$sl
#            h2 <- NULL
#            old.names <- rownames(f)
#            rownames(f) <- sub("-","",old.names)},
#            fa    = {
#              h2 <- f$communality
#              com <- f$complexity
#              f <- f$loading        },
#            principal= {
#              h2 <- f$communality
#              com <- f$complexity
#              f <- f$loading},
#            iclust = {f <- f$loadings
#            h2 <- NULL},
#            none = {f <- f
#            h2 <- NULL})
#
#     if (!(is.matrix(f) || is.data.frame(f))) {
#       h2 <- f$communality
#       com <- f$complexity
#       ord <- rownames(f$loadings)
#       nfact <- ncol(f$loadings)
#       f <- f$loadings
#     }
#     else {
#       h2 <- NULL
#       com <- NULL
#       ord <- rownames(f)
#       nfact <- ncol(f)
#     }
#     means <- m[ord]   #incorrectly added a comma to allow it sort dataframes 6/22/21
#     f <- data.frame(unclass(f),means=means)
#
#     contents <- lookup(rownames(f), y=dictionary)
#     if (!is.null(h2)) {
#       results <- data.frame(round(f, digits = digits),
#                             com = round(com, digits = digits), h2 = round(h2,
#                                                                           digits = digits))
#     }
#     else {
#       results <- data.frame(round(f, digits = digits))
#     }
#     results <- merge(results, contents, by = "row.names", all.x = TRUE,
#                      sort = FALSE)
#     rownames(results) <- results[, "Row.names"]
#     results <- results[ord, -1]
#     if(!is.null(old.names)) rownames(results) <- old.names
#     res <- results
#     #   res <- results[0,]  #make an empty data frame of the structure of results
#     #     for (i in 1:nfact) { temp <- results[abs(results[,i]) > cut,]
#     #      ord <- order(temp[,"means"])
#     #      res <- rbind(res,temp[ord,])
#     #      }
#     return(res)
#   }
#
# setCorLookup<- function(x,dictionary=NULL,cut=0,digits=2,p=.05) {
#   coef <- x$coefficients
#   probs <- x$Probability
#   labels <- dictionary[rownames(coef),,drop=FALSE]
#
#   coef[probs > p] <- NA
#   result <- list()
#   nvar <- NCOL(coef)
#   for (i in 1:nvar) {
#     ord <- order(abs(coef[,i]),decreasing=TRUE)
#     temp <- cbind(coef=round(coef[ord,i],digits),labels[ord,])
#     result[[i]] <- data.frame(temp[!is.na(temp[,1]),])
#   }
#   names(result) <- colnames(coef)
#   result
# }
#
#
# "lookupItems" <- function(content=NULL,dictionary=NULL,search=c("Item","Content","item")) {
#   location <-which(colnames(dictionary) %in% search)
#   value <- grep(content,dictionary[,location])
#   dictionary[value,,drop=FALSE]
# }
#
#
# "falsePositive" <- function(sexy=.1,alpha=.05,power=.8) {
#   pf <- alpha * (sexy)
#   vp <- power * (1-sexy)
#   pf/(pf+vp)}
#
# "build.html.help" <- function(p="psych",fn = "/Volumes/Test/psych/man",out="/Volumes/Test/help/") {
#
#   db <- list.files(fn)
#   for (f in db) {tools::Rd2HTML(paste(fn,db[f]),out=paste(out,db[f]),package=p)
#   }
# }
#
#
#
# "bullseye" <- function(x,y,n) {
#   for(i in 1:n) {dia.ellipse(x,y,e.size=i)}}
#
#
# "rel.val" <- function(n,sdx=.2,bars=TRUE,arrow.len=.05) {
#   if(n>20) {pc <- "."} else {pc <- 16}
#
#   plot(NA,xlim=c(0,10),ylim=c(0,10),axes=FALSE,xlab="",ylab="",main="Reliability and Validity as target shooting")
#   #Reliable and valid
#   x=3
#   y=2
#   bullseye(x,y,4)
#   x1 <- x + rnorm(n,0,sdx)
#   y1 <- y + rnorm(n,0,sdx)
#   xm <- mean(x1)
#   ym <- mean(y1)
#   points(x1,y1,pch=pc)
#   points(xm,ym,pch=20,col="red")
#   if(bars) error.crosses(x1,y1,add=TRUE,arrow.len=arrow.len,labels="")
#   text(x,y-2,"Reliable and valid")
#
#   #unReliable and  invalid
#   x=7
#   y=7
#   bullseye(x,y,4)
#   x1 <- x + rnorm(n,1,1)
#   y1 <- y + rnorm(n,1,1)
#   xm <- mean(x1)
#   ym <- mean(y1)
#   points(x1,y1,pch=pc)
#   points(xm,ym,pch=20,col="red")
#   if(bars) error.crosses(x1,y1,add=TRUE,arrow.len=arrow.len,labels="")
#   text(x,y-2,"Unreliable and Invalid")
#
#   #reliable and invalid
#   x=7
#   y=2
#   bullseye(x,y,4)
#   x1 <- x + rnorm(n,1,sdx)
#   y1 <- y + rnorm(n,1,sdx)
#   xm <- mean(x1)
#   ym <- mean(y1)
#   points(x1,y1,pch=pc)
#   points(xm,ym,pch=20,col="red")
#   if(bars)error.crosses(x1,y1,add=TRUE,arrow.len=arrow.len,labels="")
#
#   text(x,y-2,"Reliable and Invalid")
#
#
#   #unreliable, but valid
#   x=3
#   y=7
#
#   bullseye(x,y,4)
#   x1 <- x + rnorm(n,0,1)
#   y1 <- y + rnorm(n,0,1)
#   xm <- mean(x1)
#   ym <- mean(y1)
#   points(x1,y1,pch=pc)
#   points(xm,ym,pch=20,col="red")
#   if(bars) error.crosses(x1,y1,add=TRUE,arrow.len=arrow.len,labels="")
#   text(x,y-2,"Unreliable but Valid")
# }
# #rel.val(10,.5)
#
#
# # "cor2" <- function(x,y,digits=2,use="pairwise",method="pearson") {
# # R <- cor(x,y,use=use,method=method)
# # print(round(R,digits))
# # invisible(R)}
#
# #finally added the char2numeric so it will not choke on character variables   1/3/21
#
# "cor2" <- function(x,y=NULL,digits=2,use="pairwise",method="pearson") {
#   multi <- FALSE
#   if(is.list(x) && is.null(y)) {multi <- TRUE
#   n <- length(x)
#   xi <- x[[1]]
#   for (i in 2:n) {xi <- cbind(xi,x[[i]])}
#
#   R <- cor(xi,use=use,method=method) }else {
#     x <- char2numeric(x)
#     y <- char2numeric(y)
#     R <- cor(x,y,use=use,method=method)}
#   if(multi) {lowerMat(R,digits) } else {print(round(R,digits))}
#   invisible(R)}
#
# levels2numeric <- function(x) {
#   n.var <- ncol(x)
#   for(item in 1:n.var) {
#     if (is.factor(x[,item])) x[,item] <- as.numeric(x[,item])}
#   invisible(x)
# }
#
#
# signifNum <-  function(x,digits=2) {
#   if(!is.null(ncol(x))) {sign <- rep(1,prod(dim(x)))} else {
#     sign <- rep(1,length(x))}
#   sign[which(x < 0)] <- -1
#   base <- trunc(log10(sign*x))
#   mantissa <-  x/10^base
#   pretty <- round(mantissa,digits=digits-1) * 10^base
#   pretty[which ((sign * x) == 0,arr.ind=TRUE)] <- 0 #fix the ones that are -Inf
#   pretty}
#
#
# #October 25, 2016
# #June 18 2020  added  as.factor to convert character strings that are not stored as factors
# #this has a downsize that it converts numbers stored as characters to factors (see nchar2numeric)
# #added the flag option and the change the colname option 1/2/21
#
# "char2numeric" <- function(x,flag=TRUE) {
#   nvar <- NCOL(x)
#   for(i in 1:nvar) {
#     if(!is.numeric(x[[i]] ))  {
#       if(is.factor(unlist(x[[i]])) | is.character(unlist(x[[i]]))) {  x[[i]] <- as.numeric(as.factor(x[[i]]))
#       } else {x[[i]] <- NA}
#       if(flag) colnames(x)[i] <- paste0(colnames(x)[i],"*")
#     }
#
#   }
#   invisible(x)}
#
#
# #added June 25, 2020 to handle the case of numeric data stored as characters
# #added flag and colnames option  1/2/21
# "nchar2numeric" <- function(x,flag=TRUE) {
#   nvar <- NCOL(x)
#   for(i in 1:nvar) {
#     if(!is.numeric(x[[i]] ))  {
#       if(is.factor(unlist(x[[i]])) | is.character(unlist(x[[i]]))) {
#         if(is.factor(unlist(x[[i]]))) {  x[[i]] <- as.numeric(as.factor(x[[i]])) } else {x[[i]] <- as.numeric(x[[i]])}
#       } else {x[[i]] <- NA}
#       if(flag) colnames(x)[i] <- paste0(colnames(x)[i],"*")
#     }
#   }
#   invisible(x)}
#
#
# #this just shows if it is a matrix is symmetric and has diagonals of 1
# #Added the unclass to  handle a problem with class partial.r  4/10/21
# "isCorrelation" <-  function(x) {value <- FALSE
# if(NROW(x) == NCOL(x)) {
#   if( is.data.frame(x)) {if(isSymmetric(unclass(unname(as.matrix(x))))) { value <- TRUE}} else {if(isSymmetric(unclass(unname(x)))) {value <- TRUE}}}
# value <- value && isTRUE(all.equal(prod(diag(as.matrix(x))),1) )
# value <- value && isTRUE((min(x,na.rm=TRUE)>= -1) & (max(x,na.rm=TRUE) <= 1))
# return(value)}
#
# #this just shows if it is a symmetric matrix
# "isCovariance" <-  function(x) {value <- FALSE
# if(NROW(x) == NCOL(x)) {
#   if( is.data.frame(x)) {if(isSymmetric(unclass(unname(as.matrix(x))))) { value <- TRUE}} else {if(isSymmetric(unclass(unname(x)))) {value <- TRUE}}}
# # value <- value && isTRUE(all.equal(prod(diag(as.matrix(x))),1) )  #don't check for diagonal of 1
# return(value)}
#
#
#
#
# #cs is taken from Hmisc:::Cs
# cs <- function(...) {as.character(sys.call())[-1]}
# #acs is modified to produce a single string
# acs <- function(...) {gsub(",","",toString(sys.call()[-1]))}
#
# fromTo <- function(data,from,to=NULL) {cn <- colnames(data)
# if(is.null(to)) {to <- from[2]
# from <- from[1]}
# from <- which(cn == as.character(from))
# to =  which(cn == as.character(to))
# select <- from:to
# return(data[select])}
#
#
#
# #flip -- not public but useful trick
# flip <- function(R,key) {#reverse correlations with keys  < 0
#   R[key<0,] <- -R[key < 0,]
#   R[,key<0] <- -R[,key < 0]
#   return(R)}
#
#
# matMult <- function(A,B) {  #matrix multiplication without matrices!
#   nvar1 <- ncol(A)
#   nvar2 <- ncol(B)
#   M <- matrix(NA,ncol=nvar1,nrow=nrow(B))
#   if(nrow(A) != nvar2) {stop("incompatible dimensions")}
#   for(i in 1:nvar1) {
#     for (j in 1:nrow(B))  {
#       M[j,i] <-  sum(A[,i] * B[j,], na.rm=TRUE)
#     }
#   }
#   return(M)}
#
#
#
# factorScoresSapa  <- function(weights,items) {
#   nf <- NCOL(weights)
#   scores<- matrix(NA,nrow=NROW(items),ncol=nf)
#   n.obs <- NROW(items)
#   n.items <- NCOL(items)
#   titem <- t(items)
#   for (factors in (1:nf)) {
#
#     scores[,factors] <-t(colMeans( weights[,factors] * titem,  na.rm=TRUE))
#   }
#   return(scores)
# }
#
#
# "factor2cluster" <-
#   function (loads,cut=.0,aslist=FALSE)
#   {
#
#     if (!is.matrix(loads) ) {l <-loads$loadings} else {l <- loads}
#
#     l <- as.matrix(l)
#     nrows <- dim(l)[1]
#     ncols <- dim(l)[2]
#     if (ncols ==1) {m1 <- matrix(rep(1,nrows),ncol=1) } else {
#       m1 <- matrix(apply(t(apply(l, 1, abs)), 1, which.max),
#                    ncol = 1)}
#     id <- matrix(c(1:nrows, m1), ncol = 2)  #index row and column
#     factor2cluster <- matrix(rep(0, ncols * nrows), ncol = ncols)
#     factor2cluster[id] <- sign(l[id])*( (abs(l[id]) >cut)+0)  #only loadings > cut
#     rownames(factor2cluster) <- rownames(l)
#     colnames(factor2cluster) <- colnames(l)
#     nitems <- colSums(abs(factor2cluster))
#     for (i in ncols:1) {if (nitems[i]<1) {factor2cluster <- factor2cluster[,-i,drop=FALSE]} }#remove columns with no variables
#     if(aslist) factor2cluster <- keys2list(factor2cluster)
#     return(factor2cluster)
#   }
#
# "factor.scores" <- function(x,f,Phi=NULL,method=c("Thurstone","tenBerge","Anderson","Bartlett","Harman","components"),rho=NULL,impute="none") {
#   #the normal case is f is the structure matrix and Phi is not specified
#   #Note that the Grice formulas distinguish between Pattern and Structure matrices
#   #I need to confirm that I am doing this
#
#   if(length(method) > 1) method <- "tenBerge"   #the default
#   if(method=="regression") method <- "Thurstone"
#   if(method %in% c("tenberge", "Tenberge","tenBerge","TenBerge")) method <- "tenBerge"
#   if(length(class(f)) > 1) { if(inherits(f[2] ,"irt.fa" )) f <- f$fa  }
#
#   if(!is.matrix(f)) {Phi <- f$Phi
#   f <- loadings(f)
#   if(ncol(f)==1) {method <- "Thurstone"}
#   }
#   nf <- dim(f)[2]
#   if(is.null(Phi)) Phi <- diag(1,nf,nf)
#   if(dim(x)[1] == dim(f)[1]) {r <- as.matrix(x)
#   square <- TRUE} else {
#     square <- FALSE
#     if(!is.null(rho)) {r <- rho } else {
#       r <- cor(x,use="pairwise") #find the correlation matrix from the data
#     }}
#
#   S <- f %*% Phi   #the Structure matrix
#   switch(method,
#          "Thurstone" = { w <- try(solve(r,S),silent=TRUE )  #these are the factor weights (see Grice eq. 5)
#          if(inherits(w,"try-error")) {message("In factor.scores, the correlation matrix is singular, an approximation is used")
#            r <- cor.smooth(r)}
#
#          w <- try(solve(r,S),silent=TRUE)
#          if(inherits(w,"try-error")) {message("I was unable to calculate the factor score weights, factor loadings used instead")
#            w <- f}
#          colnames(w) <- colnames(f)
#          rownames(w) <- rownames(f)
#          },
#
#          "tenBerge" = { #Following Grice equation 8 to estimate scores for oblique solutions (with a correction to the second line where r should r.inv
#            L <- f %*% matSqrt(Phi)
#            r.5 <- invMatSqrt(r)
#
#            r <- cor.smooth(r)
#            inv.r <- try(solve(r),silent=TRUE)
#            if(inherits(inv.r, as.character("try-error")))  {warning("The tenBerge based scoring could not invert the correlation matrix, regression scores found instead")
#              ev <- eigen(r)
#              ev$values[ev$values < .Machine$double.eps] <- 100 * .Machine$double.eps
#              r <- ev$vectors %*% diag(ev$values) %*% t(ev$vectors)
#              diag(r)  <- 1
#              w <- solve(r,f)}  else {
#                C <- r.5 %*% L %*% invMatSqrt(t(L) %*% inv.r %*% L)    #note that this is the correct formula, per Grice personal communication
#                w <- r.5 %*% C %*% matSqrt(Phi)}
#            colnames(w) <- colnames(f)
#            rownames(w) <- rownames(f)
#          },
#
#
#
#          "Harman" = { #Grice equation 10 --
#            #   m <- t(f)  %*% f  #factor intercorrelations
#            m <- f %*% t(S)  #should be this  (the model matrix)  Revised August 31, 2017
#            diag(m) <- 1  #Grice does not say this, but it is necessary to make it work!
#            inv.m <- solve(m)
#            #  w <- f %*%inv.m
#            w <- inv.m %*% f
#          },
#
#
#
#          "Anderson" =  { #scores for orthogonal factor solution will be orthogonal  Grice Eq 7 and 8
#            I <- diag(1,nf,nf)
#            h2 <-  diag( f %*% Phi %*% t(f))
#            U2 <- 1 - h2
#            inv.U2 <- diag(1/U2)
#            w <- inv.U2 %*% f %*% invMatSqrt(t(f) %*% inv.U2 %*% r %*% inv.U2 %*% f)
#            colnames(w) <- colnames(f)
#            rownames(w) <- rownames(f)
#          },
#
#          "Bartlett" = {    #Grice eq 9  # f should be the pattern, not the structure
#            I <- diag(1,nf,nf)
#            h2 <-  diag( f %*% Phi %*% t(f))
#            U2 <- 1 - h2
#            inv.U2 <- diag(1/U2)
#            w <- inv.U2 %*% f %*% (solve(t(f) %*% inv.U2 %*% f))
#            colnames(w) <- colnames(f)
#            rownames(w) <- rownames(f)
#          },
#          "none" = {w <- NULL},
#
#          "components" = {w <- try(solve(r,f),silent=TRUE )    #basically, just do the regression/Thurstone approach for components
#          w <- f }
#   )
#
#
#   #now find a few fit statistics
#   if(is.null(w)) {results <- list(scores=NULL,weights=NULL)} else {
#     R2 <- diag(t(w) %*% S)  #this had been   R2 <- diag(t(w) %*% f)   Corrected Sept 1, 2017
#     if(any(R2 > 1) || (prod(!is.nan(R2)) <1) || (prod(R2) < 0) ) {#message("The matrix is probably singular -- Factor score estimate results are likely incorrect")
#       R2[abs(R2) > 1] <- NA
#       R2[R2 <= 0] <- NA
#     }
#     #if ((max(R2,na.rm=TRUE) > (1 + .Machine$double.eps)) ) {message("The estimated weights for the factor scores are probably incorrect.  Try a different factor extraction method.")}
#     r.scores <- cov2cor(t(w) %*% r %*% w) #what actually is this?
#
#
#     if(square) {  #that is, if given the correlation matrix
#       class(w) <- NULL
#       results <- list(scores=NULL,weights=w)
#       results$r.scores <- r.scores
#       results$R2 <- R2   #this is the multiple R2 of the scores with the factors
#     } else {
#       missing <- rowSums(is.na(x))
#       if(impute !="none") {
#         x <- data.matrix(x)
#         miss <- which(is.na(x),arr.ind=TRUE)
#         if(impute=="mean") {
#           item.means <- colMeans(x,na.rm=TRUE)   #replace missing values with means
#           x[miss]<- item.means[miss[,2]]} else {
#             item.med   <- apply(x,2,median,na.rm=TRUE) #replace missing with medians
#             x[miss]<- item.med[miss[,2]]}   #this only works if items is a matrix
#       }
#
#       if(method !="components") {scores <- scale(x) %*% w } else {  #standardize the data before doing the regression if using factors,
#         scores <- x %*% w}       # for components, the data have already been zero centered and, if appropriate, scaled
#       results <- list(scores=scores,weights=w)
#       results$r.scores <- r.scores
#       results$missing <- missing
#       results$R2 <- R2   #this is the multiple R2 of the scores with the factors
#     }
#   }
#
#   return(results) }
# #how to treat missing data?  see score.item
#
#
#
#
# "matSqrt" <- function(x) {
#   e <- eigen(x)
#   e$values[e$values < 0] <- .Machine$double.eps
#   sqrt.ev <- sqrt(e$values)   #need to put in a check here for postive semi definite
#   result <- e$vectors %*% diag(sqrt.ev) %*% t(e$vectors)
#   result}
#
#
# "invMatSqrt" <- function(x) {
#   e <- eigen(x)
#   if(is.complex(e$values)) {warning("complex eigen values detected by invMatSqrt, results are suspect")
#     result <- x
#   } else {
#
#     e$values[e$values < .Machine$double.eps] <- 100 * .Machine$double.eps
#     inv.sqrt.ev <- 1/sqrt(e$values)   #need to put in a check here for postive semi definite
#     result <- e$vectors %*% diag(inv.sqrt.ev) %*% t(e$vectors) }
#   result}
#
#
# #modified Dec 10, 2008 to return 1 on diagonal if non-invertible
# #modifed March 20, 2009 to return smcs * variance if covariance matrix is desired
# #modified April 8, 2009 to remove bug introduced March 10 when using covar from data
# #modified Jan 14, 2010 to test if matrix before cov2cor call.
# #modified October 2, 2010 to convert smcs < 0 to 0 -- this is situation encountered with extreme missingness in sapa matrices
# #modified April 23, 2015 to handle NAs in the correlation matrix
# #smcs are found for the non-NA variables, then, smcs for the remaining ones are found from the  correlations for those with NAs
# "smc" <-
#   function(R,covar =FALSE) {
#     failed=FALSE
#     wcc <- maxr <-  NULL
#     p <- dim(R)[2]
#     if(is.null(colnames(R))) colnames(R) <- paste0("V",1:p)
#     smc.all <- rep(NA,p)
#     names(smc.all) <- colnames(R)
#     if (dim(R)[1] != p) {if(covar) {C <- cov(R, use="pairwise")
#     vari <- diag(C)
#     R <- cov2cor(C)
#     } else {R <- cor(R,use="pairwise")}}  else {vari <- diag(R)
#     if (!is.matrix(R)) R <- as.matrix(R)
#     R <- cov2cor(R)
#     }
#     tempR <- NULL
#     if(any(is.na(R))) {
#       bad <- TRUE
#       tempR <- R
#       vr <- diag(tempR)
#       diag(tempR) <- 0
#
#       maxr <- apply(tempR,2,function(x) max(abs(x),na.rm=TRUE))
#       diag(tempR) <- vr
#       wcl <-NULL
#       while(bad) {
#         wc <- table(which(is.na(tempR), arr.ind=TRUE))  #find the correlations that are NA
#         wcl <- c(wcl,as.numeric(names(which(wc==max(wc)))))
#         tempR <- R[-wcl,-wcl]
#         if(any(is.na(tempR))) {bad <- TRUE} else {bad <- FALSE}
#       }
#       warning("Missing values (NAs) in the correlation matrix do not allow for SMC's to be found for all variables.  \nI will try to estimate SMCs for those variables by their non-NA  correlations.")
#       cat('\nSMC(s) for variables ',colnames(R)[wcl], 'were replaced (if possible) with smcs based upon their  (its) non-NA correlations\n')
#       #now, try to find the smcs for the other ones
#       wc <-(which(is.na(R[,wcl]),arr.ind=TRUE))
#
#       if(is.null(dim(wc))) {wcc <- as.numeric(names(table(wc))) } else {
#         wcc <- as.numeric(names(table(wc[,1])))}
#       tempR <- R[-wcc,-wcc]
#       R <- R[-wcl,-wcl]
#
#     }
#     if(!covar) { R <- cor.smooth(R) }
#
#     # R.inv <- try(solve(R),TRUE)
#     # if(inherits(R.inv, as.character("try-error"))) {smc <- rep(1,p)
#     # message("In smc, the correlation matrix was not invertible, smc's returned as 1s")} else  {smc <- 1 -1/diag(R.inv)}
#     R.inv <- Pinv(R)
#     smc <- 1 - 1/diag(R.inv)
#     names(smc) <- colnames(R)
#     if(!is.null(tempR)) {# R.na.inv <- try(solve(tempR),TRUE)
#       R.na.inv <- Pinv(tempR)
#       smc.na <- smc.na <- 1 -1/diag(R.na.inv)
#       # if(inherits(R.na.inv, as.character("try-error"))) {smc.na <- rep(1,p)
#       #  message("Oh bother, in smc, the correlation matrix of the adjusted part was not invertible, smc's returned as 1s")} else  {smc.na <- 1 -1/diag(R.na.inv)}
#     } else {smc.na <- smc}
#
#     if(all(is.na(smc))) {message ("Something is seriously wrong the correlation matrix.\nIn smc, smcs were set to 1.0")
#       smc[is.na(smc)] <- 1}
#     if(max(smc,na.rm=TRUE) > 1.0) {message("In smc, smcs > 1 were set to 1.0")
#       smc[smc >1 ]  <- 1.0}
#     if(min(smc,na.rm=TRUE) < 0.0) {message("In smc, smcs < 0 were set to .0")
#       smc[smc < 0]  <- 0}
#
#     smc.all[names(smc.all) %in% names(smc)] <- smc
#
#     if(!is.null(wcc)) {smc.all[wcl] <- smc.na[names(smc.all[wcl])] }
#
#     smc <- smc.all
#
#     if(!is.null(maxr)) { if(any(is.na(smc)))  {warning("The SMCs with NA values were replaced by their maximum correlation.")
#       cat('\nSMC(s) for variables ',names(smc)[is.na(smc)], 'were replaced with their maximum correlation \n')}
#       smc[is.na(smc) ] <- maxr[is.na(smc)]  #in case we could not fix everything
#     }
#     if(covar) {smc <- smc * vari}
#     return(smc)
#   }
#
#
# "Pinv" <- function(X,tol = sqrt(.Machine$double.eps)) {
#   svdX <- svd(X)
#   p <- svdX$d > max(tol * svdX$d[1],0 )
#   if(all(p)){ Pinv <- svdX$v %*% (1/svdX$d * t(svdX$u)) } else {
#     Pinv <- svdX$v[,p,drop=FALSE] %*% (1/svdX$d[p] * t(svdX$u[,p,drop=FALSE])) }
#   return(Pinv)
# }
#
# "factor.stats" <-
#   function(r=NULL,f,phi=NULL,n.obs=NA,np.obs=NULL,alpha=.1,fm=NULL) {
#     fa.stats(r=r,f=f,phi=phi,n.obs=n.obs,np.obs=np.obs,alpha=alpha,fm=fm)}
#
# "fa.stats" <-
#   function(r=NULL,f,phi=NULL,n.obs=NA,np.obs=NULL,alpha=.05,fm=NULL) {
#     #revised June 21, 2010 to add RMSEA etc.
#     #revised August 25, 2011 to add cor.smooth for smoothing
#     #revised November 10, 2012 to add stats for the minchi option of factoring
#     #revised February 28, 2014 to emphasize empirical chi 2 and report empirical BIC
#     #revised March 9, 2015 to report NA if RMSEA values are not in the confidence intervals
#     cl <- match.call()
#     conf.level <- alpha
#     if((!is.matrix(f)) && (!is.data.frame(f)))  {#do a number of things that use f as list
#
#       if(is.null(r) && (!is.null(f$r)) ) r <- f$r   #we found the correlation while factoring
#
#       #if(is.na(n.obs) && (!is.null(f$np.obs))) {np.obs <- f$np.obs}
#
#       f <- as.matrix(f$loadings)} else {f <- as.matrix(f)}
#
#     n <- dim(r)[2]  #number of variables
#     if(dim(r)[1] !=n ) {n.obs = dim(r)[1]
#     r <- cor(r,use="pairwise")
#     }
#     if(is.data.frame(r)) r <- as.matrix(r)
#     nfactors <- dim(f)[2]  # number of factors
#     if(is.null(phi)) {model <- f %*%  t(f)} else {model <- f %*% phi %*% t(f)}
#
#     residual<- r - model
#
#     r2 <- sum(r*r)
#     rstar2 <- sum(residual*residual)
#     result <- list(residual = residual)
#
#     result$dof <- dof <-  n * (n-1)/2 - n * nfactors + (nfactors *(nfactors-1)/2)
#     #r2.off <- r
#     #diag(r2.off) <- 0
#     # r2.off <- sum(r2.off^2)
#     r2.off <- r2 - tr(r)
#     diag(residual) <- 0
#     if(is.null(np.obs))  {rstar.off <- sum(residual^2)
#     result$ENull <- r2.off * n.obs  #the empirical null model
#     result$chi <- rstar.off * n.obs  #this is the empirical chi square
#     result$rms <- sqrt(rstar.off/(n*(n-1)))  #this is the empirical rmsea
#
#     result$nh <- n.obs
#     if (result$dof > 0) {result$EPVAL <- pchisq(result$chi, result$dof, lower.tail = FALSE)
#     result$crms <- sqrt(rstar.off/(2*result$dof) )
#     result$EBIC <- result$chi - result$dof * log(n.obs)
#     result$ESABIC <- result$chi - result$dof * log((n.obs+2)/24) } else {result$EPVAL <- NA
#     result$crms <- NA
#     result$EBIC <- NA
#     result$ESABIC <- NA}
#
#     } else {
#       rstar.off <- sum(residual^2 * np.obs)  #weight the residuals by their sample size
#       r2.off <-(r*r * np.obs)   #weight the original by sample size
#       r2.off <- sum(r2.off) -tr(r2.off)
#       result$chi <- rstar.off  #this is the sample size weighted chi square
#       result$nh <- harmonic.mean(as.vector(np.obs)) #this is the sample weighted cell size
#       result$rms <- sqrt(rstar.off/(result$nh*n*(n-1))) #this is the sample size weighted square root average squared residual
#       if (result$dof > 0) {result$EPVAL <- pchisq(result$chi, result$dof, lower.tail = FALSE)
#       result$crms <- sqrt(rstar.off/(2*result$nh*result$dof) )
#       result$EBIC <- result$chi - result$dof * log(result$nh)
#       result$ESABIC <- result$chi - result$dof * log((result$nh+2)/24) } else {   #added 2/28/2014
#         result$EPVAL <- NA
#         result$crms <- NA
#         result$EBIC <- NA
#         result$ESABIC <- NA
#       }
#     }
#
#     result$fit <-1-rstar2/r2
#     result$fit.off <- 1-rstar.off/r2.off
#     result$sd <- sd(as.vector(residual)) #this is the none sample size weighted root mean square residual
#     result$factors <- nfactors
#
#     result$complexity <- (apply(f,1,function(x) sum(x^2)))^2/apply(f,1,function(x)sum(x^4))
#
#     diag(model) <- diag(r)
#     model <- cor.smooth(model)  #this replaces the next few lines with a slightly cleaner approach
#     r <- cor.smooth(r)  #this makes sure that the correlation is positive semi-definite
#     #although it would seem that the model should always be positive semidefinite so this is probably not necessary
#     #cor.smooth approach  added August 25,2011
#
#     #    }
#
#     m.inv.r <- try(solve(model,r),silent=TRUE) #modified Oct 30, 2009 to perhaps increase precision -- #modified 2015/1/2 to use try
#
#     if(inherits(m.inv.r,"try-error")) {warning("the model inverse times the r matrix is singular, replaced with Identity matrix which means fits are wrong")
#       m.inv.r <- diag(1,n,n)}
#     if(is.na(n.obs)) {result$n.obs=NA
#     result$PVAL=NA} else {result$n.obs=n.obs}
#     result$dof <-  n * (n-1)/2 - n * nfactors + (nfactors *(nfactors-1)/2)
#     result$objective <- sum(diag((m.inv.r))) - log(det(m.inv.r)) -n   #this is what Tucker Lewis call F
#     if(is.infinite(result$objective)) {result$objective <- rstar2
#     message("The determinant of the smoothed correlation was zero.\nThis means the objective function is not defined.\nChi square is based upon observed residuals.")}
#     result$criteria <- c("objective"=result$objective,NA,NA)
#
#     if (!is.na(n.obs)) {result$STATISTIC <-  chisq <- result$objective * ((n.obs-1) -(2 * n + 5)/6 -(2*nfactors)/3) #from Tucker  and from factanal
#     # if (!is.na(n.obs)) {result$STATISTIC <-  chisq <- result$objective * ((n.obs-1)) #from Fox and sem
#     if(!is.nan(result$STATISTIC)) if (result$STATISTIC <0) {result$STATISTIC <- 0}
#     if (result$dof > 0) {result$PVAL <- pchisq(result$STATISTIC, result$dof, lower.tail = FALSE)} else {result$PVAL <- NA}
#     }
#     result$Call <- cl
#
#     #find the Tucker Lewis Index of reliability
#     #Also known as the NNFI which is expressed in terms of Chisq
#     #NNFI <- (chisqNull/dfNull - chisq/df)/(chisqNull/dfNull - 1)
#     #first find the null model
#     F0 <- sum(diag((r))) - log(det(r)) -n
#     if(is.infinite(F0))  {F0 <- r2
#     message("The determinant of the smoothed correlation was zero.\nThis means the objective function is not defined for the null model either.\nThe Chi square is thus based upon observed correlations.")}
#     Fm <-  result$objective   #objective function of model
#     Mm <- Fm/( n * (n-1)/2 - n * nfactors + (nfactors *(nfactors-1)/2))
#     M0 <- F0* 2 /(n*(n-1))
#     nm <- ((n.obs-1) -(2 * n + 5)/6 -(2*nfactors)/3) #
#     result$null.model <- F0
#     result$null.dof <- n * (n-1) /2
#     if (!is.na(n.obs)) {result$null.chisq <-  F0 * ((n.obs-1) -(2 * n + 5)/6 )
#     result$TLI <- (M0 - Mm)/(M0 - 1/nm)        #NNFI in Fox's sem
#     if(is.numeric(result$TLI) & !is.nan(result$TLI) & (result$TLI >1)) result$F0 <-1
#
#     #The estimatation of RMSEA and the upper and lower bounds are taken from John Fox's summary.sem with minor modifications
#     if(!is.null(result$objective) && (result$dof >0) &&(!is.na(result$objective))) {
#       # RMSEA <- sqrt(max(result$objective/result$dof - 1/(n.obs-1), 0))        #this is x2/(df*N ) -  1/(N-1)   #put back 4/21/17
#       #however, this is not quite right and should be
#       RMSEA <- sqrt(max(chisq/(result$dof* n.obs) - 1/(n.obs-1), 0))        #this is x2/(df*N ) -  1/(N-1)   #fixed 4/5/19
#       #note that the result$objective is not actually the chi square unless we adjust it ala Tucker
#       #thus, the RMSEA was slightly off.  This was fixed October 29, 2016 to be
#       # RMSEA <- sqrt(max( (chisq/(result$dof * (n.obs))-1/(n.obs)),0))   #changed to this from above October 29, 2016 and then changed to N February 28, 2017
#       #Seem to have dropped the sqrt part of this at some point
#
#       tail <- conf.level/2    #this had been incorrectly listed as (1-conf.level)/2  which gave extraordinarily narrow confidence boundaries, fixed August 25, 2011
#
#       N <- max <- n.obs
#       df <- result$dof
#       #chi.sq.statistic <- RMSEA^2 * df * (N - 1) + df
#
#       #why isn't this  just chi.sq?
#       chi.sq.statistic <- chisq
#
#
#
#
#       max <- max(n.obs,chi.sq.statistic) +2* n.obs
#
#       #the alternative to this is to use the uniroot technique of Yves Rosseel in  lavaan
#
#       #### from Hao Wu
#       #          LB<-function(T){
#       # + if (pchisq(df=df,q=T)<=0.95) return(0) else
#       # + sqrt(uniroot(function(x) {pchisq(df=df,ncp=x,q=T)-0.95},c(0,10000))$root/nstar/df)
#       # + }
#       #
#       # > UB<-function(T){
#       # + if (pchisq(df=df,q=T)<=0.05) return(0) else
#       # + sqrt(uniroot(function(x) {pchisq(df=df,ncp=x,q=T)-0.05},c(0,10000))$root/nstar/df)
#       # + }
#
#       ##
#       #Finally implement February 2017
#       #        upperlambda <- function(lam)   {tail - pchisq(chi.sq.statistic, df, ncp=lam)^2 }
#       RMSEA.U <- 0  #in case we can not find it
#       if(pchisq(df=result$dof,q=result$STATISTIC) > tail){ RMSEA.U <-    try( sqrt(uniroot(function(x) {pchisq(df=result$dof,ncp=x,q=result$STATISTIC)- tail},c(0,max))$root/(n.obs-1)/result$dof),silent=TRUE)
#
#       if(inherits( RMSEA.U,"try-error")) {if(RMSEA <= 0 ) {RMSEA.U <- 0} else {message("In factor.stats, I could not find the RMSEA upper bound . Sorry about that")
#         #if the fit is super good, then the chisq is too small to get an upper bound.  Report it as 0.
#         RMSEA.U <- NA}}
#
#       }
#       #                                           lam.U <- NA} else {lam.U <- res}
#       #  		#	if (is.null(res) || is.na(res$objective) || res$objective < 0){
#       #  		#		max <- 0
#       #  		#		warning("cannot find upper bound of RMSEA")
#       #  		#		break
#       #  		#		}
#       #
#       #  	 lowerlambda <- function(lam)   {1- tail - pchisq(chi.sq.statistic, df, ncp=lam)^2 }
#
#       RMSEA.L <- 0  #in case we can not find it
#       if(pchisq(df=result$dof,q=result$STATISTIC) > (1-tail)) {    RMSEA.L   <-     try( sqrt(uniroot(function(x) {pchisq(df=result$dof,ncp=x,q=result$STATISTIC)-1 + tail},c(0,max))$root/(n.obs-1)/result$dof) ,silent=TRUE)
#       if(inherits(RMSEA.L,"try-error")) {#message("In factor.stats, I could not find the RMSEA lower bound . Sorry about that")
#         RMSEA.L <- NA}
#       } else {RMSEA.L <- 0}
#       #                                           lam.L <- 0} else {lam.L <- res}
#       #  		#	if (is.null(res) || is.na(res$objective) || res$objective < 0){
#       #  		#		max <- 0
#       #  		#		warning("cannot find lower bound of RMSEA")
#       #  		#		break
#       #  		#		}
#       #However, this was giving the wrong results and so I implemented the following
#       #suggested by Hao Wu April, 2017
#       #RMSEA.U <-     sqrt(uniroot(function(x) {pchisq(df=result$dof,ncp=x,q=result$STATISTIC)- alpha},c(0,10000))$root/(n.obs-1)/result$dof)
#       #RMSEA.L   <-      sqrt(uniroot(function(x) {pchisq(df=result$dof,ncp=x,q=result$STATISTIC)-1 + alpha},c(0,10000))$root/(n.obs-1)/result$dof)
#
#       #       while (max > 1){
#       #              res <- try(optimize(function(lam) (tail - pchisq(chi.sq.statistic, df, ncp=lam))^2, interval=c(0, max)),silent=TRUE)
#       #               if(class(res)=="try-error") {message("In factor.stats, I could not find the RMSEA upper bound . Sorry about that")
#       #                                          res <- NULL}
#       #  			if (is.null(res) || is.na(res$objective) || res$objective < 0){
#       #  				max <- 0
#       #  				warning("cannot find upper bound of RMSEA")
#       #  				break
#       #  				}
#       #              if (sqrt(res$objective) < tail/100) break
#       #              max <- max/2
#       #              }
#       #          lam.U <- if (max <= 1) NA else res$minimum
#       #        # max <- max(max,lam.U)
#       #        max <- lam.U
#       #        if(is.na(max)) max <- N
#       #          while (max > 1){#        this just iterates in to get a value
#       #              res <- try(optimize(function(lam) (1 - tail - pchisq(chi.sq.statistic, df, ncp=lam))^2, interval=c(0, max)),silent=TRUE)
#       #               if(class(res)=="try-error") {message("In factor.stats, I could not find the RMSEA lower bound. Sorry about that")
#       #                                          res <- NULL}
#       #              if (is.null(res)) {break}
#       #              if (sqrt(res$objective) < tail/100) break
#       #              max <- max/2
#       #  			if (is.na(res$objective) || res$objective < 0){
#       #  				max <- 0
#       #  				warning("cannot find lower bound of RMSEA")
#       #  				break
#       #  				}
#       #              }
#       #
#       #
#       #       lam.L <- if (max <= 1) NA else res$minimum  #lam is the ncp
#       #        this RMSEA calculation is probably not right because it will sometimes (but rarely) give cis that don't include the estimate
#
#       #  RMSEA.U <- sqrt(lam.U/((N)*df) )   #lavaan uses sqrt(lam.U/((N)*df) )  sem uses sqrt(lam.U/((N-1)*df) )
#       #   RMSEA.L <- min(sqrt(lam.L/((N)*df) ),RMSEA)
#       if(!is.na(RMSEA.U) && RMSEA.U < RMSEA) RMSEA.U <- NA
#       if(!is.na(RMSEA.L) && RMSEA.L  > RMSEA) RMSEA.L  <- NA
#       result$RMSEA <- c(RMSEA, RMSEA.L, RMSEA.U, 1-conf.level)
#       names(result$RMSEA) <- c("RMSEA","lower","upper","confidence")
#       result$BIC <- chisq - df * log(N)
#       result$SABIC <- chisq - df * log((N+2)/24) # added 1/27/2014
#     }
#     }
#
#
#     #now, find the correlations of the factor scores, even if not estimated, with the factors
#     #this repeats what was done in factor.scores and does not take into account the options in factor.scores
#
#     if(!is.null(phi)) f <- f %*% phi   #convert the pattern to structure coefficients
#     r <- cor.smooth(r)
#     w <- try(solve(r,f) ,silent=TRUE)  #these are the regression factor weights
#     if(inherits(w,"try-error")) {message("In factor.stats, the correlation matrix is singular, an approximation is used")
#       ev <- eigen(r)
#       if(is.complex(ev$values)) {warning("complex eigen values detected by factor stats, results are suspect")
#
#       } else {
#         ev$values[ev$values < .Machine$double.eps] <- 100 * .Machine$double.eps
#         r <- ev$vectors %*% diag(ev$values) %*% t(ev$vectors)
#         diag(r)  <- 1
#         w <- try(solve(r,f) ,silent=TRUE)  #these are the factor weights
#         if(inherits(w,"try-error")) {warning("In factor.stats, the correlation matrix is singular, and we could not calculate the beta weights for factor score estimates")
#           w <- diag(1,dim(r)[1])
#         }   #these are the beta weights
#       }}
#     R2 <- diag(t(w) %*% f)   #but, we actually already found this in factor scores -- these are the Thurstone values
#     if(is.null(fm)) {
#       if(prod(R2) < 0 ) {message("In factor.stats: The factor scoring weights matrix is probably singular -- Factor score estimate results are likely incorrect.\n Try a different factor score estimation method\n")
#         R2[abs(R2) > 1] <- NA
#         R2[R2 <= 0] <- NA
#       }
#       if ((max(R2,na.rm=TRUE) > (1 + .Machine$double.eps)) ) {warning("The estimated weights for the factor scores are probably incorrect.  Try a different factor score estimation method.")}
#     }
#     r.scores <- cov2cor(t(w) %*% r %*% w)
#     result$r.scores <- r.scores
#     result$R2 <- R2   #this is the multiple R2 of the scores with the factors
#
#     # result$R2.corrected <- factor.indeterm(r,f)
#     # result$R2.total <- R2.cor$R2
#     # result$beta.total <- R2.cor$beta.total
#     #coarse coding
#     keys <- factor2cluster(f)
#     covar <- t(keys) %*% r %*% keys
#
#     if((nfactors >1) && (dim(covar)[2] >1  )) {
#       sd.inv <- diag(1/sqrt(diag(covar)))
#       cluster.correl <- sd.inv %*% covar  %*% sd.inv   #this is just cov2cor(covar)
#       valid <- t(f) %*% keys %*% sd.inv
#       result$valid <- diag(valid)
#       if(NCOL(cluster.correl) < NCOL(r.scores)) {#we need to pad out the cluster.correl matrix so that fa.organize does not choke   9/2/20
#         temp <- cluster.correl
#         n.temp <- NROW(temp)
#         cluster.correl <- matrix(NA,ncol(r.scores),nrow(r.scores))
#         cluster.correl[1:n.temp,1:n.temp] <- temp
#
#       }
#       result$score.cor <- cluster.correl} else {sd.inv <- 1/sqrt(covar)
#       if(dim(sd.inv)[1] == 1) sd.inv <- diag(sd.inv)
#       valid <- try(t(f) %*% keys * sd.inv)
#       result$valid <- valid}
#     result$weights <- w  #the beta weights for factor scores
#     class(result) <- c("psych","stats")
#     return(result)
#   }
#
#
# tr <- function (m)
# {
#   if (!is.matrix(m) | (dim(m)[1] != dim(m)[2]))
#     stop("m must be a square matrix")
#   return(sum(diag(m), na.rm = TRUE))
# }
#
# cor.smooth <- function (x, eig.tol = 10^-12)
# {
#   eigens <- try(eigen(x), TRUE)
#   if (inherits(eigens, as.character("try-error"))) {
#     warning("I am sorry, there is something seriously wrong with the correlation matrix,\ncor.smooth failed to  smooth it because some of the eigen values are NA.  \nAre you sure you specified the data correctly?")
#   }
#   else {
#     if (min(eigens$values) < .Machine$double.eps) {
#       warning("Matrix was not positive definite, smoothing was done")
#       eigens$values[eigens$values < eig.tol] <- 100 * eig.tol
#       nvar <- dim(x)[1]
#       tot <- sum(eigens$values)
#       eigens$values <- eigens$values * nvar/tot
#       cnames <- colnames(x)
#       rnames <- rownames(x)
#       x <- eigens$vectors %*% diag(eigens$values) %*% t(eigens$vectors)
#       x <- cov2cor(x)
#       colnames(x) <- cnames
#       rownames(x) <- rnames
#     }
#   }
#   return(x)
# }
#
#
# pairwiseCount <- function (x, y = NULL, diagonal = TRUE)
# {
#   x <- !is.na(x)
#   if (is.null(y)) {
#     n <- crossprod(x)
#   }
#   else {
#     n <- crossprod(x, !is.na(y))
#   }
#   if (!diagonal & is.null(y))
#     diag(n) <- NA
#   return(n)
# }
#
#
# harmonic.mean <- function (x, na.rm = TRUE, zero = TRUE)
# {
#   if (!zero) {
#     x[x == 0] <- NA
#   }
#   if (is.null(nrow(x))) {
#     1/mean(1/x, na.rm = na.rm)
#   }
#   else {
#     1/(apply(1/x, 2, mean, na.rm = na.rm))
#   }
# }





# funzione ausiliaria (non visibile all'utente)
# DIPENDE DAL PACKAGE 'mirt'
#   IRT unidimensionale basato sul modello di Samejima (1969)
# restituisce una lista con 3 componenti:
#  1) factor loadings
#  2) fit indices
#  3) scores
gradedIRT_1 <- function(data, method, extraction, maxit=5000, tol=1e-4, n.input = 4) {
  mod <- mirt::mirt(data,
                    model=1,
                    itemtype="graded",
                    dentype="Gaussian",
                    TOL=tol,
                    method = method,
                    technical=list(NCYCLES=maxit),
                    verbose=F, calcNull=T)
  Z <- mirt::fscores(mod, method=extraction, rotate="none", verbose=F)
  Zok <- Z[,1]*sd(unlist(data), na.rm = TRUE)+mean(unlist(data), na.rm = TRUE)
  if (n.input > 3){
  stats <- mirt::M2(mod,type="C2")
  }else{
  stats <- "Statistics cannot be calculated due to too few degrees of freedom."
  }
  list(loadings=mod@Fit$F,
       indices=stats,
       scores=Zok)
}


# le tre funzioni seguenti devono essere visibili all'utente
#   esse sfruttano la funzione 'gradedIRT_1' e hanno
#   la stessa struttura di ritorno

# IRT con scores EAP
gradedIRT_1_eap <- function(data) {
  gradedIRT_1(data=data, method="EAP")
}

# IRT con scores MAP
gradedIRT_1_map <- function(data) {
  gradedIRT_1(data=data, method="MAP")
}

# IRT con scores ML
gradedIRT_1_ml <- function(data) {
  gradedIRT_1(data=data, method="ML")
}



###########################################################


## calcolo fit indices di oggetti creati con la funzione psych::fa
#    fammi sapere se usate altre funzioni oltre questa
fitIndices <- function(x) {
  df <- x$dof
  p <- x$PVAL
  RMSEA <- x$RMSEA[1]
  RMSEA_5 <- x$RMSEA[2]
  RMSEA_95 <- x$RMSEA[3]
  resmat <- x$residual
  SRMSR <- sqrt(mean(resmat[lower.tri(resmat,diag=T)]^2))
  TLI <- x$TLI
  CFI <- ((x$null.chisq-x$null.dof)-(x$STATISTIC-x$dof))/(x$null.chisq-x$null.dof)
  df <- data.frame(df = df, p = p,
             RMSEA = RMSEA,
             RMSEA_5 =RMSEA_5,
             RMSEA_95 = RMSEA_95,
             SRMSR = SRMSR, TLI = TLI, CFI = CFI)
  row.names(df) <- "stats"
  return(df)
  #list(chisq=chisq,rmsea=rmsea,srmr=srmr,cfi=cfi,tli=tli)
}

# handle non-numeric columns in the data
my_update_function <- function(data){
  tryCatch(
    # This is what I want to do...
    {
      res <- apply(data,2,sum)
      return(res)
    },
    # ... but if an error occurs, tell me what happened:
    error=function(error_message) {
      message("Input data must be integer or numeric values only")

    }
  )
}

# handle non-numeric columns in the data
my_update_function2 <- function(data){
  tryCatch(
    # This is what I want to do...
    {
      res <- sum(data)
      return(res)
    },
    # ... but if an error occurs, tell me what happened:
    error=function(error_message) {
      message("Input data must be integer or numeric values only")

    }
  )
}



#### Final call of the app wrapping function

clc()

