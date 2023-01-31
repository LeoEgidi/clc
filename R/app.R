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
  titlePanel(imageOutput("logo"),
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
      h2("Estimation options"),
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
                         choiceValues=c("ml", "ols", "wls", "gls", "EM"),
                         inline =FALSE, width ='600px',
                         choiceNames = c("EFA maximum likelihood", "EFA ordinary least squares", "EFA weighted least squares", "EFA generalized least squares", "IRT expectation-maximization"),
                         animation = "pulse", status = "danger", shape ="round", thick =TRUE),
      prettyRadioButtons("comp","Select the score estimation method", icon = icon("check"),
                         choiceValues=c("Weighted average", "Weighted sum", "Unweighted average", "Unweighted sum",    "Bartlett", "regression", "EAP", "MAP", "ML"),
                         choiceNames=c("Weighted average", "Weighted sum", "Unweighted average", "Unweighted sum", "Bartlett", "Regression", "Expected a-posteriori", "Maximum a-posteriori", "Maximum likelihood"),
                         inline =FALSE, width ='600px',
                         animation = "pulse", status = "danger", shape ="round", thick =TRUE),
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
                   selected = ","))),
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
            div(HTML("<b>Software version</b>: v1.2")),
            div(style = "padding: 5px 5px"),
            div(HTML("<b>To cite the CLC Estimator (APA 7th)</b>: Marzi, G., Balzano, M., Egidi, L., Magrini, A. (2023). CLC estimator: a tool for latent construct estimation via congeneric approaches in survey research. <em>Multivariate Behavioral Research</em>, (in press). DOI: ")),
            a("https://plsdeams.shinyapps.io/CLC_Estimator/", href ="https://plsdeams.shinyapps.io/CLC_Estimator/"),
            div(style = "padding: 5px 5px"),
            div(HTML("<b>Code</b>:"), a("https://github.com/LeoEgidi/clc", href= "https://github.com/LeoEgidi/clc")),
            div(style = "padding: 5px 5px"),
            div(HTML("<b>Sample Dataset (comma separated)</b>:"), a("https://dx.doi.org/10.6084/m9.figshare.21786335", href = "https://dx.doi.org/10.6084/m9.figshare.21786335")),
            div(style = "padding: 5px 5px"),
            div(HTML("<b> Data policy</b>: Uploaded files will not be saved locally by the CLC estimator. All data will be deleted after when closing the CLC Estimator session.")),
            div(style = "padding: 5px 5px"),
            div(HTML("<b>Usage notes</b>")),
            div(),
            div(HTML("<em>What can the CLC estimator do?</em>")),
            div(),
           "The app only estimates unidimensional latent constructs based on congeneric approaches. The CLC estimator is not intended for statistical procedures commonly available in existing statistical packages, such as exploratory factor analysis, principal component analysis or confirmatory factor analysis.",
            div(style = "padding: 5px 5px"),
            div(HTML("<em>When should the CLC estimator be used? </em>")),
            div(),
            "The CLC estimator performs latent construct estimation via congeneric approaches when the available statistical packages do not include this function. For example, scholars can use the CLC estimator when they perform analysis with PLS-SEM and want to integrate such analysis with a QCA analysis.",
            div(style = "padding: 5px 5px"),
            div(HTML("<em>At what point in the data analysis process the CLC estimator should be used?  </em>")),
            div(),
            "The CLC estimator should be integrated into the data analysis process after review (including reliability analysis) of the items to be retained in the latent construct estimation.",
            div(style = "padding: 5px 5px"),
            div(HTML("<em> When the CLC estimator should not be used?</em>")),
            "The CLC estimator assumes a common factor model, i.e. a reflective latent construct. Therefore, it is not appropriate when the latent construct is a composite (Rhemtulla et al., 2020).",
            div(style = "padding: 5px 5px"),
            div(HTML("<em>How to deal with missing data, outliers, and other data issues in the CLC estimator? </em>")),
            div(),
            "The CLC estimator does not handle missing data, outliers or incomplete data. Users are encouraged to address these issues before using the CLC estimator. Specifically for missing data, note that the CLC estimator uses the standard missing data handling procedures embedded in the 'psych' and 'mirt' R packages. For the EFA model, the correlation matrix is computed based on pairwise complete observations. For the graded IRT model, full information maximum likelihood is used. If the data contain missing values, the CLC estimator warns the user immediately.",
            div(style = "padding: 5px 5px"),
            div(HTML("<em>How to create a usable .csv file for the CLC estimator?</em>")),
            div(),
            div(HTML("The CLC estimator supports standard .csv files generated by popular statistical packages. The first row of the .csv file should contain the labels of the variables (i.e. the header), while the other rows should contain the data. The .csv file loading interface allows the user to select the delimiter between values (comma, semicolon, tab). The .csv file should be formatted in UNICODE, with decimal symbols expressed in points according to international scientific conventions. A sample dataset in .csv format (comma delimiter) is available at: ")),
           a( "https://dx.doi.org/10.6084/m9.figshare.21786335"  , href= "https://dx.doi.org/10.6084/m9.figshare.21786335"),
            div(style = "padding: 5px 5px"),
            div(HTML("<em>What type of model estimation method should be used to estimate the latent constructs?  </em>")),
            div(),
            "When social scientists use previously validated scales, the same method of estimation should be used for those scales. If neither the estimation method is specified by the original developers of the scales nor such scales have been previously validated, maximum likelihood estimation (MLE) is recommended (for a detailed discussion of this topic, see Thompson, 2004).",
            div(style = "padding: 5px 5px"),
            div(HTML("<em>What type of output does the CLC estimator produce?  </em>")),
            "CLC estimator generates a standard .csv file with variable names in the first row and values in the remaining rows. The generated .csv file output can be easily imported into popular statistical packages.",
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
    volumes = c("UserFolder"="C:/"),
    first_name = "")

   observeEvent(input$do ==0,{
    reactives$first_name <- input$name.var
   })



    #Observe file being selected: input file
    observeEvent(input$file1, {
      updateTabsetPanel(session, "panel", selected = "Loaded data")

    #Store loaded data in reactive
    reactives$mydata <- read.csv(file = input$file1$datapath, sep = input$sep, header = TRUE)
    updateSelectInput(session,"new.var",choices=colnames(reactives$mydata))

    reactives$new.dataset <- reactives$mydata
    reactives$new <- matrix(NA, dim(reactives$new.dataset)[1],10^3)

    #update select input
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

    read.csv(inFile$datapath, sep = input$sep, header = TRUE)})


    # calculation step
    observeEvent(input$do,{
      data <- reactives$mydata[, input$new.var]
      # Handle missing data

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
          }else if(input$comp == "Unweighted average"){
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
              factor_prod_vec[i,] <- data[, input$new.var[i]]
            }
            # new latent construct
            var.new <- apply(factor_prod_vec,2, mean, na.rm = TRUE)
          }else if(input$comp == "Unweighted sum"){
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
              factor_prod_vec[i,] <- data[, input$new.var[i]]
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
        }else if (input$comp == "Unweighted average"){
          fit <- gradedIRT_1(data, method = input$method, extraction = "EAP", n.input = length(input$new.var))
          n_items <- length(input$new.var)
          mat <- as.data.frame(as.table(fit$loadings))
          tib <- as_tibble(mat)
          factor_loading <- c()
          factor_prod_vec <- matrix(NA, n_items, dim(data)[1])

          for (i in 1:n_items){
            new.tib <- tib %>% filter(Var1 == input$new.var[i])
            factor_loading[i] <- round(as.numeric(new.tib[3]),3)
            factor_prod_vec[i,] <- data[, input$new.var[i]]
          }
          var.new <- apply(factor_prod_vec,2, mean, na.rm = TRUE)
        }else if (input$comp == "Unweighted sum"){
          fit <- gradedIRT_1(data, method = input$method, extraction = "EAP", n.input = length(input$new.var))
          n_items <- length(input$new.var)
          mat <- as.data.frame(as.table(fit$loadings))
          tib <- as_tibble(mat)
          factor_loading <- c()
          factor_prod_vec <- matrix(NA, n_items, dim(data)[1])

          for (i in 1:n_items){
            new.tib <- tib %>% filter(Var1 == input$new.var[i])
            factor_loading[i] <- round(as.numeric(new.tib[3]),3)
            factor_prod_vec[i,] <- data[, input$new.var[i]]
          }
          var.new <- apply(factor_prod_vec,2, sum, na.rm = TRUE)
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
        lambda <- round(as.table(fit$loadings),3)
        p <- length(lambda)
        mode <- function(x){f <- table(x); as.numeric(names(which.max(f)))}
        stat_table <- matrix(NA, 17, 2)
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
          round(skewness(var, na.rm = TRUE),2),  # skewness
          round(psych::omega(m=data)$alpha,2),  # alpha cronbach
          round(psych::omega(m=data,1)$omega_h ,2),  #  omega
          round(mean(lambda^2),2), # ave
          round(sum(lambda)^2/(sum(lambda)^2+sum(1-lambda^2)),2))    # cr
          colnames(stat_table) <- c("Statistics", input$name.var)
          stat_table[,1] <- c("Mean", "Mode", "Median", "SD",
                            "1st Quartile", "3rd Quartile", "Min", "Max",
                            "Range", "N", "Sum", "Kurtosis", "Skewness",
                            "Cronbach alpha", "McDonald omega",
                            "AVE", "Composite reliability")
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

        reactives$new[,input$do] <- round(as.numeric(as.vector(var.new)),3)
        reactives$name_vectors[input$do] <- input$name.var
        colnames(reactives$new) <- reactives$name_vectors
        scores <- as.matrix(reactives$new[, (1:input$do)])
        colnames(scores)[1] <- reactives$first_name
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
                              !is.null(input$new.var) &&  input$name.var!="" && length(input$new.var)>1)
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
         mychoices <- c("Weighted average", "Weighted sum", "Unweighted average", "Unweighted sum", "Bartlett", "regression" )
         updatePrettyRadioButtons(session, "comp",
                                  choiceNames = c("Weighted average", "Weighted sum", "Unweighted average", "Unweighted sum", "Bartlett", "Regression"),
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
         mychoices <- c("Weighted average", "Weighted sum", "Unweighted average", "Unweighted sum", "EAP", "MAP", "ML")
         updatePrettyRadioButtons(session, "comp",
                                  choiceNames = c("Weighted average", "Weighted sum", "Unweighted average", "Unweighted sum", "Expected a-posteriori", "Maximum a-posteriori", "Maximum likelihood"),
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


       observeEvent(input$do <1,{
           reactives$first_name = input$name.var
        })



 }

shinyApp(ui, server)
}



## Function for mirt, it provides:
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
       indices=stats[c(1:4, 7,8,9)],
       scores=Zok)
}



## Fit indices with function psych::fa
fitIndices <- function(x) {
  chi <- x$chi
  df <- x$dof
  p <- x$PVAL
  RMSEA <- x$RMSEA[1]
  RMSEA_5 <- x$RMSEA[2]
  RMSEA_95 <- x$RMSEA[3]
  resmat <- x$residual
  SRMSR <- sqrt(mean(resmat[lower.tri(resmat,diag=T)]^2))
  TLI <- x$TLI
  CFI <- ((x$null.chisq-x$null.dof)-(x$STATISTIC-x$dof))/(x$null.chisq-x$null.dof)
  df <- data.frame(
             Chi = chi,
             df = df, p = p,
             RMSEA = RMSEA,
             #RMSEA_5 =RMSEA_5,
             #RMSEA_95 = RMSEA_95,
             SRMSR = SRMSR, TLI = TLI, CFI = CFI)
  row.names(df) <- "stats"
  return(df)
}

# Handle non-numeric columns in the data
my_update_function <- function(data){
  tryCatch(
    {
      res <- apply(data,2,sum)
      return(res)
    },
    error=function(error_message) {
      message("Input data must be integer or numeric values only")

    }
  )
}




#### Final call of the app wrapping function

clc()

