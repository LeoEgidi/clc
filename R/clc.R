#'  CLC
#'  @description Congeneric Latent Construct Estimator
#'
#'  @import shiny
#'  @import shinyWidgets
#'  @import shinyjs
#'  @import dplyr
#'  @import moments
#'  @import GPArotation
#'  @export

library(shiny)

clc <- function(...){
ui <- fluidPage(
  #tags$head(includeCSS('www/style.css')),
  shinyjs::useShinyjs(),
  titlePanel(  imageOutput("logo"),
               tags$head(tags$style(('text{width: 10px; font-size:35px; text-align:center; padding:-250px;}')))
               ),
  div(style = "font-size: 14px;
                   padding: 0px 0px;
                   margin-top:-25em",
  sidebarLayout(
    sidebarPanel(
       tags$head(tags$style(
         "#sidebarItemExpanded {
            overflow: auto;
            max-height: 100vh;
        }"
      #   "body {overflow-y: hidden;}"
       )),
      style = paste0("height: 100vh; overflow-y: auto;"), ##CHANGE
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),

      selectizeInput("new.var", "Select items for the latent construct",
                  choices = "",
                  multiple = TRUE,
                  options = list(
                    placeholder = 'Please select an item',
                    onInitialize = I('function() { this.setValue(""); }'),
                    choices = names(data())
                  )),
      textInput("name.var", "Name of the new latent construct", value = "", placeholder = 'Please provide a valid name'
                ),
      prettyRadioButtons("method","Select factors extraction method", icon = icon("check"),
                         choiceValues=c("mle", "wls", "ols", "gls", "uls", "pa", "alpha", "minchi", "minrank", "minres", "old.min"),
                         inline =FALSE, width ='600px',
                         choiceNames = c("Maximum Likelihood", "Weighted Least Squares",
                                          "Ordinary Least Squares", "Generalized Least Squares", "Unweighted Least Squares", "Principal Factor",
                                          "Alpha Factor Analysis", "Minimum Weight. Chi Square", "Minimum Rank",
                                          "Minimum Residual", "Minimum Residual 2" ),
                         animation = "pulse", status = "danger", shape ="round", thick =TRUE),
      prettyRadioButtons("comp","Latent construct computation", icon = icon("check"),
                         choices=c("Weighted Average", "Weighted Sum"), inline =FALSE, width ='600px',
                         animation = "pulse", status = "danger", shape ="round", thick =TRUE),
      checkboxInput("missing", "Ignore missing values", TRUE),
      #numericInput("n.fac", "Choose the number of factors", 1),
      column(3, offset =1, actionButton("do", "Calculate",
                   style="position: relative;height: 70px;width: 100%;text-align:center;color:black;font-weight: bold;background-color:#98E0B5;border-radius: 6px;border-color:gray;border-width:2px;text-decoration:none")),
      column(width = 6, offset = 0, style='padding:40px;'),
      column(3, offset =1, actionButton("reset", "Clean & Prepare for new variable",
                                        style="position: relative;height: 70px;width: 290%;text-align:center;color:black;font-weight: bold;background-color:yellow;border-radius: 6px;border-color:gray;border-width:2px;text-decoration:none")),
      column(width = 6, offset = 0, style='padding:40px;'),
      column(8, offset =1, actionButton("download", "Download the computed .csv",
                                        style="position: relative;height: 70px;width: 100%;text-align:center;color:black;font-weight: bold;background-color:lightblue;border-radius: 6px;border-color:gray;border-width:2px;text-decoration:none"))
      ),
    mainPanel(
      fluidRow(
        tabsetPanel(id="panel",
          tabPanel("Loaded Data" , tableOutput("contents"),
                   tags$head(tags$style("#text{color: black;
                          font-size: 40px;
                          font-style: italic;
                          font-family: 'NSimSun';
                          }"
                   ))),
          tabPanel("Estimated Output" , tableOutput("contents2"),
                   tags$head(tags$style("#text{color: black;
                          font-size: 40px;
                          font-style: italic;
                          font-family: 'NSimSun';
                          }"
                   ))),
          tabPanel("Extracted Loadings", tableOutput("fl"),
                        tags$head(tags$style("#text{color: black;
                          font-size: 40px;
                          font-style: italic;
                          font-family: 'NSimSun';
                          }"
                        ))),
          tabPanel("Construct Statistics", tableOutput("stat"),
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
          #, style='padding:10px;'
          ))

    )
  )
  )
)




server <- function(input, output, session) {



  output$logo <- renderImage({
    return(list(
      src =  "inst/logos/logo_iper_small.png",
      height = 60,
      contentType = "image/png",
      alt = "Face",
      align ="center"
    ))
  }, deleteFile = FALSE)

  url <- a("Github page", href="https://github.com/LeoEgidi/clc")
  url2 <- a("Youtube tutorial", href="https://github.com/LeoEgidi")

  output$info <- renderUI({
    #tagList("URL link:", url)
    tagList(a("Github page", href="https://github.com/LeoEgidi"),
            div(),
            a("Youtube tutorial", href="https://github.com/LeoEgidi"),
            div(),
            "NOTE: Uploaded files will not be saved locally by CLC estimator. All data will be deleted after the usage of the CLC Estimator"
            )
  })


  #Reactive to store loaded data
  reactives <- reactiveValues(
    mydata = NULL,
    new.dataset = NULL,
    new = NULL,
    name_vectors = rep("",10^3)

  )

   # reactives2 <- reactiveValues(
   #   new.dataset = reactives$mydata
   # )


  #Observe file being selected
  observeEvent(input$file1, {

    # observe({
    #   shinyjs::toggleState(id ="do")
    # })
    #Store loaded data in reactive
    reactives$mydata <- read.csv(file = input$file1$datapath)
    reactives$new.dataset <- reactives$mydata
    reactives$new <- matrix(NA, dim(reactives$new.dataset)[1],10^3)


    #Update select input
    updateSelectInput(session, inputId = 'new.var', label = 'Select items for the latent construct',
                      choices  = colnames(reactives$mydata))

  })


  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1


    if (is.null(inFile))
      return(NULL)

    read.csv(inFile$datapath, header = TRUE)

  })


    observeEvent(input$do,{

      data <- as.matrix(reactives$mydata)[, input$new.var]



      if (input$missing == TRUE){
        if (sum(is.na(data))>0){
        showModal(modalDialog(
          title= "Your dataset has missing values: CLC estimator treats missing values as empty cells and ignores the rows with mising data",
          easyClose = TRUE))
        }
      }

      if (input$name.var==""){
        showModal(modalDialog(
          title= "Please, provide a valid name for your new latent construct!",
          easyClose = TRUE))

      }


      fit <- fa(r = data, rotate = "varimax",
                nfactors = 1,
                 fm = input$method)

      output$fl <- renderTable({
        a <- round(as.table(fit$loadings),3)
        b <- cbind(names(a[,1]), a[,1])
        colnames(b) <- c("Item", "Factor Loadings")
        b
      })



      # compute weighted sum or average

      if (input$comp == "Weighted Average"){

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

          var.new <- apply(factor_prod_vec,2, sum)/sum(factor_loading)



          output$stat <- renderTable({
            var <- as.numeric(as.vector(var.new))
            mode <- function(x){f <- table(x); as.numeric(names(which.max(f)))}
            stat_table <- matrix(NA, 13, 2)
            stat_table[, 2] <- c(
             round(mean(var, na.rm = input$missing),2),   # mean
             round(mode(var),2),   # mode
             round(median(var, na.rm = input$missing),2), # median
             round(sd(var, na.rm = input$missing),2),     # sd
             round(quantile(var, 0.25, na.rm = input$missing),2), # 1st quartile
             round(quantile(var, 0.75, na.rm = input$missing),2), # 3rd quartile
             round(min(var, na.rm = input$missing),2),   # min
             round(max(var, na.rm = input$missing),2),   # max
             round(max(var, na.rm = input$missing)- min(var, na.rm = input$missing),2), # range
             round(length(var),2), # n
             round(sum(var, na.rm = input$missing),2), # sum
             round(kurtosis(var, na.rm = input$missing),2), # kurtosis
             round(skewness(var, na.rm = input$missing),2)  # skewness
             )
            colnames(stat_table) <- c("Statistics", input$name.var)
            stat_table[,1] <- c("Mean", "Mode", "Median", "SD",
                                "1st Quartile", "3rd Quartile", "Min", "Max",
                                "Range", "N", "Sum", "Kurtosis", "Skewness")
            stat_table

          })




      }else if (input$comp == "Weighted Sum") {
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

        var.new <- apply(factor_prod_vec,2, sum)

      }



      output$contents2 <- renderTable({

        reactives$new[,input$do] <- round(as.numeric(as.vector(var.new)),3)
        reactives$name_vectors[input$do] <- input$name.var
        colnames(reactives$new[, (1:input$do)]) <- reactives$name_vectors[input$do]
        reactives$new[, (1:input$do)]
      })

      if (input$missing == FALSE){
      if (sum(is.na(data))>0){
        showModal(modalDialog(
          title= "Your dataset has missing values and you decided to not ignore them:
          CLC estimator can give errors and for such reason no results are displayed.",
          easyClose = FALSE))


        output$fl <- renderTable({
          NULL
        })

        output$stat <- renderTable({
          NULL
        })

        output$output2 <- renderTable({
          NULL
        })
        }
      }

      reactives$new.dataset$daje <- round(as.numeric(as.vector(var.new)),2)
      colnames(reactives$new.dataset)[dim(reactives$new.dataset)[2]] <- input$name.var

      observeEvent(input$download,{
        showModal(modalDialog(
          title= "The new database has been correctly extracted and saved in the directory.",
          easyclose = TRUE))
        write.csv(as.data.frame(reactives$new.dataset),
                  file = "new_dataset.csv" )
        })



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

      output$fl <- renderTable({
        NULL
      })

      output$stat <- renderTable({
        NULL
      })

      output$contents2 <- renderTable({
        NULL
      })



    })

     observe({
       shinyjs::toggleState(id ="do", condition = !is.null(colnames(input$file1)) &&
                              !is.null(input$new.var) &&  input$name.var!="" )
     })

     observe({
       shinyjs::toggleState(id ="reset", condition = input$do > input$reset )
     })

     observe({
       shinyjs::toggleState(id ="download", condition = input$do > 0 &&
                              !is.null(reactives$mydata ))
     })





}

shinyApp(ui, server)
}
