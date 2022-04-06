
##################################################
#### webMASEM: a shiny-app for one-stage MASEM ###
########### created by Suzanne Jak ###############
##################################################

# Load packages
if(!require(shiny)){install.packages('shiny')}
if(!require(metaSEM)){install.packages('metaSEM')}
if(!require(semPlot)){install.packages('semPlot')}
if(!require(rmarkdown)){install.packages('rmarkdown')}
if(!require(shinycssloaders)){install.packages('shinycssloaders')}
if(!require(DT)){install.packages('DT')}
if(!require(tidyverse)){install.packages('tidyverse')}
if(!require(symSEM)){install.packages('symSEM')}
if(!require(shinyBS)){install.packages('shinyBS')}

library(shiny)
library(metaSEM)
library(semPlot)
library(rmarkdown)
library(shinycssloaders)
library(DT)
library(tidyverse)
library(symSEM)
library(shinyBS)

# load additional functions
source("http://www.suzannejak.nl/MASEM_functions.R")

# Define UI ----
ui <- navbarPage(title="webMASEM",
                 
# 'About' PAGE
                 tabPanel(
                   tags$h4(style = "font-family:Impact; color: #2404b6","Home"),
                   
                   tags$h4("A shiny app for one-stage MASEM"),
                   tags$h3(style = "font-family:Impact; color: #2404b6","Read before use"),
                   
                   p("This Shiny app makes it very easy to fit MASEM models using the one-stage MASEM approach. However, users should not use this app without familiarizing themselves with the basic properties of the one-stage MASEM approach. This can be done by reading the accompanying tutorial and the introduction article carefully (see references below). The following are some issues that should be understood as a minimum before using the app:"),
                   
                   tags$ul(
                     tags$li("One-stage MASEM fits the specified SEM model on the (weighted) average correlations across studies. It is therefore important to set the inclusion criteria such that the information obtained by fitting a SEM on the average correlations across studies will be meaningful, or such that the heterogeneity across studies can be investigated"),
                     br(),
                     tags$li("One-stage MASEM handles missing data using full information maximum likelihood. This method assumes that the missingness process is Missing At Random (MAR), and not Missing Not At Random (MNAR). Data are MNAR when the missing correlation coefficients are related to the values of the correlation coefficients, even after controlling for other variables. Unfortunately there do not exist MASEM methods that can handle MNAR data correctly."),
                     br(),
                     tags$li("The between-studies variances that are estimated with one-stage MASEM quantify the heterogeneity of the correlation coefficients between the variables, and not the heterogeneity of the SEM-parameters itself. If one wants an estimate of the between-studies variances of SEM-parameters one should use another approach (such as Bayesian MASEM by Ke et al., 2018)"),
                     br(),
                     tags$li("Moderation analysis is possible by regressing certain SEM parameters on a study-level moderating variable. So, the moderator variables can be used to explain differences in SEM parameters. The percentage of variance explained in the SEM parameters is however not provided, as a result of the previous point."),
                     br(),
                     tags$li("With standard meta-analysis, it is quite common to include large numbers of moderator variables for which there are no a priori hypotheses formulated. One-Stage MASEM is not suited for such exploratory analyses."),
                     br(),
                     tags$li("As with all SEM models, a satisfactory fitting SEM model does not prove that the specified model is correct. There may be other models fitting the data equally well.")
                   ),
                   
                   p("This app is build around functions from the metaSEM package (Cheung, 2015), the semPlot package (Epskamp, 2019), and the symSEM package (Cheung, 2020)."), 
                   p("For instructions and examples of using webMASEM see:"),
                   p(a("Jak, S., Hongli, L., Kolbe, L. & Cheung, M.W.-L. (under review). Meta-analytic structural equation modeling made easy: A tutorial and web application for one-stage MASEM.", href="https://onlinelibrary.wiley.com/doi/10.1002/jrsm.1498", target="_blank")),
                   p(a("Jak, S. & Cheung, M.W.-L. (2020). Meta-analytic structural equation modeling with moderating effects on SEM parameters. Psychological Methods, 25(4), 430-455.", href="https://psyarxiv.com/ce85j", target="_blank")),
                   p("This is the article about Bayesian meta-analytic SEM of Ke et al. (2018), which is an alternative approach:"),
                   p(a("Ke, Z., Zhang, Q., & Tong, X. (2019). Bayesian meta-analytic SEM: A one-stage approach to modeling between-studies heterogeneity in structural parameters. Structural Equation Modeling, 26(3), 348-370.", href="https://www.tandfonline.com/doi/full/10.1080/10705511.2018.1530059", target="_blank"))
                 ),
                 
# DATA INPUT PAGE ###########################################################               
tabPanel(tags$h4(style = "font-family:Impact; color: black","Data input"),
  sidebarLayout(
  sidebarPanel(
    
  # user dataset
    
    # Input: Select a file
    fileInput("uploaded_file", "Choose file",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),

    # Input: Checkbox if file has header
    checkboxInput("header", "Header", TRUE),
    
    # Input: Select separator
    radioButtons("sep", "Separator",
                 choices = c(Semicolon = ";",
                             Comma = ",",
                             Tab = "\t"),
                 selected = "\t"),

    numericInput("no_var", "Number of variables in MASEM", value = 4, step = 1),
    
    textInput("varnames", "Enter names of variables in MASEM (keep them short, separate by comma)", "pos,neg,enga,achiev"),
    
    # Select correlations 
    uiOutput("checkboxcor"),
    
    # Select sample sizes 
    uiOutput("checkboxn"),
    
    # Select moderator 
    uiOutput("checkboxM"),
    
    actionButton("update", "Update view user dataset",
                 style="color: #fff; background-color: black; border-color: black"),

  hr(),
  hr(),

  # metaSEM dataset
  
  p("Tick the box below to select a dataset from the metaSEM package"),
  checkboxInput("datcheckbox", label = "Choose dataset from metaSEM", value = FALSE),
  
  # Select dataset
  uiOutput("metasemdata")
   ),
           
   
  mainPanel(
             
  tabsetPanel(
    id = "dataset",
    tabPanel("User dataset", DT::dataTableOutput("rendered_file"),br(),"Click the 'Update View' button on the left after uploading the data to view the dataset"),
    
    tabPanel("Summary",
     h4("Variable names"),
     verbatimTextOutput("printvarnames"),
     
     h4("Variables in dataset"),
     verbatimTextOutput("summary"),
     
     h4("Number of studies per bivariate correlation"),
     verbatimTextOutput("kpercor"),
     
     h4("Total sample size per bivariate correlation"),
     verbatimTextOutput("npercor"),

     h4("First six correlation matrices"),
     verbatimTextOutput("datalist")),
    
    tabPanel("Average correlation matrix",
             h5("Click the button below to obtain the unrestricted average correlation matrix and summary output of fitting a multivariate meta-analysis with the tssem() function of metaSEM."),
                          actionButton("fitStage1", "Estimate average correlations",
                          style="color: #fff; background-color: black; border-color: black"),
             br(),
             br(),
             textOutput("headerpooledR"),
             withSpinner(verbatimTextOutput("stage1cor")),
             textOutput("summary1"),
             verbatimTextOutput("stage1sum"),
             textOutput("text_rerunStage1"),
             uiOutput("rerunStage1"))
   )
   )
   )                   
), 

# OVERALL ANALYSIS PAGE ###########################################################                                  
  tabPanel(
  tags$h4(style = "font-family:Impact; color: black","Overall analysis"),
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    
    # Sidebar panel for inputs 
    sidebarPanel(
      
      textAreaInput("lavmodel",
                    label = "Specify the model using lavaan syntax",
                    width = "300px",
                    height = "300px",
                    value =
"# Regression coefficients
enga ~ b31*pos + b32*neg
achiev ~ b43*enga + b41*pos + b42*neg

# Covariances
pos ~~ p21*neg

# Variances
pos ~~ 1*pos
neg ~~ 1*neg
enga ~~ p33*enga
achiev ~~ p44*achiev
"),

  actionButton("fitmodel", "Run analysis",
               style="color: #fff; background-color: black; border-color: black"),
  br(),
  br(),
  actionButton("View", "View model implied matrix",
               style="color: #fff; background-color: grey; border-color: grey"),

  bsModal("ViewSigma", "Model implied correlation matrix", "View", size = "large",
        "Model implied correlation matrix from specified model.",
        withSpinner(verbatimTextOutput("checkH0")),
        "This matrix is most readable if the parameters have short labels, such as 'b21' and 'p44'.",
        br(),
        "If no labels are specified in the syntax, the default (longer) labels from metaSEM will be used."),
 
  hr(),

  p("If there are problems, you may try rerunning the model while fixing between-study variances that are difficult to estimate, by hitting the button below"),
  actionButton("rerun", "Rerun",
               style="color: #fff; background-color: grey; border-color: black"),

  hr(),

  # mxAlgebra does not accept R-objects so it's difficult to use user-labels for indirect effects
  checkboxInput("medcheckbox", label = "Test indirect effect", value = FALSE),
  p("Give the two effects that make up the indirect effect the labels 'beta1' and 'beta2' 
    in the lavaan model. A list of all parameters with likelihood based confidence intervals will appear 
    in the summary. The IndirectEffect will be the last parameter in the list."),

  hr(),

  p("Optional: Change default values of arguments"),

  selectInput("REtype", "Type of random effects", 
              choices= c("Diag","Zero","Symm"),
              selected = "z"),

  selectInput("intervals", "Get standard errors ('z') only or also likelihood based confidence intervals ('LB'):", 
              choices= c("z","LB"),
              selected = "z"),

  selectInput("transform", "Applied transformation on random-effects variances:", 
              choices= c("expLog","sqSD"),
              selected = "expLog"),

  numericInput("startv", "Startvalue for the random-effects variances:", 
              value = 0.05,
              min = 0)
  
  ),
    
    # Main panel for displaying outputs 
    mainPanel(
      h4("Plot of model"),
      withSpinner(plotOutput("plotmodel",width = "60%")),
      
      h4("Output"),
      
      withSpinner(htmlOutput("status")),
      
      hr(),
      
      withSpinner(textOutput("fit")),
      
      hr(),
      
      withSpinner(verbatimTextOutput("results")), 

      withSpinner(verbatimTextOutput("residualvar")),
      
      withSpinner(textOutput("headertau")),

      withSpinner(verbatimTextOutput("varcorr"))
      

  ))),


# MODERATOR PAGE ----

tabPanel(
  tags$h4(style = "font-family:Impact; color: black","Moderator analysis"),
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    
    # Sidebar panel for inputs 
  sidebarPanel(
      
      
  # select from user defined dataset
  uiOutput("selectMod"),
  p("Note: Studies with missing values for the moderating variable should be deleted from the dataset."),
  checkboxInput("centermod", label = "Standardize the moderator", value = FALSE),
  
  hr(),
  
  span(textOutput("missmod"),style="color:red"),
  
  hr(),
     
  uiOutput("selectPar"),
      
  actionButton("Fitmod", "Run analysis",
               style="color: #fff; background-color: black; border-color: black"),
      
  hr(),
  
  p("If there seem to be estimation problems, you may try rerunning the model while automatically fixing between-study variances that are difficult to estimate, by hitting the button below"),
  actionButton("rerunmod", "Rerun",
               style="color: #fff; background-color: grey; border-color: black"),

      hr()
    ),
    
    # Main panel for displaying outputs 
    mainPanel(
      h4("Output"),   
      withSpinner(htmlOutput("statusmod")),
      br(),
      textOutput("text_modmean"),
      withSpinner(htmlOutput("sumstatmod")),
      br(),
            textOutput("text_omnibus"),
      withSpinner(textOutput("modtest")),
      br(),
      textOutput("text_indmod"),
      textOutput("transmod"),
      withSpinner(verbatimTextOutput("modsumind")),
      br(),
      textOutput("text_modsum"),
      withSpinner(verbatimTextOutput("modsum")),
      br(),
      textOutput("text_tau1"),
      withSpinner(verbatimTextOutput("tau1"))
    )
    
  ) # sidebarLayout
), # tabPanel


# DOWNLOAD REPORT PAGE
tabPanel(
  tags$h4(style = "font-family:Impact; color: black","Report"),
  
  p("You can download and save a Word document with the results by clicking one of the buttons below. 
    You may need to add the extension .docx to the filename when saving it to disk."),
  
  downloadButton("report_overall", "Generate report overall analysis"),
  
  downloadButton("report_moderation", "Generate report with moderation analysis")

  )

) # navbarPage





# SERVER ----
server <- function(input, output) {
 
# Read user file ----
  df <- reactive({
    req(input$uploaded_file)
    read.csv(input$uploaded_file$datapath,
             header = input$header,
             sep = input$sep)  
    
  })
  
 # Dynamically generate variable selection
  output$checkboxcor <- renderUI({
    checkboxGroupInput(inputId = "select_var", 
                       label = "Select columns with the correlations", 
                       choices = names(df()))
  })
  
  # Dynamically generate sample size selection
  output$checkboxn <- renderUI({
    checkboxGroupInput(inputId = "select_n", 
                       label = "Select column with sample sizes", 
                       choices = names(df()))
  })
  
  # Dynamically generate moderator selection
  output$checkboxM <- renderUI({
    checkboxGroupInput(inputId = "select_mod", 
                       label = "Select column with moderator (optional)", 
                       choices = names(df()))
  })
  
  output$metasemdata <- renderUI({
    if (input$datcheckbox == TRUE){
        selectInput("choosedataset", "Select a dataset from the metaSEM package:",
              choices = c("Roorda11", "Nohe15A1", "Scalco17","Digman97","Becker94","Becker09","Norton13"))
    }
      })
  
  
  # Select columns to print
  df_sel <- reactive({
    req(input$select_var)
    df_sel <- df() %>% select(input$select_var)
  })
  
  # Select N variable 
  df_n <- reactive({
    req(input$select_n)
    df_n <- df() %>% select(input$select_n)
  })
  
  # Select moderator variable ----
  df_mod <- reactive({
    req(input$select_mod)
    df_mod <- df() %>% select(input$select_mod)
  })
  
  # Make and print data table  
  datatable <- eventReactive(input$update,{
    data.frame(df_sel(),df_n())
  })
  
  output$rendered_file <- DT::renderDataTable({
    datatable()
  })
  
  # create list with list of cormatrices and vector of sample sizes
  # Selected the first (and only) column of df_n() because it is a data.frame
  
  var_names <- reactive({
    if(input$datcheckbox==FALSE){trimws(unlist(strsplit(input$varnames, ",")))}
    else {colnames(dataset()$data[[1]])}
  })
  
  output$printvarnames <- renderPrint(var_names())
  
  corlist <- reactive({readstack(df_sel(), no.var = input$no_var, var.names = var_names(), diag = FALSE)})
  
 # add moderator to dataset if it exists
  datasetuser <- reactive({
    if (is.null(input$select_mod)) {list(data=corlist(),n=df_n()[,1])}
      else {dat0 <- list(data=corlist(),n=df_n()[,1],as.numeric(df_mod()[,1]))
        names(dat0)[3] <- input$select_mod
        dat0}
  })
  
  datasetmasem <- reactive({
    switch(input$choosedataset,
           "Roorda11" = Roorda11,
           "Nohe15A1" = Nohe15A1,
           "Scalco17" = Scalco17,
           "Digman97" = Digman97,
           "Becker94" = Becker94,
           "Becker09" = Becker09,
           "Norton13" = Norton13)
  })

  
  # user or preloaded dataset depending on checkbox
   dataset <- reactive({
        if(input$datcheckbox==FALSE){datasetuser()}
        else {datasetmasem()}
     })

  
  
  # Generate a summary of the dataset
  
  sumdat <- reactive(summary(dataset())[,-2]) 
  output$summary <- renderPrint({
     sumdat()
  })
  
  # Generate Number of studies per correlation
  kpercor <- reactive(pattern.na(dataset()$data, show.na = FALSE))
  
  output$kpercor <- renderPrint({
    kpercor()
  })
  
  # Generate Total N per correlation
  npercor <- reactive(pattern.n(dataset()$data, dataset()$n))
  
  output$npercor <- renderPrint({
    npercor()
  })
  
  output$datalist <- renderPrint({
    head(dataset()$data)
   })
  
  stage1_0 <- eventReactive({input$fitStage1}, {
    metaSEM::tssem1(dataset()$data, dataset()$n)
   })
  
  stage1_1 <- eventReactive(input$rerunSt1, {
    metaSEM::rerun(stage1_0(), autofixtau2 = TRUE)
  })
  
  # Take rerunned Stage 1 model if it exists
  stage1 <- reactive({
    req(stage1_0())
    if(input$rerunSt1==0){stage1_0()}
    else {stage1_1()}
  })
  
  headerpooledR <- eventReactive({input$fitStage1}, {
    "Average correlation matrix"
  })
  
  headersummarySt1 <- eventReactive({input$fitStage1}, {
    "Summary"
  })
  
  output$summary1 <- renderText({headersummarySt1()})
  
  output$headerpooledR <- renderText({headerpooledR()})
  
  
  rerunbutton <- eventReactive({input$fitStage1}, {
    actionButton("rerunSt1", "Rerun",
                 style="color: #fff; background-color: grey; border-color: grey")
   })
  
  rerun_text <-  eventReactive({input$fitStage1}, {
    "If the OpenMx status is not 0 or 1, you may try rerunning the model while fixing between-study variances that are difficult to estimate, by hitting the button below."
  })
  
  # Rerun button and text appear after running the model
  output$rerunStage1 <- renderUI({
    rerunbutton()
    })
  
  output$text_rerunStage1 <- renderText({
    rerun_text()
  })
  
  output$stage1sum <- renderPrint({
    req(stage1())
    summary(stage1())
    })
  
  output$stage1cor <- renderPrint({
    req(stage1())
    x <- round(metaSEM::vec2symMat(coef(stage1(),"fixed"),diag=FALSE),3)
    dimnames(x) <- list(varnames(),varnames()); x})
  
  # Prepare fitting model
  varnames <- reactive(rownames(dataset()$data[[1]]))
  nvar <- reactive(nrow(dataset()$data[[1]]))
  RAM1 <- reactive(metaSEM::lavaan2RAM(input$lavmodel, obs.variables=varnames()))
  T0 <- reactive(metaSEM::create.Tau2(RAM=RAM1(), RE.type=input$REtype, Transform=input$transform, RE.startvalues=input$startv))
  my.df <- reactive(metaSEM::Cor2DataFrame(dataset()$data, dataset()$n, acov = "weighted"))
  
  # Show symbolic model implied correlation matrix 
  checkSigma <- eventReactive({input$ViewSigma}, {
    symSEM::impliedS(RAM1(), corr = TRUE)
  })
  
  output$checkH0 <- renderPrint(checkSigma()$Sigma)
  

  # Fit the model using one-Stage MASEM with or without test of indirect effect
  ossemfit1 <- eventReactive(input$fitmodel, {
   M0 <- create.vechsR(A0=RAM1()$A, S0=RAM1()$S, F0 = RAM1()$F)
   ind <- mxAlgebra(beta1*beta2, name="IndirectEffect") 
     if (input$medcheckbox == FALSE){
       osmasem(model.name="One Stage MASEM", Mmatrix=M0, Tmatrix=T0(), data=my.df(), intervals.type = input$intervals)}
     else{osmasem(model.name="One Stage MASEM", Mmatrix=M0, Tmatrix=T0(), data=my.df(), intervals.type = "LB",
                                 mxModel.Args = list(ind, mxCI(c("IndirectEffect"))))}
   })
  
  ossemfit2 <- eventReactive(input$rerun, {
    metaSEM::rerun(ossemfit1(), autofixtau2 = TRUE)
  })
  
# Take rerunned model if it exists
  ossemfit <- reactive({
    if(input$rerun==0){ossemfit1()}
    else {ossemfit2()}
  })

  # summary of results
  sumfit <- reactive({
    req(ossemfit())
    summary(ossemfit(), fitIndices = TRUE)})
  
  # plot of model
  plotmodel <- reactive(plot(ossemfit(), color = "white"))
  
  output$plotmodel <- renderPlot({plotmodel()})
  
  # Show results of one-stage MASEM
  output$status <- renderText({
    if(sumfit()$statusCode == "OK"){"<font color=\"#008000\"><b>Converged!</b></font>"}
      else{"<font color=\"#FF0000\"><b>No convergence..., maybe rerunning the model helps.</b></font>"
          }
  })
  
  fit <- reactive({paste0("Model fit: Chi2(",sumfit()$ChiDoF,") = ",round(sumfit()$Chi,3), ", p = ", round(sumfit()$p,3),", RMSEA (with 95% CI) = ",
          round(sumfit()$RMSEA,3)," [",round(sumfit()$RMSEACI,3)[1]," ; ",round(sumfit()$RMSEACI,3)[2],
          "], SRMR = ",round(osmasemSRMR(ossemfit()),3),".")
  })
  
  output$fit <- renderText({
    if(sumfit()$ChiDoF==0){"This is a saturated model (df = 0), so the fit statistics are not informative, but the parameter estimates are (see below)."}
    else{fit()}
  })
  
  output$results <- renderPrint({
    sumfit()
  })
  
  output$srmr <- renderPrint({paste0("SRMR= ",round(osmasemSRMR(ossemfit()),3))})
  
  # Residual variances
  
  Sres <- reactive({
    Sres <- ossemfit()$mx.fit$Smatrix$result
     if(is.null(Sres)) {
    Sres <- mx.fit0$mx.fit$algebras$Smatrix$result}
    Sres <- as.matrix(diag(Sres))
    dimnames(Sres) <- list(colnames(ossemfit()$mx.model$S0$labels),'(residual) variance')
    round(Sres,4)
  })
  
  output$residualvar <- renderPrint(Sres())
  
  # variance components
  vcor <- reactive({
    vcor <- round(diag(metaSEM::VarCorr(ossemfit())),3)
    if(input$REtype=='Diag'){
      names(vcor) <- my.df()$ylabels}
    list(Between_Studies_Variances_Of_Correlation_Coefficients = vcor)
    })
  
  output$headertau <- eventReactive(input$fitmodel, "Between studies variances of the correlation coefficients. (You can take the square root of the variances to obtain the standard deviations).")
  
  output$varcorr <- renderPrint({vcor()})
  

  
  # generate dropdown for moderator variable
  output$selectMod <- renderUI({
    moderator <- names(dataset())
    selectInput("moderator", "Select the moderator:",
                choices = c(names(dataset())), selected = names(dataset())[length(names(dataset()))])
  })
  
  # generate checkbox for direct effects to be moderated
   output$selectPar <- renderUI({
     parameters <- sumfit()$parameters$name[sumfit()$parameters$matrix=="A0"]
     checkboxGroupInput("ModPar", "Choose parameter(s) to be moderated", parameters)
   })
  
   # Create A1 matrix 
  A1 <- reactive({
     tempA <- matrix("0",nrow(RAM1()$A),ncol(RAM1()$A))
     for (i in 1:length(input$ModPar)){
       tempA[RAM1()$A==paste0("0*",input$ModPar[i])] <- paste0("0*data.","mod")
     }
     tempA
   })
  

  # add (standardized) moderator to dataset my.df2
  my.df2 <- reactive({
    dat1 <- metaSEM::Cor2DataFrame(dataset()$data, dataset()$n, acov = "weighted")
    if(input$centermod==TRUE){
      dat1$data <- data.frame(dat1$data, mod=scale(dataset()[[input$moderator]]),
                               check.names=FALSE)}
    else{
      dat1$data <- data.frame(dat1$data, mod=dataset()[[input$moderator]],
                            check.names=FALSE)}
    dat1
    })
  
  # fit model with moderator
  modfit1 <- eventReactive(input$Fitmod, {
    
    M1 <- create.vechsR(A0=RAM1()$A, S0=RAM1()$S, F0 = RAM1()$F, Ax=A1())

    osmasem(model.name=paste0("Moderation by ", input$moderator), Mmatrix=M1, Tmatrix=T0(), data=my.df2())
  })
  
  modfit2 <- eventReactive(input$rerunmod, {
    metaSEM::rerun(modfit1(), autofixtau2 = TRUE)
  })
  
  # Take rerunned model if it exists
  modfit <- reactive({
    if(input$rerunmod==0){modfit1()}
    else {modfit2()}
  })
  
  # Check missings on moderator 
  
  modmis <- eventReactive(input$Fitmod, {
   # req(my.df2()$data)
    nmodmis <- sum(is.na(my.df2()$data[,ncol(my.df2()$data)]))
    if(nmodmis>0)
      {paste0("Missing values for the moderating variable are not allowed. This moderator has ",nmodmis," missing values.")}
  })
  
  output$missmod <- renderText({modmis()})

  # overall moderator test
  test <- eventReactive(input$Fitmod, {anova(modfit(),ossemfit())})
  
  testout <- reactive({paste0("Chi2(",test()$diffdf[2],") = ", round(test()$diffLL[2],3), ", p = ",round(test()$p[2],3),".")})
   
  # mx status
  output$statusmod <- renderText({
    if(modsum()$statusCode == "OK"){"<font color=\"#008000\"><b>Converged!</b></font>"}
    else{"<font color=\"#FF0000\"><b>No convergence..., maybe rerunning the model or standardizing the moderator helps.</b></font>"
    }
  })
  
  # min,max,median,mean of moderator
  
  sumstatmod <- eventReactive(input$Fitmod,
    data.frame(
    mean = mean(dataset()[[input$moderator]]),
    sd = sd(dataset()[[input$moderator]]),
    median = median(dataset()[[input$moderator]]),
    min = min(dataset()[[input$moderator]]),
    max = max(dataset()[[input$moderator]]),
    standardized_in_analysis = input$centermod))
  
  output$sumstatmod <- renderTable({
    sumstatmod()
  })
  
  # Show overall test of moderator
  output$modtest <- renderText({
    testout()
  })
  
  modsum <- reactive({
    summary(modfit())
  })
  
  # Show individual moderating effects
  modsumind <- reactive(
    modsum()$parameters[modsum()$parameters$matrix=="A1",c(1,2,5,6,11,12)]
  )
  
  output$modsumind <- renderPrint({
    modsumind()
  })
  
  # Show results of moderator analysis
   output$modsum <- renderPrint({
     modsum()
   })
   
   # add corlabels if REtype = 'Diag'
   modR2 <- reactive({
     r2 <- metaSEM::osmasemR2(modfit(), ossemfit())
     if(input$REtype=='Diag'){
        r2 <- lapply(r2,function(x){names(x)<-my.df()$ylabels;x})}
     round(r2$Tau2.1,4)
   })

   # Show tau1
   output$tau1 <- renderPrint({
     modR2()
   })
   
   text_modmean <- eventReactive({input$Fitmod}, {
     "Summary statistics of moderator"
   })
   
   text_transmod <- eventReactive({input$Fitmod}, {
     if(input$centermod==TRUE){
     "Note: To obtain the moderating effects in the original metric (with unstandardized moderator variable), one can divide the reported regression coefficients by the standard deviation (sd) of the moderator"}
   })
   
   text_omnibus <- eventReactive({input$Fitmod}, {
     "Omnibus test of moderating effects"
   })
   
   text_indmod <- eventReactive({input$Fitmod}, {
     "Individual moderating effects"
   })
   
   text_modsum <- eventReactive({input$Fitmod}, {
     "Summary from metaSEM"
   })
   
   text_tau1 <- eventReactive({input$Fitmod}, {
     "Between-studies variances of model implied correlation coefficients (take square root to obtain standard deviations)"
   })
   
   output$text_modmean <- renderText(text_modmean())
   output$text_omnibus <- renderText(text_omnibus())
   output$text_indmod <- renderText(text_indmod())
   output$text_modsum <- renderText(text_modsum())
   output$text_tau1 <- renderText(text_tau1())
   output$transmod <- renderText(text_transmod())
   
   ## Code for downloadable reports
   
   output$report_moderation <- downloadHandler(
     filename = "report_moderation.docx",
     content = function(file) {
       tempReport <- file.path(tempdir(), "report_moderation.Rmd")
       file.copy("report_moderation.Rmd", tempReport, overwrite = TRUE)
       
       # Set up parameters to pass to Rmd document
       params <- list(lavmodel = input$lavmodel,
                      REtype = input$REtype,
                      intervals = input$intervals,
                      transform = input$transform,
                      startv = input$startv,
                      moderator = input$moderator,
                      sumstatmod = sumstatmod(), 
                      kpercor = kpercor(),
                      npercor = npercor(),
                      ossemfit = ossemfit(),
                      sumfit = sumfit(),
                      fit = fit(),
                      varcorr = vcor(),
                      Sres = Sres(),
                      testout = testout(),
                      modsumind = modsumind(),
                      modsum = modsum(),
                      modR2 = modR2())
       
       # Knit the document, passing in the `params` list, and eval it in a
       # child of the global environment (this isolates the code in the document
       # from the code in this app).
       rmarkdown::render(tempReport, output_file = file,
                         params = params,
                         envir = new.env(parent = globalenv())
       )
     }
   )
   
   
   output$report_overall <- downloadHandler(
     filename = "report_overall.docx",
     content = function(file) {
       tempReport <- file.path(tempdir(), "report_overall.Rmd")
       file.copy("report_overall.Rmd", tempReport, overwrite = TRUE)
       
       # Set up parameters to pass to Rmd document
       params <- list(lavmodel = input$lavmodel,
                      REtype = input$REtype,
                      intervals = input$intervals,
                      transform = input$transform,
                      startv = input$startv,
                      moderator = input$moderator,
                      centermod = input$centermod, 
                      kpercor = kpercor(),
                      npercor = npercor(),
                      ossemfit = ossemfit(),
                      sumfit = sumfit(),
                      fit = fit(),
                      varcorr = vcor(),
                      Sres = Sres())
       
       # Knit the document, passing in the `params` list, and eval it in a
       # child of the global environment (this isolates the code in the document
       # from the code in this app).
       rmarkdown::render(tempReport, output_file = file,
                         params = params,
                         envir = new.env(parent = globalenv())
       )
     }
   )

}

# Create Shiny app 
shinyApp(ui, server)




