#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
library(devtools)

# Specify the URL of the raw file on GitHub
file_url <- "https://raw.githubusercontent.com/giantomm/IRSA/main/BA_functions_only.R"
data_url <- "https://raw.githubusercontent.com/giantomm/IRSA/main/Data/"
# Read the file and execute its contents in the R environment
source_url(file_url)
library(FRSA)
library(timeSeries)
# Define server logic
function(input, output, session) {
  
  
  #---------------------------------------------------
  #------------------Scenario Analysis----------------
  #---------------------------------------------------
  
  # Scenario Selection -------------------------------------------
  
  # Define reactive data object for yieldCurve data frame
  yieldCurve_df <- reactiveVal(data.frame())
  
  # Define reactive data object for defaultCurve data frame
  defaultCurve_df <- reactiveVal(data.frame())
  
  # Define reactive list object for yieldCurve data frame
  yieldCurve_ls <- reactiveVal(list())
  
  # Define reactive list object for defaultCurve data frame
  defaultCurve_ls <- reactiveVal(list())
  
  real_rates <- retrieve_rates()
  
  applied_date <- reactiveVal()
  stop_date <- reactiveVal()
  
  fedFunds2date <- reactiveVal()
  
  # yieldCurve_vec <- reactiveVal()
  # yieldCurve_original <- reactiveVal()
  
  applied_scenario <- reactiveVal()
  
  base_scenario <- reactiveVal()
  stress_scenario1 <- reactiveVal()
  stress_scenario2 <- reactiveVal()
  
  default_YC_vec <- reactiveVal()
  default_YC <- reactiveVal()
  
  base_YC1 <- reactiveVal()
  base_YC2 <- reactiveVal()
  base_YC3 <- reactiveVal()
  base_YC4 <- reactiveVal()
  base_YC5 <- reactiveVal()
  
  scen1_YC1 <- reactiveVal()
  scen1_YC2 <- reactiveVal()
  scen1_YC3 <- reactiveVal()
  scen1_YC4 <- reactiveVal()
  scen1_YC5 <- reactiveVal()
  
  scen2_YC1 <- reactiveVal()
  scen2_YC2 <- reactiveVal()
  scen2_YC3 <- reactiveVal()
  scen2_YC4 <- reactiveVal()
  scen2_YC5 <- reactiveVal()
  
  base_con <- reactiveVal()
  scen1_con <- reactiveVal()
  scen2_con <- reactiveVal()
  
  # yieldCurve_1 <- reactiveVal()
  # yieldCurve_2 <- reactiveVal()
  # yieldCurve_3 <- reactiveVal()
  # yieldCurve_4 <- reactiveVal()
  # yieldCurve_5 <- reactiveVal()
  
  
  # Update data frames with file input (bulk function)
  # observeEvent(input$rf_import, {
  #   
  #   if(is.null(input$rf_file)){
  #     output$rf_file_notification <- renderUI({
  #       tags$div("Please add a file!", style = 'color: red;')
  #     })
  #   }else{
  #     
  #     filename <- input$rf_file$datapath
  #     
  #     rf_df <- utils::read.csv(filename)
  #     rf_type <- if(!is.null(rf_df$rfType[1])) rf_df$rfType[1] else 'NULL'
  #     
  #     if(rf_type %in% c('YieldCurve', 'DefaultCurve')){
  #       file_df <- riskFile2dataframe(filename)
  #       yc_df <- file_df[file_df$rfType == 'YieldCurve',]
  #       dc_df <- file_df[file_df$rfType == 'DefaultCurve',]
  #     
  #       temp_yc_df <- bind_rows(yieldCurve_df(), yc_df)
  #       temp_dc_df <- bind_rows(defaultCurve_df(), dc_df)
  #     
  #       temp_yc_df <- temp_yc_df[!duplicated(temp_yc_df),]
  #       temp_dc_df <- temp_dc_df[!duplicated(temp_dc_df),]
  #     
  #       yc_empty_cols <- apply(temp_yc_df, 2, function(x) all(is.na(x) | x == ""))
  #       dc_empty_cols <- apply(temp_dc_df, 2, function(x) all(is.na(x) | x == ""))
  #     
  #       # Subset the data frame to remove empty columns
  #       temp_yc_df <- subset(temp_yc_df, select = !yc_empty_cols)
  #       temp_dc_df <- subset(temp_dc_df, select = !dc_empty_cols)
  #     
  #       yieldCurve_df(temp_yc_df)
  #       defaultCurve_df(temp_dc_df)
  #       
  #       if (nrow(yieldCurve_df()) > 0){
  #         new_yc_list <- riskFactors_df2list(temp_yc_df)
  #         yieldCurve_ls(new_yc_list)
  #       }
  #       
  #       if (nrow(defaultCurve_df()) > 0){
  #         new_dc_list <- riskFactors_df2list(temp_dc_df)
  #         defaultCurve_ls(new_dc_list)
  #       }
  #     }else{
  #       output$rf_file_notification <- renderUI({
  #         tags$div("Not supported risk factor type or wrong file type!", style = 'color: red;')
  #       })
  #     }
  #   }
  # })
  
  observeEvent(input$scenarioDate, {
    realDate <- input$scenarioDate
    formatted_date <- floor_date(realDate, unit = "month")
    applied_date(formatted_date)
    stop_date(applied_date() + years(5))
    output$appliedDate <- renderUI({
      tags$p(
        class = "form-control datepicker-input",
        style = "background-color: #fff; color: #495057; border-color: #ced4da;",
        formatted_date
      )
    })
  })
  
  observeEvent(input$goRisk, {
    split_date <- applied_date()
    limited_rates <- date_limit(split_date, real_rates)
    default_YC_vec(YC_generator(split_date, limited_rates))
    default_YC(YCvector2YC(default_YC_vec()))
    # original_YC <- YC_generator(split_date, limited_rates)
    # yieldCurve_vec(original_YC)
    # 
    # original_YC_obj <- YCvector2YC(yieldCurve_vec())
    # yieldCurve_original(original_YC_obj)
    fedFunds2date(fedfunds_xts(limited_rates))
    
    base_scenario(basescenario(fedFunds2date()))
    stress_scenario1(scenario1(fedFunds2date()))
    stress_scenario2(scenario2(fedFunds2date()))
    
    # Store all YCs for the simulation
    base_YC1(YCvector2YC(shifted_YC(default_YC_vec(), 1, base_scenario()$curve)))
    base_YC2(YCvector2YC(shifted_YC(default_YC_vec(), 2, base_scenario()$curve)))
    base_YC3(YCvector2YC(shifted_YC(default_YC_vec(), 3, base_scenario()$curve)))
    base_YC4(YCvector2YC(shifted_YC(default_YC_vec(), 4, base_scenario()$curve)))
    base_YC5(YCvector2YC(shifted_YC(default_YC_vec(), 5, base_scenario()$curve)))
    
    scen1_YC1(YCvector2YC(shifted_YC(default_YC_vec(), 1, stress_scenario1()$curve)))
    scen1_YC2(YCvector2YC(shifted_YC(default_YC_vec(), 2, stress_scenario1()$curve)))
    scen1_YC3(YCvector2YC(shifted_YC(default_YC_vec(), 3, stress_scenario1()$curve)))
    scen1_YC4(YCvector2YC(shifted_YC(default_YC_vec(), 4, stress_scenario1()$curve)))
    scen1_YC5(YCvector2YC(shifted_YC(default_YC_vec(), 5, stress_scenario1()$curve)))
    
    scen2_YC1(YCvector2YC(shifted_YC(default_YC_vec(), 1, stress_scenario2()$curve)))
    scen2_YC2(YCvector2YC(shifted_YC(default_YC_vec(), 2, stress_scenario2()$curve)))
    scen2_YC3(YCvector2YC(shifted_YC(default_YC_vec(), 3, stress_scenario2()$curve)))
    scen2_YC4(YCvector2YC(shifted_YC(default_YC_vec(), 4, stress_scenario2()$curve)))
    scen2_YC5(YCvector2YC(shifted_YC(default_YC_vec(), 5, stress_scenario2()$curve)))
    
    # output$testPlot1 <- renderPlot({
    #   plot(scen1_YC1()[[1]])
    # })
    # 
    # output$testPlot2 <- renderPlot({
    #   plot(scen1_YC4()[[1]])
    # })
    
    # Store all YCs in respective RFConn
    base_con(RFConn(list(default_YC(), base_YC1(), base_YC2(), base_YC3(), base_YC4(), base_YC5())))
    scen1_con(RFConn(list(default_YC(), scen1_YC1(), scen1_YC2(), scen1_YC3(), scen1_YC4(), scen1_YC5())))
    scen2_con(RFConn(list(default_YC(), scen2_YC1(), scen2_YC2(), scen2_YC3(), scen2_YC4(), scen2_YC5())))
    
    # Plot selected Scenario
    observeEvent(input$scenarioSelect, {
      if (input$scenarioSelect == "Base Scenario"){
        out <- base_scenario()
        applied_scenario(out)
      } else if(input$scenarioSelect == "Stress Scenario 1"){
        out <- stress_scenario1()
        applied_scenario(out)
      } else if(input$scenarioSelect == "Stress Scenario 2"){
        out <- stress_scenario2()
        applied_scenario(out)
      }
      # shifted1 <- YCvector2YC(shifted_YC(yieldCurve_vec(), 1, applied_scenario()$curve))
      # shifted2 <- YCvector2YC(shifted_YC(yieldCurve_vec(), 2, applied_scenario()$curve))
      # shifted3 <- YCvector2YC(shifted_YC(yieldCurve_vec(), 3, applied_scenario()$curve))
      # shifted4 <- YCvector2YC(shifted_YC(yieldCurve_vec(), 4, applied_scenario()$curve))
      # shifted5 <- YCvector2YC(shifted_YC(yieldCurve_vec(), 5, applied_scenario()$curve))
      # 
      # yieldCurve_1(shifted1)
      # yieldCurve_2(shifted2)
      # yieldCurve_3(shifted3)
      # yieldCurve_4(shifted4)
      # yieldCurve_5(shifted5)
      
      output$scenarioPlot <- renderPlot({
        plot(out$curve, main = out$title, ylab = out$scale)
      })
    })
  })
      
  #     observeEvent(input$showYC, {
  #       if (input$showYC == 0) {
  #         show_plot <- yieldCurve_original()
  #       } else if (input$showYC == 1) {
  #         show_plot <- yieldCurve_1()
  #       } else if (input$showYC == 2) {
  #         show_plot <- yieldCurve_2()
  #       } else if (input$showYC == 3) {
  #         show_plot <- yieldCurve_3()
  #       } else if (input$showYC == 4) {
  #         show_plot <- yieldCurve_4()
  #       } else if (input$showYC == 5) {
  #         show_plot <- yieldCurve_5()
  #       }
  #       output$scenarioYCplot <- renderPlot({
  #         plot(show_plot[[1]])
  #       })
  #     })
  #   # output$scenarioYCplot <- renderPlot({
  #   #   plot(shifted1[[1]])
  #   # })
  #   })
  #   output$originalYCplot <- renderPlot({
  #     plot(original_YC_obj[[1]])
  #   })
  # })
  
  # Institutions -------------------------------------
  
  institution_vec <- reactiveVal(c())
  institution_ls <- reactiveVal(list())
  
  empty_inst <- reactiveVal()
  current_inst <- reactiveVal()
  scale_inst <- reactiveVal()
  
  contracts <- reactiveVal(data.frame())
  all_nodes <- reactiveVal(c())
  
  str_nodes <- reactiveVal(c())
  str_node_parents <- reactiveVal(c())
  
  nodes <- reactiveVal(c())
  
  ctrs <- reactiveVal(data.frame())
  
  market_obj_vec <- reactiveVal(c())
  market_obj_dr_vec <- reactiveVal(c())
  
  default_inst_vec <- reactiveVal(c())
  
  # Structure of the institution -------------------
  
  bank <- createInstitution("Bank")
  
  bank$Assets$ShortTermAssets$AddChild("Cash and cash equivalents")
  bank$Assets$ShortTermAssets$AddChild("AFS securities")
  bank$Assets$ShortTermAssets$AddChild("HTM securities")
  
  bank$Assets$LongTermAssets$AddChild("Non-marketable securities")
  bank$Assets$LongTermAssets$AddChild("Loans")
  bank$Assets$LongTermAssets$AddChild("Other assets")
  
  bank$Assets$FixedAssets$AddChild("Premises and equipment")
  bank$Assets$FixedAssets$AddChild("Goodwill")
  bank$Assets$FixedAssets$AddChild("Other intangible assets")
  bank$Assets$FixedAssets$AddChild("Lease right-of-use assets")
  
  bank$Liabilities$ShortTermLiabilities$AddChild("Demand deposits")
  bank$Liabilities$ShortTermLiabilities$AddChild("Deposits")
  bank$Liabilities$ShortTermLiabilities$AddChild("Short-term borrowings")
  
  bank$Liabilities$LongTermLiabilities$AddChild("Lease liabilities")
  bank$Liabilities$LongTermLiabilities$AddChild("Long-term debt")
  bank$Liabilities$LongTermLiabilities$AddChild("Other liabilities")
  
  empty_inst(bank)
  current_inst(bank)
  
  
  observe({
    if (!is.null(current_inst())){
      children <- Traverse(current_inst())
      children_names <- sapply(seq_along(children), function(i) children[[i]]$name)
      new_vector <- c(children_names)[-1]
      all_nodes(new_vector)
    }
  })
  
  # SVB or own institution, tree and scale -------------------------
  
  output$inst_structure <- renderPrint({
    print(current_inst())
  })
  
  observeEvent(input$createOrLoad, {
    temp_bank <- empty_inst()
    current_inst(temp_bank)
    
    # all_assets <- getContractsAsDataFrames(current_inst(), "Assets")[,2]
    # all_liabilities <- getContractsAsDataFrames(current_inst(), "Liabilities")[,2]
    # all_ops <- getContractsAsDataFrames(current_inst(), "Operations")[,2]
    # 
    # for (i in all_assets){
    #   removeContract(current_inst(), "Assets", i)
    # }
    # 
    # for (i in all_liabilities){
    #   removeContract(current_inst(), "Liabilities", i)
    # }
    # 
    # for (i in all_ops){
    #   removeContract(current_inst(), "Operations", i)
    # }
  })
  
  observeEvent(input$loadSVB, {
    temp_bank <- empty_inst()
    temp_bank$name <- "SVB"
    current_inst(temp_bank)

    scale_inst("in millions")
    
    output$inst_structure <- renderPrint({
      print(current_inst())
    })
  })
  
  observeEvent(input$saveInst, {
    temp_bank <- empty_inst()
    temp_bank$name <- input$instName
    current_inst(temp_bank)
    
    scale_inst(input$scaleSelect)
    
    output$inst_structure <- renderPrint({
      print(current_inst())
    })
  })
  
  output$scaleSelection <- renderUI({
    p("All values are", strong(scale_inst()), "and in the contract's respective currency.")
  })
  
  # Import contracts -----------------------------

  observeEvent(input$loadSVB, {
    ann_ptf <- readRDS(url(paste0(data_url, "ann_ptf_2021.rds")))
    pam_ptf <- readRDS(url(paste0(data_url, "pam_ptf_2021.rds")))
    ops_ptf <- readRDS(url(paste0(data_url, "ops_ptf_2021.rds")))
    
    temp_bank <- empty_inst()
    temp_bank <- assignContracts2Tree(temp_bank, ann_ptf)
    temp_bank <- assignContracts2Tree(temp_bank, pam_ptf)
    temp_bank <- assignContracts2Tree(temp_bank, ops_ptf)
    current_inst(temp_bank)
    
    temp_contracts <- getContractsAsDataFrames(current_inst(), input$showContracts)
    contracts(temp_contracts)
    
    output$contractsDF <- renderDataTable({
      contracts() %>% datatable(options = list(
        scrollX = TRUE,
        columnDefs = list(list(className = "nowrap", targets = "_all"))
      ),
      selection = list(mode = 'single')
      )
    })
  })
  
  observeEvent(input$contractImport, {
    if(is.null(input$contractFile)){
      output$ct_file_notification <- renderUI({
        tags$div('Please upload a file!', style = 'color: red;')
      })
    }else{
      output$ct_file_notification <- NULL
      inst <- current_inst()
      
      path <- input$contractFile$datapath
      
      ct_df <- utils::read.csv(path)
      ct_type <- if(!is.null(ct_df$contractType[1])) ct_df$contractType[1] else 'NULL'
      
      if((ct_type == 'ANN' && input$contractType == 'Annuities') || 
         (ct_type == 'PAM' && input$contractType == 'PrincipalAtMaturities') ||
         (ct_type == 'Investments' && input$contractType == 'Operations') ||
         (ct_type == 'OperationalCF' && input$contractType == 'Operations')){
        
        if(input$contractType != 'Operations'){
          ct_ptf <- samplePortfolio(path, 'contracts')
        }else{
          ct_ptf <- samplePortfolio(path, 'operations')
        }
        
        inst <- assignContracts2Tree(inst, ct_ptf)
        
        current_inst(inst)
        temp_contracts <- getContractsAsDataFrames(current_inst(), input$showContracts)
        contracts(temp_contracts)
        
        output$contractsDF <- renderDataTable({
          contracts() %>% datatable(options = list(
            scrollX = TRUE,
            columnDefs = list(list(className = "nowrap", targets = "_all"))
          ),
          selection = list(mode = 'single')
          )
        })
      }else{
        output$ct_file_notification <- renderUI({
          tags$div("Uploaded file doesn't match 'Portfolio Type'!", style = 'color: red;')
        })
      }
    }
  })
  
  observeEvent(input$showContracts, {
    temp_contracts <- getContractsAsDataFrames(current_inst(), input$showContracts)
    contracts(temp_contracts)
    output$contractsDF <- renderDataTable({
      contracts() %>% datatable(options = list(
        scrollX = TRUE,
        columnDefs = list(list(className = "nowrap", targets = "_all"))
      ),
      selection = list(mode = 'single')
      )
    })
  })
  
  observe({
    updateSelectInput(session, inputId = "showContracts", choices = all_nodes())
  })
  
  # Simulation -------------------------------------
  simulation_done <- reactiveVal(FALSE)
  simulation_bs <- reactiveVal()
  simulation_is <- reactiveVal()
  
  observeEvent(input$startSim, {
    sim_res <- simulation_buckets(current_inst(), base_con(), scen1_con(), scen2_con(), applied_date(), stop_date())
    simulation_bs(sim_res[1:6])
    simulation_is(sim_res[[7]])
    simulation_done(TRUE)
  })
  
  # Performance Analysis ----------------------------------
  
  BS_IFRS <- reactiveVal()
  BS_Market <- reactiveVal()
  
  Withdrawal1 <- reactiveVal()
  Withdrawal2 <- reactiveVal()
  MaxLoss <- reactiveVal()
  
  BS_Withdrawal1 <- reactiveVal()
  BS_Withdrawal2 <- reactiveVal()
  
  NI_Withdrawal1 <- reactiveVal()
  NI_Withdrawal2 <- reactiveVal()
  
  Ratios_Withdrawal1 <- reactiveVal()
  Ratios_Withdrawal2 <- reactiveVal()
  
  observe({
    if (simulation_done()){
      observeEvent(input$applyScenario, {
        if (input$applyScenario == "Base Scenario"){
          BS_IFRS(merge_svb(simulation_bs()[[1]], simulation_bs()[[4]]))
          BS_Market(merge_fv_nv(simulation_bs()[[1]], simulation_bs()[[4]]))
          
          Withdrawal1(withdrawals(0.2, 0.1, simulation_bs()[[1]], simulation_bs()[[4]], simulation_is()))
          Withdrawal2(withdrawals(0.4, 0.2, simulation_bs()[[1]], simulation_bs()[[4]], simulation_is()))
        } 
        else if (input$applyScenario == "Stress Scenario 1"){
          BS_IFRS(merge_svb(simulation_bs()[[2]], simulation_bs()[[5]]))
          BS_Market(merge_fv_nv(simulation_bs()[[2]], simulation_bs()[[5]]))
          
          Withdrawal1(withdrawals(0.2, 0.1, simulation_bs()[[2]], simulation_bs()[[5]], simulation_is()))
          Withdrawal2(withdrawals(0.4, 0.2, simulation_bs()[[2]], simulation_bs()[[5]], simulation_is()))
        }
        else if (input$applyScenario == "Stress Scenario 2"){
          BS_IFRS(merge_svb(simulation_bs()[[3]], simulation_bs()[[6]]))
          BS_Market(merge_fv_nv(simulation_bs()[[3]], simulation_bs()[[6]]))
          
          Withdrawal1(withdrawals(0.2, 0.1, simulation_bs()[[3]], simulation_bs()[[6]], simulation_is()))
          Withdrawal2(withdrawals(0.4, 0.2, simulation_bs()[[3]], simulation_bs()[[6]], simulation_is()))
        }
        MaxLoss(Withdrawal1()$MaxLoss)
        
        BS_Withdrawal1(Withdrawal1()$BS)
        NI_Withdrawal1(Withdrawal1()$NI)
        Ratios_Withdrawal1(ratios(BS_Withdrawal1(), NI_Withdrawal1()))
        
        BS_Withdrawal2(Withdrawal2()$BS)
        NI_Withdrawal2(Withdrawal2()$NI)
        Ratios_Withdrawal2(ratios(BS_Withdrawal2(), NI_Withdrawal2()))
        
        output$Bal_IFRS <- renderPrint({
          print(BS_IFRS())
        })
        
        output$Bal_Market <- renderPrint({
          print(BS_Market())
        })
        
        output$Max_Loss <- renderPrint({
          print(MaxLoss())
        })
        
        if (input$withdrawalCase == "Withdrawal Case 1"){
          BS_out <- BS_Withdrawal1()
          NI_out <- NI_Withdrawal1()
          Ratios_out <- Ratios_Withdrawal1()
        }
        else if (input$withdrawalCase == "Withdrawal Case 2"){
          BS_out <- BS_Withdrawal2()
          NI_out <- NI_Withdrawal2()
          Ratios_out <- Ratios_Withdrawal2()
        }
        output$BS_Withdrawals <- renderPrint({
          print(BS_out)
        })
        
        output$NI_Withdrawals <- renderPrint({
          print(NI_out)
        })
        
        output$Ratios_Withdrawals <- renderPrint({
          print(Ratios_out)
        })
      })
      
      output$incomeStatement <- renderPrint({
        print(simulation_is())
      })
      
      
      observeEvent(input$withdrawalCase, {
        if (input$withdrawalCase == "Withdrawal Case 1"){
          BS_out <- BS_Withdrawal1()
          NI_out <- NI_Withdrawal1()
          Ratios_out <- Ratios_Withdrawal1()
        } 
        else if (input$withdrawalCase == "Withdrawal Case 2"){
          BS_out <- BS_Withdrawal2()
          NI_out <- NI_Withdrawal2()
          Ratios_out <- Ratios_Withdrawal2()
        }
        
        output$BS_Withdrawals <- renderPrint({
          print(BS_out)
        })
        
        output$NI_Withdrawals <- renderPrint({
          print(NI_out)
        })
        
        output$Ratios_Withdrawals <- renderPrint({
          print(Ratios_out)
        })
      })
      
      # observeEvent(input$accountingMethod, {
      #   if (input$accountingMethod == "SVB Balance Sheet (IFRS)"){
      #     if (input$showMaxLoss == T){
      #       output$without <- renderUI({
      #         tagList(
      #           h5("SVB Balance Sheet (IFRS)"),
      #           div(
      #             style = "white-space: nowrap; overflow: auto;",
      #             verbatimTextOutput("Bal_IFRS")
      #           ),
      #           br(),
      #           h5("Maximum possible loss"),
      #           verbatimTextOutput("Max_Loss")
      #         )
      #       })
      #     }
      #     else {
      #       output$without <- renderUI({
      #         tagList(
      #           h5("SVB Balance Sheet (IFRS)"),
      #           verbatimTextOutput("Bal_IFRS")
      #         )
      #       })
      #     }
      #   }
      #   else if (input$accountingMethod == "HTM Market Valuation"){
      #     if (input$showMaxLoss == T){
      #       output$without <- renderUI({
      #         tagList(
      #           h5("HTM Market Valuation"),
      #           verbatimTextOutput("Bal_Market"),
      #           br(),
      #           h5("Maximum possible loss"),
      #           verbatimTextOutput("Max_Loss")
      #         )
      #       })
      #     }
      #     else {
      #       output$without <- renderUI({
      #         tagList(
      #           h5("HTM Market Valuation"),
      #           verbatimTextOutput("Bal_Market")
      #         )
      #       })
      #     }
      #   }
      #   else if (input$accountingMethod == "Both"){
      #     if (input$showMaxLoss == T){
      #       output$without <- renderUI({
      #         tagList(
      #           h5("SVB Balance Sheet (IFRS)"),
      #           verbatimTextOutput("Bal_IFRS"),
      #           br(),
      #           h5("HTM Market Valuation"),
      #           verbatimTextOutput("Bal_Market"),
      #           br(),
      #           h5("Maximum possible loss"),
      #           verbatimTextOutput("Max_Loss")
      #         )
      #       })
      #     }
      #     else {
      #       output$without <- renderUI({
      #         tagList(
      #           h5("SVB Balance Sheet (IFRS)"),
      #           verbatimTextOutput("Bal_IFRS"),
      #           br(),
      #           h5("HTM Market Valuation"),
      #           verbatimTextOutput("Bal_Market")
      #         )
      #       })
      #     }
      #   }
      # })
      # observeEvent(input$showMaxLoss, {
      #   if (input$showMaxLoss == T){
      #     if (input$accountingMethod == "SVB Balance Sheet (IFRS)"){
      #       output$without <- renderUI({
      #         tagList(
      #           h5("SVB Balance Sheet (IFRS)"),
      #           div(
      #             style = "white-space: nowrap; overflow: auto;",
      #             verbatimTextOutput("Bal_IFRS")
      #           ),
      #           br(),
      #           h5("Maximum possible loss"),
      #           verbatimTextOutput("Max_Loss")
      #         )
      #       })
      #     }
      #     else if (input$accountingMethod == "HTM Market Valuation"){
      #       output$without <- renderUI({
      #         tagList(
      #           h5("HTM Market Valuation"),
      #           verbatimTextOutput("Bal_Market"),
      #           br(),
      #           h5("Maximum possible loss"),
      #           verbatimTextOutput("Max_Loss")
      #         )
      #       })
      #     } else if (input$accountingMethod == "Both"){
      #       output$without <- renderUI({
      #         tagList(
      #           h5("SVB Balance Sheet (IFRS)"),
      #           verbatimTextOutput("Bal_IFRS"),
      #           br(),
      #           h5("HTM Market Valuation"),
      #           verbatimTextOutput("Bal_Market"),
      #           br(),
      #           h5("Maximum possible loss"),
      #           verbatimTextOutput("Max_Loss")
      #         )
      #       })
      #     }
      #   }
      #   else {
      #     if (input$accountingMethod == "SVB Balance Sheet (IFRS)"){
      #       output$without <- renderUI({
      #         tagList(
      #           h5("SVB Balance Sheet (IFRS)"),
      #           verbatimTextOutput("Bal_IFRS")
      #         )
      #       })
      #     }
      #     else if (input$accountingMethod == "HTM Market Valuation"){
      #       output$without <- renderUI({
      #         tagList(
      #           h5("HTM Market Valuation"),
      #           verbatimTextOutput("Bal_Market")
      #         )
      #       })
      #     }
      #     else if (input$accountingMethod == "Both"){
      #       output$without <- renderUI({
      #         tagList(
      #           h5("SVB Balance Sheet (IFRS)"),
      #           verbatimTextOutput("Bal_IFRS"),
      #           br(),
      #           h5("HTM Market Valuation"),
      #           verbatimTextOutput("Bal_Market")
      #         )
      #       })
      #     }
      #   }
      # })
    }
  })
  
  #---------------------------------------------------
  #---------------------Downloads---------------------
  #---------------------------------------------------
  
  # Dataset Download ---------------------------------
  annuities <- reactiveVal()
  principalAtMaturities <- reactiveVal()
  operations <- reactiveVal()
  
  annuities(readRDS(url(paste0(data_url, "/downloadable/annuities.rds"))))
  principalAtMaturities(readRDS(url(paste0(data_url, "/downloadable/principalAtMaturities.rds"))))
  operations(readRDS(url(paste0(data_url, "/downloadable/operations.rds"))))
  
  # Reactive value for selected dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "Annuities" = annuities(),
           "PrincipalAtMaturities" = principalAtMaturities(),
           "Operations" = operations())
  })
  
  # Table of selected dataset
  output$table <- renderDataTable({
    datasetInput()
  }, options = list(scrollX = TRUE,
                    columnDefs = list(list(className = "nowrap", targets = "_all"))
  )
  )
  
  # Table documentation of selected dataset
  output$tableDoc <- renderText({
    temp = Rd2HTML(Rd_fun(input$dataset), out = tempfile("doc"))
    content = read_file(temp)
    file.remove(temp)
    content
  })
  
  
  # Downloadable csv of selected dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  
  
  
  #---------------------------------------------------
  #----------------------Closure----------------------
  #---------------------------------------------------
  
  # Close App ----------------------------------------
  
  # Define function to trigger when close button is clicked
  observeEvent(input$close, {
    removeModal()  # Hide the confirmation dialog
    session$close()  # Close the Shiny session
  })
  
  # Define function to trigger when download button is clicked
  output$downloadAll <- downloadHandler(
    # Define download logic here
  )
  
  
  
}
