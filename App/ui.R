#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

fluidPage(
  theme = shinytheme("readable"),
  img(
    src = "actus-logo.png", height = 100, width = 220,
    style = "float:right; padding-right:25px"
  ),
  img(src = "soe.png", height = 138, width = 239),
  
  # Link custom CSS styling
  tags$head(
    tags$style(HTML(css))
  ),

  navbarPage("Stress Tests for Financial Institutions",
    fluid = TRUE,
    tabPanel("Scenario Analysis",
      id = "scenarioAnalysis",
      tabsetPanel(
        tabPanel("Introduction", id = "intro",
                 fluidPage(
                   fluidRow(
                     column(
                       width = 12,
                       h1("Welcome to the Stress Test Application!")
                     )
                   ),
                   fluidRow(
                     column(
                       width = 8,
                       h3("Purpose of this Application"),
                       h5("This tool is part of the Bachelor's Thesis of Lucas Heller and Gian-Andrin Tommasini with the title 'Development of Stress Tests for the Analysis of the SVB Bank Run'.
                          Additionally to the written thesis, this tool should allow users to compare the results from the thesis in an interactive environment.
                          Also, users can upload their own contracts representing a bank's statement in the form of CSV-files allowing them to apply the scenarios on a different bank.")
                     )
                   ),
                   fluidRow(
                     column(
                       width = 8,
                       h3("Description of Tabs"),
                       h5("Scenario Selection:"),
                       p("You will be selecting from three different interest rate scenarios and can apply them to a portfolio of annuities or principal-at-maturity contracts (PAM) of an institution which simulates the institution's balance sheet and income statement. 
                         You will then get insights on how the financials would for the institution would look like under these scenarios and compare the results from the different cases. 
                         As our thesis is about the collapse of Silicon Valley Bank (SVB), the contracts we created to simulate their statements can be imported via an import button in the 'Load/Create Institution' tab. 
                         Interest rate scenarios will be applied to the federal funds rate, effective from a date you choose. 
                         The changes in the Federal Funds Effective Rate will determine the changes of the yield curve (interest rate for different tenors of contracts) through a parallel shift. 
                         Please find a short description of the three scenarios below"),
                       h5("Load/Create Institution:"),
                       p("You can choose between loading the contracts of SVB or creating your own institution by uploading CSV-files. The content of the files should fit the institution tree, which is also visible in this tab. 
                       For reference purposes, you can also download the contracts of SVB in CSV-format. 
                         The tree of the institution is fixed and you can add your own contracts to the respective nodes, where you want them to be part of."),
                       h5("Performance Analysis"),
                       p("Here you will be seeing the results of how the institution would perform under the applied scenarios. 
                         You will be able to choos between two withdrawal cases, leading to losses or gains on the sale of investment securities. 
                         For reference purposes, you can compare the results with the base scenario. The two withdrawal cases are described below.")
                     )
                   ),
                   fluidRow(
                     column(
                       width = 8,
                       h3("Scenario Description"),
                       p("You can choose between three scenarios. Each scenario will have a duration of 60 months."),
                       br(),
                       h5("Base Scenario:"),
                       p("For reference, a base scenario is created. In this scenario, interest rates will remain approximately constant over the whole testing period. Therefore, the Yield Curve will look very similar each year. 
                         In the case of SVB, this should illustrate what the management probably thought would be the development of interest rates when they purchased a huge amount of securities."),
                       h5("Stress Scenario 1:"),
                       p("In stress scenario 1, an increase in the Federal Funds Effective Rate will be simulated as follows:"),
                       p("- increase of 4% over the first 16 months"),
                       p("- increase of 1% over the next 24 months"),
                       p("- interest rate remains flat over the last 20 months"),
                       h5("Stress Scenario 2:"),
                       p("In stress scenario 2, an increase in the Federal Funds Effective Rate will be simulated as follows:"),
                       p("- increase of 2.5% over the first 24 months"),
                       p("- interest rate remains constant for the last 36 months")
                     )
                   ),
                   fluidRow(
                     column(
                       width = 8,
                       h3("Withdrawal Case Description"),
                       p("Two withdrawal cases can be selected from. Both should simulate an outflow in deposits, forcing the bank to sell securities, ordered by liquidity. 
                         Therfore, first, 'Cash and cash equivalents' will be reduced, followed by 'AFS securities' and then 'HTM securities'."),
                       br(),
                       h5("Withdrawal Case 1:"),
                       p("- decrease of 20% in 'Demand deposits'"),
                       p("- decrease of 10% in 'Deposits'"),
                       h5("Withdrawal Case 2:"),
                       p("- decrease of 40% in 'Demand deposits'"),
                       p("- decrease of 20% in 'Deposits'")
                     )
                   ),
                   fluidRow(
                     column(
                       width = 8,
                       h3("Disclaimer"),
                       h5("This tool is only for illustration purposes. The authors of the application will not take on any responsibilities following from any decisions based on the results obtained in this program. 
                          The user of the program agrees with the terms and conditions of ACTUS Financial Research Association and takes on full responsibility for behavior and actions."),
                       h5("For further information or possible extensions and modifications, please contact: contact@actusfrf.org or gianandrin.to@bluewin.ch")
                     )
                   )
                 )
                 
        ),
        tabPanel("Scenario Selection",
          id = "market",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              h4("Scenario Start Date:"),
              p("Please select a date at which the scenario should be applied.
                The interest rate will be generated following on from the date you choose.
                According to the Scenario Description, the next 60 months will be simulated from the date you select."),
              br(),
              h5("Please confirm your selection by hitting the 'Save settings' button. If you wish to change the date, simply hit the button with a different date again."),
              br(),
              dateInput("scenarioDate", "Select Date", value = Sys.Date() - months(1),
                        min = Sys.Date() - months (1) - years(5), max = Sys.Date() - months(1)
                        ),
              p("Note: The program will then automatically select the first date of the month you chose as the simulation works with monthly data."),
              p("The applied date is:"),
              uiOutput("appliedDate"),
              # fileInput("rf_file", "Upload Risk Factors",
              #   accept = c(
              #     "text/csv",
              #     "text/comma-separated-values,text/plain",
              #     ".csv"
              #   ),
              #   placeholder = "No file selected."
              # ),
              # uiOutput("rf_file_notification"),
              # p(
              #   "There are predefined Yield Curves and Default Curves datasets in the ",
              #   strong("downloads"),
              #   " section. You can upload them as they are or extend the downloaded files with your own riskfactors."
              # ),
              # actionButton("rf_import", "Import Risk Factor", width = "100%"),
              # p("- or -"),
              # br(),
              # selectInput("rf_type", "Risk Factor Type",
              #   choices = c(
              #     "YieldCurve",
              #     "DefaultCurve"
              #   )
              # ),
              # textInput("rf_label", "Label", placeholder = "YC_CH_AAA"),
              # dateInput("rf_ref_date", "Reference Date", value = Sys.Date(), format = "yyyy-mm-dd"),
              # fluidRow(
              #   id = "rf_tenors",
              #   column(3, textInput("rf_tenor1", label = "Tenor 1", value = '1Y')),
              #   column(3, textInput("rf_tenor2", label = "Tenor 2", value = NULL)),
              #   column(3, textInput("rf_tenor3", label = "Tenor 3", value = NULL)),
              #   column(3, textInput("rf_tenor4", label = "Tenor 4", value = NULL))
              # ),
              # fluidRow(
              #   id = "rf_rates",
              #   column(3, numericInput("rf_rate1", label = "Rate 1", value = 0.01, step = 0.01, min = 0.01, max = 1.0)),
              #   column(3, numericInput("rf_rate2", label = "Rate 2", value = NULL, step = 0.01, min = 0.01, max = 1.0)),
              #   column(3, numericInput("rf_rate3", label = "Rate 3", value = NULL, step = 0.01, min = 0.01, max = 1.0)),
              #   column(3, numericInput("rf_rate4", label = "Rate 4", value = NULL, step = 0.01, min = 0.01, max = 1.0))
              # ),
              # uiOutput("rf_single_notification"),
              # br(),
              # actionButton("rf_add", "Add Risk Factor", width = "100%")
              br(),
              actionButton("goRisk", "Save settings", width = "100%")
            ),
            mainPanel(
              width = 9,
              br(),
              p("Please select the scenario you wish to apply. Descriptions can be found in the 'Instructions' tab."),
              selectInput("scenarioSelect", "Scenario Selection:",
                          c("Base Scenario", 
                            "Stress Scenario 1", 
                            "Stress Scenario 2")
              ),
              plotOutput("scenarioPlot"),
              p("After hitting the 'Save settings' button, you can see how the Federal Funds Effective Rate would develop under your selected scenario above. 
                You can change the applied scenaio whenever you like.")
              # div(
      #           style = "width: 100%; max-height: 100vh; overflow-x: hidden; overflow-y: hidden; display: flex; flex-direction: column; align-items: center; justify-content: center;",
      #           splitLayout(
      #             cellWidths = c("50%", "50%"),
      #             
      #             # First section
      #             div(
      #               style = "height: 100%; word-wrap: break-word; overflow-y: auto; margin-right: 20px;",  # Add margin
      #               h3("Constant Yield Curve"),
      #               plotOutput("originalYCplot"),
      #               p(style = "white-space: normal;", "Above you can see the yield curve if it remains constant. 
      # In the simulation, this yield curve will be applied to the contracts and this should count as a reference.")
      #             ),
      #             
      #             # Second section
      #             div(
      #               style = "height: 100%; word-wrap: break-word; overflow-y: auto; margin-left: 20px;",
      #               h3("Stress Scenario Yield Curves"),
      #               plotOutput("scenarioYCplot"),
      #               p(style = "white-space: normal;", "Which year's yield curve would you like to see?"),
      #               selectInput("showYC", "Show", c(0, 1, 2, 3, 4, 5), selected = 0),
      #               p(style = "white-space: normal;", "Above you can see the yield curve for the year you selected. 
      # Note that the year with the date you selected is year 0. 
      # The yield curve for each year will be applied to the contracts and influence their value."),
      #               br(),
      #               br(),
      #               br(),
      #               br(),
      #               br(),
      #               br()
      #             )
      #           )
      #         ),
      #         div(
      #           style = "text-align: center; margin-top: 20px;",
      #           p("You may see how your yield curves look and if you think you are good to continue to the next step, hit the 'Ready for Institution' 
      #             button below and continue in the next tab."),
      #           actionButton("readyForInstitution", "Ready for Institution", width = "20%")
      #         ),
      #         br()
              # DTOutput("yieldCurve_df"),
              # br(),
              # actionButton("yc_duplicate", "Duplicate"),
              # actionButton("yc_remove", "Remove"),
              # div(downloadButton("downloadYC", "Download"), style = "float:right"),
              # br(),
              # plotOutput("ycPlot"),
              # verbatimTextOutput("ycD")
            )
          )
        ),
        tabPanel("Load/Create Institution",
          id = "institution",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              fluidRow(
                column(
                  width = 12,
                  p("You may see how your yield curves look and if you think you are good to continue to the next step, hit the 'Ready for Institution'
                  button below and continue in the next tab."),
                  selectInput("createOrLoad", 
                              label = "Load SVB or create an institution:",
                              choices = c("Load SVB", "Create own institution"),
                              selected = "Load SVB",
                              width = "100%")
                ),
                conditionalPanel(
                  condition = "input.createOrLoad == 'Load SVB'",
                  column(
                    width = 12,
                    actionButton("loadSVB", "Load SVB", width = "100%")
                  )
                ),
                conditionalPanel(
                  condition = "input.createOrLoad == 'Create own institution'",
                  column(
                    width = 12,
                    textInput("inst_name", NULL, placeholder = "Add institution name...", width = "100%"),
                    uiOutput('inst_warning'),
                    actionButton("saveName", "Save Name", width = "100%"),
                    br(),
                    br(),
                    selectInput("scaleSelect", "Select scale",
                                choices = c("in millions", "in thousands", "no scale")),
                    selectInput("ct_ptf_type", "Portfolio Type", choices = c("Annuities", "PrincipalAtMaturities", "Operations")),
                    fileInput("ct_file", "Upload Financial Contracts",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"
                              ),
                              placeholder = "No file selected."
                    ),
                    uiOutput("ct_file_notification"),
                    p(
                      "For reference, the Financial Contract datasets of SVB are in the ",
                      strong("Download SVB Data"),
                      " section. You can adjust downloaded files with your own contracts."
                    ),
                    actionButton("ct_import", "Import Financial Contracts", width = "100%")
                  )
                )
              )
            ),
            mainPanel(
              width = 9,
              tabsetPanel(
                id = "institutions",
                tabPanel(
                  "Structure"
                ),
                tabPanel(
                  "Financial Contracts"
                )
              )
              # fluidRow(
              #   id = 'inst_creation_div',
              #   column(
              #     width = 2,
              #     textInput("inst_name", NULL, placeholder = "Add institution name...", width = "100%"),
              #     uiOutput('inst_warning')
              #   ),
              #   column(
              #     width = 1,
              #     actionButton("inst_add", "Create", icon = icon("plus"), width = "100%"),
              #   ),
              #   conditionalPanel(
              #     condition = 'output.inst_panel == true',
              #     column(
              #       width = 7,
              #       selectInput("inst_view", NULL, choices = NULL, width = '100%')
              #     ),
              #     column(
              #       width = 1,
              #       actionButton("inst_delete", "Delete", width = "100%")
              #     ),
              #     column(
              #       width = 1,
              #       actionButton("inst_clone", "Clone", width = "100%")
              #     )
              #   )
              # ),
              # conditionalPanel(
              #   condition = 'output.inst_panel == true',
              #   sidebarLayout(
              #     sidebarPanel(
              #       width = 3,
              #       selectInput("ct_ptf_type", "Portfolio Type", choices = c("Annuities", "PrincipalAtMaturities", "Operations")),
              #       fileInput("ct_file", "Upload Financial Contracts",
              #                 accept = c(
              #                   "text/csv",
              #                   "text/comma-separated-values,text/plain",
              #                   ".csv"
              #                 ),
              #                 placeholder = "No file selected."
              #       ),
              #       uiOutput("ct_file_notification"),
              #       p(
              #         "There are predefined Financial Contract datasets in the ",
              #         strong("downloads"),
              #         " section. You can upload them as they are or extend the downloaded files with your own contracts."
              #       ),
              #       actionButton("ct_import", "Import Financial Contracts", width = "100%"),
              #       p("- or -", style = "text-align: center;"),
              #       br(),
              #       selectInput("node", "Account", choices = NULL),
              #       selectInput("contractType", "Contract Type",
              #                   choices = c(
              #                     "ANN",
              #                     "PAM",
              #                     "Investments",
              #                     "OperationalCF"
              #                   )
              #       ),
              #       conditionalPanel(
              #         condition = 'input.contractType == "ANN" || input.ct_type == "PAM"',
              #         fluidRow(
              #           column(
              #             width = 6,
              #             selectInput("contractRole", "Contract Role",
              #                         choices = c(
              #                           "RPA",
              #                           "RPL"
              #                         )
              #             ),
              #             selectInput("dayCountConvention", "Day Count Convention",
              #                         choices = c(
              #                           "30E360",
              #                           "B252",
              #                           "A360",
              #                           "A365"
              #                         )
              #             ),
              #             numericInput("notionalPrincipal", "Nominal Principal", value = 1000, step = 100, min = 100),
              #             numericInput("nominalInterestRate", "Nominal Interest Rate", value = 0.01, step = 0.001, min = 0.001, max = 1.0)
              #           ),
              #           column(
              #             width = 6,
              #             textInput("contractID", "Contract Identifier", placeholder = "CTR0001"),
              #             dateInput("initialExchangeDate", "Initial Exchange Date", value = Sys.Date(), format = "yyyy-mm-dd"),
              #             dateInput("maturityDate", "Maturity Date", value = "", format = "yyyy-mm-dd"),
              #             selectInput("currency", "Currency",
              #                         choices = c(
              #                           "CHF",
              #                           "USD",
              #                           "EUR"
              #                         )
              #             )
              #           )
              #         ),
              #         fluidRow(
              #           column(
              #             width = 12,
              #             br(),
              #             actionButton("calendarToggle", "Calendar", width = "100%"),
              #           )
              #         ),
              #         conditionalPanel(
              #           condition = "input.calendarToggle % 2 == 1",
              #           toggle = TRUE,
              #           fluidRow(
              #             column(
              #               width = 12,
              #               br(),
              #               selectInput("calendar", "Calendar", choices = c('NOCALENDAR')),
              #               selectInput("businessDayConvention", "Business Day Convention", choices = NULL),
              #               selectInput("endOfMonthConvention", "End Of Month Convention", choices = NULL)
              #             )
              #           )
              #         ),
              #         fluidRow(
              #           column(
              #             width = 12,
              #             br(),
              #             actionButton("contractIdentificationToggle", "Contract Identification", width = "100%"),
              #           )
              #         ),
              #         conditionalPanel(
              #           condition = "input.contractIdentificationToggle % 2 == 1",
              #           toggle = TRUE,
              #           fluidRow(
              #             column(
              #               width = 12,
              #               br(),
              #               dateInput("statusDate", "Status Date", value = "", format = "yyyy-mm-dd"),
              #               textInput("legalEntityIDRecordCreator", "Legal Entity ID Record Creator"),
              #               textInput("legalEntityIDCounterparty", "Legal Entity ID Counterparty"),
              #             )
              #           )
              #         ),
              #         fluidRow(
              #           column(
              #             width = 12,
              #             br(),
              #             actionButton("interestToggle", "Interest", width = "100%"),
              #           )
              #         ),
              #         conditionalPanel(
              #           condition = "input.interestToggle % 2 == 1",
              #           toggle = TRUE,
              #           fluidRow(
              #             column(
              #               width = 12,
              #               br(),
              #               dateInput("cycleAnchorDateOfInterestPayment", "Cycle Anchor Date of Interest Payment", value = "", format = "yyyy-mm-dd"),
              #               selectInput("cycleOfInterestPayment", "Cycle of Interest Payment", choices = c('P1YL0', 'P2YL0', 'P6ML0', 'P3ML0', 'P1ML0', 'None')),
              #               selectInput("cyclePointOfInterestPayment", "Cycle Point Of Interest Payment", choices = c('E', 'B')),
              #               numericInput("accruedInterest", "Accrued Interest", value = NULL, min = 0),
              #               
              #             )
              #           )
              #         ),
              #         fluidRow(
              #           column(
              #             width = 12,
              #             br(),
              #             actionButton("notionalPrincipalToggle", "NotionalPrincipal", width = "100%"),
              #           )
              #         ),
              #         conditionalPanel(
              #           condition = "input.notionalPrincipalToggle % 2 == 1",
              #           toggle = TRUE,
              #           fluidRow(
              #             column(
              #               width = 12,
              #               br(),
              #               dateInput("amortizationDate", "Amortization Date", value = "", format = "yyyy-mm-dd"),
              #               dateInput("contractDealDate", "Contract Deal Date", value = Sys.Date(), format = "yyyy-mm-dd"),
              #               dateInput("terminationDate", "Termination Date", value = "", format = "yyyy-mm-dd"),
              #               numericInput("premiumDiscountAtIED", "Premium Discount At IED", value = 0, min = 0),
              #               dateInput("cycleAnchorDateOfPrincipalRedemption", "Cycle Anchor Date Of Principal Redemption", value = "", format = "yyyy-mm-dd"),
              #               selectInput("cycleOfPrincipalRedemption", "Cycle Of Principal Redemption", choices = c('P1YL0', 'P2YL0', 'P6ML0', 'P3ML0', 'P1ML0', 'None'), selected = NULL, multiple = FALSE),
              #               numericInput("nextPrincipalRedemptionPayment", "Next Principal Redemption Payment", value = NULL, min = 0)
              #             )
              #           )
              #         ),
              #         fluidRow(
              #           column(
              #             width = 12,
              #             br(),
              #             actionButton("rateResetToggle", "Rate Reset", width = "100%"),
              #           )
              #         ),
              #         conditionalPanel(
              #           condition = "input.rateResetToggle % 2 == 1",
              #           toggle = TRUE,
              #           fluidRow(
              #             column(
              #               width = 12,
              #               br(),
              #               dateInput("cycleAnchorDateOfRateReset", "Cycle Anchor Date of Rate Reset", value = "", format = "yyyy-mm-dd"),
              #               selectInput("cycleOfRateReset", "Cycle of Rate Reset", choices = c('None', 'P1YL0', 'P2YL0', 'P6ML0', 'P3ML0', 'P1ML0'), selected = NULL, multiple = FALSE),
              #               selectInput("cyclePointOfRateReset", "Cycle Point Of Rate Reset", choices = c('B', 'E')),
              #               numericInput("rateSpread", "Rate Spread", value = 0, step = 0.001, min = 0),
              #               numericInput("rateMultiplier", "Rate Multiplier", value = 1, step = 0.001, min = 1),
              #               selectInput("marketObjectCodeOfRateReset", "Market Object Code of Rate Reset",
              #                           choices = NULL
              #               )
              #             )
              #           )
              #         )
              #       ),
              #       conditionalPanel(
              #         condition = 'input.ct_type == "Investments" || input.ct_type == "OperationalCF"',
              #         p("Investments / OpCF fields")
              #       ),
              #       br(),
              #       uiOutput("ct_single_notification"),
              #       actionButton("ct_add", "Add Financial Contract", width = "100%")
              #     ),
              #     mainPanel(
              #       width = 9,
              #       tabsetPanel(
              #         id = "institutions",
              #         tabPanel(
              #           "Structure",
              #           br(),
              #           fluidRow(
              #             column(
              #               width = 6,
              #               verbatimTextOutput('inst_structure')
              #             ),
              #             column(
              #               width = 6,
              #               p('In this section, you have the ability to modify the existing structure of the institution by adding new account types and/or accounts, renaming or removing them as needed.',
              #                 style = "text-align: justify;"),
              #               p('Note that any changes made to the structure will affect the accounts and balances associated with the institution, so be careful when making changes and ensure that the structure accurately reflects the financial hierarchy of the institution.', 
              #                 style = "text-align: justify;"),
              #               fluidRow(
              #                 column(
              #                   width = 4,
              #                   selectInput('str_node', 'Node', choices = NULL),
              #                   uiOutput('str_notification')
              #                 ),
              #                 column(
              #                   width = 4,
              #                   uiOutput('str_node_options_1_1'),
              #                   uiOutput('str_node_options_1_2')
              #                 ),
              #                 column(
              #                   width = 4,
              #                   uiOutput('str_node_options_2_1'),
              #                   uiOutput('str_node_options_2_2')
              #                 )
              #               )
              #             )
              #           )
              #         ),
              #         tabPanel(
              #           "Financial Contracts",
              #           br(),
              #           fluidRow(
              #             column(
              #               width = 12,
              #               selectInput("fc_view", NULL, choices = NULL, width = "100%"),
              #               uiOutput("fc_ui"),
              #               br()
              #             )
              #           )
              #         ),
              #         tabPanel(
              #           "Market Objects",
              #           br(),
              #           fluidRow(
              #             column(
              #               width = 12,
              #               p('This section captures the effective market object per financial contract.'),
              #               DTOutput("inst_market_df", width = '100%')
              #             )
              #           )
              #         ),
              #         tabPanel(
              #           "Log",
              #           br(),
              #           fluidRow(
              #             column(
              #               width = 12,
              #               p('This section captures only the logs of the last initiated financial contract import whether it was a file upload or a single financial contract import.'),
              #               DTOutput("error_log_df", width = '100%')
              #             )
              #           )
              #         )
              #       )
              #     )
              #   )
              # )
            )
          )
        ),
        tabPanel("Performance Analysis",
                 id = "performanceAnalysis", # riskAnalysis
                 sidebarLayout(
                   sidebarPanel(
                     width = 3,
                     selectInput("withdrawalCase", "Select Withdrawal Case", 
                                 choices = c("Withdrawal Case 1", "Withdrawal Case 2")),
                     p("Select whether you wish to apply the IFRS Accounting Standards or your prefer to see Market Valuation in the",
                     strong("Balance Sheet without Withdrawals"), "tab. You may also select both if you wish to see and compare the balance sheets under the two different methods."),
                     p("In the", strong("Results with withdrawals"), "section the IFRS standards are used."),
                     selectInput("accountingMethod", "Select Accounting Standard",
                                 choices = c("IFRS Standards (SVB Balance Sheet)", "Market Valuation", "Both")),
                     p("Further, you can choose whether you wish to see the maximum possible loss under the scenario you applied."),
                     checkboxInput("showMaxLoss", "Show maximum possible loss", value = F),
                     br(),
                     p(strong("Note:"), "The income statement in the", strong("Income Statement"), "section is without gains and losses on the sales of investment securities.")
                     # selectInput("ra_inst", "Select Institution", choices = NULL),
                     # selectInput("ra_scenario", "Risk Scenario", choices = c("Interest Rate Risk", "Default Risk")),
                     # conditionalPanel(
                     #   condition = "input.ra_scenario == 'Interest Rate Risk'",
                     #   selectInput("ra_sub_scenario", "Interest Rate Risk Scenario", choices = c('Parallel Shift')),
                     #   selectInput("ra_mocs", "Market Objects", choices = NULL, multiple = TRUE),
                     #   p("Please add new Market Object in tab 'Market' if you wish to proceed with different Risk Factor Object for the valuation."),
                     #   fluidRow(
                     #     id = "ra_irr_shift_amounts",
                     #     column(3, numericInput("ra_irr_shift_amount1", label = "Shift 1", value = 0.01, step = 0.001, min = 0.001, max = 1.0)),
                     #     column(3, numericInput("ra_irr_shift_amount2", label = "Shift 2", value = NULL, step = 0.001, min = 0.001, max = 1.0)),
                     #     column(3, numericInput("ra_irr_shift_amount3", label = "Shift 3", value = NULL, step = 0.001, min = 0.001, max = 1.0)),
                     #     column(3, numericInput("ra_irr_shift_amount4", label = "Shift 4", value = NULL, step = 0.001, min = 0.001, max = 1.0))
                     #   )
                     # ),
                     # conditionalPanel(
                     #   condition = "input.ra_scenario == 'Default Risk'",
                     #   selectInput("ra_dr_sub_scenario", "Default Risk Scenario", choices = c('Probability')),
                     #   selectInput("ra_dr_mocs", "Market Objects", choices = NULL, multiple = TRUE),
                     #   p("Please add new Market Object in tab 'Market' if you wish to proceed with different Risk Factor Object for the simulation."),
                     #   fluidRow(
                     #     id = "ra_dr_recovery_rates",
                     #     column(3, numericInput("ra_dr_recovery_rate1", label = "Recovery 1", value = 0.01, step = 0.001, min = 0.001, max = 1.0)),
                     #     column(3, numericInput("ra_dr_recovery_rate2", label = "Recovery 2", value = NULL, step = 0.001, min = 0.001, max = 1.0)),
                     #     column(3, numericInput("ra_dr_recovery_rate3", label = "Recovery 3", value = NULL, step = 0.001, min = 0.001, max = 1.0)),
                     #     column(3, numericInput("ra_dr_recovery_rate4", label = "Recovery 4", value = NULL, step = 0.001, min = 0.001, max = 1.0))
                     #   ),
                     #   dateInput("ra_dr_from", "Default From", value = as.Date(paste0(format(Sys.Date() + years(1), "%Y"), "-01-01")), format = "yyyy-mm-dd")
                     # ),
                     # selectInput("ra_value_view", "Value View", choices = c("nominal", "market")),
                     # selectInput("ra_income_view", "Income View", choices = c("marginal", "cumulative")),
                     # selectInput("ra_scale", "Scale", choices = c("in millions", "in thousands", "no scale")),
                     # dateInput("ra_from", "From", value = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")), format = "yyyy-mm-dd"),
                     # dateInput("ra_to", "To", value = as.Date(paste0(format(Sys.Date() + years(1), "%Y"), "-01-01")), format = "yyyy-mm-dd"),
                     # br(),
                     # actionButton("ra_start", "Start Risk Analysis", width = "100%")
                   ),
                   mainPanel(
                     width = 9,
                     tabsetPanel(
                       id = "results",
                       tabPanel(
                         "Income Statement"
                         ),
                       tabPanel(
                         "Balance Sheet without Withdrawals"
                       ),
                       tabPanel(
                         "Results With Withdrawals"
                       )
                     )
                     # fluidRow(
                     #   column(
                     #     width = 11,
                     #     selectInput("ra_view", NULL, choices = NULL, width = '100%')
                     #   ),
                     #   column(
                     #     width = 1,
                     #     actionButton("ra_delete", "Delete", icon = icon("minus"), width = "100%")
                     #   )
                     # ),
                     # fluidRow(
                     #   column(
                     #     width = 12,
                     #     actionButton("ra_detailsToggle", "Details", width = "100%", style = "margin-bottom: 20px !important;")
                     #   )
                     # ),
                     # conditionalPanel(
                     #   condition = "input.ra_detailsToggle % 2 == 1",
                     #   toggle = TRUE,
                     #   div(
                     #     fluidRow(
                     #       column(6,
                     #              verbatimTextOutput("ra_inst_output"),
                     #              verbatimTextOutput("ra_scenario_output"),
                     #              verbatimTextOutput("ra_sub_scenario_output"),
                     #              verbatimTextOutput("ra_from_output"),
                     #              verbatimTextOutput("ra_to_output")
                     #       ),
                     #       column(6,
                     #              verbatimTextOutput("ra_mocs_output"),
                     #              verbatimTextOutput("ra_rates_output"),
                     #              verbatimTextOutput("ra_value_view_output"),
                     #              verbatimTextOutput("ra_income_view_output"),
                     #              verbatimTextOutput("ra_scale_output")
                     #       )
                     #     ),
                     #     style = "margin-bottom: 20px; padding: 19px; border: 1px solid #e5e5e5; border-radius:4px;"
                     #   )
                     # ),
                     # uiOutput("ra_uiOutput")
                   )
                 )
        )
      )
    ),
    tabPanel("Download SVB Data",
      id = "downloads",
      fluidRow(
        column(
          width = 3,
          selectInput("dataset",
            "Choose a dataset:",
            choices = c("Annuities", "PrncipalAtMaturities", "Operations") # annuities was before
          ),
          downloadButton("downloadData", "Download")
        ),
        column(
          width = 9,
          tabsetPanel(
            tabPanel(
              "Data",
              br(),
              DTOutput("table")
            ),
            tabPanel(
              "Description",
              br(),
              htmlOutput("tableDoc")
            )
          )
        )
      )
    )
  ) # close navbarPage
) # close fluidPage
