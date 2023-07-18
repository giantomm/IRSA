#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
options(width = 150)
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
                         For reference purposes, you can compare the results with the base scenario. The two withdrawal cases are described below. 
                         To compare the results under the different stress scenarios, you are able to swith between all three scenarios in this section.")
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
                         Therefore, first, 'Cash and cash equivalents' will be reduced, followed by 'AFS securities' and then 'HTM securities'."),
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
              p("Please select a date at which scenarios should be applied.
                The interest rate will be generated following on from the date you choose.
                According to the Scenario Description, the next 60 months will be simulated from the date you select."),
              p("In the", strong("Performance Analysis"), "tab, you can switch between the scenarios to compare the different results."),
              br(),
              h5("Please confirm your selection by hitting the 'Save settings' button. If you wish to change the date, simply hit the button with a different date again."),
              br(),
              dateInput("scenarioDate", "Select Date", value = "2022-01-01",
                        min = Sys.Date() - months (1) - years(5), max = Sys.Date() - months(1)
                        ),
              h5("If you wish to load SVB's data, the", strong("Applied Date"), "must be 2022-01-01!"),
              p(strong("Note:"), "The program will then automatically select the first date of the month you chose as the simulation works with monthly data."),
              p("The", strong("Applied Date"), "for the scenarios is:"),
              uiOutput("appliedDate"),
              br(),
              actionButton("goRisk", "Save settings", width = "100%")
            ),
            mainPanel(
              width = 9,
              br(),
              p("Please select the scenario you wish to see. Descriptions can be found in the 'Instructions' tab."),
              selectInput("scenarioSelect", "Scenario Selection:",
                          c("Base Scenario", 
                            "Stress Scenario 1", 
                            "Stress Scenario 2")
              ),
              plotOutput("scenarioPlot"),
              br(),
              p("After hitting the 'Save settings' button, you can see how the Federal Funds Effective Rate would develop under your selected scenario above. 
                These scenarios will be used for the simulation after creating an institution in the", 
                strong("Load/Create Institution"), "tab. You will be able to switch between the scenarios in the", strong("Performance Analysis"), 
                "section and hence compare the results under different stress scenarios."),
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
                    textInput("instName", "Bank", placeholder = "Add institution name...", width = "100%"),
                    selectInput("scaleSelect", "Select scale",
                                choices = c("in millions", "in thousands", "without scale")),
                    actionButton("saveInst", "Save Institution", width = "100%"),
                    br(),
                    br(),
                    selectInput("contractType", "Portfolio Type", choices = c("Annuities", "PrincipalAtMaturities", "Operations")),
                    fileInput("contractFile", "Upload Financial Contracts",
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
                    p(strong("Note:"), "Make sure to use the Yield Curve from the", strong("Applied Date"),
                      "as a market object for", strong("ALL"), "contracts with variable rate. Otherwise the income statement will be wrong.
                      This means if your applied date is in the year 2022, you should use 'YC_2022' as the market object."),
                    actionButton("contractImport", "Import Financial Contracts", width = "100%"),
                    br(),
                    p(strong("Important:"), "Use the same market object for all variable rate contracts, otherwise the income statement
                      will be calculated wrong.")
                  )
                ),
                column(
                  width = 12,
                  br(),
                  p("If everything is set and you are satisfied with the contracts you may start the simulation by hitting the", 
                    strong("Start Simulation"), "button. You can then switch to the", strong ("Performance Analysis"),
                    "tab."),
                  p("Attention: The simulation takes some time."),
                  br(),
                  actionButton("startSim", "Start Simulation", width = "100%")
                )
              )
            ),
            mainPanel(
              width = 9,
              tabsetPanel(
                id = "institutions",
                tabPanel(
                  "Structure",
                  br(),
                  fluidRow(
                    column(
                      width = 6,
                      withSpinner(verbatimTextOutput('inst_structure'))
                    ),
                    column(
                      width = 6,
                      p("On the left you can see the structure allowed for institutions. 
                        If you decide to load SVB, it will have this structure. Should you decide to 
                        create your own institution, you", strong("must"), "follow this structure. 
                        All other nodes (leaves of the tree) will not be taken into account."),
                      p("This means that if you create your own contracts,", strong("always"), "insert a leaf-element of the 
                        given tree structure into the", strong("node column"), "of your CSV-files.")
                    )
                  )
                ),
                tabPanel(
                  "Financial Contracts",
                  br(),
                  selectInput("showContracts", NULL, choices = NULL, width = "100%"),
                  br(),
                  withSpinner(DTOutput("contractsDF"))
                )
              )
            )
          )
        ),
        tabPanel("Performance Analysis",
                 id = "performanceAnalysis", # riskAnalysis
                 sidebarLayout(
                   sidebarPanel(
                     width = 3,
                     selectInput("applyScenario", "Apply Scenario", 
                                 choices = c("Base Scenario", "Stress Scenario 1", "Stress Scenario 2")),
                     selectInput("withdrawalCase", "Select Withdrawal Case", 
                                 choices = c("Withdrawal Case 1", "Withdrawal Case 2")),
                     # p("Select whether you wish to apply the IFRS Accounting Standards or your prefer to see Market Valuation in the",
                     # strong("Balance Sheet without Withdrawals"), "tab. You may also select both if you wish to see and compare the balance sheets under the two different methods."),
                     # p("In the", strong("Results with withdrawals"), "section the IFRS standards are used."),
                     # selectInput("accountingMethod", "Select Accounting Standard",
                     #             choices = c("SVB Balance Sheet (IFRS)", "HTM Market Valuation", "Both")),
                     # p("Further, you can choose whether you wish to see the maximum possible loss under the scenario you applied."),
                     # checkboxInput("showMaxLoss", "Show maximum possible loss", value = F),
                     # br(),
                     uiOutput("scaleSelection"),
                     br(),
                     h5("Income Statement"),
                     p("Here you will find the income statement for the institution."),
                     h5("Results without Withdrawals"),
                     p("In this section you can find the balance sheets without withdrawals. Both, a balance sheet with the accounting 
                       standards of SVB (per IFRS) and with HTM securities valued by market value can be found. Additionally, the maxmimum 
                       possible loss under the selected scenario is illustrated."),
                     h5("Results with Withdrawals"),
                     p("In this section, the balance sheet with SVB standards (IFRS accounting), the realized gain/loss on the sales of investment 
                       securities and resulting net income, and liquidity, profitability and leverage ratios are shown. The results here depend on the 
                       selected withdrawal case and scenario."),
                     br(),
                     p(strong("Note:"), "The simulation takes some time, please be patient.")
                   ),
                   mainPanel(
                     width = 9,
                     tabsetPanel(
                       id = "results",
                       tabPanel(
                         "Income Statement",
                         br(),
                         fluidRow(
                           column(
                             width = 12,
                             # style = "overflow-x: scroll; white-space: nowrap;",
                             h5("Income Statement without Gains/Losses on Investment Securities"),
                             withSpinner(verbatimTextOutput('incomeStatement')),
                             p("The income statement for all scenarios and withdrawal cases is the same. 
                               It is before gains and losses on the sales of investment securities as
                               all contracts with variable rate use the same Yield Curve. Therefore, it is necessary that the market object for all contracts is the Yield Curve
                               from the", strong("Applied Date."))
                           )
                         )
                         ),
                       tabPanel(
                         "Results without Withdrawals",
                         br(),
                         fluidRow(
                           column(
                             width = 12,
                             h5("Balance Sheet with SVB Accounting Method (IFRS)"),
                             withSpinner(verbatimTextOutput("Bal_IFRS")),
                             br(),
                             h5("Balance Sheet with HTM Market Valuation"),
                             withSpinner(verbatimTextOutput("Bal_Market")),
                             br(),
                             h5("Maximum Possible Loss Under Selected Scenario"),
                             withSpinner(verbatimTextOutput("Max_Loss")),
                           )
                           )
                       ),
                       tabPanel(
                         "Results with Withdrawals",
                         br(),
                         fluidRow(
                           column(
                             width = 12,
                             h5("Balance Sheet after Withdrawals"),
                             withSpinner(verbatimTextOutput("BS_Withdrawals")),
                             br(),
                             h5("Realized Gain/Loss and Net Income"),
                             withSpinner(verbatimTextOutput("NI_Withdrawals")),
                             br(),
                             h5("Liquidity, Profitability and Leverage Ratios"),
                             withSpinner(verbatimTextOutput("Ratios_Withdrawals"))
                           )
                         ),
                       )
                     )
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
            choices = c("Annuities", "PrincipalAtMaturities", "Operations") # annuities was before
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
              p("You can download the contracts of SVB in the form of CSV files. 
                This should give you an idea, what the contracts for your own institution could look like and illustrate the required format for the use of this pplication. 
                In a next step, you can then modify the files and upload it yourself. This way you can analyze your own institution with some sort of guide.")
            )
          )
        )
      )
    )
  ) # close navbarPage
) # close fluidPage
