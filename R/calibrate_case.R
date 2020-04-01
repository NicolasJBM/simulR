#' Shiny gadget to select exam questions and generate both questions and solutions.
#' @return Save the different versions of exams and solutions.
#' @importFrom miniUI miniPage
#' @importFrom miniUI gadgetTitleBar
#' @importFrom miniUI miniTabstripPanel
#' @importFrom miniUI miniTabPanel
#' @importFrom miniUI miniContentPanel
#' @importFrom shiny fillCol
#' @importFrom shiny fillRow
#' @importFrom shiny icon
#' @importFrom shiny fileInput
#' @importFrom shiny textInput
#' @importFrom shiny dateInput
#' @importFrom shiny numericInput
#' @importFrom shiny textAreaInput
#' @importFrom shiny selectInput
#' @importFrom shiny checkboxInput
#' @importFrom shiny downloadButton
#' @importFrom shiny downloadHandler
#' @importFrom shiny stopApp
#' @importFrom shiny runGadget
#' @importFrom shiny conditionalPanel
#' @importFrom shiny tags
#' @importFrom shiny tableOutput
#' @importFrom shiny uiOutput
#' @importFrom shiny plotOutput
#' @importFrom shiny actionButton
#' @importFrom shiny renderUI
#' @importFrom shiny renderPlot
#' @importFrom shiny renderText
#' @importFrom shiny renderTable
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny withProgress
#' @importFrom shiny incProgress
#' @importFrom shiny includeHTML
#' @importFrom shiny withMathJax
#' @importFrom shiny dialogViewer
#' @importFrom shiny browserViewer
#' @importFrom shiny textOutput
#' @importFrom shiny htmlOutput
#' @importFrom shiny HTML
#' @importFrom shinythemes shinytheme
#' @importFrom readxl read_excel
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable rhandsontable
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarise
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom purrr map_lgl
#' @importFrom forcats fct_reorder
#' @import ggplot2
#' @export


calibrate_case <- function() {
  ui <- miniPage(
    theme = shinythemes::shinytheme("spacelab"),
    
    gadgetTitleBar("Calibrate case"),
    miniTabstripPanel(
      miniTabPanel("Parameters",
                   icon = icon("sliders"),
                   miniContentPanel(
                     fillCol(
                       flex = c(1,1,1,8),
                       fillRow(flex = c(1,1),
                               shiny::fileInput("case", label = "Select an excel template", multiple = FALSE, accept = c(".xlsx")),
                               shiny::actionButton("import", "Import the selected excel tempate")
                       ),
                       tags$hr(),
                       fillRow(flex = c(1,1),
                               shiny::dateInput("start_date", "Starting date"),
                               shiny::numericInput("number_months", "Number of months to simulate", value = 12, min = 1)
                       ),
                       tags$hr()
                     )
                   )
      ),
      
      miniTabPanel("Environment",
                   icon = icon("table"),
                   miniContentPanel(
                     rHandsontableOutput("environmentsEdit", width = "100%", height = "200px")
                   )
      ),
      
      miniTabPanel("Seasons",
                   icon = icon("table"),
                   miniContentPanel(
                     rHandsontableOutput("seasonsEdit", width = "100%", height = "800px")
                   )
      ),
      
      miniTabPanel("Accounts",
                   icon = icon("table"),
                   miniContentPanel(
                     rHandsontableOutput("accountsEdit", width = "100%", height = "800px")
                   )
      ),
      
      miniTabPanel("Capacity",
                   icon = icon("table"),
                   miniContentPanel(
                     rHandsontableOutput("capacityEdit", width = "100%", height = "800px")
                   )
      ),
      
      miniTabPanel("Technology",
                   icon = icon("table"),
                   miniContentPanel(
                     rHandsontableOutput("technologyEdit", width = "100%", height = "800px")
                   )
      ),
      
      miniTabPanel("Costing",
                   icon = icon("table"),
                   miniContentPanel(
                     rHandsontableOutput("costingEdit", width = "100%", height = "300px")
                   )
      ),
      
      miniTabPanel("Usages",
                   icon = icon("chart-bar"),
                   miniContentPanel(
                     fillCol(
                       flex = c(1,1,5,5),
                       shiny::actionButton("simulate", "Run simulation"),
                       tags$hr(),
                       plotOutput("demand_sales_production"),
                       plotOutput("capacity_usage")
                     )
                   )
      ),
      
      miniTabPanel("Balance sheet",
                   icon = icon("chart-bar"),
                   miniContentPanel(
                     fillCol(flex = c(1), plotOutput("balance_sheet"), width = "100%", height = "1000px")
                   )
      ),
      
      miniTabPanel("Reports",
                   icon = icon("file_alt"),
                   miniContentPanel(
                     
                   )
      )
      
    )
  )
  
  
  server <- function(input, output, session) {
    
    # Bind variables
    account_custom_label <- NULL
    account <- NULL
    account_metric <- NULL
    consumed <- NULL
    data <- NULL
    demand <- NULL
    keep <- NULL
    metric <- NULL
    period <- NULL
    purpose <- NULL
    quantity <- NULL
    sales <- NULL
    sold <- NULL
    unused <- NULL
    usage <- NULL
    used <- NULL
    volume <- NULL
    account_section <- NULL
    account_statement <- NULL
    account_subcategory <- NULL
    amount <- NULL
    bloc <- NULL
    section <- NULL
    
    
    values <- reactiveValues()
    
    observeEvent(input$import, {
      values$information <- readxl::read_excel(input$case$datapath[[1]], sheet = 1)
      values$environments <- readxl::read_excel(input$case$datapath[[1]], sheet =  2)
      values$seasons <- readxl::read_excel(input$case$datapath[[1]], sheet = 3)
      values$accounts <- readxl::read_excel(input$case$datapath[[1]], sheet = 4) %>%
        dplyr::mutate(account_label = dplyr::case_when(
          is.na(account_custom_label) ~ account_label,
          TRUE ~ account_custom_label
        )) %>%
        na.omit() %>%
        dplyr::select(-account_custom_label)
      values$capacity <- readxl::read_excel(input$case$datapath[[1]], sheet = 5)
      values$technology <- readxl::read_excel(input$case$datapath[[1]], sheet = 6)
      values$costing <- readxl::read_excel(input$case$datapath[[1]], sheet = 7)
      values$simulation <- NULL
    })
    
    
    ###############################################################################################################
    
    #Update the reactive values based on user input
    observe({ if(!is.null(input$environmentsEdit)) values$environments <- suppressWarnings(hot_to_r(input$environmentsEdit)) })
    observe({ if(!is.null(input$seasonsEdit)) values$seasons <- suppressWarnings(hot_to_r(input$seasonsEdit)) })
    observe({ if(!is.null(input$accountsEdit)) values$accounts <- suppressWarnings(hot_to_r(input$accountsEdit)) })
    observe({ if(!is.null(input$capacityEdit)) values$capacity <- suppressWarnings(hot_to_r(input$capacityEdit)) })
    observe({ if(!is.null(input$technologyEdit)) values$technology <- suppressWarnings(hot_to_r(input$technologyEdit)) })
    observe({ if(!is.null(input$costingEdit)) values$costing <- suppressWarnings(hot_to_r(input$costingEdit)) })
    
    
    #Format and display tables for input
    output$environmentsEdit <- renderRHandsontable({
      rhandsontable(values$environments)
    })
    output$seasonsEdit <- renderRHandsontable({
      rhandsontable(values$seasons)
    })
    output$accountsEdit <- renderRHandsontable({
      rhandsontable(values$accounts)
    })
    output$capacityEdit <- renderRHandsontable({
      rhandsontable(values$capacity)
    })
    output$technologyEdit <- renderRHandsontable({
      rhandsontable(values$technology)
    })
    output$costingEdit <- renderRHandsontable({
      rhandsontable(values$costing)
    })
    
    
    ###############################################################################################################
    
    observeEvent(input$simulate, {
      
      withProgress(message = "Running simulation", value = 0, {
        
        
        incProgress(1 / (1+input$number_months), detail = "Initializing...")
        case <- simulR::create_case(case = values,
                                    start_date = input$start_date,
                                    number_months = input$number_months,
                                    number_companies = 1)
        
        for (i in 1:input$number_months){
          incProgress(1 / (1+input$number_months), detail = paste0("Simulating period ", i, "..."))
          case <- simulR::simulate_period(
            case,
            period = i
          )
        }
        
      })
      
      values$simulation <- case
    })
    
    
    ###############################################################################################################
    
    output$demand_sales_production <- shiny::renderPlot({
      
      if (!is.null(values$simulation)){
        part1 <- values$simulation$competition[[1]]$profile  %>%
          dplyr::group_by(period, account) %>%
          dplyr::summarise(demand = sum(demand), sales = sum(sales)) %>%
          dplyr::ungroup() %>%
          tidyr::pivot_longer(cols = c("sales","demand"), names_to = "metric", values_to = "volume") %>%
          dplyr::mutate(account = as.numeric(stringr::str_replace_all(account, "400", "131")))
        
        part2 <- values$simulation$competition[[1]]$activity %>%
          dplyr::filter(unit == "unit", purpose == "inventory") %>%
          dplyr::select(period, account = output, volume = quantity) %>%
          dplyr::mutate(metric = "production")
        
        part1 %>%
          dplyr::bind_rows(part2) %>%
          dplyr::mutate(
            account = as.character(account),
            account_metric = paste0(account,"_",metric)
          )%>%
          ggplot(aes(x = period, y = volume, group = account_metric, color = account, lty = metric)) +
          geom_line() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
    })
    
    output$capacity_usage <- shiny::renderPlot({
      
      if (!is.null(values$simulation)){
        values$simulation$competition[[1]]$usage  %>%
          dplyr::mutate(used = (sold + consumed)) %>%
          dplyr::select(account, period, used, unused) %>%
          dplyr::group_by(account) %>%
          tidyr::nest() %>%
          dplyr::mutate(keep = purrr::map_lgl(data, function(x) max(x$unused) > 0)) %>%
          dplyr::filter(keep == TRUE) %>%
          tidyr::unnest(data) %>%
          dplyr::select(-keep) %>%
          tidyr::pivot_longer(cols = c("used","unused"), names_to = "usage", values_to = "volume") %>%
          ggplot(aes(x = period, y = volume, fill = usage)) +
          geom_bar(stat = "identity", position = "stack") +
          facet_wrap(account ~ .) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }
      
    })
    
    
    output$balance_sheet <- shiny::renderPlot({
      
      if (!is.null(values$simulation)){
        values$simulation$competition[[1]]$journal %>%
          dplyr::left_join(values$simulation$base_market$accounts, by = "account") %>%
          dplyr::left_join(values$simulation$base_market$market, by = "date") %>%
          dplyr::filter(!is.na(date), !is.na(period)) %>%
          tidyr::replace_na(list(debit = 0, credit = 0)) %>%
          dplyr::mutate(amount = dplyr::case_when(
            account_section == "assets" ~ debit - credit,
            TRUE ~ credit - debit
          )) %>%
          dplyr::filter(account_statement == "balance sheet") %>%
          dplyr::select(period, section = account_section, bloc = account_subcategory, account, amount) %>%
          dplyr::group_by(period, section, bloc) %>%
          dplyr::summarise(account = mean(account, na.rm = TRUE), amount = sum(amount, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(section, bloc, period) %>%
          dplyr::group_by(section, bloc) %>%
          dplyr::mutate(amount = cumsum(amount)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(bloc = as.factor(as.character(bloc))) %>%
          dplyr::mutate(bloc = forcats::fct_reorder(bloc, account)) %>%
          ggplot(aes(x = section, y = amount, fill = bloc)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_fill_brewer(type = "qual", palette = 2) +
          facet_wrap(period~.)
      }
      
    })
    
    #################
    # On exit
    
    observeEvent(input$done, {
     
       
     
      
      stopApp(values$simulation)
    })
  }
  
  runGadget(ui, server, viewer = browserViewer())
}
