module_0_Tool_TabUI <- function(id, GPScreen) {
  ns <- NS(id)
  tagList(
    #Title ----
    div(
      class = "central-column",
      div(
        hpReview("04 April 2025"),
        HTML("<h1>Local Bowel Screening Tool</h1>"),
        hr(style="width: 4rem;
                 height: 0.5rem;
                 color: black;
                 border-top: 0.5rem solid black;
                 opacity: 1"),
        # Top explainer text ----
        layout_columns(
          div(id = "Intro",
              p("This tool explores how socio-demographic factors may affect ", 
                bslib::tooltip(span("screening coverage", bs_icon("info-circle"), ".", 
                                    style = "color: #00007e;", .noWS = "inside"), 
                               "Coverage is the proportion of the eligible population that is screened and has a result documented."), 
                "It compares estimates for coverage based on local socio-demographic factors against actual screening coverage for GP practice areas across England.", .noWS = c("before", "after", "inside")),
              includeMarkdown("www/Overview.Rmd")
          )
        )
      )
    ),
    br(),
    # ICB Overview ----
    ## Header
    card(
      card_header(
        id = 'icb-header',
        fillable = FALSE,
        style = 'background-color: #ffffff',
        div(
          id = 'inline-icb',
          style = 'display: flex; align-items: left; gap: 10px; width: 100%; flex-wrap: wrap;', 
          h2('ICB Focus', style = 'margin: 0; flex-shrink: 0; padding-top: 5px'),
          div(class = "ICB_select",
              style = 'flex-grow: 1; min-width: 200px;',
              pickerInput(
                inputId = ns('select_ICB'),
                label = '',
                width = '100%',
                choices = unique(GPScreen$ICB),
                options = pickerOptions(
                  placeholder = "Select an ICB to get started",
                  onInitialize = I('function() { this.setValue(" "); }'),
                  `live-search` = TRUE,
                  liveSearchPlaceholder = "Search...",
                  selectOnTab = TRUE,
                  dropupAuto = FALSE,
                  style = "ICB_dropdown" #for some reason this applies a css class, not the css style
                )
              )
          )
        )
      ),
      ## Main body ----
      ## higher and lower practice boxes/tables ----
      layout_column_wrap(
        fillable = FALSE,
        width = 1/2,
        accordion(open = FALSE,
                  accordion_panel(title = span(style = 'font-family: Progress Medium;', 
                                               uiOutput(ns("LowerNo"))),
                                  icon = icon("arrow-down",
                                              class = "color: 'black';",
                                              style="font-size: 24px"),
                                  value = "lower",
                                  HTML("<i>Click a row to highlight a practice in the chart and GP focus section</i>"),
                                  DTOutput(ns("LowerTable")),
                                  textOutput(ns('LowerTableSelection'))
                  )
        ),
        accordion(open = FALSE,
                  accordion_panel(title = span(style = 'font-family: Progress Medium;',
                                               uiOutput(ns("HigherNo"))),
                                  icon = icon("arrow-up",
                                              class = "color: 'black';",
                                              style= "font-size: 24px"),
                                  value = "higher",
                                  HTML("<i>Click a row to highlight a practice in the chart and GP focus section</i>"),
                                  div(class = "higher-table",
                                      DTOutput(ns("HigherTable")))
                  )
        )
      ),
      
      ## Charts ----
      ### Coverage chart ----
      card(
        class = 'borderless_card',
        layout_sidebar(
          sidebar = sidebar(
            position = 'right',
            open = 'always',
            width = '200px',
            radioGroupButtons(
              inputId = ns('CoverageView'),
              label = 'Order graph by: ',
              choices = c("Coverage", "Predicted coverage"),
              direction = 'vertical'
            ),
            div(class = "PCN-select",
                pickerInput(
                  inputId = ns('select_PCN'),
                  label = 'Highlight a PCN',
                  width = '99%',
                  choices = "None",
                  options = list(
                    `live-search` = TRUE,  # Enables search box
                    `live-search-placeholder` = "Search...",
                    selectOnTab = TRUE
                  )
                ),
                actionButton(ns("reset_PCN"), "Clear PCN highlight",
                             style = "float: right; 
                                       margin-top: 5px; 
                                       font-size: 12px; 
                                       background: #ff0087; 
                                       color: white"
                )
            )
            
            #Output from coverage graph click event ----
            #Disabled for now but leaving in for future debugging
            # renderPrint(ns('PlotlyClick'))
          ),
          # plotlyOutput(ns("CoveragePlot"), height = '450px')
          uiOutput(ns("CoveragePlotUI")) 
        )
      ),
      
      ### SSR Chart ----
      #### Explainer ----
      layout_columns(
        col_widths = c(-1, 10, -1),
        div(HTML("<p>
         This next chart uses a "), 
         bslib::tooltip(
           span(style = "color: #00009e;",
                "standardised screening ratio",
                bs_icon("info-circle")
           ),
           "the observed number of people screened divided by the predicted number of people screened",
           placement = "top"
         ),
         HTML(
           " to allow for a comparison between each practice area in the selected ICB 
      (that has available data) and their bowel cancer screening coverage in respect
      to their local context. </br></br>
      If you select a particular GP practice area (by clicking one of the bars), the same practice area will be highlighted on both graphs.</br></br>
      This view is useful to understand how big the difference is between our model's prediction and the observed screening levels. It can help highlight practice areas that might be struggling or might be employing best practice, but still might not have particularly low or high screening coverage. Also, because the ratio is standardised, it can allow for comparison between practice areas (in relation to how high/low screening coverage is compared to our prediction).
      </p>
      ")
        )
      ),
      #### Graph ----
      card(
        class = 'borderless_card',
        layout_sidebar(
          sidebar = sidebar(
            position = 'right',
            open = 'always',
            width = '200px',
            radioGroupButtons(
              inputId = ns('ICBView'),
              label = 'Order graph by: ',
              choices = c("Difference to prediction", "Index of Multiple Deprivation"),
              direction = 'vertical'
            )
          ),
          uiOutput(ns("SSRPlotUI")) 
        )
      ),
      ## Standardised ratio explainer ----
      accordion(open = FALSE,
                accordion_panel(value = "SignExplainerAcc",
                                icon = icon("circle-question",  style = "font-weight: bold;"
                                ),
                                title = HTML("<span style = 'font-family: Progress Medium;'>
                                           Understanding the standardised screening ratio
                                           </span>"),
                                HTML("<p>
                                   The standardised screening ratio is: "),
                                withMathJax("$$
                                          \\frac{observed\\ number\\ of\\ people\\ screened}
                                                {predicted\\ number\\ of\\ people\\ screened}
                                          $$"),
                                HTML("If a practice screens the same number of people that our model predicts, 
                                   then the standardised screening ratio would be 1. In the above graph we 
                                   are showing the difference between the standardised screening ratio and 
                                   1. This is to highlight how different the number of people screened in each 
                                   practice is compared to the models' prediction.
                                   </br></br> 
                                   Practices that are:
                                   </p>
                                    <ul><li>
                                    <p>
                                    <span style='color: #009cee;'><strong>
                                    Significantly higher
                                    </strong></span>
                                    have screened a statistically significant number of people 
                                    <span style='color: #009cee;'><strong>
                                    more
                                    </strong></span>
                                    than our model predicts.
                                    This is calculated based on:
                                    </p>"),
                                withMathJax("$$\\frac{Lower\\ confidence\\ interval\\ for\\ the\\ number\\ screened}
                                                    {Upper\\ confidence\\ interval\\ for\\ the\\ predicted\\ number\\ screened}
                                                > 1
                                          $$"),
                                HTML("<li><p><span style='color: #ff0087;'><strong>
                                    Significantly lower
                                    </strong></span>
                                    have screened a statistically significant number of people 
                                    <span style='color: #ff0087;'><strong>
                                    less
                                    </strong></span> 
                                    than predicted.
                                    This is calculated based on:
                                    $$
                                      \\frac{Upper\\ confidence\\ interval\\ for\\ the\\ number\\ screened}
                                            {Lower\\ confidence\\ interval\\ for\\ the\\ predicted\\ number\\ screened}
                                      < 1
                                    $$
                                    </p>"
                                )
                ))
    ),
    
    br(),
    
    # GP focus ----
    card(
      card_header(
        id = 'gp-header',
        fillable = FALSE,
        style = 'background-color: #ffffff',
        div(
          id = 'inline-gp',
          style = 'display: flex; align-items: left; gap: 10px; width: 100%; flex-wrap: wrap;',
          h2('GP Focus', style = 'margin: 0; flex-shrink: 0; padding-top: 5px;'),
          div(class = "GP_select",
              style = 'flex-grow: 1; min-width: 200px;',
              uiOutput(ns("GPSelection"))
          )
        )
      ),
      uiOutput(ns('GPPlotUI')) 
    ),
    br(),
    
    #Further resources ----
    div(
      class = "central-column",
      div(),
      div(
        id = "FurtherResources",
        class = "FurtherResources",
        box(width = 12, solidHeader = TRUE,
            h2("Further Information and Resources"),
            uiOutput(ns("Resources"))
        )
      ),
      div()
    ),
    br(), 
  )
  
}

module_0_Tool_TabServer <- function(id, GPScreen, DemoFactors, parent) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      # Create ICB specific data frame ----
      selectedICB <- reactive({
        GPScreen %>% 
          filter(ICB == input$select_ICB) %>%
          arrange(SSRatio) %>%
          mutate(SSRfrom1 = SSRatio - 1) 
      })  
      
      # Build GP selection box ----
      output$GPSelection <- renderUI({
        pickerInput(
          inputId = ns('select_GP'),
          label = '',
          width = '100%',
          choices = #c(#"Not selected", 
            unique(selectedICB()$NameCode)#)
          ,
          options = pickerOptions(
            placeholder = "Select a GP practice area",
            onInitialize = I('function() { this.setValue(" "); }'),
            `live-search` = TRUE,
            liveSearchPlaceholder = "Search...",
            selectOnTab = TRUE,
            dropupAuto = FALSE,
            style = "GP_dropdown" #for some reason this applies a css class, not the css style
          )
        )
      })
      
      # Number of GPs lower ----
      output$LowerNo <- renderText({
        if(sum(selectedICB()$signif_change == "lower") == 1) {
          paste0(sum(selectedICB()$signif_change == "lower"), 
                 " practice area performing 
                 <span style = 'color: #ff0087;'>
                 significantly lower
                 </span> 
                 than predicted")
        } else {
          paste0(sum(selectedICB()$signif_change == "lower"), 
                 " practice areas performing 
                 <span style = 'color: #ff0087;'>
                 significantly lower
                 </span> 
                 than predicted")
        }
      })
      
      # Number of GPs higher ----
      output$HigherNo <- renderText({
        if(sum(selectedICB()$signif_change == "higher") == 1) {
          paste0(sum(selectedICB()$signif_change == "higher"), 
                 " practice area performing 
                 <span style = 'color: #009cee;'>
                 significantly higher
                 </span>
                 than predicted")  
        } else {
          paste0(sum(selectedICB()$signif_change == "higher"), 
                 " practice areas performing 
                 <span style = 'color: #009cee;'>
                 significantly higher
                 </span>
                 than predicted")
        }
        
        
      })
      
      observeEvent(input$select_ICB, {
        updatePickerInput(
          session = session,
          inputId = "select_PCN",
          # label = paste("Select", input$category),
          choices = c("None", unique(selectedICB()$PCNName))
        )
      })
      
      # Observer for the reset highlight button
      observeEvent(input$reset_PCN, {
        updatePickerInput(
          session = session,
          inputId = "select_PCN",
          selected = "None" 
        )
      })
      
      #Store PCN name as reactive
      chosenPCN <- reactive({
        input$select_PCN
      })
      
      # Coverage graph ----
      output$CoveragePlot <- renderPlotly({
        req(input$select_ICB) 
        
        chart <- Coverage_graph(selectedICB(), order = input$CoverageView, chosenGP = chosenGP(), chosenPCN = chosenPCN())
        
        chart
      })
      
      output$CoveragePlotUI <- renderUI({
        if (is.null(input$select_ICB) | input$select_ICB == "") { 
          div(
            div(style = "background-color: #f9f9f9; min-height: 55px; display: flex; padding: 16px; margin: 10px; margin-bottom: 15px;",
                p("Select an ICB in the dropdown above to view a comparison of coverage and predicted coverage", 
                  style = "font-family: Poppins Medium")),
            div(style = "background-color: #f9f9f9; height: 400px; display: flex; padding: 16px; margin: 10px;")
          ) 
        } else {
          plotlyOutput(ns('CoveragePlot')) 
        }
      })
      
      ## Coverage click observer ----
      # Update the selected GP when the bars of the coverage graph are clicked
      observeEvent(event_data("plotly_click", source = "coverage_click"),
                   {
                     event.data <- event_data("plotly_click", source = "coverage_click")
                     
                     # if(is.null(event.data) == T) return(NULL)
                     if (input$CoverageView == 'Coverage') {
                       info <- selectedICB() %>%
                         mutate(Coverage = NumberScreened/NumberEligible) %>%
                         arrange(Coverage)
                       Id <- as.numeric(event.data[3])
                       new_selection <- info[Id,]$NameCode
                     } else if (input$CoverageView == 'Predicted coverage') {
                       info <- selectedICB() %>%
                         mutate(Coverage = NumberScreened/NumberEligible) %>%
                         arrange(PredictedCoverage)
                       Id <- as.numeric(event.data[3])
                       new_selection <- info[Id,]$NameCode
                     }
                     
                     #info to console for debugging 
                     print("Coverage graph info: ")
                     print(event.data)
                     print(paste0(info[Id,]$NameCode))
                     
                     updatePickerInput(session, "select_GP",
                                       choices = sort(unique(selectedICB()$NameCode)),
                                       selected = new_selection)
                   })
      
      ## Output from coverage graph click event ----
      # #Disabled for now but leaving in for future debugging
      # output$PlotlyClick <- renderPrint({
      #   # event.data <-
      #     event_data("plotly_click", source = "coverage_click")
      #   # event.data
      #   # if(is.null(event.data) == T) return(NULL)
      #   #
      #   # info <- selectedICB() %>%
      #   #   mutate(Coverage = NumberScreened/NumberEligible) %>%
      #   #   arrange(Coverage)
      #   # Id <- as.numeric(event.data[3])
      #   # paste0(info[Id,]$NameCode)
      # })
      
      # observeEvent(event_data("plotly_click", source = "ratio_click"), {
      #   event.data <- event_data("plotly_click", source = "ratio_click")
      #   
      #   # if(is.null(event.data) == T) return(NULL)
      #   
      #   if (input$ICBView == 'Difference to prediction'){
      #     info <- selectedICB() %>% 
      #       arrange(SSRfrom1) 
      #     Id <- as.numeric(event.data[1, 3])
      #   } else if (input$ICBView == 'Index of Multiple Deprivation') {
      #     info <- selectedICB() %>% 
      #       arrange(IMD_quintile, SSRfrom1) 
      #     Id <- as.numeric(event.data[1, 3])
      #   }
      #   
      # })
      # 
      # observeEvent(event_data("plotly_click", source = "ratio_click"), {
      #   event.data <- event_data("plotly_click", source = "ratio_click")
      # 
      #   if(is.null(event.data) == T) return(NULL)
      # 
      #   info <- selectedICB() %>%
      #     mutate(Coverage = NumberScreened/NumberEligible) %>%
      #     arrange(Coverage)
      #   Id <- as.numeric(event.data[1, 3])
      #   print(Id)
      # })
      
      # ICB overview graph (plotly edition) ----
      output$SSRPlot <- renderPlotly({
        req(input$select_ICB)
        
        ICB_graph(ICBData = selectedICB(), 
                  chosenGP = chosenGP(), 
                  order = input$ICBView, 
                  chosenPCN = chosenPCN())
      })
      
      output$SSRPlotUI <- renderUI({
        if (is.null(input$select_ICB) | input$select_ICB == "") { 
          div(
            div(style = "background-color: #f9f9f9; min-height: 55px; display: flex; padding: 16px; margin: 10px; margin-bottom: 15px;",
                p("Select an ICB in the dropdown above to view graph of the standardised screening ratio", 
                  style = "font-family: Poppins Medium")),
            div(style = "background-color: #f9f9f9; height: 400px; display: flex; padding: 16px; margin: 10px;")
          ) 
        } else {
          plotlyOutput(ns('SSRPlot'))
        }
      })
      
      ## Screening ratio click observer ----
      # Update the selected GP when the bars of the coverage graph are clicked
      observeEvent(event_data("plotly_click", source = "ratio_click"),
                   {
                     event.data <- event_data("plotly_click", source = "ratio_click")
                     
                     # if(is.null(event.data) == T) return(NULL)
                     if (input$ICBView == 'Difference to prediction'){
                       info <- selectedICB() %>% 
                         arrange(SSRfrom1) 
                       Id <- as.numeric(event.data[1, 3])
                       new_selection <- paste0(info[Id,]$NameCode)
                     } else if (input$ICBView == 'Index of Multiple Deprivation') {
                       info <- selectedICB() %>% 
                         arrange(IMD_quintile, SSRfrom1) 
                       Id <- as.numeric(event.data[1, 3])
                       new_selection <- info[Id,]$NameCode
                     }
                     
                     #Info to console for debugging
                     print("SSR graph info: ")
                     print(event.data)
                     print(paste0(info[Id,]$NameCode))
                     
                     updatePickerInput(session, "select_GP",
                                       choices = unique(selectedICB()$NameCode),
                                       selected = new_selection)
                     
                   }) 
      
      
      
      
      
      # Lower sig table ----
      output$LowerTable <- renderDT({
        selectedICB() %>%
          filter(signif_change == "lower")%>%
          arrange(SSRfrom1) %>%
          select(NameCode, OrganisationName) %>%
          datatable(options = list(dom = 'tp',
                                   ordering = FALSE,
                                   columnDefs = list(list(visible = FALSE,
                                                          targets = 'NameCode'))),
                    selection = 'single',
                    rownames= FALSE,
                    class = list(stripe = FALSE, hover = TRUE, compact = TRUE),
                    colnames=c("GP Practice")
          )
      })
      
      ## Lower table observer ----
      #Update the selected GP when the lower table row is clicked
      observeEvent(input$LowerTable_rows_selected,
                   {
                     selected <- selectedICB() %>%
                       filter(signif_change == "lower")%>%
                       arrange(SSRfrom1)
                     
                     cat('\nLower table input return value', input$LowerTable_rows_selected)
                     
                     selected <- selected[input$LowerTable_rows_selected,]$NameCode
                     
                     cat('\nLower table input return name code:', selected)
                     updateSelectInput(session, "select_GP",
                                       choices = unique(selectedICB()$NameCode),
                                       selected = as.character(selected))
                   })
      
      #Output to double check what the lower table click input is returning
      # output$LowerTableSelection <- renderText({
      #   selected <- selectedICB() %>%
      #     filter(signif_change == "lower")%>%
      #     arrange(SSRfrom1)
      #   selected <- selected[input$LowerTable_rows_selected,]$NameCode
      #   cat('\nLower table input return value', input$LowerTable_rows_selected)
      # })
      
      # Higher table ----
      output$HigherTable <- renderDT({
        selectedICB() %>%
          filter(signif_change == "higher")%>%
          arrange(SSRfrom1) %>%
          select(NameCode, OrganisationName) %>%
          datatable(options = list(dom = 'tp',
                                   ordering = FALSE,
                                   columnDefs = list(list(visible = FALSE,
                                                          targets = 'NameCode'))),
                    selection = 'single',
                    rownames= FALSE,
                    class = list(stripe = FALSE, hover = TRUE, compact = TRUE),
                    colnames=c("GP Practice")
          )
      })
      
      #Update the selected GP when the higher table row is clicked 
      observeEvent(input$HigherTable_rows_selected,
                   {
                     selected <- selectedICB() %>%
                       filter(signif_change == "higher")%>%
                       arrange(SSRfrom1)
                     selected <- selected[input$HigherTable_rows_selected,]$NameCode
                     updateSelectInput(session, "select_GP",
                                       choices = unique(selectedICB()$NameCode),
                                       selected = selected)
                   })
      
      # Get chosen GP Organisation Code ----
      chosenGP <- reactive({
        str_split_i(input$select_GP,  "[\\[\\]]", 2)
      })
      
      # Get organisation name based on organisation code ----
      chosenGPName <- reactive({
        filter(GPScreen, OrganisationCode == chosenGP())$OrganisationName
      })
      
      # Filter data to selected GP ----
      GPData <- reactive({
        selectedICB() %>%
          filter(OrganisationCode == chosenGP())
      })
      
      # Text about GP ----
      output$DiffStatement <- renderText({
        validate(need(input$select_GP, 'Select an ICB and GP on the left first'))
        
        GPText(GPData())
      })
      
      # Create table ----
      output$PredTable <- render_gt({
        validate(need(chosenGP(), ''))
        
        GP_table(GPData(), DemoFactors)
      })
      
      # Create text highlighting high impact factors
      output$FactorHighlight <- renderUI({
        validate(need(chosenGP(), ''))
        
        FactorStatement(GPData(), DemoFactors)
      })
      
      # GP graph ----
      output$GPPlot <- renderPlotly({
        req(chosenGP())
        # validate(need(chosenGP(), ""))
        
        GP_graph(GPData())
      })
      
      output$GPPlotUI <- renderUI({
        if (is.null(input$select_GP) | input$select_GP == "") { 
          layout_columns(
            col_widths = c(8, 4),
            div(
              div(style = "background-color: #f9f9f9; min-height: 55px; display: flex; padding: 16px; margin: 10px 5px 20px 10px",
                  p("Select a GP practice area in the dropdown above to view detailed information", 
                    style = "font-family: Poppins Medium")),
              div(style = "background-color: #f9f9f9; height: 400px; display: flex; padding: 16px; margin: 10px 5px 10px 10px;")
            ),
            div(style = "height: 96%; min-height: 400px; background-color: #f9f9f9; display: flex; padding: 16px; margin: 10px 10px 20px 5px")
          )
          
          
          
        } else {
          layout_columns(
            col_widths = c(8, 4),
            card(
              class = 'borderless_card',
              uiOutput(ns("DiffStatement")), #%>%
                
              gt_output(ns("PredTable")) 
              # uiOutput(ns("FactorHighlight"))
            ), 
            card(
              class = 'borderless_card',
              plotlyOutput(ns("GPPlot"), height = '600px') 
            )
          )
        }
      })
      
      #Further resources text ----
      output$Resources <- renderUI({
        includeMarkdown("www/Further Resources.Rmd")
      })
      
      output$download_button  <- downloadHandler(
        filename = function() {
          paste("CRUK GP Bowel Screening Tool ", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(GPScreen, file)
        }
      )
    }
  )
  
}