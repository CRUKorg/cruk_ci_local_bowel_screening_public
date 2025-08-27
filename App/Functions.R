# Data building ----
#source("Data for running functions offline.R")

# Ui Styling Functions
# box with "health professional and date of last review
hpReview <- function(last_review_date) {
  div(
    span("Health professionals", 
         style = "background: #e2f4fd; 
                  color: #0076b4; 
                  padding: 8px 12px 8px 12px;"),
    div(style = "flex: 1; 
                   align-self: stretch;"),
    span(paste0("Last reviewed: ", last_review_date),
         style = "color: #666666;
                    padding: 8px 12px 8px 12px;"),
    style = "margin-bottom: 8px; 
               display: flex;")
}

onThisPage <- function(...) {
  # Process arguments
  args <- list(...)
  
  # Check if arguments are provided as separate pairs or as lists
  if (length(args) == 2 && is.atomic(args[[1]]) && is.atomic(args[[2]])) {
    # Single pair case: div_id, link_text as separate arguments
    div_ids <- args[[1]]
    link_texts <- args[[2]]
  } else {
    # Multiple pairs case: c(div_id1, link_text1), c(div_id2, link_text2), ...
    div_ids <- sapply(args, function(x) x[1])
    link_texts <- sapply(args, function(x) x[2])
  }
  
  # Ensure div_ids and link_texts are the same length
  if (length(div_ids) != length(link_texts)) {
    stop("Number of div_ids must match number of link_texts")
  }
  
  # Create links for each pair
  links <- lapply(1:length(div_ids), function(i) {
    # Pre-append # to div_id if it doesn't already start with #
    href_id <- ifelse(!startsWith(div_ids[i], "#"), paste0("#", div_ids[i]), div_ids[i])
    
    div(
      HTML(paste0(
        span(class = "material-icons", 
             style = "vertical-align: middle; color: #00007e", 
             "arrow_downward"),
        a(href = href_id, link_texts[i], 
          style = "color: #00007e !important; text-decoration: underline !important; font-weight: 500; font-size: 1.2rem; margin-top: 0.5rem;")
      )),
      
    )
  })
  
  # Build the main container with all links
  div(
    style = "background: #f0f0ff; padding:1.5rem; margin: 0;",
    tags$link(href = "https://fonts.googleapis.com/icon?family=Material+Icons", rel = "stylesheet"),
    div(
      style = "font-size: 1.5rem; padding-bottom: 0.8rem; margin: 0px; font-family: 'Progress Medium';",
      "On this page"
    ),
    do.call(tagList, links)
  )
}

crukTitleHR <- function(Title) {
  div(h1(Title),
      hr(style = "width: 4rem;
                  height: 0.5rem;
                  color: black;
                  border-top: 0.5rem solid black;
                  opacity: 1"))
}

# # CRUK Footer -----
# crukFooter <- function() {
#   div(class = "responsive-grid",
#       hr(style = "opacity: 1;
#                   margin-right: calc(50% - 50vw) !important;
#                   border: 0;
#                   border-color: #000000;
#                   border-style: solid;
#                   border-bottom-width: 0.25rem;
#                   margin: 0px;
#                   width: auto;"),
#       h3("Together we are", br(), 
#          " beating cancer",
#          style = "padding-top: 16px;"),
#       a(
#         img(src = "logo-fundraising-regulator-reg.svg", 
#             height = "97px", 
#             style = "margin-right: 10px; height: 51px; margin-top: 16px; margin-bottom: 16px;"), 
#         href = "https://www.fundraisingregulator.org.uk/"),
#       tags$address("Cancer Research UK is a registered charity in England and Wales (1089464), Scotland (SC041666), the Isle of Man (1103) and Jersey (247). A company limited by guarantee. Registered company in England and Wales (4325234) and the Isle of Man (5713F). Registered address: 2 Redman Place, London, E20 1JQ.")
#   )
# }

# Graph Functions ----
configPreference <- list(
  'sendDataToCloud',
  'autoScale2d',
  #'resetScale2d',
  'hoverClosestCartesian',
  'hoverCompareCartesian',
  'zoom2d',
  'pan2d',
  'select2d',
  'lasso2d',
  #'zoomIn2d',
  #'zoomOut2d',
  'toggleSpikelines'
)

crukLogoPreference <- list(
  source = "logoCRUK.jpg",
  x = 0.98,
  y = 1,
  sizex = 0.15,
  sizey = 0.15,
  xref = "paper",
  yref = "paper",
  xanchor = "left",
  yanchor = "bottom"
)

# toImageButtonOptions = list(
#   format = "png",
#   filename = "CWT_FDSTWW_Comparison",
#   width = customWidth,
#   height = customHeight
# )

# ICB functions  -------


#Coverage graph ----
Coverage_graph <- function(ICBData, order, chosenGP, chosenPCN) {
  # Set palette colours
  pal <- c("#ff0087", "#bbc7d9", "#009cee")
  
  # Set colour names, will appear on legend
  pal <- setNames(pal, c("lower", "similar", "higher"))
  
  ICBData <- ICBData %>%
    mutate(signif_change = factor(signif_change, 
                                  levels = c('higher', 'similar', 'lower'))) 
  
  # order data based on the selector
  if (order == 'Coverage') {
    ICBData <- ICBData %>% 
      arrange(Coverage) %>%
      mutate(Id = row_number()) 
  } else if (order == 'Predicted coverage') {
    ICBData <- ICBData %>%
      arrange(PredictedCoverage) %>%
      mutate(Id = row_number()) 
  }
  
  #Vary the size of the predicted coverage circle on the number of practices
  AreaCount <- n_distinct(ICBData$OrganisationCode)
  
  # SymbolSize <- case_when(AreaCount < 100 ~ 8,
  #                         AreaCount >= 100 & AreaCount < 200 ~ 6,
  #                         AreaCount >= 200 & AreaCount < 300 ~ 4,
  #                         AreaCount >= 300 ~ 2)
  
  base_size <- 5  # Base size for smaller datasets
  
  SymbolSize <- base_size * log(417 / AreaCount + 1) 
  
  #If a GP area is highlighted, don't plot it on the graph, otherwise plot all areas
  ChartData <- if (!is.null(chosenGP) && !is.na(chosenGP)) {
    ICBData %>%
      filter(OrganisationCode != chosenGP)
  } else {
    ICBData
  }
  
  # Get data for chosen PCN, and remove from the main chart data
  if (!is.null(chosenPCN)) {
    
    PCNData <- ICBData %>%
      filter(PCNName == chosenPCN)
    
    #If a GP area is highlighted, do not plot it with PCN data
    PCNData <- if (!is.null(chosenGP) && !is.na(chosenGP)) {
      PCNData %>%
        filter(OrganisationCode != chosenGP)
    } else {
      PCNData
    }
    
    ChartData <- ChartData %>%
      filter(PCNName != chosenPCN)
  }
  
  ## main chart code -----
  chart <- plot_ly(source = 'coverage_click') %>%
    #bars for coverage
    add_bars(data = ChartData,
             x = ~Id,
             y = ~Coverage,
             color = ~signif_change,
             colors = pal,
             opacity = if_else(chosenPCN == "None", 
                               1, 
                               0.3),
             width = switch(chosenPCN,
                            "None" = NULL,
                            0.3),
             hoverinfo = 'text',
             text = ~paste0('<b>',OrganisationName, '</b>',
                            '\n Coverage: ', round(Coverage * 100, 1), '%',
                            '\n Predicted coverage: ', round(PredictedCoverage * 100, 1), '%'),
             hoverlabel = list(font = list(family = 'Poppins', size = 14)),
             legendgroup = ~signif_change,
             showlegend = T) %>%
    #dots for predicted coverage
    add_markers(data = ChartData, 
                x = ~Id,
                y = ~PredictedCoverage,
                marker = list(color = '#ffffff',
                              size = SymbolSize,
                              # sizeref = 0.2,
                              line = list(color = '#000000', width = 1)),
                opacity = if_else(chosenPCN == "None", 
                                  1, 
                                  0.3),
                name = 'Predicted coverage',
                legendgroup = "Markers",
                hoverinfo = 'skip') 
  
  ## Highlighted PCN code ----
  if (!is.null(chosenPCN)) {
    
    chart <- chart %>%
      add_bars(data = PCNData,
               x = ~Id,
               y = ~Coverage,
               color = ~signif_change,
               colors = pal,
               # name = 'selected',
               hoverinfo = 'text',
               text = ~paste0('<b>', OrganisationName, '</b>',
                              '\n Coverage: ', round(Coverage * 100, 1), '%',
                              '\n Predicted coverage: ', round(PredictedCoverage * 100, 1), '%'),
               hoverlabel = list(font = list(family = 'Poppins', size = 14)),
               textposition = "none",
               legendgroup = ~signif_change,
               showlegend = F) %>%
      #dots for predicted coverage
      add_markers(data = PCNData, 
                  x = ~Id,
                  y = ~PredictedCoverage,
                  marker = list(color = '#ffffff',
                                size = SymbolSize,
                                line = list(color = '#000000', width = 1)),
                  name = 'Predicted coverage',
                  legendgroup = "Markers",
                  hoverinfo = 'skip',
                  showlegend = F) %>%
      add_markers(data = PCNData, 
                  x = ~Id,
                  # y = ~PredictedCoverage + 0.1,
                  # y = max(abs(ChartData$PredictedCoverage)) + 0.1,
                  y = 0.95,
                  marker = list(color = '#000000',
                                size = SymbolSize,
                                symbol = 'x'
                                # line = list(color = '#000000', width = 1)
                  ),
                  name = 'Area in PCN',
                  legendgroup = "PCN Highlight",
                  hoverinfo = 'skip',
                  showlegend = T) 
    
  }
  
  #selected GP code -----
  #If a GP practice area has been chose this will plot it with the black border
  if (!is.null(chosenGP) && !is.na(chosenGP)) {
    ChosenGPData <- ICBData %>%
      filter(OrganisationCode == chosenGP)
    
    chart <- chart %>%
      add_bars(data = ChosenGPData,
               x = ~Id,
               y = ~Coverage,
               marker = list(color = ~I(case_when(signif_change == 'lower' ~ '#ff0087',
                                                  signif_change == 'similar' ~ '#bbc7d9',
                                                  signif_change == 'higher' ~ '#009cee')),
                             line = list(color = '#000000',
                                         width = 1.5)),
               opacity = if_else(ICBData[ICBData$OrganisationCode == chosenGP,]$PCNName == chosenPCN 
                                 | chosenPCN == "None", 
                                 true = 1, false = 0.4), 
               hoverinfo = 'text',
               text = ~paste0('<b>',OrganisationName, '</b>',
                              '\n Coverage: ', round(Coverage * 100, 1), '%',
                              '\n Predicted coverage: ', round(PredictedCoverage * 100, 1), '%'),
               hoverlabel = list(font = list(family = 'Poppins', size = 14)),
               textposition = "none",
               name = 'Selected practice area',
               legendgroup = ~signif_change
      ) %>%
      #dots for predicted coverage
      add_markers(data = ChosenGPData, 
                  x = ~Id,
                  y = ~PredictedCoverage,
                  marker = list(color = '#ffffff',
                                size = SymbolSize,
                                line = list(color = '#000000', width = 1)),
                  opacity = if_else(ICBData[ICBData$OrganisationCode == chosenGP,]$PCNName == chosenPCN 
                                    | chosenPCN == "None", 
                                    true = 1, false = 0.4),
                  name = 'Selected practice area',
                  legendgroup = "Markers",
                  showlegend = F,
                  hoverinfo = 'skip'
      ) %>%
      add_annotations(data = ChosenGPData, 
                      x = ~Id,
                      yref = "paper",
                      y = -0.02,
                      ax = 0,
                      ay = 5,
                      arrowhead = 1,
                      text = "") 
    
    #if the selected GP is in the highlighted PCN, add on the x marker
    
    if (ChosenGPData$PCNName == chosenPCN) {
      chart <- chart %>%
        add_markers(data = ChosenGPData,
                    x = ~Id,
                    y = ~0.95,
                    marker = list(color = '#000000',
                                  size = SymbolSize,
                                  symbol = 'x'),
                    name = 'Area in PCN',
                    legendgroup = "PCN Highlight",
                    hoverinfo = 'skip',
                    showlegend = F) 
    }
    
  }
  
  #Chart formatting and layout ----
  
  chart <- chart %>%
    layout(
      yaxis = list(
        title = "Bowel Screening Coverage", 
        range = c(0,1),
        tickformat = "1%",
        zeroline = F,
        showgrid = T, 
        titlefont = list(family = "Poppins", color = "#000000", size = 18),
        tickfont = list(family = "Poppins", color= "#000000", size= 14)),
      xaxis = list(
        title = "", 
        tickvals = '', # make the x axis title empty
        range = c(0.5, max(ICBData$Id)+0.5), # needs to be max +0.5 otherwise the bar gets cut a little
        # rangeslider = list(visible = TRUE, 
        #                    range = c(0.5, max(ICBData$Id)+0.5),
        #                    thickness = 0.1,
        #                    showactive = FALSE),
        zeroline = F,
        tickfont = list(family = "Poppins", color= "#000000", size= 14),
        titlefont = list(family = "Poppins", color = "#000000", size = 18)),
      title = list(text = paste0("Comparing model prediction and actual screening coverage",
                                 "<sup><br>Mouseover individual bars to see practice area name, click to select for GP focus section below</sup>"),
                   font = list(family = "Progress Medium", color = "#000000",size = 22),
                   x = 0.01,
                   y = 0.92),
      margin = list(t = 100),
      legend = list(orientation = "h",
                    xanchor = "center",
                    x = 0.5),
      shapes = list(
        list(
          type = "line",
          y0 = ICBData$ICBCoverageAvg[1],
          y1 = ICBData$ICBCoverageAvg[1],
          yref = "paper",
          x0 = 0,
          x1 = max(ICBData$Id) + 0.5,
          line = list(color = '#00000077')
        )
      )
    )  %>%
    add_annotations(x = max(ICBData$Id) + 0.5,
                    y = ICBData$ICBCoverageAvg[1],
                    yshift = -2,
                    text = paste0("ICB average coverage: ", round(ICBData$ICBCoverageAvg[1]*100, 1), "%"),
                    font = list(size = 16, family = "Poppins"),
                    bgcolor = '#ffffff55',
                    showarrow = FALSE,
                    xanchor = "right", yanchor = "top") %>%
    config(modeBarButtonsToRemove = configPreference,
           displaylogo = FALSE,
           showAxisDragHandles = TRUE  
    ) 
  
  return(chart)
}

# Standardised screening ratio function -----
ICB_graph <- function(ICBData, order, chosenGP, chosenPCN) {
  # Set up data ----
  #The graph plots row ID on the x-axis to make it easier to set the order
  if (order == 'Difference to prediction') {
    ICBData <- ICBData %>% 
      arrange(SSRfrom1) %>%
      mutate(Id = row_number())  
  } else if (order == 'Index of Multiple Deprivation') {
    ICBData <- ICBData %>%
      arrange(IMD_quintile, SSRfrom1) %>%
      mutate(Id = row_number())
  }
  
  #If a GP area is highlighted, don't plot it on the graph, otherwise plot all areas
  ChartData <- if (!is.null(chosenGP) && !is.na(chosenGP)) {
    ICBData %>%
      filter(OrganisationCode != chosenGP)
  } else {
    ICBData
  }
  
  # Get data for chosen PCN, and remove from the main chart data
  if (!is.null(chosenPCN)) {
    
    PCNData <- ICBData %>%
      filter(PCNName == chosenPCN)
    
    #If a GP area is highlighted, do not plot it with PCN data
    PCNData <- if (!is.null(chosenGP) && !is.na(chosenGP)) {
      PCNData %>%
        filter(OrganisationCode != chosenGP)
    } else {
      PCNData
    }
    
    ChartData <- ChartData %>%
      filter(PCNName != chosenPCN)
  }
  
  #Set colours and assign them to variables
  pal <- c("#ff0087", "#bbc7d9", "#009cee")
  pal <- setNames(pal, c("lower", "similar", "higher"))
  
  
  #Vary the size of the predicted coverage circle on the number of practices
  AreaCount <- n_distinct(ICBData$OrganisationCode)
  
  # SymbolSize <- case_when(AreaCount < 100 ~ 8,
  #                         AreaCount >= 100 & AreaCount < 200 ~ 6,
  #                         AreaCount >= 200 & AreaCount < 300 ~ 4,
  #                         AreaCount >= 300 ~ 2)
  
  base_size <- 5  # Base size for smaller datasets
  
  SymbolSize <- base_size * log(417 / AreaCount + 1) 
  
  # Build main graph ----
  # the source argument sets up the listener for the chart
  chart <- ICBData %>%
    plot_ly(source = 'ratio_click') %>%
    add_bars(data = ChartData,
             x = ~Id,
             y = ~SSRfrom1,
             color = ~signif_change,
             colors = pal,
             #If PCN highlight is on, then fade out and get narrower
             opacity = if_else(chosenPCN == "None", 
                               1, 
                               0.3),
             width = switch(chosenPCN,
                            "None" = NULL,
                            0.3),
             hoverinfo = 'text',
             text = ~paste0('<b>',OrganisationName, '</b>',
                            '\n SSR: ', round(SSRatio, 2),
                            '\n Coverage: ', round(Coverage * 100, 1), '%',
                            '\n Predicted coverage: ', round(PredictedCoverage * 100, 1), '%'),
             hoverlabel = list(font = list(family = 'Poppins', size = 14)),
             textposition = 'none',
             legendgroup = ~signif_change,
             showlegend = T
    ) 
  
  #Highlight the PCN bars ----
  if (!is.null(chosenPCN)) {
    
    chart <- chart %>%
      add_bars(data = PCNData,
               x = ~Id,
               y = ~SSRfrom1,
               color = ~signif_change,
               colors = pal,
               #If PCN highlight is on, then fade out and get narrower
               hoverinfo = 'text',
               text = ~paste0('<b>',OrganisationName, '</b>',
                              '\n SSR: ', round(SSRatio, 2),
                              '\n Coverage: ', round(Coverage * 100, 1), '%',
                              '\n Predicted coverage: ', round(PredictedCoverage * 100, 1), '%'),
               hoverlabel = list(font = list(family = 'Poppins', size = 14)),
               textposition = 'none',
               legendgroup = ~signif_change,
               showlegend = F
      ) %>%
      add_markers(data = PCNData, 
                  x = ~Id,
                  y = max(abs(ChartData$SSRfrom1)),
                  marker = list(color = '#000000',
                                size = SymbolSize,
                                symbol = 'x'
                                # line = list(color = '#000000', width = 1)
                  ),
                  name = 'Area in PCN',
                  legendgroup = "PCN Highlight",
                  hoverinfo = 'skip',
                  showlegend = T) 
    
  }
  
  # Add gp practice highlight ----
  #If a GP practice area has been chose this will plot it with the black border
  if (!is.null(chosenGP) && !is.na(chosenGP)) {
    chosenGPData <- filter(ICBData, OrganisationCode == chosenGP)
    
    chart <- chart %>%
      add_bars(data = chosenGPData,
               x = ~Id,
               y = ~SSRfrom1,
               marker = list(color = ~I(case_when(signif_change == 'lower' ~ '#ff0087',
                                                  signif_change == 'similar' ~ '#bbc7d9',
                                                  signif_change == 'higher' ~ '#009cee')),
                             line = list(color = '#000000',
                                         width = 1.5)),
               opacity = if_else(ICBData[ICBData$OrganisationCode == chosenGP,]$PCNName == chosenPCN 
                                 | chosenPCN == "None", 
                                 true = 1, false = 0.4), 
               hoverinfo = 'text',
               text = ~paste0('<b>',OrganisationName, '</b>'),
               # '\n Coverage: ', round(Coverage * 100, 1), '%',
               # '\n Predicted coverage: ', round(PredictedCoverage * 100, 1), '%'),
               hoverlabel = list(font = list(family = 'Poppins', size = 14)),
               textposition = "none",
               legendgroup = ~signif_change,
               name = 'Selected practice area') %>%
      add_annotations(data = chosenGPData, 
                      x = ~Id,
                      yref = "paper",
                      y = -0.02,
                      ax = 0,
                      ay = 5,
                      arrowhead = 1,
                      text = "") 
    
    if (chosenGPData$PCNName == chosenPCN) {
      chart <- chart %>%
        add_markers(data = chosenGPData,
                    x = ~Id,
                    y = max(abs(ChartData$SSRfrom1)),
                    marker = list(color = '#000000',
                                  size = SymbolSize,
                                  symbol = 'x'),
                    name = 'Area in PCN',
                    legendgroup = "PCN Highlight",
                    hoverinfo = 'skip',
                    showlegend = F) 
    }
  }
  
  if (order == 'Index of Multiple Deprivation') {
    #initialise a list that will be used for the vertical lines
    lineList <- list()
    
    #Some ICBs don't have practice areas for every quintile so we need to only add the relevant quintiles on
    for (i in unique(ICBData$IMD_quintile)) {
      #find the row id of the highest SSR practice area in the given IMD quintile
      Max <- ICBData %>%
        filter(IMD_quintile == i) %>%
        filter(SSRfrom1 == max((SSRfrom1))) %>%
        pull(Id)
      
      #Add the relevant annotation
      chart <- chart %>%
        add_annotations(x = Max + 0.5, 
                        y = 0 - ceiling(max(abs(ICBData$SSRfrom1)) * 10)/10,
                        text = case_when(i == 1 ~ paste0("<b>↑</b> Quintile  1 ↑\n(most deprived)"),
                                         i == 2 ~ paste0("<b>↑</b> Quintile  2 ↑"),
                                         i == 3 ~ paste0("<b>↑</b> Quintile  3 ↑"),
                                         i == 4 ~ paste0("<b>↑</b> Quintile  4 ↑"),
                                         i == 5 ~ paste0("<b>↑</b> Quintile  5 ↑\n(least deprived)")),
                        textangle = 270,
                        font = list(size = 14, family = "Poppins"),
                        showarrow = FALSE,
                        xanchor = "right", yanchor = "bottom") 
      
      #add to the list that is called later
      lineList <- append(lineList, list(
        list(
          type = "line",
          y0 = 0,
          y1 = 1,
          yref = "paper",
          x0 = Max + 0.5,
          x1 = Max + 0.5,
          line = list(color = "#000000", dash = "dot"),
          name = i
        )
      ))
    }
    
    #remove the line for the fifth quintile if it exists
    filteredList <- lineList[!sapply(lineList, function(x) 5 %in% x)]
    
    #add the vertical lines to the chart
    chart <- chart %>%
      layout(shapes = filteredList) 
  }
  
  # Chart formatting ---- 
  chart <- chart %>%
    layout(
      yaxis = list(
        title = paste0("Standardised screening ratio\n(difference from 1)"), 
        range = c(min = 0 - ceiling(max(abs(ICBData$SSRfrom1)) * 10)/10, 
                  max = 0 + ceiling(max(abs(ICBData$SSRfrom1)) * 10)/10),
        zeroline = F,
        showgrid = T, 
        titlefont = list(family = "Poppins", color = "#000000", size = 18),
        tickfont = list(family = "Poppins", color= "#000000", size= 14)),
      xaxis = list(
        title = "", 
        tickvals = '', #make the x axis empty
        range = c(0.5, max(ICBData$Id)+0.5), #needs to be +0.5 for max otherwise the bar gets cut a little
        zeroline = F,
        tickfont = list(family = "Poppins", color= "#000000", size= 14),
        titlefont = list(family = "Poppins", color = "#000000", size = 18)),
      title = list(text = paste0("Comparing across practice areas with the standardised screening ratio",
                                 "<sup><br>Mouseover individual bars to see practice area name, click to select for GP focus section below</sup>"),
                   font = list(family = "Progress Medium", color = "#000000",size = 22),
                   x = 0.01,
                   y = 0.92),
      margin = list(t = 100),
      legend = list(orientation = "h",
                    xanchor = "center",
                    x = 0.5),
      hovermode = "x"
    ) %>%
    
    config(modeBarButtonsToRemove = configPreference,
           displaylogo = FALSE)
  
  chart
}

# GP functions ----

GPText <- function(GPData) {
  HTML(paste0("<p> 
                The bowel screening coverage for this practice area is ", 
              case_when(GPData$signif_change == "similar" ~ paste0("<strong>", GPData$signif_change, "</strong>", " to"), 
                        GPData$signif_change == "higher" ~ paste0("<span style = 'color: #009cee;'><strong> significantly ", GPData$signif_change, "</strong></span> than"), 
                        GPData$signif_change == "lower" ~ paste0("<span style = 'color: #ff0087;'><strong> significantly ", GPData$signif_change, "</strong></span> than")), 
              " our model's prediction for this practice area. In other words, 
                similar patient populations to this practice area would usually have 
                a ", case_when(GPData$signif_change == "similar" ~ paste0("similar"), 
                               GPData$signif_change == "higher" ~ paste0("lower"), 
                               GPData$signif_change == "lower" ~ paste0("higher")), 
              " bowel screening coverage.
                </p>
                <p>Out of <b>", format(GPData$NumberEligible, big.mark = ","), "</b> people eligible for screening, <strong>", 
              format(GPData$NumberScreened, big.mark = ","), 
              " (", round(GPData$NumberScreened / GPData$NumberEligible * 100, 1), "%)",
              "</strong> people were screened while our model estimated <strong>", 
              format(round(GPData$Expected), big.mark = ","), 
              " (", round(GPData$Expected / GPData$NumberEligible * 100, 1), "%)",
              "</strong>, a difference of <strong>", abs(GPData$NumberScreened - GPData$Expected), " people.</strong></p>"))
}

GP_graph <- function(GPData) {
  bar_colour <- case_when(GPData$signif_change == "higher" ~ "#009cee",
                          GPData$signif_change == "lower" ~ "#ff0087",
                          GPData$signif_change == "similar" ~ "#bbc7d9")
  error_bar_colour <- case_when(GPData$signif_change == "higher" ~ "#5bc5f2", #"#009cee",
                                GPData$signif_change == "lower" ~ "#ff99cf", #"#878787",
                                GPData$signif_change == "similar" ~ "#878787" #"#bbc7d9"
  )
  
  plot_ly(GPData) %>%
    #Expected
    add_trace(x = ~OrganisationName, y = ~Expected, 
              type = "scatter", mode = "markers", 
              marker = list(color = c("#00009e"),
                            symbol = "141",
                            size = 2),
              error_y = ~list(array = Expected_UpperCI-Expected, 
                              arrayminus = Expected - Expected_LowerCI,
                              color = "#6666b2",
                              thickness = 2,
                              width = 10),
              name = ~paste0("Predicted: ", Expected, " (95% CI: ", round(Expected_LowerCI, 0), " - ", round(Expected_UpperCI, 0), ")"),
              textposition = "none",
              hoverinfo = "none") %>%
    #Screened
    add_bars(x = ~OrganisationName, y = ~NumberScreened, 
             marker = list(color = bar_colour),
             error_y = ~list(array = NumberScreened_UpperCI-NumberScreened, 
                             arrayminus = NumberScreened - NumberScreened_LowerCI,
                             color = error_bar_colour,
                             thickness = 2,
                             width = 10),
             name = ~paste0("Screened: ", NumberScreened, " (95% CI: ", round(NumberScreened_LowerCI, 0), " - ", round(NumberScreened_UpperCI, 0), ")"),
             textposition = "none",
             hoverinfo = "none") %>%
    #Eligibile
    add_bars(x = ~OrganisationName, y = ~NumberEligible, 
             marker = list(line = list(color = '#000',
                                       width = 1.5),
                           color = "#f7f7f700"),
             name = ~paste0("Eligible: ", NumberEligible),
             textposition = "none",
             hoverinfo = "none"
    ) %>%
    layout(
      #Draw a horizontal line for number of predicted screened
      legend = list (itemsizing = "constant", orientation = 'h'),
      shapes = list(line = list(color = "#00009e"),
                    x0 = -0.1, x1 = 0.1, 
                    y0 = ~Expected, y1 = ~Expected),
      #Set bars to overlap
      barmode = "overlay",
      showlegend = T, 
      yaxis = list(
        title = "Number of people", #change to correct title
        zeroline = F,
        showgrid = F, 
        titlefont = list(family = "Poppins", color = "#000000", size = 18),
        tickfont = list(family = "Poppins", color= "#000000", size= 14)),
      xaxis = list(
        title = "", #change to correct title
        zeroline = T,
        showticklabels = F
      ), #set these values to your x axis values
      title = list(text = ~OrganisationName, #change to correct title
                   font = list(family = "Progress Medium", color = "#000000",size = 28),
                   x = 0.01),
      margin = list(t = 75) #Set margin sizes with t, b, r, l. Wide r margin is for annotations
    ) %>%
    config(modeBarButtonsToRemove = configPreference,
           displaylogo = FALSE)
  
}


GP_table <- function(GPData, DemoFactors) {
  TableData <- GPData %>%
    mutate(GenderSplit = paste0("Male: ", round(Prop_M_6074), "% 
                              </br>Female: ", 100 - round(Prop_M_6074), "%"),
           prop_6074 = paste0(round(prop_6074), "%"),
           gp_listsize = format(gp_listsize, big.mark=","),
           ethnic_whitebrit_perc = paste0(round(ethnic_whitebrit_perc), "%"),
           IMD = md(paste0(IMD_quintile, 
                           "</br>(IMD score ", round(IMD), ")"))) %>%
    select(gp_listsize, prop_6074, GenderSplit, 
           ethnic_whitebrit_perc, IMD, ruc11desc) %>%
    rename("Practice list size" = gp_listsize,
           "Practice area population aged 60-74 year olds" = prop_6074,
           "Proportion of 60-74 year olds that are male or female" = GenderSplit,
           "Proportion of local area residents who are White British" = ethnic_whitebrit_perc,
           "Deprivation quintile (1 = most deprived, 5 = least deprived)" = IMD,
           "Practice area's rural/urban classification" = ruc11desc) %>%
    pivot_longer(cols = everything(), 
                 names_to = "Predictors", 
                 values_to = "Value",
                 values_transform = list("Value" = as.character))
  
  
  
  # DemoData <- DemoFactors %>%
  #   # janitor::clean_names() %>%
  #   filter(OrgCode == GPData$OrganisationCode) %>%
  #   select(
  #     names(.)[!grepl("ru11", names(.)) | sapply(., function(x) !(is.numeric(x) && all(x == 0)))],
  #     -OrgCode, -OrgName, -ICB, -intercept
  #   )
  # 
  # #if there is a rural urban column (i.e. this gp does not have the reference value for ru11)
  #  if (any(grepl("ru11",colnames(DemoData)))) {
  #    #Get that column name
  #   ruralUrbanCol <- colnames(DemoData)[grepl("ru11",colnames(DemoData))]
  #   DemoData <- DemoData %>%
  #     rename("Practice's rural/urban classification" = ruralUrbanCol)
  #  } else {
  #    DemoData <- DemoData %>%
  #      mutate(`Practice's rural/urban classification` = 0)
  #  }
  # 
  # DemoData <- DemoData %>%
  #   rename("Practice list size" = listsize_contr,
  #          "Practice population aged 60-74 year olds" = Prop6074_contr,
  #          "Proportion of 60-74 year olds that are male or female" = PropM_contr,
  #          "Proportion of local area residents who are White British" = ethnicity_contr,
  #          "Deprivation decile" = IMD_contr) %>%
  #   pivot_longer(cols = everything(), 
  #                names_to = "Predictors", 
  #                values_to = "Effect")
  # 
  # TableData <- left_join(TableData, DemoData, join_by(Predictors))
  # 
  # TableData %>%
  #   mutate(Colour = case_when(Effect > 0 ~ "#3fb618", 
  #                             Effect == 0 ~ "#bbc7d9", 
  #                             Effect < 0 ~ "#dc3545")) %>%
  #   arrange(desc(Effect)) %>%
  #   reactable(
  #     style = list(fontFamily = "Poppins, Work Sans, sans-serif", fontSize = "0.875rem"),
  #     highlight = TRUE,
  #     # borderless = TRUE,
  #     columns = list(
  #       Value = colDef(align = 'center', html = TRUE),
  #       Effect = colDef(name = "Relative contribution to model's prediction",
  #                       align = 'center',
  #                       cell = data_bars(.,
  #                                        text_position = "none",
  #                                        fill_color_ref = "Colour",
  #                                        bar_height = 20)
  #       ),
  #       Colour = colDef(show = FALSE)
  #     )
  #   )
  
  
  #Create table
  TableData %>%
    gt(rowname_col = "Predictors") %>%
    tab_header(title = ("The predicted bowel screening coverage for this practice area is based on the following information:")) %>%
    tab_options(column_labels.hidden = TRUE, table.width = "100%") %>%
    opt_table_font(font = c("Poppins", "Arial")) %>%
    #Turn off all borders
    opt_table_lines("none") %>%
    #Set table title font
    tab_style(style = list(
      cell_text(font= "Progress Medium", weight = 500, align = "left")),
      locations = cells_title(groups = c("title"))
    ) %>%
    #Turn on thin borders for top of each value row
    tab_style(
      style = cell_borders(
        sides = c("t"),
        color = "#000000",
        weight = px(1)
      ),
      locations = list(cells_body())
    ) %>%
    #Turn on thick border beneath title
    tab_style(
      style = cell_borders(
        sides = c("b"),
        color = "#000000",
        weight = px(5)
      ),
      locations = list(cells_title())
    ) %>%
    #align predictor values
    cols_align(align = "center", columns = -Predictors) %>%
    #Apply markdown format so the line breaks work
    fmt_markdown(columns = everything())
}



FactorStatement <-  function(GPData, DemoFactors) {
  TableData <- GPData %>%
    mutate(GenderSplit = paste0("Male: ", round(Prop_M_6074), "% 
                              </br>Female: ", 100 - round(Prop_M_6074), "%"),
           prop_6074 = paste0(round(prop_6074), "%"),
           gp_listsize = format(gp_listsize, big.mark=","),
           ethnic_whitebrit_perc = paste0(round(ethnic_whitebrit_perc), "%"),
           IMD = md(paste0(IMD_decile, 
                           "</br>(IMD score ", round(IMD), ")"))) %>%
    select(gp_listsize, prop_6074, GenderSplit, 
           ethnic_whitebrit_perc, IMD, ruc11desc) %>%
    rename("Practice list size" = gp_listsize,
           "Practice area population aged 60-74 year olds" = prop_6074,
           "Proportion of 60-74 year olds that are male or female" = GenderSplit,
           "Proportion of local area residents who are White British" = ethnic_whitebrit_perc,
           "Deprivation decile" = IMD,
           "Practice area's rural/urban classification" = ruc11desc) %>%
    pivot_longer(cols = everything(), 
                 names_to = "Predictors", 
                 values_to = "Value",
                 values_transform = list("Value" = as.character))
  
  
  
  DemoData <- DemoFactors %>%
    # janitor::clean_names() %>%
    filter(OrgCode == GPData$OrganisationCode) %>%
    select(
      names(.)[!grepl("ru11", names(.)) | sapply(., function(x) !(is.numeric(x) && all(x == 0)))],
      -OrgCode, -OrgName, -ICB, -intercept
    )
  
  #if there is a rural urban column (i.e. this gp does not have the reference value for ru11)
  if (any(grepl("ru11",colnames(DemoData)))) {
    #Get that column name
    ruralUrbanCol <- colnames(DemoData)[grepl("ru11",colnames(DemoData))]
    DemoData <- DemoData %>%
      rename("Practice's rural/urban classification" = ruralUrbanCol)
  } else {
    DemoData <- DemoData %>%
      mutate(`Practice's rural/urban classification` = 0)
  }
  
  DemoData <- DemoData %>%
    rename("Practice list size" = listsize_contr,
           "Practice population aged 60-74 year olds" = Prop6074_contr,
           "Proportion of 60-74 year olds that are male or female" = PropM_contr,
           "Proportion of local area residents who are White British" = ethnicity_contr,
           "Deprivation decile" = IMD_contr) %>%
    pivot_longer(cols = everything(), 
                 names_to = "Predictors", 
                 values_to = "Effect")
  
  TableData <- left_join(TableData, DemoData, join_by(Predictors))
  
  BiggestPred <- TableData %>% filter(Effect == min(Effect)) %>% pull(Predictors) 
  
  BiggestPred <- case_when(BiggestPred == "Practice population aged 60-74 year olds" ~ "percent of the practice population that are aged between 60-74",
                           BiggestPred == "Practice's rural/urban classification" ~ "practice's rural/urban classification")
  
  paste0("For your practice area the socio-demographic factor that has the biggest negative impact on our model's prediction is the ", BiggestPred, ".")
  
}
