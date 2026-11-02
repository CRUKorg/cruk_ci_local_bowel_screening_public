library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(gt)
library(bslib)
library(reactable)
library(reactablefmtr)
library(DT)
library(htmltools)
library(bsicons)
library(shinyjs)
library(shinycssloaders)
library(shinyCRUK)

source("Functions.R")
source("UIFunctions.R")
source("modules/module_0_Tool.R")


data_dir <- if (file.exists("data/final_dataset.csv")) "data" else "../data/processed"
GPScreen <- read_csv(file.path(data_dir, "final_dataset.csv"), show_col_types = FALSE) %>%
  mutate(NameCode = paste0(OrganisationName, " [", OrganisationCode, "]")) 
DemoFactors <- read_csv(file.path(data_dir, "model_contributions.csv"))
  

# UI ----
ui <- page_navbar(
  useShinyjs(),
  id = "nav-bar",
  # page head -----
  tags$head(
    # Link external CSS file
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
    #Google analytics 
    crukGA(),
    #Add CRUK favicon
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    # this loads the google material icons (filled versions backup on brand options)
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined",
      rel = "stylesheet"
    ),
    #js to ensure menu closes properly on mobile
    tags$script(src = "menu-collapse.js")
  ),
  
  # cruk logo and page title ----
  title =  div(class = "navbar-brand",
               style = "display: flex; justify-content: space-between;",
               div(class = "title",
                   id = "title_text",
                   style = "cursor: pointer;",
                   HTML("Local Bowel Screening Tool")),
               div(class ="logo",
                   a(img(src = "cruk-logo.svg", 
                         class = 'cruk-logo',
                         style = "margin: 8px; height: 60px;"), 
                     href = "https://www.cancerresearchuk.org/"))
  ),
  
  # general themeing variables ----
  theme = bs_add_variables( 
    bs_theme(version = 5, primary = "#e60078"),
    "min-contrast-ratio"	= 3,
    'popover-max-width'	= '650px',
    'popover-font-size'	= '16px',
    "nav-link-font-size"	= "1.2rem !important",
    "card-spacer-x"	= 'none',
    'card-border-width'	= 'none', 
    'popover-header-bg'	= '#f7f7f7'),
  
  navbar_options = navbar_options(underline = FALSE),
  #Make sure app can scroll
  fillable = FALSE,

  # title for browser tab ----
  window_title = "Local Bowel Screening Tool",
  
  # footer for each page ----
  footer = crukFooter(),
  
  # the tool ---- 
  nav_panel(title = 'The Tool',
            module_0_Tool_TabUI(id = 'module_0_Tool', GPScreen)),
  
  # the user guide ----
  nav_panel(title = 'User guide', 
            div(class = "central-column",
                div(
                  hpReview(REVIEW_DATE), #last review date 
                  crukTitleHR("How to use and interpret the tool"), #Page title
                  # Contents box
                  onThisPage(c("background-purpose", "Background & purpose"),
                             c("icb-focus-section", "ICB focus section"),
                             c("model-prediction-compared-to-screening-coverage-chart", "Model prediction compared to screening coverage chart"),
                             c("standardised-screening-ratio-chart", "Standardised screening ratio chart"),
                             c("practice-population-level-data-view", "Practice population level data view"),
                             c("further-resources", "Further information and resources")),
                  includeHTML("NarrativeText.html"),
                  br(),
                  #Link out to other resources
                  div(id = "further-resources",
                      class = "FurtherResources",
                      h2("Further Information and Resources"),
                      includeMarkdown("www/Further Resources.Rmd")
                  )
                ),
                toTopButton()
            ),
  ),
  
  # the methodology ----
  nav_panel(title = 'Methodology & Data', #Name on navbar
            div(              
              class = "central-column",
              div(),
              div(
                hpReview(REVIEW_DATE),
                crukTitleHR("Tool methodology & data export"), #Page title
                onThisPage(c("data-export", "Tool data export"),
                           c("methodology", "Methodology"),
                           c("data-used", "Data used"),
                           c("analysis", "Analysis"),
                           c("data-sources", "Data sources")),
                br(),
                div(id = "data-export",
                    style = " padding: 15px; 
                              background: #FFFFFF; 
                              border: black 2px solid;",
                    h2("Tool data export"),
                    p("This tool would not be possible without public data and so we think it is only fair that we make our outputs available to use as well. If you would like to use the results from our modelling in another project or piece of work you may download it as a .csv file. If you do so, please cite Cancer Research UK as the source."),
                    p("Full workings and code for this app can be found on our", a(icon("github"), "GitHub repository", href = "https://github.com/CRUKorg/cruk_ci_local_bowel_screening_public")),
                    span(style = "display: flex; justify-content: flex-end;",
                         downloadButton(style = " margin-top: 5px;
                                                  background: #e60078;
                                                  color: white;
                                                  border-radius: 0;",
                                        outputId = "download_button", 
                                        label = "Download analysis data"))
                ),
                includeHTML("Methodology.html"), 
                br(),
                div(id = "data-sources",
                    style = "padding: 16px 48px; background: #f1f1f1;",
                    h2("Data sources"), 
                    p("Bowel screening coverage, 2024/25 - Persons, 60-74, screened for bowel cancer in last 30 months (2.5 year coverage). ", 
                      a(href  = "https://fingertips.phe.org.uk/search/cancer%20screening%20coverage#page/9/gid/1/pat/204/par/U00000/ati/7/are/D82060/iid/92600/age/280/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1", 
                        "Fingertips, Office for Health Improvement and Disparities")),
                    p("Deprivation IMD 2025, Practice Summary.",
                      a(href = "https://fingertips.phe.org.uk/profile/general-practice/data#page/9/gid/2000005/pat/204/par/U22471/ati/7/are/M85736/iid/94240/age/1/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1",
                        "Fingertips, Office for Health Improvement and Disparities."
                      )),
                    p("Patients Registered at a GP Practice, March 2025.",
                      a(href = "https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/march-2025",
                        "NHS Digital."
                      )),
                    p("GP and GP practice related data - Primary care networks and sub-ICB linked to GP practice.",
                      a(href = "https://digital.nhs.uk/services/organisation-data-service/data-search-and-export/csv-downloads/gp-and-gp-practice-related-data",
                        "NHS Digital.")),
                    p("GP practice names, postcodes and addresses. ",
                      a(href = "https://digital.nhs.uk/services/organisation-data-service/data-search-and-export/csv-downloads/gp-and-gp-practice-related-data",
                        "NHS Digital.")),
                    p("Rural Urban Classification 2021 for England and Wales.",
                      a(href = "https://geoportal.statistics.gov.uk/documents/c8e8e6db38e04cb8937569d74bce277a/about", 
                        "Office for National Statistics")),
                    p("Census 2021, ethnic group by Middle layer Super Output Area (MSOA).",
                      a(href = "https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=2041",
                        "NOMIS, Office for National Statistics")),
                    p("ONS Postcode Directory (August 2026) for the UK.", 
                      a(href = "https://geoportal.statistics.gov.uk/datasets/9e5a92a3cfb14dc7ad43d6ea7a7b8c7f/about",
                        "ONS Geography"))
                ),
                br()
              ),
              div(),
              toTopButton()
            )
  )
  # )
)

# Server ----
server <- function(input, output, session) {
  
  onclick("title_text", {
    nav_select(id ="nav-bar", selected = "The Tool")
  })
  
  module_0_Tool_TabServer(id = "module_0_Tool", 
                          GPScreen = GPScreen,
                          DemoFactors = DemoFactors,
                          parent = session)
  
  output$download_button  <- downloadHandler(
    filename = function() {
      paste("CRUK Local Bowel Screening Tool ", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(GPScreen, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)