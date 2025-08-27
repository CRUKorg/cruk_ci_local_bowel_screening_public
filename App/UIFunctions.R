# Health professional box and date ----
# box with "health professional and date of last review
hpReview <- function(last_review_date) {
  div(
    span("Health professionals", 
         style = "background: #e2f4fd; 
                  color: #0076b4; 
                  display: inline-flex;
                  align-items: center;
                  padding: 8px 12px 8px 12px;"),
    div(style = "flex: 1; 
                   align-self: stretch;"),
    span(HTML("Last reviewed: ", last_review_date),
         style = "color: #666666;
                  padding: 8px 12px 8px 12px;
                  text-align: right;"),
    style = "margin-bottom: 8px; 
             display: flex;")
}

hpNextReview <- function(last_review_date, next_review_date) {
  div(
    span("Health professionals", 
         style = "background: #e2f4fd; 
                  color: #0076b4; 
                  display: inline-flex;
                  align-items: center;
                  padding: 8px 12px 8px 12px;"),
    div(style = "flex: 1; 
                   align-self: stretch;"),
    span(HTML("Last reviewed: ", last_review_date, "<br>",
              "Next review due: ", next_review_date),
         style = "color: #666666;
                  padding: 8px 12px 8px 12px;
                  text-align: right;"),
    style = "margin-bottom: 8px; 
             display: flex;")
}

# Formatted title with horizontal rule ----
crukTitleHR <- function(Title, Subheading = '') {
  div(h1(Title, 
         style = "font-family: Progress Medium;
                  margin: 0px;"),
      span(Subheading,
           class = "subheading",
           style = "font-family: Poppins;
                  margin: 28px 0px 0px 0px;"),
      hr(style = "width: 4rem;
                  height: 0.5rem;
                  color: black;
                  border-top: 0.5rem solid black;
                  opacity: 1"))
}

# Standardised footer with charity info ----
crukFooter <- function() {
  div(class = "central-column",
      div(class = "info-footer",
          style = "padding: 16px 48px 16px 48px; border: 2px solid black; margin-bottom: 32px; margin-top: 32px;",
          #CRUK logo
          div(a(img(src = "cruk-logo.svg", 
                  class = 'cruk-logo',
                  style = "margin: 8px; margin-bottom: 16px; height: 60px;"), 
              href = "https://www.cancerresearchuk.org/")),
          bslib::layout_column_wrap(width = 1/2,
                                    div(includeMarkdown('email-site-text.Rmd')),
                                    div(includeMarkdown('citation-text.Rmd'))
                                    ),
          
      ),
      
      hr(style = "opacity: 1;
                  margin-right: calc(50% - 50vw) !important;
                  border: 0;
                  border-color: #000000;
                  border-style: solid;
                  border-bottom-width: 0.25rem;
                  margin: 0px;
                  width: auto;"),
      h3("Together we are", br(), 
         " beating cancer",
         style = "padding-top: 16px;"),
      a(
        img(src = "logo-fundraising-regulator-reg.svg", 
            height = "97px", 
            style = "margin-right: 10px; height: 51px; margin-top: 16px; margin-bottom: 16px;"), 
        href = "https://www.fundraisingregulator.org.uk/"),
      tags$address("Cancer Research UK is a registered charity in England and Wales (1089464), Scotland (SC041666), the Isle of Man (1103) and Jersey (247). A company limited by guarantee. Registered company in England and Wales (4325234) and the Isle of Man (5713F). Registered address: 2 Redman Place, London, E20 1JQ.")
  )
}

# Page contents box -----
onThisPage <- function(...) {
  # Process arguments
  args <- list(...)
  
  div_ids <- sapply(args, function(x) x[1])
  link_texts <- sapply(args, function(x) x[2])
  
  # Ensure div_ids and link_texts are the same length
  if (length(div_ids) != length(link_texts)) {
    stop("Number of div_ids must match number of link_texts")
  }
  
  # Create links for each pair
  links <- lapply(1:length(div_ids), function(i) {
    # Pre-append # to div_id if it doesn't already start with #
    href_id <- ifelse(!startsWith(div_ids[i], "#"), paste0("#", div_ids[i]), div_ids[i])
    
    div(style = "padding-bottom: 0.5rem;",
      HTML(paste0(
        span(class = "material-icons", 
             style = "vertical-align: middle; color: #00007e;", 
             "arrow_downward"),
        a(href = href_id, link_texts[i], 
          style = "color: #00007e !important; text-decoration: underline !important; font-weight: 600; 
          font-size: 1.2rem; margin-top: 0.5rem;")
      )),
      
    )
  })
  
  # Build the main container with all links
  div(
    style = "background: #f0f0ff; padding:1.5rem; margin: 0; margin-bottom: 12px;",
    tags$link(href = "https://fonts.googleapis.com/icon?family=Material+Icons", rel = "stylesheet"),
    div(
      style = "font-size: 1.5rem; padding-bottom: 0.8rem; margin: 0px; font-family: 'Progress Medium';",
      "On this page"
    ),
    do.call(tagList, links)
  )
}

crukValueBox <- function(id, title= "", value = "", icon = "", 
                         icon_colour = "#00007e", detail = "", 
                         link = "") {
  ns <- NS(id)
  
  value_box(
    id = id,
    title = p(title,
              hr(style="width:100%; 
                 text-align:left; 
                 margin-left:0; 
                 border-top: 5px solid #000; 
                 margin-top: 5px; 
                 margin-bottom: 5px;
                 opacity: 1;")),
    value = span(value, style = "font-family: Progress Medium;"),
    showcase_layout = showcase_left_center(width = 0.25),
    showcase = span(icon, 
                    style = paste0("font-size: 3rem !important; 
                                   color: ", icon_colour, ";")),
    theme = value_box_theme(bg = "#ffffff", fg = "#000000"),
    detail,
    p("explore metric ", icon('circle-chevron-right'), 
      style = 'text-align: right; 
               padding: 5px; 
               text-decoration: none; 
               color: #009cee;') 
  )
}

toTopButton <- function() {
  # CSS styles
  css_styles <- tags$style(HTML("
    /* scroll to top arrow */
    .scroll-to-top-button {
        display: none;
        position: fixed;
        bottom: 20px;
        left: 51%;
        margin-left: 480px;
        z-index: 99;
        border: none;
        outline: none;
        color: #ffffff;
        background-color: #00007e;
        cursor: pointer;
        padding: 7px 5px 3px 5px;
        border-radius: 9999px;
        width: 56px !important;
        height: 56px !important;
    }
    
    .scroll-to-top-arrow {
      font-size: 2.5rem !important;
    }
    
    @media (max-width: 1030px) {
      .scroll-to-top-button {
        left: auto !important;
        margin-left: auto !important;
        right: 5%;
        width: 42px !important;
        height: 42px !important;
      }
      
      .scroll-to-top-arrow {
        font-size: 1.5rem !important;
      }
    }
  "))
  
  # JavaScript functionality
  js_script <- tags$script(HTML("
    // Define the scroll function globally
    window.scrollToTop = function () {
      window.scrollTo({
        top: 0,
        behavior: 'smooth'
      });
    };
    document.addEventListener('DOMContentLoaded', function () {
      // Select all scroll-to-top buttons
      const scrollToTopButtons = document.querySelectorAll('.scroll-to-top-button');
      // Handle scroll event once for all buttons
      window.addEventListener('scroll', () => {
        const shouldShow = window.scrollY > 200;
        scrollToTopButtons.forEach(button => {
          button.style.display = shouldShow ? 'block' : 'none';
        });
      });
      // Attach click listener to each button
      scrollToTopButtons.forEach(button => {
        button.addEventListener('click', scrollToTop);
      });
    });
  "))
  
  # R HTML button element
  button_element <- tags$button(
    class = "scroll-to-top-button",
    tags$i('arrow_upward', 
           class = c("material-icons", "scroll-to-top-arrow"))
  )
  
  # Return all components as a tagList
  return(tagList(
    css_styles,
    js_script,
    button_element
  ))
}
