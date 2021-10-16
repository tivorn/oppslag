library(shiny)
library(shinydashboard)
library(shinyWidgets)

library(uuid4)
library(stringr)
library(rvest)
library(polite)
library(purrr)
library(tidyverse)
library(lubridate)
library(reactable)
library(googlesheets4)

js_code <- '
  $(document).on("click", ".word", function(event) {
    Shiny.setInputValue("word_id", event.target.id)
    
      Shiny.setInputValue("word_label", event.target.name)
  })
  
  $(document).on("click", ".word_translation", function(event) {
    Shiny.setInputValue("word_translation_id", event.target.id)
    
      Shiny.setInputValue("word_translation_label", event.target.name)
  })
'

SHEET_ID <- Sys.getenv("SHEET_ID")

options(gargle_oauth_cache = ".secrets")

saveData <- function(data) {
  sheet_append(SHEET_ID, data)
}

loadData <- function() {
  read_sheet(SHEET_ID)
}

data <- loadData()

listActionButton <- function(word_label, input_id) {
  comprehension_class <- data %>%
    filter(word == word_label) %>%
    pull(comprehension_level) %>%
    str_to_lower()
  
  if (word_label != "") {
    if (!is_empty(comprehension_class)) {
      button <- actionButton(
        inputId = input_id,
        label = word_label,
        name = word_label,
        class = c("word", str_glue("{comprehension_class}"))
      )
    } else {
      button <- actionButton(
        inputId = input_id,
        label = word_label,
        name = word_label,
        class = c("word", "unseen")
      )
  }
    
    return(button)
  } else {
    return(HTML("<br/><br/>"))
  }
  
}

sample_text <- readLines("sample_text.txt", encoding = "UTF-8")

words <- sample_text %>%
  str_split(" ") %>%
  unlist()

ui <- navbarPage(
  title = div(img(src = "logo_icon.svg", width = "50px"),"oppslag"),
  header = withTags({
    head(
      link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      script(HTML(js_code))
    )
  }),
  tabPanel(
    "Lesson",
    box(
      title = "My Text",
      map2(words, uuid(length(words)), listActionButton)
    ),
    
    box(
      title = "English Translation",
      uiOutput("some_box")
    )
  ),
  tabPanel(
    "Vocabulary",
    box(
      title = "Vocabulary",
      reactableOutput("vocabulary_table")
    )
  )
)

server <- function(input, output, session) {
  
  trex_scrape_word_translation <- function(word) {
    
    print("Realizando extração...")
    
    trex_base_url <- "https://tr-ex.me/translation/norwegian-english/"
    
    trex_word_url <- str_glue("{trex_base_url}{word}?search={word}")
    
    trex_session <- bow(trex_base_url, delay = 5)
    
    html_page <- nod(trex_session, trex_word_url) %>%
      scrape()
    
    words_translation <- html_page %>%
      html_elements(".translation-exact-0") %>%
      html_text2()
    
    print("Extração realizada!")
    
    return(words_translation)
  }
  
  observeEvent(input$word_id, {
    output$some_box <- renderUI({
        words_translation <- trex_scrape_word_translation(input$word_label)    
      
        map(words_translation, ~ a(id = uuid(),
                                   .x,
                                   icon('external-link-alt'),
                                   name = .x,
                                   class = "word_translation"))

      })
  })
  
  observeEvent(input$word_translation_id, {
    showModal(modalDialog(
      title = str_glue('{input$word_label}'),
      size = "m",
      radioGroupButtons(
        inputId = "word_comprehension",
        label = "Comprehension level",
        choices = c("New", "Recognized", "Familiar","Learned",
                    "Known", "Ignore")
      ),
      actionButton(
        inputId = "submit_comprehension_level",
        label = "Add"
      )
    ))
  })
  
  df_comprehension_level <- reactive({
    tibble(comprehension_level = input$word_comprehension,
           id = uuid(),
           date = today(),
           word = input$word_label,
           word_translation = input$word_translation_label)
  })
  
  observeEvent(input$submit_comprehension_level, {
    gs4_auth(email = TRUE)
    saveData(df_comprehension_level())
  })
  
  output$vocabulary_table <- renderReactable({
    input$submit_comprehension_level
    
    data <- loadData()

    if (!is.null(data)) {
      data %>%
        select(date,
               word,
               word_translation,
               comprehension_level) %>%
        reactable(
          columns = list(
            date = colDef(name = "Data"),
            word = colDef(name = "Norwegian"),
            word_translation = colDef(name = "English"),
            comprehension_level = colDef(name = "Comprehension Level")
          )
        )
    }
  })

}

shinyApp(ui, server)