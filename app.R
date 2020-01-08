library(shiny)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)

globalClusters_questions <- suppressMessages(read_csv('globalClusters_indicators_questions.csv'))
globalClusters_choices <- suppressMessages(read_csv('globalClusters_indicators_choices.csv'))

remove_langLabels <- function(df, language){
  if(language == 'English'){
    if(!paste0('label::', language)%in% names(df) && !"label" %in% names(df)){
      stop('language not recognised. Only English supported for now. Remove ::language from label.')
    }else if(paste0('label::', language)%in% names(df)){
      label_pattern <- paste0('label::',language)
      names(df) <- suppressWarnings(gsub(label_pattern, 'label', names(df)))
    }
  }
  return(df)
}

checkIndic_all <- function(questions, choices,
                           global_choices = globalClusters_choices,
                           global_questions = globalClusters_questions,
                           lang = 'English'){
  
  report_df <- questions
  
  report_df <- remove_langLabels(report_df, lang)
  choices <- remove_langLabels(choices, lang)
  global_q <- remove_langLabels(global_questions, lang = 'English')
  global_c <- remove_langLabels(global_choices, lang = 'English')
  
  final_df <- data.frame( Sector = NA,
                          Question = NA,
                          GlobalIndicator = NA,
                          GlobalQuestion = NA,
                          QinGlobalIndicators = NA,
                          ChoicesInGlobalIndicators = NA,
                          ChoicesNotInlist =NA
  )
  
  for(i in 1:nrow(report_df)){
    this_question <- report_df$label[i]
    choices_list <- choices[choices$list == report_df$choices_list[i],]
    
    final_df <- rbind(final_df, checkIndic(this_question, choices_list, global_choices = global_c, global_questions = global_q, language = lang))
  }
  
  final_df <- final_df %>% filter_all(any_vars(!is.na(.)))

  return(final_df)
}

checkIndic <- function(question,choices_q, global_choices = globalClusters_choices,
                       global_questions = globalClusters_questions, language = 'English'){

  choices_q <- remove_langLabels(choices_q, language)
  global_questions <- remove_langLabels(global_questions, language = 'English')
  global_choices <- remove_langLabels(global_choices, language = 'English')
  
  question <- gsub('^([0-9]\\.*)*\\s', '', question)
  
  choices_labels <- as.character(choices_q$label)
  
  row_question <- match(question, global_questions$label)
  matched_q <- !is.na(row_question)
  choicesInForm <- match(choices_labels, global_choices$label)
  
  matched_compl_c <- sum(is.na(choicesInForm)) == 0
  matched_c <- sum(is.na(choicesInForm)) < length(choicesInForm)
  matched_no_c <- sum(is.na(choicesInForm)) == length(choicesInForm)
  
  if(matched_c == TRUE){
    
    if(sum(is.na(choicesInForm)) == 0){
      choicesNotMatched <- 'All choices matched'
    }else{
      choicesNotMatched <- as.character(unlist(global_choices[!choices_labels %in% global_choices$label, "label"]))
    }  
  }
  if(matched_no_c == TRUE){
    choicesNotMatched <- 'No choice matched'
  }
  
  indicReport <- data.frame(  Sector = global_questions$sector[row_question],
                              Question = question,
                              GlobalIndicator = global_questions$label[row_question],
                              GlobalQuestion = global_questions$label[row_question],
                              QinGlobalIndicators = matched_q,
                              ChoicesInGlobalIndicators = matched_compl_c,
                              ChoicesNotInlist =choicesNotMatched
                            )

  return(indicReport)
}

ui <- fluidPage(
      titlePanel("Form validation against Core Indicators"),
      sidebarPanel(
        fileInput("formfile", label = h4("Upload your XLSform"),accept = c(".xlsx",".xls")),
        radioButtons('language', 'Language of the form', choices = list('English' = 'English')),
        radioButtons('type_form', 'Type of data collection methodology:', choices = list('Household' = 'hh',
                                                                                         'Key Informant' = 'ki')),
        selectizeInput("checkbox_clusters", label = 'Against with sectors/themes do you want to check your form?',
                      choices = list('All' = 'all','REACH' = 'REACH', 'WASH' = 'WASH', 'Education' = 'edu',
                                     'Food Security and Livelihoods' = 'FSL', 'Shelter/NFI' = 'SNFI',
                                     'Protection' = 'protection','Cash' ='cash', 'CCCM' = 'CCCM',
                                     'Emergency Telecommunication' = 'ETC', 'Health' = 'health', 'Nutrition' = 'nut',
                                     'Demographic' = 'demo', 'People on the move' = 'peopleMove'),
                      selected = 'all',
                      multiple = TRUE),
        actionButton('validate_form', 'Validate form'),
        
        downloadButton("downloadReport", "Download report")
      ),
      mainPanel(
        tableOutput("report")
      )
  
)

server <- function(input, output) {
  
  all_clusters <- list('REACH' = 'REACH', 'WASH' = 'WASH', 'Education' = 'edu',
                       'Food Security and Livelihoods' = 'FSL', 'Shelter/NFI' = 'SNFI',
                       'Protection' = 'protection','Cash' ='cash', 'CCCM' = 'CCCM',
                       'Emergency Telecommunication' = 'ETC', 'Health' = 'health', 'Nutrition' = 'nut',
                       'Demographic' = 'demo', 'People on the move' = 'peopleMove'
                       )
  
  observeEvent(input$validate_form,{
    
    inForm <- input$formfile
    langs <- input$language
    typeForm <- input$type_form
    
    clusters <- input$checkbox_clusters
    
    if('all' %in% clusters){
      clusters <- unlist(unlist(all_clusters))
    }
    
    if (is.null(inForm)) return(NULL)
    
    sheets <- excel_sheets(inForm$datapath)
    
    if(!'survey' %in% sheets){
      output$report <- renderText('No "survey" sheet in the uploaded form. Please make sure there is one.')
    }else if(!'choices' %in% sheets){
      output$report <- renderText('No "choices" sheet in the uploaded form. Please make sure there is one.')
    } else{
      
      questions <- read_excel(inForm$datapath, sheet = 'survey')

      choices <- read_excel(inForm$datapath, sheet = 'choices')
      
      types_q <- unique(globalClusters_questions$qtype)
      
      questions_filtered <- questions %>%
        separate(type, into = c('qtype', 'list_name'), sep = ' ', fill = "right")%>%
        filter(qtype %in% types_q)
      
      globalClusters_questions_filtered <- globalClusters_questions%>%
        filter(sector %in% clusters,interview_method == typeForm)
      
      report_all <- checkIndic_all(questions_filtered, choices,
                                   global_choices = globalClusters_choices, global_questions = globalClusters_questions_filtered,
                                   lang = langs)
      
      output$report <- renderTable(report_all)

      output$downloadReport <- downloadHandler(
        filename = function() {
          paste("indicatorValidation_",clusters,Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(report_all, file)
        }
      )
    }
    }
  )
                          
}


shinyApp(ui = ui, server = server)