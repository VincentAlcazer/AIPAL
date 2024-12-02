#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  shinyalert(
    title = "Disclaimer",
    text = HTML('AI-PAL is provided for research use only.
    Diagnosis or clinical decisions made based on AI-PAL predictions are solely the responsibility of the user.
    </br>
    The model has been trained and validated on over 1,400 patients and published in
      <b>Lancet Digital Health</b> (2024 May;6(5):e323-e333).
      </br>
      PMID: 38670741
      DOI: <a href="https://doi.org/10.1016/S2589-7500(24)00044-X" target="_blank">10.1016/S2589-7500(24)00044-X</a>,
      <a href="https://www.thelancet.com/journals/landig/article/PIIS2589-7500(24)00044-X/fulltext" target="_blank">Read the full text</a>'
    ),
    html = T,
    type = "warning",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    showConfirmButton = TRUE,
    confirmButtonText = "I Understand"
  )



  #################### ==================== Data loading  ====================  ####################
  r <- reactiveValues(
    test = reactiveValues()
  )


  ##### ===== New data

  newdata <- reactive({

    req(input$run >= 1)
    input$run
    isolate({

      dat <- data.frame(
                        "MCV_fL"            = if_else(input$MCV_na == F, as.numeric(input$MCV), as.numeric(NA)),
                        "PT_percent"        = if_else(input$PT_na == F, as.numeric(input$PT),   as.numeric(NA)),
                        "LDH_UI_L"          = if_else(input$LDH_na == F, as.numeric(input$LDH), as.numeric(NA)),
                        "MCHC_g_L"          = if_else(input$MCHC_na == F, as.numeric(input$MCHC), as.numeric(NA)),
                        "WBC_G_L"           = if_else(input$WBC_na == F, as.numeric(input$WBC), as.numeric(NA)),
                        "Fibrinogen_g_L"    = if_else(input$fibri_na == F, as.numeric(input$fibri), as.numeric(NA)),
                        "age"               = if_else(input$age_na == F, as.numeric(input$age), as.numeric(NA)),
                        #"Phosphorus_mmol_L" = input$phospho,
                        #"ANC_G_L"           = input$ANC,
                        "Monocytes_G_L"     = if_else(input$mono_na == F, as.numeric(input$mono), as.numeric(NA)),
                        "Platelets_G_L"     = if_else(input$platelets_na == F, as.numeric(input$platelets), as.numeric(NA)),
                        "Lymphocytes_G_L"   = if_else(input$ALC_na == F, as.numeric(input$ALC), as.numeric(NA))
                        ) %>%
        mutate(Monocytes_percent = Monocytes_G_L*100 / WBC_G_L)


      return(dat)

    })
  })


  ##### ===== R list

  observe({
    r$test$newdata <- newdata()
  })




  #################### ==================== App modules ====================  ####################

  mod_prediction_server("prediction_1", r=r)
}
