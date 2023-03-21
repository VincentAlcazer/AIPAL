#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

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
