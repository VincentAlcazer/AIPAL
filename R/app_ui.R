#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(

      header = dashboardHeader(
        title = "AI-PAL v0.3",
        tags$li(class = "dropdown",
                shinyWidgets::dropMenu(
                  shinyWidgets::dropdownButton("Info", status = 'success', icon = icon('info')),
                  h3(strong('Information')),
                  br(),
                  h5('This is really helpful'),
                  textInput('text', 'You can also put UI elements here'),
                  placement = "bottom",
                  arrow = TRUE)

        )
      ),

      #################### ==================== SIDEBAR ====================  ####################

      sidebar = dashboardSidebar(
        # sidebarMenu(
        #   id = "tabs",
        #   menuItem("Prediction", tabName = "prediction", icon = icon("play-circle")),
        #   menuItem("Info/Help", tabName = "info", icon = icon("info")),

          column(12,
                 actionButton("run",
                              tags$span(style = "font-weight: bold;",
                                        "Predict leukemia type"))),
          column(12,

                 h2("Variables", align = "center"),
                 div(style = "margin-top: -20px"),
                 splitLayout(cellWidths = sidebar_split,
                             numericInput("age",
                                          label = ("Age (years)"),
                                          value = 55),
                             div(style = checkbox_align,
                                 checkboxInput("age_na", label = "NA", value = F))
                             ),
                div(style = "margin-top: -20px"),

                h3("Blood", align = "center"),
                div(style = "margin-top: -20px"),
                splitLayout(cellWidths = sidebar_split,
                            numericInput("MCV",
                                         label = ("MCV (fL)"),
                                         value = 90.2),
                            div(style = checkbox_align,
                                checkboxInput("MCV_na", label = "NA", value = F))
                            ),
                div(style = "margin-top: -20px"),
                splitLayout(cellWidths = sidebar_split,
                            numericInput("MCHC",
                                         label = "MCHC (g/L)",
                                         value = 330),
                            div(style = checkbox_align,
                                checkboxInput("MCHC_na", label = "NA", value = F)),
                ),
                div(style = "margin-top: -20px"),
                splitLayout(cellWidths = sidebar_split,
                            numericInput("platelets",
                                         label = ("Platelets (G/L)"),
                                         value = 50),
                            div(style = checkbox_align,
                                checkboxInput("platelets_na", label = "NA", value = F)),
                ),
                div(style = "margin-top: -20px"),
                splitLayout(cellWidths = sidebar_split,
                            numericInput("WBC",
                                         label = ("WBC (G/L)"),
                                         value = 10),
                            div(style = checkbox_align,
                                checkboxInput("WBC_na", label = "NA", value = F)),
                ),
                # div(style = "margin-top: -20px"),
                # splitLayout(cellWidths = sidebar_split,
                #             numericInput("ANC",
                #                          label = ("ANC (G/L)"),
                #                          value = 5),
                #             div(style = checkbox_align,
                #                 checkboxInput("ANC_na", label = "NA", value = F)),
                # ),

                div(style = "margin-top: -20px"),
                splitLayout(cellWidths = sidebar_split,
                            numericInput("ALC",
                                         label = ("ALC (G/L)"),
                                         value = 1),
                            div(style = checkbox_align,
                                checkboxInput("ALC_na", label = "NA", value = F)),
                ),

                div(style = "margin-top: -20px"),
                splitLayout(cellWidths = sidebar_split,
                            numericInput("mono",
                                         label = ("Monocytes (G/L)"),
                                         value = 6),
                            div(style = checkbox_align,
                                checkboxInput("mono_na", label = "NA", value = F)),
                ),


          h3("Coag", align = "center"),
          div(style = "margin-top: -20px"),
          splitLayout(cellWidths = sidebar_split,
                      numericInput("PT",
                                   label = ("PT (%)"),
                                   value = 6),
                      div(style = checkbox_align,
                          checkboxInput("PT_na", label = "NA", value = F)),
          ),
          div(style = "margin-top: -20px"),
          splitLayout(cellWidths = sidebar_split,
                      numericInput("fibri",
                                   label = ("Fibrinogen (g/L)"),
                                   value = 6),
                      div(style = checkbox_align,
                          checkboxInput("fibri_na", label = "NA", value = F)),
          ),


          h3("Bioch", align = "center"),
          # div(style = "margin-top: -20px"),
          # numericInput("phospho",
          #              label = ("phosphorus (mmol/L)"),
          #              value = 1.47),
          div(style = "margin-top: -20px"),
          splitLayout(cellWidths = sidebar_split,
                      numericInput("LDH",
                                   label = ("LDH (UI/L)"),
                                   value = 250),
                      div(style = checkbox_align,
                          checkboxInput("LDH_na", label = "NA", value = F)),
          )


          ) # column
         # ) #dashboard sidebar
      ), # sidebar


      #################### ==================== BODY ====================  ####################

      body = dashboardBody(

        mod_prediction_ui("prediction_1")
       #  tabItems(
       #
       #    ##### ===== Home
       #
       #    tabItem("prediction", mod_prediction_ui("prediction_1")),
       #
       #    ##### ===== Info / Help
       #
       #    tabItem("info", mod_info_ui("info_1"))
       #
       # )#tabItems

      ) #body

    ) #dashboard page
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "AIPAL"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
