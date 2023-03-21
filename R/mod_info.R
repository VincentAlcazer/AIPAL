#' info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_info_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidPage(
      column(10,


             h2("Change log"),

             HTML("
            01-12-2021: v0.1 - Data loading & Expression modules <br/>
            ")


      ) #column
    )# FLuid Page

  )
}

#' info Server Functions
#'
#' @noRd
mod_info_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_info_ui("info_1")

## To be copied in the server
# mod_info_server("info_1")
