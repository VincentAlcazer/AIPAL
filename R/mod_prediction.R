#' prediction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_prediction_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidPage(
      column(12,

             h1("Welcome to AI-PAL"),

             p("Artificial Intelligence-based Prediction of Acute Leukemia"),
             br(),

             htmlOutput(ns("prediction_text"),style = "font-size:120%;"),

             box(width=6, title  = "Prediction overview", collapsible = T,
                 p("Overview of the raw predictions computed
                   on the 1410 acute leukemia cases used in the paper
                   (including: training (n=477), testing (n=202) and
                   validation (n=731) sets).
                   "),
                 shinycssloaders::withSpinner(plotOutput(ns("prediction")),type = 6)),

             box(width=6, title  = "Individual scores", collapsible = T,
                 p("Solid lines determine the overall prediction cutoff."),
                 p("Dashed lines determine the confident zone (PPV and NPV of 99%)."),
                 shinycssloaders::withSpinner(plotOutput(ns("prediction_indiv")),type = 6)),

             box(width=6, title = "Outlier detection", collapsible = T,
                 shinycssloaders::withSpinner(plotOutput(ns("density_plot")),type = 6)),

             box(width=6, title  = "Preview", collapsible = T,
                 shinycssloaders::withSpinner(DT::DTOutput(ns("preview")),type = 6))


             # tabsetPanel(
             #   id = "predictions", type = "tabs",
             #   tabPanel("Prediction",
             #   column(12,
             #          h3("AIPAL predictions"),
             #          shinycssloaders::withSpinner(DT::DTOutput(ns("preview")),type = 6)
             #   )),# Column & tabPanel
             #   tabPanel("Distribution",
             #            column(12,
             #                   h3("AIPAL predictions"),
             #                   shinycssloaders::withSpinner(DT::DTOutput(ns("preview")),type = 6)
             #            )),# Column & tabPanel

      ), #column,

      column(2,
             absolutePanel(
               width = 200, right = 20, draggable = T,
               style = "opacity: 0.85",
               wellPanel(




               )# Wellpanel


             ) # Absolutepanel
      ) # Column



    ) #fluidPage

  )
}

#' prediction Server Functions
#'
#' @noRd
mod_prediction_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    newdata <- reactive({r$test$newdata})

    newdata_pc <- reactive({

      req(newdata())
      dat <- predict(pca,
                      newdata=newdata())[1:2]
      return(dat)

    })

    output$density_plot <- renderPlot({
      req(newdata())
      pca_plot +
          geom_point(aes(x = newdata_pc()[1], y = newdata_pc()[2], size = 10)) +
          default_theme


    })

    ##### ===== Prediction

    prediction <- reactive({

      req(newdata())

      prediction <- predict(model, newdata = newdata(), type = "prob", na.action = "na.pass")
      #   mutate(ALL_mod = (ALL - AML)*100/max_ALL,
      #          AML_mod = (AML - ALL)*100/max_AML,
      #          APL_mod = (APL - AML)*100/max_APL,
      #          .keep = "unused")
      #
      # colnames(prediction) <- gsub("_mod","",colnames(prediction))

      return(prediction)


    })

    predictions_df <- reactive({

      req(prediction())

      dat <- rbind(cbind(Response = "Prediction",
                         prediction()),
                         overall_pred[,-1])

      return(dat)


    })

    output$prediction <- renderPlot({
      req(predictions_df())

      predictions_df() %>%
        arrange(Response) %>%
        ggplot(aes(x = APL, y = ALL,
                   color = Response, shape = Response, size = Response, alpha = Response)) +
        ### APL overall
        # geom_rect(xmin = cutoffs$ACC[cutoffs$category == "APL"], xmax = 1,
        #           ymin = 0, ymax = 0.25,
        #           fill = NA, alpha = 1,
        #           color = pal[3],
        #           linejoin = "round", size = 1.2, linetype = 1) +
        #
        # # ### AML overall
        # geom_rect(xmin = 0, xmax = cutoffs$ACC[cutoffs$category == "APL"],
        #           ymin = 0, ymax = cutoffs$ACC[cutoffs$category == "ALL"], fill = NA, alpha = 1,
        #           color = pal[2],
        #           linejoin = "round", size = 1.2, linetype = 1) +
        #
        # # ### ALL overall
        # geom_rect(xmin = 0, xmax = 0.25,
        #           ymin = cutoffs$ACC[cutoffs$category == "ALL"], ymax = 1, fill = NA,
        #           color = pal[1],
        #           linejoin = "round", size = 1.2, linetype = 1) +

        geom_point() +
        scale_color_met_d("Hiroshige", -1)+
        scale_color_manual(values = c(pal, "black")) +
                                      #MetBrewer::MetPalettes$Hiroshige[[1]][1]))+
        scale_alpha_manual(values = c(1,1,1,1)) +
        scale_size_manual(values = c(2,2,2,8)) +
        scale_shape_manual(values = c(15,16,17,18)) +
        labs(x = "APL raw probability", y = "ALL raw probability") +
        default_theme +
        theme(legend.position = "right")

    })


    ##### ===== Individual scores


    output$prediction_indiv <- renderPlot({
      req(predictions_df())

      predictions_df_mod <-  predictions_df() %>%
        arrange(Response) %>%
        pivot_longer(c("ALL","AML","APL"), names_to = "category", values_to = "prediction")

        ggplot(data = predictions_df_mod,
               aes(x = "", y = prediction, color = Response, shape = Response,
                   alpha = Response, size = Response)) +
        geom_jitter(aes(x = "", y = prediction, color = Response, shape = Response,
                        alpha = Response, size = Response),
                    width = 0.2) +
        geom_hline(data = cutoffs,
                  aes(yintercept = PPV), linetype = 2) +
        geom_hline(data = cutoffs,
                     aes(yintercept = NPV), linetype = 2) +
          geom_hline(data = cutoffs,
                     aes(yintercept = ACC), linetype = 1) +
        scale_color_manual(values = c(pal,"black"))+
          scale_alpha_manual(values = c(1,1,1,1)) +
          scale_size_manual(values = c(2,2,2,8)) +
          scale_shape_manual(values = c(15,16,17,18)) +
        facet_wrap(~category) +
        labs(x = "", y = "Prediction probability") +
        default_theme

    })




    ##### ===== Parameters table

    prediction_params <- reactive({
      req(newdata())
      req(prediction())

      ### Get highest pred
      #highest =

      APL_params <- params_df %>%
        filter(model == "APL") %>%
        filter(abs(prediction()$APL - Cutoff) == min(abs(prediction()$APL - Cutoff)) )%>%
        top_n( 1, SPEC) %>% top_n(1, ACC)%>% dplyr::slice(1)

      AML_params <- params_df %>%
        filter(model == "AML") %>%
        filter(abs(prediction()$AML - Cutoff) == min(abs(prediction()$AML - Cutoff)) )%>%
        top_n( 1, SPEC) %>% top_n(1, ACC)%>% dplyr::slice(1)

      ALL_params <- params_df %>%
        filter(model == "ALL") %>%
        filter(abs(prediction()$ALL - Cutoff) == min(abs(prediction()$ALL - Cutoff)) )%>%
        top_n( 1, SPEC) %>% top_n(1, ACC)%>% dplyr::slice(1)

      prediction_params <- rbind(APL_params,
                                 AML_params,
                                 ALL_params) %>%
        select(-Depth, -TP, -FP, -TN, -FN, -ACC, -percent_FP, -percent_FN)

      return(prediction_params)

    })

    ### Final pred

    final_pred <- reactive({
      req(newdata())
      req(prediction())

      final_pred <- prediction() %>%
        mutate(
          ## Overall
          overall_is_APL = if_else(APL >= cutoffs$ACC[cutoffs$category == "APL"], T, F),
          overall_is_ALL = if_else(ALL >= cutoffs$ACC[cutoffs$category == "ALL"], T, F),
          overall_is_AML = if_else(AML >= cutoffs$ACC[cutoffs$category == "AML"], T, F),

          ## Confident
          confident_APL = if_else(APL >= cutoffs$PPV[cutoffs$category == "APL"], T, F),
          confident_not_APL = if_else(APL <= cutoffs$NPV[cutoffs$category == "APL"], T, F),
          confident_AML = if_else(AML >= cutoffs$PPV[cutoffs$category == "AML"], T, F),
          confident_not_AML = if_else(AML <= cutoffs$NPV[cutoffs$category == "AML"], T, F),
          confident_ALL = if_else(ALL >= cutoffs$PPV[cutoffs$category == "ALL"], T, F),
          confident_not_ALL = if_else(ALL <= cutoffs$NPV[cutoffs$category == "ALL"], T, F)
        )

      ### 1. Set Confident pred
      #final_pred$confident_pred


      ### 2. Set overall pred
      # Start by confident pred
      final_pred$overall_pred[final_pred$confident_APL == T] <- "APL"
      final_pred$overall_pred[final_pred$confident_AML == T] <- "AML"
      final_pred$overall_pred[final_pred$confident_ALL == T] <- "ALL"

      # Then complete with overall pred from max acc cutoffs
      final_pred$overall_pred[is.na(final_pred$overall_pred) & final_pred$overall_is_APL == T] <- "APL"
      final_pred$overall_pred[is.na(final_pred$overall_pred) & final_pred$overall_is_AML == T] <- "AML"
      final_pred$overall_pred[is.na(final_pred$overall_pred) & final_pred$overall_is_ALL == T] <- "ALL"


      return(final_pred)

    })


    output$preview <- DT::renderDT(
      prediction_params(), # data
      class = "display nowrap compact", # style
      filter = "top", # location of column filters
      server = T,
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        lengthChange = TRUE,
        columnDefs = list(list(className = "dt-left", targets = "_all"))
      )
    )



    ##### ===== Prediction text

    output$prediction_text <- renderText({

      raw_pred <-   paste0(names(which.max(final_pred()[,1:3])),
                           " (",
                           round(final_pred()[which.max(final_pred()[,1:3])][[1]],3),
                           ")")

      overall_pred <- paste0(final_pred()$overall_pred)

      confident_pred <-   if(length(colnames(final_pred()[,7:12])[which(final_pred()[,7:12] == T)]) == 0){

        paste0("<font color=\"#E88B1A\">",
               paste0("No confident prediction"),
               " </font>")

      } else {
        paste0("<font color=\"#21bf88\">",
        paste(colnames(final_pred()[,7:12])[which(final_pred()[,7:12] == T)], collapse = " / "),
        " </font>")
      }



      pos_pred <- colnames(prediction())[which(prediction() == max(prediction()))]

      neg_pred1 <- colnames(prediction())[which(prediction() == min(prediction()))]
      neg_pred2 <-  colnames(prediction())[!colnames(prediction()) %in% c(pos_pred,neg_pred1)]

      ### === Positive pred
      pos_pred_text <- if(prediction()[pos_pred] >= cutoffs$PPV[cutoffs$category == pos_pred] ){
        paste0("<font color=\"#21bf88\">", pos_pred, " with a positive predictive value of ",
               prediction_params()$PPV[prediction_params()$model == pos_pred]*100," %</font>")

      } else {
        paste0("<font color=\"#E88B1A\">",pos_pred, " with a positive predictive value of ",
        prediction_params()$PPV[prediction_params()$model == pos_pred]*100," %</font>")

      }

      ### === Negative pred 1
      neg_pred_text1 <- if(prediction()[neg_pred1] <= cutoffs$NPV[cutoffs$category == neg_pred1] ){
        paste0("<font color=\"#21bf88\"> NOT ", neg_pred1, " with a negative predictive value of ",
               prediction_params()$NPV[prediction_params()$model == neg_pred1]*100," %</font>")

      } else {
        paste0("<font color=\"#E88B1A\">NOT ", neg_pred1, " with a negative predictive value of ",
               prediction_params()$NPV[prediction_params()$model == neg_pred1]*100," %</font>")

      }

      ### === Negative pred 2
      neg_pred_text2 <- if(prediction()[neg_pred2] <= cutoffs$NPV[cutoffs$category == neg_pred2] ){
        paste0("<font color=\"#21bf88\"> NOT ", neg_pred2, " with a negative predictive value of ",
               prediction_params()$NPV[prediction_params()$model == neg_pred2]*100," %</font>")

      } else {
        paste0("<font color=\"#E88B1A\">NOT ", neg_pred2, " with a negative predictive value of ",
               prediction_params()$NPV[prediction_params()$model == neg_pred2]*100," %</font>")

      }



      text <- paste(paste0("Raw prediction (probability): <b>",raw_pred,"</b><br>",
                           "Affined prediction using overall model: <b>", overall_pred,"</b><br>",
                           "Confident prediction: <b>", confident_pred,"</b><br><br>",



                           "Predictions PPV/NPV :<b><br> ",
                           pos_pred_text,
                   "<br>",
                   neg_pred_text1,
                   "<br>",
                   neg_pred_text2,
                   "</b><br><br>"

                   ),
            "Confidence levels:
            <font color=\"#21bf88\"> Confident </font> //
            <font color=\"#E88B1A\"> Not confident <font>",
            #<font color=\"#CE3434\"> <95% <font>

      sep = "\n")

      return(text)

      #              as.character(input$y_var), " (Levels = ", paste(levels(unlist(select(data(), input$y_var))), collapse = " / "), ")"),
      #
      #       "<br> <i> Note that for categorical variables, each level of the variables is compared to the baseline level
      # (which is the first level by alphabetical order).</i>",
      #       sep = "\n"
      # )
    })


  })
}

## To be copied in the UI
# mod_prediction_ui("prediction_1")

## To be copied in the server
# mod_prediction_server("prediction_1")
