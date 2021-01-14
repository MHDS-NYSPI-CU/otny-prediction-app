library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)
library(purrr)
library(caret)
library(tibble)
library(ggalluvial)
library(shiny)
library(patchwork)

shinyServer(function(input, output) {

    pred_point_2 <- reactive({
        req(input$pred_point_2)
        pred_df_2 %>%
            filter(preds == input$pred_point_2) %>%
            pull(pred_nums)
    })
    
    # WORK/EDU
    pred_point <- reactive({
        req(input$pred_point)
        pred_df %>%
            filter(preds == input$pred_point) %>%
            pull(pred_nums)
    })
    
    output$secondSelect <- renderUI({
        selectInput(inputId = "selected_id", "Select Client",
                                choices = get_available_ids(pred_point(), "edu_work"))
    })
    
    output$secondSelect_2 <- renderUI({
        selectInput(inputId = "selected_id_2", "Select Client",
                    choices = get_available_ids_long("edu_work"))
    })
    
    prediction_df <- reactive({
        req(input$selected_id)
        req(input$pred_point)
        pred_data <- get_predictor_data(pred_point(), input$selected_id, "edu_work") 
        predictions <- get_1y_predictions(pred_point(), pred_data, "edu_work")
        return(predictions)
    })
    
    prediction_df2 <- reactive({
        req(input$selected_id_2)
        req(input$pred_point_2)
        predictions <- get_cumu_preds(pred_point_2(), input$selected_id_2, "edu_work")
        return(predictions)
    })
    
    output$eduwork_plot <- renderPlot({
    
        req(input$pred_point)
        req(input$selected_id)
        observed_dat <- final_edu %>%
            filter(SCREENID_DID == input$selected_id) %>%
            filter(out_edu_work != "Missing", out_edu_work != "Discharged") %>%
            mutate(predictions = case_when(out_edu_work == "Positive" ~ 1,
                                           out_edu_work == "Negative" ~ 0,
                                           TRUE ~ NA_real_)) %>%
            mutate(outcome_fup = (INT1-1)*3) %>%
            filter(outcome_fup <= pred_point() + 12)
        
        
        edu_plot <- reactive({
            ggplot(data = prediction_df()) +
            geom_rect(aes(xmin = -0.05, xmax = pred_point(), ymin = -0.0, ymax = 1.0, fill = "Data Used to Predict"), alpha = 0.1) +
            scale_fill_manual(values = c("Data Used to Predict" = "steelblue")) +
            geom_point(aes(x = outcome_fup, y = predictions, color = "Model Prediction"), size = 3.5) +
            geom_line(aes(x = outcome_fup, y = predictions), color = "red", linetype = "dashed", alpha = 0.8) +
            geom_text_repel(aes(x = outcome_fup, y = predictions, label = round(predictions,2)), color = "red",
                            box.padding = 0.5, min.segment.length = 0.75) +
            geom_point(data = observed_dat, aes(x = outcome_fup, y = predictions, color = "Observed"), size = 3.5, pch = 15) +
            scale_color_manual(values = c("Model Prediction" = "red", "Observed" = "black")) +
            theme_bw() +
            scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1.0), 
                               labels = c("0.0", "0.25", "0.5", "0.75", "1.0")) +
            scale_x_continuous(breaks = c(observed_dat$outcome_fup, prediction_df()$outcome_fup)) +
            labs(x = "Follow-up (months)", y = "Predicted Probability Positive",
                 title = "Education/Work Predictions for Next Year",
                 subtitle = "Click on Prediction for Model Information",
                 color = "",
                 fill = "") +
            theme(legend.position = "bottom",
                  panel.grid.minor = element_blank()) +
            guides(color = guide_legend(override.aes = list(pch = c(16, 15))),
                   fill = guide_legend(override.aes = list(alpha = 0.9)))
        })
        edu_plot()

    })
    
    #### SECOND TYPE OF GRAPH


    num_plots <- function(){
        150*(pred_point_2()/3 + 1)
    }

    output$eduwork_plot_2 <- renderPlot({
        req(input$pred_point_2)
        req(input$selected_id_2)
        
        observed_dat <- final_edu %>%
            filter(SCREENID_DID == input$selected_id_2) %>%
            filter(out_edu_work != "Missing", out_edu_work != "Discharged") %>%
            mutate(predictions = case_when(out_edu_work == "Positive" ~ 1,
                                           out_edu_work == "Negative" ~ 0,
                                           TRUE ~ NA_real_)) %>%
            mutate(fup_a = (INT1-1)*3) %>%
            filter(fup_a <= pred_point_2()) %>%
            select(fup_a, predictions) 
        
        lookup_table <- tibble(predictor_fup = observed_dat$fup_a) %>%
            mutate(observed_fup = map(predictor_fup, function(x) seq(from = 0, to = x, by = 3))) %>%
            unnest(col = observed_fup) %>%
            left_join(observed_dat, by = c("observed_fup" = "fup_a")) %>%
            filter(predictor_fup != pred_point_2())
        
        pred_df <- prediction_df2()
        
        facet_labeller <- function(string){
            case_when(string == 0 ~ "Using Only Baseline",
                      string == 3 ~ "Using Baseline & 3 Months",
                      string == 6 ~ "Using Baseline to 6 Months",
                      string == 9 ~ "Using Baseline to 9 Months",
                      string == 12 ~ "Using Baseline to 12 Months")
        }
        
        edu_plot_2 <- reactive({
            ggplot(data = pred_df) +
            geom_rect(aes(xmin = -0.05, xmax = predictor_fup, ymin = -0.0, ymax = 1.0, fill = "Data Used to Predict"), alpha = 0.3) +
            facet_wrap(~predictor_fup, ncol = 1,
                       labeller = labeller(predictor_fup = facet_labeller)) +
            scale_fill_manual(values = c("Data Used to Predict" = "steelblue")) +
            geom_point(aes(x = outcome_fup, y = predictions, color = "Model Prediction"), size = 3.5) +
            geom_text_repel(aes(x = outcome_fup, y = predictions, label = round(predictions,2)), color = "red", size = 3,
                            box.padding = 0.5, min.segment.length = 0.75) +
            geom_point(data = lookup_table, aes(x = observed_fup, y = predictions, color = "Observed"), size = 3.5, pch = 15) +
            scale_color_manual(values = c("Model Prediction" = "red", "Observed" = "black")) +
            theme_bw() +
            scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1.0),
                               labels = c("0.0", "0.25", "0.5", "0.75", "1.0")) +
            scale_x_continuous(breaks = seq(from = 0, to = pred_point_2(), by = 3)) +
            labs(x = "Follow-up (months)", y = "Predicted Probability Positive",
                 title = "Education/Work Predictions",
                 color = "",
                 fill = "") +
            theme(legend.position = "bottom",
                  panel.grid.minor = element_blank(),
                  strip.background = element_rect(fill = NA)) +
            guides(color = guide_legend(override.aes = list(pch = c(16, 15))),
                   fill = guide_legend(override.aes = list(alpha = 0.9)))
        
        
        })
        
        
        if(input$edu_show_out == FALSE){
            edu_plot_2()
        }else if(input$edu_show_out == TRUE){
            observed_outcome <- final_edu %>%
                filter(SCREENID_DID == input$selected_id_2) %>%
                filter(out_edu_work != "Missing", out_edu_work != "Discharged") %>%
                mutate(predictions = case_when(out_edu_work == "Positive" ~ 1,
                                               out_edu_work == "Negative" ~ 0,
                                               TRUE ~ NA_real_)) %>%
                mutate(fup_a = (INT1-1)*3) %>%
                filter(fup_a == pred_point_2()) %>%
                select(fup_a, predictions) 
            
            
            edu_plot_2() +
                geom_point(data = observed_outcome, aes(x = fup_a, y = predictions), color = "black", pch = 15, size = 3.5)
        }
        
        
        
        },
        height = num_plots,
        width = 550
        
    )
    
    
    ### HOSP
    
    pred_point_hosp <- reactive({
        req(input$pred_point_hosp)
        pred_df %>%
            filter(preds == input$pred_point_hosp) %>%
            pull(pred_nums)
    })
    
    output$secondSelect_hosp <- renderUI({
        selectInput(inputId = "selected_id_hosp", "Select Client",
                    choices = get_available_ids(pred_point_hosp(), "hosp"))
    })
    
    prediction_df_hosp <- reactive({
        req(input$pred_point_hosp)
        req(input$selected_id_hosp)
        pred_data <- get_predictor_data(pred_point_hosp(), input$selected_id_hosp, "hosp") 
        predictions <- get_1y_predictions(pred_point_hosp(), pred_data, "hosp")
        return(predictions)
    })
    
    output$hosp_plot <- renderPlot({
        req(input$pred_point_hosp)
        req(input$selected_id_hosp)
        observed_dat <- final_hosp %>%
            filter(SCREENID_DID == input$selected_id_hosp) %>%
            filter(hosp_psych != "Missing", hosp_psych != "Discharged") %>%
            mutate(predictions = case_when(hosp_psych == "Positive" ~ 1,
                                           hosp_psych == "Negative" ~ -0,
                                           TRUE ~ NA_real_)) %>%
            mutate(outcome_fup = (INT1-1)*3) %>%
            filter(outcome_fup <= pred_point_hosp() + 12)
        
        hosp_plot <- reactive({
            
        
        ggplot(data = prediction_df_hosp()) +
            geom_rect(aes(xmin = -0.05, xmax = pred_point_hosp(), ymin = -0.0, ymax = 1.0, fill = "Data Used to Predict"), alpha = 0.1) +
            scale_fill_manual(values = c("Data Used to Predict" = "steelblue")) +
            geom_point(aes(x = outcome_fup, y = predictions, color = "Model Prediction"), size = 3.5) +
            geom_line(aes(x = outcome_fup, y = predictions), color = "red", linetype = "dashed", alpha = 0.8) +
            geom_point(data = observed_dat, aes(x = outcome_fup, y = predictions, color = "Observed"), size = 3.5, pch = 15) +
                geom_text_repel(aes(x = outcome_fup, y = predictions, label = round(predictions,2)), color = "red",
                                box.padding = 0.5, min.segment.length = 0.75) +
            scale_color_manual(values = c("Model Prediction" = "red", "Observed" = "black")) +
            theme_bw() +
            scale_y_continuous(limits = c(-0.0, 1.0), breaks = c(0, 0.25, 0.5, 0.75, 1.0), 
                               labels = c("0.0", "0.25", "0.5", "0.75", "1.0")) +
            scale_x_continuous(breaks = c(observed_dat$outcome_fup, prediction_df_hosp()$outcome_fup)) +
            labs(x = "Follow-up (months)", y = "Predicted Probability Positive",
                 title = "Hospitalization Predictions for Next Year",
                 subtitle = "Click on Prediction for Model Information",
                 color = "",
                 fill = "") +
            theme(legend.position = "bottom",
                  panel.grid.minor = element_blank()) +
            guides(color = guide_legend(override.aes = list(pch = c(16, 15))),
                   fill = guide_legend(override.aes = list(alpha = 0.9)))
        })
        hosp_plot()
    })
    
    make_horizontal_graph <- function(out_list, t){
        out_list[[1]] + out_list[[2]] + out_list[[3]] +
            plot_annotation(title = t)
    }
    
    output$edu_out_plot <- renderPlot({
        edu_outs
    })
    
    output$hosp_out_plot <- renderPlot({
        hosp_outs
    })
    
    output$edu_pred0_out3 <- renderPlot({
        make_horizontal_graph(edu_pred0_out3, "Edu/Work at 3 Months Predicted by Baseline")
    })
    
    output$edu_pred0_out6 <- renderPlot({
        make_horizontal_graph(edu_pred0_out6, "Edu/Work at 6 Months Predicted by Baseline")
    })
    
    output$edu_pred3_out6 <- renderPlot({
        make_horizontal_graph(edu_pred3_out6, "Edu/Work at 6 Months Predicted by Baseline to 3-Months")
        })
    
    output$edu_pred0_out9 <- renderPlot({
        make_horizontal_graph(edu_pred0_out9, "Edu/Work at 9 Months Predicted by Baseline")
        })
    
    output$edu_pred3_out9 <- renderPlot({
        make_horizontal_graph(edu_pred3_out9, "Edu/Work at 9 Months Predicted by Baseline to 3-Months")
    })
    
    output$edu_pred6_out9 <- renderPlot({
        make_horizontal_graph(edu_pred6_out9, "Edu/Work at 9 Months Predicted by Baseline to 6-Months")
    })
    
    #12
    output$edu_pred0_out12 <- renderPlot({
        make_horizontal_graph(edu_pred0_out12, "Edu/Work at 12 Months Predicted by Baseline")
    })
    
    output$edu_pred3_out12 <- renderPlot({
        make_horizontal_graph(edu_pred3_out12, "Edu/Work at 12 Months Predicted by Baseline to 3-Months")
    })
    
    output$edu_pred6_out12 <- renderPlot({
        make_horizontal_graph(edu_pred6_out12, "Edu/Work at 12 Months Predicted by Baseline to 6-Months")
    })
    
    output$edu_pred9_out12 <- renderPlot({
        make_horizontal_graph(edu_pred9_out12, "Edu/Work at 12 Months Predicted by Baseline to 9-Months")
    })
    
    #15
    
    output$edu_pred0_out15 <- renderPlot({
        make_horizontal_graph(edu_pred0_out15, "Edu/Work at 15 Months Predicted by Baseline")
    })
    
    output$edu_pred3_out15 <- renderPlot({
        make_horizontal_graph(edu_pred3_out15, "Edu/Work at 15 Months Predicted by Baseline to 3-Months")
    })
    
    output$edu_pred6_out15 <- renderPlot({
        make_horizontal_graph(edu_pred6_out15, "Edu/Work at 15 Months Predicted by Baseline to 6-Months")
    })
    
    output$edu_pred9_out15 <- renderPlot({
        make_horizontal_graph(edu_pred9_out15, "Edu/Work at 15 Months Predicted by Baseline to 9-Months")
    })
    
    output$edu_pred12_out15 <- renderPlot({
        make_horizontal_graph(edu_pred12_out15, "Edu/Work at 15 Months Predicted by Baseline to 12-Months")
    })
    
    #18
    
    output$edu_pred0_out18 <- renderPlot({
        make_horizontal_graph(edu_pred0_out18, "Edu/Work at 18 Months Predicted by Baseline")
    })
    
    output$edu_pred3_out18 <- renderPlot({
        make_horizontal_graph(edu_pred3_out18, "Edu/Work at 18 Months Predicted by Baseline to 3-Months")
    })
    
    output$edu_pred6_out18 <- renderPlot({
        make_horizontal_graph(edu_pred6_out18, "Edu/Work at 18 Months Predicted by Baseline to 6-Months")
    })
    
    output$edu_pred9_out18 <- renderPlot({
        make_horizontal_graph(edu_pred9_out18, "Edu/Work at 18 Months Predicted by Baseline to 9-Months")
    })
    
    output$edu_pred12_out18 <- renderPlot({
        make_horizontal_graph(edu_pred12_out18, "Edu/Work at 18 Months Predicted by Baseline to 12-Months")
    })
    
    #21
    
    output$edu_pred0_out21 <- renderPlot({
        make_horizontal_graph(edu_pred0_out21, "Edu/Work at 21 Months Predicted by Baseline")
    })
    
    output$edu_pred3_out21 <- renderPlot({
        make_horizontal_graph(edu_pred3_out21, "Edu/Work at 21 Months Predicted by Baseline to 3-Months")
    })
    
    output$edu_pred6_out21 <- renderPlot({
        make_horizontal_graph(edu_pred6_out21, "Edu/Work at 21 Months Predicted by Baseline to 6-Months")
    })
    
    output$edu_pred9_out21 <- renderPlot({
        make_horizontal_graph(edu_pred9_out21, "Edu/Work at 21 Months Predicted by Baseline to 9-Months")
    })
    
    output$edu_pred12_out21 <- renderPlot({
        make_horizontal_graph(edu_pred12_out21, "Edu/Work at 21 Months Predicted by Baseline to 12-Months")
    })
    
    #24
    
    output$edu_pred0_out24 <- renderPlot({
        make_horizontal_graph(edu_pred0_out24, "Edu/Work at 24 Months Predicted by Baseline")
    })
    
    output$edu_pred3_out24 <- renderPlot({
        make_horizontal_graph(edu_pred3_out24, "Edu/Work at 24 Months Predicted by Baseline to 3-Months")
    })
    
    output$edu_pred6_out24 <- renderPlot({
        make_horizontal_graph(edu_pred6_out24, "Edu/Work at 24 Months Predicted by Baseline to 6-Months")
    })
    
    output$edu_pred9_out24 <- renderPlot({
        make_horizontal_graph(edu_pred9_out24, "Edu/Work at 24 Months Predicted by Baseline to 9-Months")
    })
    
    output$edu_pred12_out24 <- renderPlot({
        make_horizontal_graph(edu_pred12_out24, "Edu/Work at 24 Months Predicted by Baseline to 12-Months")
    })
    
    # HOSP
    
    
    output$hosp_pred0_out3 <- renderPlot({
        make_horizontal_graph(hosp_pred0_out3, "Hospitalization at 3 Months Predicted by Baseline")
    })
    
    output$hosp_pred0_out6 <- renderPlot({
        make_horizontal_graph(hosp_pred0_out6, "Hospitalization at 6 Months Predicted by Baseline")
    })
    
    output$hosp_pred3_out6 <- renderPlot({
        make_horizontal_graph(hosp_pred3_out6, "Hospitalization at 6 Months Predicted by Baseline to 3-Months")
    })
    
    output$hosp_pred0_out9 <- renderPlot({
        make_horizontal_graph(hosp_pred0_out9, "Hospitalization at 9 Months Predicted by Baseline")
    })
    
    output$hosp_pred3_out9 <- renderPlot({
        make_horizontal_graph(hosp_pred3_out9, "Hospitalization at 9 Months Predicted by Baseline to 3-Months")
    })
    
    output$hosp_pred6_out9 <- renderPlot({
        make_horizontal_graph(hosp_pred6_out9, "Hospitalization at 9 Months Predicted by Baseline to 6-Months")
    })
    
    #12
    output$hosp_pred0_out12 <- renderPlot({
        make_horizontal_graph(hosp_pred0_out12, "Hospitalization at 12 Months Predicted by Baseline")
    })
    
    output$hosp_pred3_out12 <- renderPlot({
        make_horizontal_graph(hosp_pred3_out12, "Hospitalization at 12 Months Predicted by Baseline to 3-Months")
    })
    
    output$hosp_pred6_out12 <- renderPlot({
        make_horizontal_graph(hosp_pred6_out12, "Hospitalization at 12 Months Predicted by Baseline to 6-Months")
    })
    
    output$hosp_pred9_out12 <- renderPlot({
        make_horizontal_graph(hosp_pred9_out12, "Hospitalization at 12 Months Predicted by Baseline to 9-Months")
    })
    
    #15
    
    output$hosp_pred0_out15 <- renderPlot({
        make_horizontal_graph(hosp_pred0_out15, "Hospitalization at 15 Months Predicted by Baseline")
    })
    
    output$hosp_pred3_out15 <- renderPlot({
        make_horizontal_graph(hosp_pred3_out15, "Hospitalization at 15 Months Predicted by Baseline to 3-Months")
    })
    
    output$hosp_pred6_out15 <- renderPlot({
        make_horizontal_graph(hosp_pred6_out15, "Hospitalization at 15 Months Predicted by Baseline to 6-Months")
    })
    
    output$hosp_pred9_out15 <- renderPlot({
        make_horizontal_graph(hosp_pred9_out15, "Hospitalization at 15 Months Predicted by Baseline to 9-Months")
    })
    
    output$hosp_pred12_out15 <- renderPlot({
        make_horizontal_graph(hosp_pred12_out15, "Hospitalization at 15 Months Predicted by Baseline to 12-Months")
    })
    
    #18
    
    output$hosp_pred0_out18 <- renderPlot({
        make_horizontal_graph(hosp_pred0_out18, "Hospitalization at 18 Months Predicted by Baseline")
    })
    
    output$hosp_pred3_out18 <- renderPlot({
        make_horizontal_graph(hosp_pred3_out18, "Hospitalization at 18 Months Predicted by Baseline to 3-Months")
    })
    
    output$hosp_pred6_out18 <- renderPlot({
        make_horizontal_graph(hosp_pred6_out18, "Hospitalization at 18 Months Predicted by Baseline to 6-Months")
    })
    
    output$hosp_pred9_out18 <- renderPlot({
        make_horizontal_graph(hosp_pred9_out18, "Hospitalization at 18 Months Predicted by Baseline to 9-Months")
    })
    
    output$hosp_pred12_out18 <- renderPlot({
        make_horizontal_graph(hosp_pred12_out18, "Hospitalization at 18 Months Predicted by Baseline to 12-Months")
    })
    
    #21
    
    output$hosp_pred0_out21 <- renderPlot({
        make_horizontal_graph(hosp_pred0_out21, "Hospitalization at 21 Months Predicted by Baseline")
    })
    
    output$hosp_pred3_out21 <- renderPlot({
        make_horizontal_graph(hosp_pred3_out21, "Hospitalization at 21 Months Predicted by Baseline to 3-Months")
    })
    
    output$hosp_pred6_out21 <- renderPlot({
        make_horizontal_graph(hosp_pred6_out21, "Hospitalization at 21 Months Predicted by Baseline to 6-Months")
    })
    
    output$hosp_pred9_out21 <- renderPlot({
        make_horizontal_graph(hosp_pred9_out21, "Hospitalization at 21 Months Predicted by Baseline to 9-Months")
    })
    
    output$hosp_pred12_out21 <- renderPlot({
        make_horizontal_graph(hosp_pred12_out21, "Hospitalization at 21 Months Predicted by Baseline to 12-Months")
    })
    
    #24
    
    output$hosp_pred0_out24 <- renderPlot({
        make_horizontal_graph(hosp_pred0_out24, "Hospitalization at 24 Months Predicted by Baseline")
    })
    
    output$hosp_pred3_out24 <- renderPlot({
        make_horizontal_graph(hosp_pred3_out24, "Hospitalization at 24 Months Predicted by Baseline to 3-Months")
    })
    
    output$hosp_pred6_out24 <- renderPlot({
        make_horizontal_graph(hosp_pred6_out24, "Hospitalization at 24 Months Predicted by Baseline to 6-Months")
    })
    
    output$hosp_pred9_out24 <- renderPlot({
        make_horizontal_graph(hosp_pred9_out24, "Hospitalization at 24 Months Predicted by Baseline to 9-Months")
    })
    
    output$hosp_pred12_out24 <- renderPlot({
        make_horizontal_graph(hosp_pred12_out24, "Hospitalization at 24 Months Predicted by Baseline to 12-Months")
    })
    
    

    output$id_info <- renderText({
        str_c("You are using data from up to the ",
              pred_point(), "-Month follow-up to predict Education/Work at ",
              pred_point() + 3, "-months, ", pred_point() + 6, "-months, ", pred_point() + 9,
              "-months, and ", pred_point() + 12, "-months for the client with ID = ", input$selected_id, ".")
    })
    
    output$id_info_2 <- renderText({
        str_c("You are using data from up to the ",
              pred_point_2(), "-Month follow-up to predict Education/Work at ",
              pred_point_2() + 3, "-months, ", pred_point() + 6, "-months, ", pred_point() + 9,
              "-months, and ", pred_point_2() + 12, "-months for the client with ID = ", input$selected_id_2, ".")
    })
    
    output$id_info_hosp <- renderText({
        str_c("You are using data from up to the ",
              pred_point_hosp(), "-Month follow-up to predict Hospitalization at ",
              pred_point_hosp() + 3, "-months, ", pred_point_hosp() + 6, "-months, ", pred_point_hosp() + 9,
              "-months, and ", pred_point_hosp() + 12, "-months for the client with ID = ", input$selected_id, ".")
    })
    
    
    output$edu_plotclick_mod <- renderPlot({
        #make_plot <- reactive({
            #req(input$eduplot_click)
            df <- nearPoints(prediction_df(), input$eduplot_click, maxpoints = 1, threshold = 30,
                             xvar = "outcome_fup", yvar = "predictions") %>%
                select(outcome_fup, predictor_fup)
            
            g_name <- str_c("edu_pred", df$predictor_fup, "_out", df$outcome_fup)
            
            out_name <- case_when(df$predictor_fup == 0 ~ "Baseline",
                                  TRUE ~ str_c(df$predictor_fup, " Months"))
            
            make_horizontal_graph(get(g_name), str_c("Edu/Work at ", df$outcome_fup, " Months Predicted by up to ", out_name))
            
            
       # })
        
        #make_plot()
        
        
    })
    
    output$hosp_plotclick_mod <- renderPlot({
        
        #make_plot <- reactive({
            #req(input$hospplot_click)
            df <- nearPoints(prediction_df_hosp(), input$hospplot_click, maxpoints = 1, threshold = 30,
                             xvar = "outcome_fup", yvar = "predictions") %>%
                select(outcome_fup, predictor_fup)
            
            g_name <- str_c("hosp_pred", df$predictor_fup, "_out", df$outcome_fup)
            
            out_name <- case_when(df$predictor_fup == 0 ~ "Baseline",
                                  TRUE ~ str_c(df$predictor_fup, " Months"))
            
            make_horizontal_graph(get(g_name), str_c("Hospitalization at ", df$outcome_fup, " Months Predicted by up to ", out_name))
            
            
            
        # })
        # make_plot()
        
        
        
    })
    

    
    
})
