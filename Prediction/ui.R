library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(readr)
library(purrr)
library(caret)
library(tibble)
library(ggalluvial)
library(shiny)

shinyUI(navbarPage("OTNY Prediction",
                   
                   tabPanel("Overview", fluid = TRUE,
                            includeMarkdown("information.md")
                   ),
                
                 tabPanel("Edu/Work Predictions",
                          titlePanel("Education/Work Predictions"),
                          tabsetPanel(
                          tabPanel("Predict Next Four Time Points",
                          sidebarLayout(
                              sidebarPanel(
                                  
                                  helpText("This page allows you to select the amount of data you want to use to predict the next year (four visits) of Education/Work for an individual client."),
                                  hr(),
                                  helpText("First select the latest follow-up point you would like to use to make the predictions. The models will use data from Baseline to your selected follow-up point to make predictions."),
                                  
                                  
                                  
                                  #wellPanel(
                                      selectInput(inputId = "pred_point", "Use data up to",
                                                  choices = pred_options),
                                  hr(),
                                  helpText("Then select a client with data up to the time point you have selected and the graph will show you model predictions for the next four follow-up points in red."),
                                  
                                 
                                  # wellPanel(
                                     uiOutput("secondSelect"),
                                  
                                  hr(),
                            
                                  textOutput("id_info") 
                              ),
                              mainPanel(
                                hr(),
                                tags$style(type="text/css",
                                           ".shiny-output-error { visibility: hidden; }",
                                           ".shiny-output-error:before { visibility: hidden; }"
                                ),
                                  plotOutput(outputId = "eduwork_plot", height = 500, width = 750, click = "eduplot_click"),
                                plotOutput("edu_plotclick_mod", height = 400, width = 1000)
                              )
                              
                          )
                          
                          
                 ),
                 tabPanel("Updating Predictions",
                          sidebarLayout(
                            sidebarPanel(
                              
                              helpText("This page allows you to select a time point to predict Education/Work for an individual client using models that use increasing number of follow-up data points."),
                              hr(),
                              helpText("First select the follow-up point you would like to predict Education/Work. The models will use data from Baseline to your selected follow-up point to make predictions."),
                              
                              
                              
                              selectInput(inputId = "pred_point_2", "Predict Education/Work at",
                                          choices = pred_options_2),
                              hr(),
                              helpText("Then select a client with and the graph will show you model predictions for the time point you selected using increasing amounts of follow-up visits."),
                              
                              
                              uiOutput("secondSelect_2"),
                              
                              hr(),
                              checkboxInput("edu_show_out", label = "Show Outcome", value = FALSE)
                              
                            ),
                            mainPanel(
                              hr(),
                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"
                              ),
                              plotOutput(outputId = "eduwork_plot_2", inline = TRUE)
                            )
                            
                          )
                          
                          
                          
                          
                          
                          
                          )
                 )
                 )
                 ,
                 tabPanel("Hospitalization Predictions",
                          titlePanel("Hospitalization Predictions"),
                          sidebarLayout(
                              sidebarPanel(
                                  
                                  helpText("This page allows you to select the amount of data you want to use to predict the next year (four visits) of Hospitalization for an individual client."),
                                  hr(),
                                  helpText("First select the latest follow-up point you would like to use to make the predictions. The models will use data from Baseline to your selected follow-up point to make predictions."),
                                  
                                  #wellPanel(
                                  selectInput(inputId = "pred_point_hosp", "Use data up to",
                                              choices = pred_options),
                                  
                                  hr(),
                                  helpText("Then select a client with data up to the time point you have selected and the graph will show you model predictions for the next four follow-up points in red."),
                                  
                                  # wellPanel(
                                  uiOutput("secondSelect_hosp"),
                                  hr(),
                                  
                                  textOutput("id_info_hosp") 
                              ),
                              mainPanel(
                                  plotOutput(outputId = "hosp_plot", height = 500, width = 750, click = "hospplot_click"),
                                  plotOutput(outputId = "hosp_plotclick_mod", height = 400, width = 1000)
                              )
                          )


                 ),
                 tabPanel("Two Year Plots", fluid = TRUE,
                          titlePanel("Two Year Outcome Sankey Plots"),
                          tabsetPanel(
                              tabPanel("Education/Work", fluid = TRUE,
                                       fluidRow(
                                           column(12,
                                                  plotOutput(outputId = "edu_out_plot", height = 700, width = 900)
                                           )
                                       )
                              ),
                              tabPanel("Hospitalization", fluid = TRUE,
                                       fluidRow(
                                           column(12,
                                                  plotOutput(outputId = "hosp_out_plot", height = 700, width = 900)
                                           )
                                       )
                              )
                          )
                 )
                 ,
                 tabPanel("Model Descriptions", fluid = TRUE,
                          titlePanel("Model Details"),
                          tabsetPanel(
                            tabPanel("Education/Work Models", fluid = TRUE,
                                     titlePanel("Education/Work Models"),
                                     tabsetPanel(
                                     tabPanel("Predicting 3-Month", fluid = TRUE,
                                              fluidRow(
                                                column(12,
                                                       plotOutput(outputId = "edu_pred0_out3", height = 400, width = 1200)
                                                )
                                                
                                              )),
                                     tabPanel("Predicting 6-Month", fluid = TRUE,
                                              fluidRow(
                                                column(12,
                                                       plotOutput(outputId = "edu_pred0_out6", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred3_out6", height = 400, width = 1200)
                                                )
                                              )),
                                     tabPanel("Predicting 9-Month", fluid = TRUE,
                                              fluidRow(
                                                column(12,
                                                       plotOutput(outputId = "edu_pred0_out9", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred3_out9", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred6_out9", height = 400, width = 1200)
                                                       
                                                )
                                              )
                                              ),
                                     tabPanel("Predicting 12-Month", fluid = TRUE,
                                              fluidRow(
                                                column(12,
                                                       plotOutput(outputId = "edu_pred0_out12", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred3_out12", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred6_out12", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred9_out12", height = 400, width = 1200)
                                                )
                                              )
                                              ),
                                     tabPanel("Predicting 15-Month", fluid = TRUE,
                                              fluidRow(
                                                column(12,
                                                       plotOutput(outputId = "edu_pred0_out15", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred3_out15", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred6_out15", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred9_out15", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred12_out15", height = 400, width = 1200)
                                                       
                                                )
                                              )
                                              ),
                                     tabPanel("Predicting 18-Month", fluid = TRUE,
                                              fluidRow(
                                                column(12,
                                                       plotOutput(outputId = "edu_pred0_out18", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred3_out18", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred6_out18", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred9_out18", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred12_out18", height = 400, width = 1200)
                                                       
                                                )
                                              )
                                              ),
                                     tabPanel("Predicting 21-Month", fluid = TRUE,
                                              fluidRow(
                                                column(12,
                                                       plotOutput(outputId = "edu_pred0_out21", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred3_out21", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred6_out21", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred9_out21", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred12_out21", height = 400, width = 1200)
                                                       
                                                )
                                              )),
                                     tabPanel("Predicting 24-Month", fluid = TRUE,
                                              fluidRow(
                                                column(12,
                                                       plotOutput(outputId = "edu_pred0_out24", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred3_out24", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred6_out24", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred9_out24", height = 400, width = 1200),
                                                       plotOutput(outputId = "edu_pred12_out24", height = 400, width = 1200)
                                                )
                                              )

                                     )
                          )
                            )
                          ,
                          tabPanel("Hospitalization Models", fluid = TRUE,
                                   titlePanel("Hospitalization Models"),
                                   tabsetPanel(
                                     tabPanel("Predicting 3-Month", fluid = TRUE,
                                              fluidRow(
                                                column(12,
                                                       plotOutput(outputId = "hosp_pred0_out3", height = 400, width = 1200)
                                                )
                                                
                                              )),
                                     tabPanel("Predicting 6-Month", fluid = TRUE,
                                              fluidRow(
                                                column(12,
                                                       plotOutput(outputId = "hosp_pred0_out6", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred3_out6", height = 400, width = 1200)
                                                )
                                              )),
                                     tabPanel("Predicting 9-Month", fluid = TRUE,
                                              fluidRow(
                                                column(12,
                                                       plotOutput(outputId = "hosp_pred0_out9", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred3_out9", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred6_out9", height = 400, width = 1200)
                                                       
                                                )
                                              )
                                     ),
                                     tabPanel("Predicting 12-Month", fluid = TRUE,
                                              fluidRow(
                                                column(12,
                                                       plotOutput(outputId = "hosp_pred0_out12", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred3_out12", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred6_out12", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred9_out12", height = 400, width = 1200)
                                                )
                                              )
                                     ),
                                     tabPanel("Predicting 15-Month", fluid = TRUE,
                                              fluidRow(
                                                column(12,
                                                       plotOutput(outputId = "hosp_pred0_out15", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred3_out15", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred6_out15", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred9_out15", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred12_out15", height = 400, width = 1200)
                                                       
                                                )
                                              )
                                     ),
                                     tabPanel("Predicting 18-Month", fluid = TRUE,
                                              fluidRow(
                                                column(12,
                                                       plotOutput(outputId = "hosp_pred0_out18", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred3_out18", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred6_out18", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred9_out18", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred12_out18", height = 400, width = 1200)
                                                       
                                                )
                                              )
                                     ),
                                     tabPanel("Predicting 21-Month", fluid = TRUE,
                                              fluidRow(
                                                column(12,
                                                       plotOutput(outputId = "hosp_pred0_out21", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred3_out21", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred6_out21", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred9_out21", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred12_out21", height = 400, width = 1200)
                                                       
                                                )
                                              )),
                                     tabPanel("Predicting 24-Month", fluid = TRUE,
                                              fluidRow(
                                                column(12,
                                                       plotOutput(outputId = "hosp_pred0_out24", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred3_out24", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred6_out24", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred9_out24", height = 400, width = 1200),
                                                       plotOutput(outputId = "hosp_pred12_out24", height = 400, width = 1200)
                                                )
                                              )
                                              
                                     )

                                   )
                          
                          )

)
),
            tabPanel("Update Log", fluid = TRUE,
                     includeMarkdown("update.md")
                     )
)
)

