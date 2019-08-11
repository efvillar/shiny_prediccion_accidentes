library(shiny)
library(plotly)
library(caret)
#library(shinydashboard)
ui <- fluidPage(
  
  tags$h1("Predicción de Accidentes de Tránsito en Medellín"),
  tags$hr(),
  tags$p(strong("Edwin Villarraga-Jorge Montoya-Juan David Arango")),
  tags$p(em("Con datos de accidentalidad en Medellín historícos, se realizan predicciones de accidentres para los años 2019, 2020 y 2021.  Los accidntes se clasifican como accidentes totales, accidentes leves y accidentes graves.  Los accidentes leves solo involucran datos materiales y los accidentes graves implican lesiones personales y muertos")),
  tags$br(),
  
  
  mainPanel(
    
    
    tabsetPanel(type = "tabs",
                
                tabPanel("Pronósticos Diarios",
                         
                         plotlyOutput("plot_pronostico_D_T"),
                         plotlyOutput("plot_pronostico_D_L"),
                         plotlyOutput("plot_pronostico_D_G"),
                         DT::dataTableOutput('Pronosticos_D')),# Data as datatable
                
                tabPanel("Pronósticos Semanales",
                         
                         plotlyOutput("plot_pronostico_S_T"),
                         plotlyOutput("plot_pronostico_S_L"),
                         plotlyOutput("plot_pronostico_S_G"),
                         DT::dataTableOutput('Pronosticos_S')),# Data as datatable                       
                
                tabPanel("Pronósticos Mensuales",
                         
                         plotlyOutput("plot_pronostico_M_T"),
                         plotlyOutput("plot_pronostico_M_L"),
                         plotlyOutput("plot_pronostico_M_G"),
                         DT::dataTableOutput('Pronosticos_M')),# Data as datatable                       
                
                
                
                
                tabPanel("Gráfica Datos Históricos", 
                         plotlyOutput("Serie_Diaria_Total"),
                         plotlyOutput("Serie_Diaria_AG"),
                         plotlyOutput("Serie_diaria_AL"),
                         
                         plotlyOutput("Serie_semanal_Total"),
                         plotlyOutput("Serie_semanal_AG"),
                         plotlyOutput("Serie_semanal_AL"),
                         plotlyOutput("Serie_mensual_Total"),
                         plotlyOutput("Serie_mensual_AG"),
                         plotlyOutput("Serie_mensual_AL")),                        
                
                
                
                
                #tabPanel("Resumen Modelos Diarios",
                #h5(textOutput("Modelo Diario para Accidentes Totales")),
                #verbatimTextOutput("text_model_D_T"),
                #verbatimTextOutput("summary_D_T"), # Model output
                
                #verbatimTextOutput("text_model_D_L"),
                #verbatimTextOutput("summary_D_L"),
                
                #verbatimTextOutput("text_model_D_G"),
                #verbatimTextOutput("summary_D_G")                               
                
                
                #), # Model output
                
                
                
                tabPanel("Resumen Modelos Semanales",
                         #h5(textOutput("Modelo Semanal para Accidentes Totales")),
                         verbatimTextOutput("text_model_S_T"),
                         verbatimTextOutput("summary_S_T"), # Model output
                         
                         verbatimTextOutput("text_model_S_L"),
                         verbatimTextOutput("summary_S_L"),
                         
                         verbatimTextOutput("text_model_S_G"),
                         verbatimTextOutput("summary_S_G")),
                
                
                tabPanel("Resumen Modelos Mensuales",
                         #h5(textOutput("Modelo Mensual para Accidentes Totales")),
                         verbatimTextOutput("text_model_M_T"),
                         verbatimTextOutput("summary_M_T"), # Model output
                         
                         verbatimTextOutput("text_model_M_L"),
                         verbatimTextOutput("summary_M_L"),
                         
                         verbatimTextOutput("text_model_M_G"),
                         verbatimTextOutput("summary_M_G")),                        
                
                
                
                
                tabPanel("Data Histórica Diaria ", DT::dataTableOutput('tbl_D')), # Data as datatable
                tabPanel("Data Histórica Semanal", DT::dataTableOutput('tbl')), # Data as datatable
                tabPanel("Data Histórica Mensual", DT::dataTableOutput('tbl_M')) # Data as datatable                       
                
    )
  )
)



# SERVER
server <- function(input, output) {
  
  
  
  #load data histórica diaria
  load(file="Total_Dataset_Freq_diaria.Rda")
  Data_Historica_Diaria<-Total_Dataset_Freq[,c("FECHA","ACCIDENTES_GRAVES","ACCIDENTES_LEVES","TOTAL_ACCIDENTES","ANO","SEMANA","MES","DIA")]
  #load data historica Semanal
  load(file="Total_Dataset_Freq_S_semanal.Rda")
  XY_Sem_shiny <- Total_Dataset_Freq_S
  #load data historica Semanal
  load(file="Total_Dataset_Freq_M_mensual.Rda")
  XY_Mes_shiny <- Total_Dataset_Freq_M[,c("ANO","MES","ACCIDENTES_GRAVES","ACCIDENTES_LEVES","TOTAL_ACCIDENTES")]        
  
  
  
  #grafica historica diaria Total Accidentes
  output$Serie_Diaria_Total <- renderPlotly({plot_ly(data=Data_Historica_Diaria,
                                                     x = ~FECHA,
                                                     y = ~TOTAL_ACCIDENTES,
                                                     #color = ~ANO,
                                                     #split = ~ANO,
                                                     type = "scatter" ,mode = "lines",
                                                     line=list(width=1,color='rgb(80, 80, 80)'))%>%
      
      layout(title='Accidentalidad: Accidentes TOTALES Diarios - Datos Históricos Medellín',
             xaxis=list(title="FECHA"),
             yaxis=list(title="Accidentes Totales"))  })
  
  #grafica historica Diaria Accidentes Graves plotly
  output$Serie_Diaria_AG <- renderPlotly({plot_ly(data=Data_Historica_Diaria,
                                                  x = ~FECHA,
                                                  y = ~ACCIDENTES_GRAVES,
                                                  #color = ~ANO,
                                                  #split = ~ANO,
                                                  type = "scatter" ,mode = "lines",
                                                  line=list(width=1,color='rgb(90, 20, 120)'))%>%
      
      layout(title='Accidentalidad: Accidentes GRAVES Diarios - Datos Históricos Medellín',
             xaxis=list(title="SEMANA"),
             yaxis=list(title="Accidentes Graves"))  })
  
  #grafica historica diaria Accidentes Leves plotly
  output$Serie_diaria_AL <- renderPlotly({plot_ly(data=Data_Historica_Diaria,
                                                  x = ~FECHA,
                                                  y = ~ACCIDENTES_LEVES,
                                                  #color = ~ANO,
                                                  #split = ~ANO,
                                                  type = "scatter" ,mode = "lines",
                                                  line=list(width=1,color='rgb(140, 20, 40)'))%>%
      
      layout(title='Accidentalidad: Accidentes LEVES Diarios - Datos Históricos Medellín',
             xaxis=list(title="FECHA"),
             yaxis=list(title="Accidentes Leves"))  })     
  
  
  
  
  
  #grafica historica semanal Total plotly
  output$Serie_semanal_Total <- renderPlotly({plot_ly(data=XY_Sem_shiny,
                                                      x = ~SEMANA,
                                                      y = ~TOTAL_ACCIDENTES,
                                                      color = ~ANO,
                                                      split = ~ANO,
                                                      type = "scatter" ,mode = "lines",
                                                      line=list(width=1,color='rgb(90, 20, 120)'))%>%
      
      layout(title='Accidentalidad Accidentes TOTALES Semanal Histórica Medellín',
             xaxis=list(title="SEMANA"),
             yaxis=list(title="Accidentes Graves"))  })
  
  
  
  
  #grafica historica semanal Accidentes Graves plotly
  output$Serie_semanal_AG <- renderPlotly({plot_ly(data=XY_Sem_shiny,
                                                   x = ~SEMANA,
                                                   y = ~ACCIDENTES_GRAVES,
                                                   color = ~ANO,
                                                   split = ~ANO,
                                                   type = "scatter" ,mode = "lines",
                                                   line=list(width=1,color='rgb(90, 20, 120)'))%>%
      
      layout(title='Accidentalidad Accidentes Graves Semanal Historica Medellin',
             xaxis=list(title="SEMANA"),
             yaxis=list(title="Accidentes Graves"))  })
  
  
  
  #grafica historica semanal Accidentes Leves plotly
  output$Serie_semanal_AL <- renderPlotly({plot_ly(data=XY_Sem_shiny,
                                                   x = ~SEMANA,
                                                   y = ~ACCIDENTES_LEVES,
                                                   color = ~ANO,
                                                   split = ~ANO,
                                                   type = "scatter" ,mode = "lines",
                                                   line=list(width=1,color='rgb(140, 20, 40)'))%>%
      
      layout(title='Accidentalidad Accidentes Leves Semanal Historica Medellin',
             xaxis=list(title="SEMANA"),
             yaxis=list(title="Accidentes Leves"))  }) 
  
  
  
  #grafica historica mensual Total plotly
  output$Serie_mensual_Total <- renderPlotly({plot_ly(data=XY_Mes_shiny,
                                                      x = ~MES,
                                                      y = ~TOTAL_ACCIDENTES,
                                                      color = ~ANO,
                                                      split = ~ANO,
                                                      type = "scatter" ,mode = "lines",
                                                      line=list(width=1,color='rgb(80, 80, 80)'))%>%
      
      layout(title='Accidentalidad Total Mensual Histórica Medellin',
             xaxis=list(title="MES"),
             yaxis=list(title="Accidentes Totales"))  })
  
  
  
  
  #grafica historica Mensual Accidentes Graves plotly
  output$Serie_mensual_AG <- renderPlotly({plot_ly(data=XY_Mes_shiny,
                                                   x = ~MES,
                                                   y = ~ACCIDENTES_GRAVES,
                                                   color = ~ANO,
                                                   split = ~ANO,
                                                   type = "scatter" ,mode = "lines",
                                                   line=list(width=1,color='rgb(90, 20, 120)'))%>%
      
      layout(title='Accidentalidad Accidentes Graves Mensual Historica Medellin',
             xaxis=list(title="SMES"),
             yaxis=list(title="Accidentes Graves"))  })
  
  
  
  #grafica historica Mensual Accidentes Leves plotly
  output$Serie_mensual_AL <- renderPlotly({plot_ly(data=XY_Mes_shiny,
                                                   x = ~MES,
                                                   y = ~ACCIDENTES_LEVES,
                                                   color = ~ANO,
                                                   split = ~ANO,
                                                   type = "scatter" ,mode = "lines",
                                                   line=list(width=1,color='rgb(140, 20, 40)'))%>%
      
      layout(title='Accidentalidad Accidentes Leves Mensual Historica Medellin',
             xaxis=list(title="MES"),
             yaxis=list(title="Accidentes Leves"))  })       
  
  
  
  
  
  
  
  
  
  
  
  
  # Data historica TABLA
  
  #SEMANAL
  output$tbl <- DT::renderDataTable({
    XY_Sem_shiny
  }, options = list(aLengthMenu = c(5,25,50),
                    iDisplayLength = 5)
  )
  
  #DIARIA
  output$tbl_D <- DT::renderDataTable({
    Data_Historica_Diaria
  }, options = list(aLengthMenu = c(5,25,50),
                    iDisplayLength = 5)
  )
  
  
  #MENSUAL
  output$tbl_M <- DT::renderDataTable({
    XY_Mes_shiny
  }, options = list(aLengthMenu = c(5,25,50),
                    iDisplayLength = 5)
  )
  
  
  
  
  # PREDICCION
  
  #MODELOS DIARIOS
  
  
  #Total Accidentes, Accidentes graves y  leves
  load(file="datos_pronostico_diario.Rda")
  datos_PD1 <- subset(datos_pronostico_diario, Fecha>="2019-01-1")
  
  model_D_T <- readRDS("Prediccion_Total_Diario.Rds")
  #datos_PD1$prediccion_Total_D<-predict(model_D_T,datos_PD1[,c("Ano_Base","DIA","SEMANA","Feriado_Lunes","Feriado_Otro","Madre","Semana_Santa","Viernes_Desp_Quincena_v2","Feria_Flores")])
  
  model_D_L <- readRDS("Prediccion_leves_Diario.Rds")
  #datos_PD1$prediccion_Leves_D<-predict(model_D_L,datos_PD1[,c("Ano_Base","DIA","SEMANA","Feriado_Lunes","Feriado_Otro","Madre","Semana_Santa","Viernes_Desp_Quincena_v2","Feria_Flores")])
  
  model_D_G <- readRDS("Prediccion_Grave_Diario.Rds")
  #datos_PD1$prediccion_Graves_D<-predict(model_D_G,datos_PD1[,c("Ano_Base","DIA","SEMANA","Feriado_Lunes","Feriado_Otro","Madre","Semana_Santa","Viernes_Desp_Quincena_v2","Feria_Flores")])
  
  
  #TABLA PRONOSTICOS DIARIOS
  
  output$Pronosticos_D <- DT::renderDataTable({
    datos_PD1[,c(1,3,4,11,12,13)]
  }, options = list(aLengthMenu = c(5,25,50),
                    iDisplayLength = 5)
  )
  
  
  #Grafica diaria
  
  output$plot_pronostico_D_T <- renderPlotly({plot_ly(data=datos_PD1,
                                                      x = ~Fecha,
                                                      y = ~prediccion_Total,
                                                      #color = ~ANO,
                                                      #split = ~ANO,
                                                      type = "scatter" ,mode = "lines",
                                                      line=list(width=1,color='rgb(80, 80, 80)'))%>%
      
      layout(title='Pronóstico Diario Accidentes Totales Medellín',
             xaxis=list(title="FECHA"),
             yaxis=list(title="Accidentes Totales"))  })        
  
  output$plot_pronostico_D_L <- renderPlotly({plot_ly(data=datos_PD1,
                                                      x = ~Fecha,
                                                      y = ~prediccion_Leves,
                                                      #color = ~ANO,
                                                      #split = ~ANO,
                                                      type = "scatter" ,mode = "lines",
                                                      line=list(width=1,color='rgb(80, 160, 80)'))%>%
      
      layout(title='Pronóstico Diario Accidentes Leves Medellín',
             xaxis=list(title="FECHA"),
             yaxis=list(title="Accidentes Leves"))  })        
  
  output$plot_pronostico_D_G <- renderPlotly({plot_ly(data=datos_PD1,
                                                      x = ~Fecha,
                                                      y = ~prediccion_Graves,
                                                      #color = ~ANO,
                                                      #split = ~ANO,
                                                      type = "scatter" ,mode = "lines",
                                                      line=list(width=1,color='rgb(130, 10, 45)'))%>%
      
      layout(title='Pronóstico Diario Accidentes Graves Medellín',
             xaxis=list(title="FECHA"),
             yaxis=list(title="Accidentes Graves"))  })           
  
  
  
  #MODELOS SEMANALES
  
  load(file="./data_modelos_semana/datos_pronostico_semanal.Rda")
  datos_PS1 <- subset(datos_pronostico_semanal, ANO=="2019" | ANO=="2020" | ANO=="2021")
  
  model_S_T <- readRDS("./data_modelos_semana/Prediccion_Total_Semanal.Rds")
  #datos_PS1$prediccion_Total_S<-predict(model_S_T,datos_PS1[,c("Ano_Base","SEMANA","Feria_Flores_Semana","Semana_Santa_Semana","Feriados_Lunes","Feriados_Otros")])
  
  model_S_L <- readRDS("./data_modelos_semana/Prediccion_leves_Semanal.Rds")
  #datos_PS1$prediccion_Leves_S<-predict(model_S_L,datos_PS1[,c("Ano_Base","SEMANA","Feria_Flores_Semana","Semana_Santa_Semana","Feriados_Lunes","Feriados_Otros")])
  
  model_S_G <- readRDS("./data_modelos_semana/Prediccion_Grave_Semanal.Rds")
  #datos_PS1$prediccion_Graves_S<-predict(model_S_G,datos_PS1[,c("Ano_Base","SEMANA","Feria_Flores_Semana","Semana_Santa_Semana","Feriados_Lunes","Feriados_Otros")])
  
  
  #TABLA PRONOSTICOS SEMANALES
  
  output$Pronosticos_S <- DT::renderDataTable({
    datos_PS1[,c(1,3,9,10,11)]
  }, options = list(aLengthMenu = c(5,25,50),
                    iDisplayLength = 5)
  )        
  
  #Grafica semanal
  
  output$plot_pronostico_S_T <- renderPlotly({plot_ly(data=datos_PS1,
                                                      x = ~SEMANA,
                                                      y = ~prediccion_Total_s,
                                                      color = ~ANO,
                                                      split = ~ANO,
                                                      type = "scatter" ,mode = "lines",
                                                      line=list(width=1,color='rgb(90, 20, 120)'))%>%
      
      layout(title='Pronostico Accidentes Totales Semanales 2019 a 2021',
             xaxis=list(title="SEMANA"),
             yaxis=list(title="Accidentes Total"))  })    
  
  
  output$plot_pronostico_S_L <- renderPlotly({plot_ly(data=datos_PS1,
                                                      x = ~SEMANA,
                                                      y = ~prediccion_Leves_s,
                                                      color = ~ANO,
                                                      split = ~ANO,
                                                      type = "scatter" ,mode = "lines",
                                                      line=list(width=1,color='rgb(80, 160, 80)'))%>%
      
      layout(title='Pronostico Accidentes Leves  Semanales 2019 a 2021',
             xaxis=list(title="SEMANA"),
             yaxis=list(title="Accidentes Leves"))  })          
  
  
  
  output$plot_pronostico_S_G <- renderPlotly({plot_ly(data=datos_PS1,
                                                      x = ~SEMANA,
                                                      y = ~prediccion_Graves_s,
                                                      color = ~ANO,
                                                      split = ~ANO,
                                                      type = "scatter" ,mode = "lines",
                                                      line=list(width=1,color='rgb(130, 10, 45)'))%>%
      
      layout(title='Pronostico Accidentes Graves  Semanales 2019 a 2021',
             xaxis=list(title="SEMANA"),
             yaxis=list(title="Accidentes Graves"))  })          
  
  
  #MODELOS MENSUALES
  
  load(file="datos_pronostico_mensual.Rda")
  datos_PM1 <- subset(datos_pronostico_mensual, ANO=="2019" | ANO=="2020" | ANO=="2021")
  
  model_M_T <- readRDS("Prediccion_Total_Mensual.Rds")
  #datos_PM1$prediccion_Total_M<-predict(model_M_T,datos_PM1[,c("ANO","Ano_Base","MES","Feriados")],type="response")
  
  model_M_L <- readRDS("Prediccion_leves_Mensual.Rds")
  #datos_PM1$prediccion_Leves_M<-predict(model_M_L,datos_PM1[,c("ANO","Ano_Base","MES","Feriados")])
  
  model_M_G <- readRDS("Prediccion_Grave_Mensual.Rds")
  #datos_PM1$prediccion_Graves_M<-predict(model_M_G,datos_PM1[,c("ANO","Ano_Base","MES","Feriados")],type="response")
  
  
  #TABLA PRONOSTICOS SEMANALES
  
  output$Pronosticos_M <- DT::renderDataTable({
    datos_PM1[,c(1,3,5,6,7)]
  }, options = list(aLengthMenu = c(5,25,50),
                    iDisplayLength = 5)
  )                
  
  #Grafica mensual
  
  output$plot_pronostico_M_T <- renderPlotly({plot_ly(data=datos_PM1,
                                                      x = ~MES,
                                                      y = ~prediccion_Total_m,
                                                      color = ~ANO,
                                                      split = ~ANO,
                                                      type = "scatter" ,mode = "lines",
                                                      line=list(width=1,color='rgb(90, 20, 120)'))%>%
      
      layout(title='Pronostico Accidentes Totales Mensuales 2019 a 2021',
             xaxis=list(title="MES"),
             yaxis=list(title="Accidentes Totales"))  })    
  
  
  output$plot_pronostico_M_L <- renderPlotly({plot_ly(data=datos_PM1,
                                                      x = ~MES,
                                                      y = ~prediccion_Leves_m,
                                                      color = ~ANO,
                                                      split = ~ANO,
                                                      type = "scatter" ,mode = "lines",
                                                      line=list(width=1,color='rgb(80, 160, 80)'))%>%
      
      layout(title='Pronostico Accidentes Leves  MEnsuales 2019 a 2021',
             xaxis=list(title="MES"),
             yaxis=list(title="Accidentes Leves"))  })          
  
  
  
  output$plot_pronostico_M_G <- renderPlotly({plot_ly(data=datos_PM1,
                                                      x = ~MES,
                                                      y = ~prediccion_Graves_m,
                                                      color = ~ANO,
                                                      split = ~ANO,
                                                      type = "scatter" ,mode = "lines",
                                                      line=list(width=1,color='rgb(130, 10, 45)'))%>%
      
      layout(title='Pronostico Accidentes Graves  Mensuales 2019 a 2021',
             xaxis=list(title="MES"),
             yaxis=list(title="Accidentes Graves"))  })                   
  
}
shinyApp(ui = ui, server = server)

