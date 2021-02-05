library(shiny)
library(shinydashboard)
library(shinythemes)

#Esta parte es el análogo al ui.R
ui <- 
    
    fluidPage(
        
        dashboardPage(
            
            dashboardHeader(title = "Basic dashboard"),
            
            dashboardSidebar(
                
                sidebarMenu(
                    menuItem("Histograma", tabName = "Dashboard", icon = icon("dashboard")),
                    menuItem("Imagenes postwork 3", tabName = "img1", icon = icon("file-picture-o")),
                    menuItem("Factores de ganancia", tabName = "img2", icon = icon("file-picture-o")),
                    menuItem("Dispersión", tabName = "graph", icon = icon("area-chart")),
                    menuItem("Data Table", tabName = "data_table", icon = icon("table"))
                )
                
            ),
            
            dashboardBody(
                
                tabItems(
                    
                    # Histograma
                    tabItem(tabName = "Dashboard",
                            fluidRow(
                                titlePanel("Histograma de las variables del data set mtcars"), 
                                selectInput("x", "Seleccione el valor de X",
                                            choices = names(mtcars)),
                                
                                selectInput("zz", "Selecciona la variable del grid", 
                                            
                                            choices = c("cyl", "vs", "gear", "carb")),
                                box(plotOutput("plot1", height = 250)),
                                
                                box(
                                    title = "Controls",
                                    sliderInput("bins", "Number of observations:", 1, 30, 15)
                                )
                            )
                    ),
                    
                    # Dispersión
                    tabItem(tabName = "graph", 
                            fluidRow(
                                titlePanel(h3("Gráficos de dispersión")),
                                selectInput("a", "Selecciona el valor de x",
                                            choices = names(mtcars)),
                                selectInput("y", "Seleccione el valor de y",
                                            choices = names(mtcars)),
                                selectInput("z", "Selecciona la variable del grid", 
                                            choices = c("cyl", "vs", "gear", "carb")),
                                box(plotOutput("output_plot", height = 300, width = 460) )
                                
                            )
                    ),
                    
                    
                    
                    tabItem(tabName = "data_table",
                            fluidRow(        
                                titlePanel(h3("Data Table")),
                                dataTableOutput ("data_table")
                            )
                    ), 
                    
                    tabItem(tabName = "img1",
                            selectInput("img_p3", "Selecciona la imagen que quieres ver",
                                        choices = c('Equipo de casa', 'Equipo visitante', 'Probabilidades conjuntas')),
                            fluidRow(
                                titlePanel(h3("Postwork 3")),
                                imageOutput("img1")
                            )
                    ),
                    
                    tabItem(tabName = "img2",
                            selectInput("factores_ganancia", "Selecciona la imagen que quieres ver",
                                        choices = c('Momio maximo', 'Momio promedio')),
                            fluidRow(
                                titlePanel(h3("Imagen de los factores de ganancia")),
                                imageOutput("img2")
                            )
                    )
                    
                )
            )
        )
    )

#De aquí en adelante es la parte que corresponde al server

server <- function(input, output) {
    library(ggplot2)
    
    #Gráfico de Histograma
    output$plot1 <- renderPlot({
        
        x <- mtcars[,input$x]
        bin <- seq(min(x), max(x), length.out = input$bins + 1)
        
        ggplot(mtcars, aes(x, fill = mtcars[,input$zz])) + 
            geom_histogram( breaks = bin) +
            labs( xlim = c(0, max(x))) + 
            theme_light() + 
            xlab(input$x) + ylab("Frecuencia") + 
            facet_grid(input$zz)
        
        
    })
    
    # Gráficas de dispersión
    output$output_plot <- renderPlot({ 
        
        ggplot(mtcars, aes(x =  mtcars[,input$a] , y = mtcars[,input$y], 
                           colour = mtcars[,input$z] )) + 
            geom_point() +
            ylab(input$y) +
            xlab(input$a) + 
            theme_linedraw() + 
            facet_grid(input$z)  #selección del grid
        
    })   
    
    #Data Table
    output$data_table <- renderDataTable( {read.csv('match.data.csv')}, 
          options = list(aLengthMenu = c(5,25,50),
                         iDisplayLength = 5)
    )
    
    output$img1 <- renderImage({
        if(input$img_p3 == "Equipo de casa"){            
            list(src = "www/pw3_01.png", height = 350, width = 500)
        }     
        else if(input$img_p3 == "Equipo visitante"){            
            list(src = "www/pw3_02.png", height = 350, width = 500)
        }                                   
        else{
            list(src = "www/pw3_03.png", height = 350, width = 500)
        }
    }, deleteFile=FALSE)
    
    output$img2 <- renderImage({
        if(input$factores_ganancia == "Momio maximo"){            
            list(src = "www/graf1.png", height = 350, width = 500)
        }                                        
        else{
            list(src = "www/graf2.png", height = 350, width = 500)
        }
    }, deleteFile=FALSE)
    
    
}


shinyApp(ui, server)
