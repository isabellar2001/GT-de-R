library(shiny)
library(tidyverse)
library(elo)
library(shinydashboard)
library(readxl)
library(plotly)
library(rpivotTable)
library(fontawesome)

base_dados_administrativos <- read_xlsx("base_dados_administrativos.xlsx", col_names = TRUE)

dados_gerais <- read_xlsx("base_municipio_rj.xlsx", col_names = TRUE, range = "A3:M95",
                          col_types = c("text", "text", "text",
                                        "text", "numeric", "numeric", "numeric",
                                        "numeric", "numeric", "numeric",
                                        "numeric", "numeric", "numeric"))
turmas <- read_xlsx("turmas.xlsx")

ui <- dashboardPage(
    dashboardHeader(title = "Educação - Niterói"),
    dashboardSidebar(
        sidebarMenu(
            #colocar aqui ideb, matrículas no ensino público e privado, evasão, idhm educação, comparar com outros munic
            menuItem("Dados Gerais",
                     tabName = "dados_gerais_tab",
                     icon = icon("fas fa-school")),
            #tabela com as turmas das escolas municipais, infraestrutura das escolas talvez
            menuItem("Turmas",
                     tabName = "turmas_tab",
                     icon = icon("fas fa-chalkboard")),
            #Qualificação dos professores, etc.
            menuItem("Docentes",
                     tabName = "docentes_tab",
                     icon = icon("fas fa-chalkboard-teacher"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dados_gerais_tab",
                    selectInput("publico_id", label = "Escola pública ou privada:",
                                choices = c("Público","Privada") %>%
                                    as.list()),
                    selectInput("etapa_id", label = "Etapa de ensino:",
                                choices = c("Ensino Fundamental", "Ensino Médio") %>%
                                    as.list()),
                    box(plotlyOutput("ideb_historico")),
                    box(plotlyOutput("percentual_matrículas")),
                    box(plotlyOutput("distorcao_idade")),
                    box(plotlyOutput("evasao"))
            ),
            tabItem(tabName = "turmas_tab",
                    box(dataTableOutput("tabela_niteroi")))

        )
    ))

server <- function(input, output) {
    output$ideb_historico <- renderPlotly({
        #pegar as metas aqui depois: http://ideb.inep.gov.br/resultado/home.seam?cid=9952733
        ideb_munic <- base_dados_administrativos %>%
            filter(NOME == "Niterói") %>%
            select(ANO, NOME, IDEB_AI) %>% na.omit() %>%
            pivot_longer(cols = -c(ANO,NOME),
                         names_to = "INDICADOR",
                         values_to = "NOTA") %>% mutate_if(is.numeric, as.integer)
        ideb_munic <- ideb_munic %>% ggplot(aes(ANO, NOTA)) +
            geom_line() + scale_x_continuous(breaks = ideb_munic$ANO, labels = ideb_munic$ANO) +
            labs(title = "IDEB nos anos iniciais",
                 subtitle = "Índice de Desenvolvimento da Educação Básica para os anos iniciais do ensino fundamental (1º ao 5º ano)",
                 x = "Ano",
                 y = "Nota",
                 caption = "Fonte: INEP.")
        
        ggplotly(ideb_munic)
        
    })
    
    output$percentual_matrículas <- renderPlotly({
        
        dados_mat <- base_dados_administrativos %>%
            filter(NOME == "Niterói") %>%
            select(ANO, NOME, PMATPUB_EF, PMATPUB_EM, PMATPRI_EF, PMATPRI_EM) %>% na.omit() %>%
            pivot_longer(cols = -c(ANO,NOME),
                         names_to = "INDICADOR",
                         values_to = "MAT") %>% mutate_if(is.numeric, as.integer)

        if(input$publico_id == "Público") {
            
            if(input$etapa_id == "Ensino Fundamental") {
                
                dados_mat <- dados_mat %>% filter(INDICADOR == "PMATPUB_EF") 
                dados_mat <- dados_mat %>% ggplot(aes(ANO, MAT)) +
                    geom_line() + scale_x_continuous(breaks = dados_mat$ANO, labels = dados_mat$ANO) +
                    labs(title = "% matrículas do EF na rede pública",
                         x = "Ano",
                         y = "% Matrículas",
                         caption = "Fonte: INEP.")
                return(ggplotly(dados_mat))
                }
            else {
                    dados_mat <- dados_mat %>% filter(INDICADOR == "PMATPUB_EM")
                    dados_mat <- dados_mat %>% ggplot(aes(ANO, MAT)) +
                        geom_line() + scale_x_continuous(breaks = dados_mat$ANO, labels = dados_mat$ANO) +
                        labs(title = "% matrículas do EM na rede pública",
                             x = "Ano",
                             y = "% Matrículas",
                             caption = "Fonte: INEP.")
                    return(ggplotly(dados_mat))
            }
        }
        if(input$publico_id == "Privada") {
            if(input$etapa_id == "Ensino Fundamental") {
                dados_mat <- dados_mat %>% filter(INDICADOR == "PMATPRI_EF") 
                dados_mat <- dados_mat %>% ggplot(aes(ANO, MAT)) +
                geom_line() + scale_x_continuous(breaks = dados_mat$ANO, labels = dados_mat$ANO) +
                labs(title = "% matrículas do EF na rede privada",
                     x = "Ano",
                     y = "% Matrículas",
                     caption = "Fonte: INEP.")
                return(ggplotly(dados_mat))
            }
            else{
                dados_mat <- dados_mat %>% filter(INDICADOR == "PMATPRI_EM") 
                dados_mat <- dados_mat %>% ggplot(aes(ANO, MAT)) +
                    geom_line() + scale_x_continuous(breaks = dados_mat$ANO, labels = dados_mat$ANO) +
                    labs(title = "% matrículas do EM na rede privada",
                         x = "Ano",
                         y = "% Matrículas",
                         caption = "Fonte: INEP.")
                return(ggplotly(dados_mat))
            }
        }
        })
    
# DISTORÇÃO IDADE ----    
    
    output$distorcao_idade <- renderPlotly({
        dados_dist <- base_dados_administrativos %>%
            filter(NOME == "Niterói") %>%
            select(ANO, NOME, DIST_EF_PUB, DIST_EM_PUB, DIST_EF_PRI, DIST_EM_PRI) %>% na.omit() %>%
            pivot_longer(cols = -c(ANO,NOME),
                         names_to = "INDICADOR",
                         values_to = "DIST") %>% mutate_if(is.numeric, as.integer)
        
        if(input$publico_id == "Público") {
            
            if(input$etapa_id == "Ensino Fundamental") {
                
                dados_dist <- dados_dist %>% filter(INDICADOR == "DIST_EF_PUB") 
                dados_dist <- dados_dist %>% ggplot(aes(ANO, DIST)) +
                    geom_line() + scale_x_continuous(breaks = dados_dist$ANO, labels = dados_dist$ANO) +
                    labs(title = "Distorção Idade-Série no fundamental na rede pública",
                         x = "Ano",
                         y = "Distorção",
                         caption = "Fonte: INEP.")
                return(ggplotly(dados_dist))
            }
            else {
                dados_dist <- dados_dist %>% filter(INDICADOR == "DIST_EM_PUB")
                dados_dist <- dados_dist %>% ggplot(aes(ANO, DIST)) +
                    geom_line() + scale_x_continuous(breaks = dados_dist$ANO, labels = dados_dist$ANO) +
                    labs(title = "Distorção Idade-Série no médio na rede pública",
                         x = "Ano",
                         y = "Distorção",
                         caption = "Fonte: INEP.")
                return(ggplotly(dados_dist))
            }
        }
        if(input$publico_id == "Privada") {
            if(input$etapa_id == "Ensino Fundamental") {
                dados_dist <- dados_dist %>% filter(INDICADOR == "DIST_EF_PRI") 
                dados_dist <- dados_dist %>% ggplot(aes(ANO, DIST)) +
                    geom_line() + scale_x_continuous(breaks = dados_dist$ANO, labels = dados_dist$ANO) +
                    labs(title = "Distorção Idade-Série no fundamental na rede privada",
                         x = "Ano",
                         y = "Distorção",
                         caption = "Fonte: INEP.")
                return(ggplotly(dados_dist))
            }
            else{
                dados_dist <- dados_dist %>% filter(INDICADOR == "DIST_EM_PRI") 
                dados_dist <- dados_dist %>% ggplot(aes(ANO, DIST)) +
                    geom_line() + scale_x_continuous(breaks = dados_dist$ANO, labels = dados_dist$ANO) +
                    labs(title = "Distorção Idade-Série no médio na rede privada",
                         x = "Ano",
                         y = "Distorção",
                         caption = "Fonte: INEP.")
                return(ggplotly(dados_dist))
            }
        } 
    })
}
    
    
    shinyApp(ui = ui, server = server)
