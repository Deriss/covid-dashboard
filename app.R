## Coronavirus Dashboard in Portuguese

#Loading required libraries
library(shiny)
library(shinydashboard)
library(utils)
library(httr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(rdrop2)
library("shinyWidgets")
library(DT)

#Loading token for dropbox connection
token <- readRDS("droptoken.rds")
#Loading the csv file from dropbox
data = drop_read_csv("coronavirus/covid19.csv",dtoken = token,encoding = "UTF-8",stringsAsFactors =FALSE)


head(data)
#Pre-process the data
data$dateRep = as.Date(data$year_week) #dateRep to Date format
data=data%>%arrange(dateRep) #Order data by Date


# Define UI for the dashboard application

ui <- dashboardPage(
    skin = "green",
    
    # Header
    dashboardHeader(title="Situação CoVid-19",titleWidth=300),

    # Sidebar
    dashboardSidebar(
        width = 300,
        #Country Selection
        selectInput( "CountrySelection","País:",
                    sort(as.character(unique(data$CountryName_pt))),
                    selected = "Brasil"
                    ),

        #Date Selection
        dateRangeInput("DateRange", "Selecione as datas:",
                       start = min(data$dateRep), end = max(data$dateRep),
                       min = min(data$dateRep), max = max(data$dateRep),
                       format = "dd-mm-yy", startview = "month",
                       weekstart = 0, language = "pt-BR", separator = " a ", width = NULL,
                       autoclose = TRUE
                       ),
        sidebarMenu(
            tags$style(HTML(".sidebar-menu li a { font-size: 20px; }")),
            
            menuItem("Gráficos",tabName = "Graphs",icon = icon("area-chart"),selected = TRUE),
            menuItem("Tabelas",tabName = "Tables",icon = icon("table")),
            menuItem("Mapas",tabName = "Maps",icon = icon("globe"))
        )   
           
        
        
    ),
    # Body of the Dashboard
    dashboardBody(
      tabItems(
        #Graphs Tab
        tabItem(tabName="Graphs",
            #Top Row showing the totals deaths and cases
            fluidRow(
                valueBoxOutput("InfectedGlobalBox",width=3),
                valueBoxOutput("DeathGlobalBox",width=3),
                valueBoxOutput("InfectedLocalBox",width=3),
                valueBoxOutput("DeathLocalBox",width=3)
            ),
    
            #Row for plots
            fluidRow(
                #Box for Number of Cases' plot
                box(title="Número de Casos Acumulados",
                    solidHeader = TRUE,
                    status="success",
                    background = "green",
                    prettyCheckbox(
                        inputId = "SeeCases1", label = "Número de Casos",icon=icon("check"), outline = TRUE, inline=TRUE,value=TRUE
                    ),
                    prettyCheckbox(
                        inputId = "SeeDeaths1", label = "Número de Mortes",icon=icon("check"),
                        outline = TRUE, inline=TRUE, value=TRUE
                    ),
                    plotlyOutput("TotalCasesPlot")),
                #Box for Number of New Cases' plot
                box(title="Novos casos por dia",
                    solidHeader = TRUE,
                    status="info",
                    background = "aqua",
                    prettyCheckbox(
                        inputId = "SeeCases2", 
                        label = "Número de Casos",
                        icon=icon("check"), 
                        outline = TRUE, 
                        inline=TRUE,
                        value=TRUE
                    ),
                    prettyCheckbox(
                        inputId = "SeeDeaths2", 
                        label = "Número de Mortes",
                        icon=icon("check"),
                        outline = TRUE, 
                        inline=TRUE, 
                        value= TRUE
                    ),
                    plotlyOutput("NewCasesPlot")
                )
            )
        ),
        #Tables Tab
        tabItem(tabName = "Tables", 
                box(title="Casos Totais por País",
                    solidHeader = TRUE,
                    status="success",
                    
                    dataTableOutput('totalCasesTable'),width=6,style="overflow-x: scroll;"
                    ),
                box(title="Novos Casos por País",
                    solidHeader = TRUE,
                    status="info",
                    
                    dataTableOutput('recentCasesTable'),width=6,style="overflow-x: scroll;"
                )
                ),
        # Maps tab
        tabItem(tabName="Maps",style='border: 3px solid green;background-color: white;height: 600px',fluidRow(align = "center",
                h1("Mapa Covid-19"),
                radioButtons('graph_z','Selecione:',choices=c("Casos Acumulados","Mortes Acumuladas"),inline=TRUE),
                plotlyOutput('map1'))
        )
        )
    )
)


server <- function(input, output) {

    #Filter the data reactively
    localdata = reactive({

        if (is.null(input$CountrySelection) || input$CountrySelection == "Todos"){
            #Case where all or none of the countries are selected 
            local = data%>%group_by(dateRep)%>%
                           summarise(cumulative_count_cases = sum(cumulative_count_cases),cumulative_count_deaths=sum(cumulative_count_deaths),
                                     rate_14_day_cases=sum(rate_14_day_cases),
                                     rate_14_day_deaths =sum(rate_14_day_deaths))
            
        }
        else{
            #Filter the data by the country selected
            local = data%>%filter((CountryName_pt==input$CountrySelection) & (dateRep <= input$DateRange[2]))

        }
        #Filter the data by the date range selected
        local = local%>%filter(between(dateRep,input$DateRange[1],input$DateRange[2]))%>%arrange(dateRep)
        })
    
    # Total Cases Table

    tabledata = reactive({
                tabdata = data%>%filter(dateRep==max(dateRep))%>%
                          arrange(desc(cumulative_count_cases))%>%
                          
                          transmute("País"=CountryName_pt,
                                    "Número de Casos Totais"= cumulative_count_cases,
                                    "Número de Mortes Totais" = cumulative_count_deaths)
               tabdata = datatable(tabdata,options= list(autoWidth=TRUE,autoHeight=TRUE,scrollX=TRUE,scrollY="340px"))%>%
                          formatRound(c("Número de Casos Totais","Número de Mortes Totais"),mark=".",dec.mark = ",",digits=0)
                 
    })
    
    # New Cases Table
    tabledata2 = reactive({
      tabdata2 = data%>%filter(dateRep==max(dateRep))%>%
                    select(CountryName_pt,rate_14_day_cases,rate_14_day_deaths)%>%
                    arrange(desc(rate_14_day_cases))%>%
                    transmute("País"=CountryName_pt,
                              "Média de Casos últimos 14 dias"= round(rate_14_day_cases),
                              "Média de Mortes últimos 14 dias" = round(rate_14_day_deaths))
      tabledata2 = datatable(tabdata2,options= list(autoWidth=TRUE,autoHeight=TRUE,scrollX=TRUE,scrollY="340px"))%>%
        formatRound(c("Média de Casos últimos 14 dias","Média de Mortes últimos 14 dias"),mark=".",dec.mark = ",",digits=0)
    })

    # Output tables  
    output$totalCasesTable  = renderDataTable(tabledata() )
    output$recentCasesTable = renderDataTable(tabledata2())
    
    
    
    # Cases and Death counts

    output$InfectedGlobalBox = renderValueBox({
        valueBox(format(localdata()$cumulative_count_cases%>%last(), nsmall=1, big.mark=".") ,
                 "Casos Acumulados",
                 icon=icon("medkit"),
                 color="green")
    })
    output$DeathGlobalBox = renderValueBox({
        valueBox(format(localdata()$cumulative_count_deaths%>%last(), nsmall=1, big.mark=".",scientific=FALSE),
                 "Mortes Acumuladas",
                 icon=icon("cross"),
                 color="green")
    })
    output$InfectedLocalBox = renderValueBox({
        valueBox(format(round(localdata()%>%filter(dateRep == max(dateRep))%>%select(rate_14_day_cases)),digits=0, big.mark=".",scientific=FALSE),
                 "Média Novos Casos",
                 icon=icon("medkit"))
    })
    output$DeathLocalBox = renderValueBox({
        valueBox(format(round(localdata()$rate_14_day_deaths%>%last()),digits=0, big.mark=".",scientific=FALSE),
                 "Média Novas Mortes",
                 icon=icon("cross"))
    })

    # Plot number of cases and deaths
    
    ### colors
    green = "#5cb85c"
    aqua = "#5bc0de"
    ## Plot Total cases
    output$TotalCasesPlot = renderPlotly({
       g = ggplot(data=localdata()) +
           #Labels
            xlab("Data")+
            ylab("Número de Casos")+
           #Cases
           {if( input$SeeCases1)
            geom_area(colour=green,

                      alpha=0.8,
                      aes(dateRep,cumulative_count_cases,group=1,fill="Casos",
                          text=paste("Casos:",format(round(cumulative_count_cases),digits=0,big.mark = ".",scientific=FALSE),"<br>Data:",dateRep)))}   +
           #Deaths
           {if( input$SeeDeaths1 )
            geom_area(aes(dateRep,cumulative_count_deaths,fill="Mortes",
                          group=1,
                          text=paste("Mortes:",format(round(cumulative_count_deaths),digits=0,big.mark = ".",scientific=FALSE),"<br>Data:",dateRep)),
                      color="red",

                      alpha=0.8)} +
            scale_fill_manual(name="",values=c("Casos"=green,"Mortes"="red"))+
           #Theme
            scale_x_date(limits = c(input$DateRange[1], input$DateRange[2]),expand = c(0,0))+
            theme_economist_white() +
            theme(plot.background = element_rect(fill = "white"),
                  panel.grid.major.y = element_line(colour = aqua),
                  axis.title = element_text(family="Verdana",size = 10,face="bold"),
                  legend.background = element_rect(fill="white")
                  )
       fig = ggplotly(g,tooltip=c("datas","text"))%>%layout(legend=list(orientation='h',x=0.5,y=1.15))
       fig
        })

    ## Plot New Cases
    output$NewCasesPlot = renderPlotly({
        g2 = ggplot(data=localdata()) +
                #Labels
                xlab("Data")+
                ylab("Média Móvel de 14 días")+

                #Cases
                {if( input$SeeCases2 )
                    geom_area(colour=aqua,alpha=0.8,
                              aes(dateRep,rate_14_day_cases,group=1,fill="Casos",
                                  text=paste("Novos Casos:",format(rate_14_day_cases,digits=0,big.mark = ".",scientific=FALSE),"<br>Data:",dateRep)))}+
                #Deaths
                {if( input$SeeDeaths2 )
                    geom_area(colour="red",alpha=0.8,
                              aes(dateRep,rate_14_day_deaths,group=1,fill="Mortes",
                                  text= paste("Novas Mortes:",format(rate_14_day_deaths,digits=0,big.mark = ".",scientific=FALSE),"<br>Data:",dateRep)))}+
                scale_fill_manual(name="",values=c("Casos"=aqua,"Mortes"="red"))+
                #Theme
                scale_x_date(limits = c(input$DateRange[1], input$DateRange[2]),expand = c(0,0))+
                theme_economist_white() +
                theme(plot.background = element_rect(fill = "white"),
                      panel.grid.major.y = element_line(colour = green),
                      axis.title = element_text(family="Verdana",size = 10,face="bold"),
                      legend.background = element_rect(fill="white")
                      )

        fig2 =ggplotly(g2,tooltip="text")%>%layout(legend=list(orientation='h',x=0.5,y=1.15))
        fig2
    })
    
    # Plot maps
    
    mapdata1 =  data%>%
              filter(dateRep == max(dateRep))%>%
              mutate(countryterritoryCode = as.character(country_code))

    
    
    z_value = reactive({switch(
                        input$graph_z,
                        "Casos Acumulados"   = mapdata1$cumulative_count_cases,
                        "Mortes Acumuladas"  = mapdata1$cumulative_count_deaths
                      )
    })
    output$map1 = renderPlotly(fig <- plot_ly(data=mapdata1, 
                                              type='choropleth', 
                                              locations= mapdata1$country_code, 
                                              z=z_value(), 
                                              text=mapdata1$CountryName_pt, 
                                              colorscale="Greens",
                                              width=900,height=450,
                                              reversescale=TRUE)%>%
                                              layout(margin=list(l=5,r=5,b=5,t=5,pad=4),
                                                     geo=list(bgcolor= 'rgba(0,0,0,0)'))
                               )
    
    }
  observeEvent(input$graph_z, {
  print(paste0("You have chosen: ", input$graph_z))
  })

# Run the application
shinyApp(ui = ui, server = server)

