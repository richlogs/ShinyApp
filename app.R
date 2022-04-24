library(shiny)
library(lubridate)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(leaflet)




## Reading, cleaning and summarizing dataset 

# Reading Data and adjusting objects
dataLog <- read.csv("ShinyData.csv")[-1]
schoolHours <- read.csv("SchoolHours.csv")[-1]    # This is used to calculate the in school and out of school hours information


# Changing relevant columns to time format
dataLog$DateTime <- ymd_hms(dataLog$DateTime, tz = "Pacific/Auckland")
dataLog$Date <- as.Date(dataLog$Date)
schoolHours$start <- ymd_hms(schoolHours$start, tz = "Pacific/Auckland")
schoolHours$end <- ymd_hms(schoolHours$end, tz = "Pacific/Auckland")


# Creating summarized dataset of all observations
map.framework <- as.data.frame(filter(dataLog, schoolHours == TRUE) %>%
                                   select(2:10) %>% group_by(SchoolName, Longitude, Latitude, school.region, school.type) %>%
                                   summarise(tempPer = round(sum(roll.temp >= 18 & roll.temp <=25)/length(roll.temp), digits = 2)*100,
                                             lightPer = round(sum(roll.light >= 300 & roll.light <=2000)/length(roll.light), digits = 2)* 100,
                                             soundPer = round(sum(roll.sound >= 0 & roll.sound <=70)/length(roll.sound), digits = 2)*100,
                                             iaqPer = round(sum(roll.c02 >= 0 & roll.c02 <=1500)/length(roll.c02), digits = 2)* 100) %>%
                                   mutate(risk.group = (tempPer + lightPer + soundPer + iaqPer)/4) %>%
                                   mutate(groups = cut(risk.group,
                                                       breaks = c(0,49,74,101),
                                                       labels = c("poor","fair", "good"))))



## Defining Map information 

# Creating legend information for map
legend <- c("Poor", "Fair", "Good")
legend <- factor(legend, levels = c("Poor", "Fair", "Good"))


# Defining map and legend color palette 
pal <- colorFactor(palette = c("Red", "orange", "green"), domain = map.framework$groups)
palLeg <- colorFactor(palette = c("Red", "orange", "green"), domain = legend)








####################################### Shiny App



ui <- dashboardPage(skin = "red",                    # Adjusting theme information 
    dashboardHeader(title = "IEM Analysis"),
    dashboardSidebar(                                # Creating sidebar panels
        sidebarMenu(
            menuItem("About", tabName = "about", icon = icon("dashboard")),
            menuItem("Map", tabName = "map", icon = icon("map")),
            menuItem("Table", tabName = "tables", icon = icon("table")),
            menuItem("Graphics", tabName = "graphics", icon = icon("th"))
        )
    ),
    dashboardBody(tabItems(
        tabItem(tabName = "about",                  # About panel
                fluidRow(column(5, offset = 0.5,
                                h2("Introduction"),
                                
                                tags$p("The New Zealand Government has set a goal for all schools buildings to provide quality learning environments by 2030. The Ministry of 
                Education is currently developing a framework to facilitate the achievement of this target. An aspect of that framework involves the assesment 
                of the internal comfort of a learning space. This can be broken down into four primary varaibles, those being the lighting,
                acoustics, thermal comfort and air quality of the space. All four of which have been shown to significantly contribute student learning outcomes."),
                                
                                tags$p("The Ministry is currently undergoing a process of mass scale data collection on its assests to better understand their current performance. 
                   This process involves, among other things, installing internal environmental monitering (IEM) devices in school classrooms. These devices return
                   readings on all four interal comfort elements every ten seconds. The analysis of this data will help inform funding decisions which will in turn
                   move us closer to achieving the goal set out by the New Zealand Government."),
                                
                                tags$p("The purpose of this application is to provide an explorative experience of the IEM data currently being collected. Doing so will facilitate 
                   the process of its analysis allowing for the identification of trends and patterns that may have otherwise gone unnoticed. To do so this application 
                   has been seperated into two distinct components. The first being a map illustraiting the location of installed IEM devices. Users can interact with 
                   each location and gain insights into the performance of each variable for any given location. The second component provides the user with an interactive
                   time series analysis of each comfort factor. The user can specify date ranges and desired locations and track their performance over time. These 
                   components can be navigated between via the lefthand sidebar."),
                                
                                h2("Performance measures"),
                                
                                tags$p("Each comfort variable has a defined range of good performance. If a building falls within this range during school hours, it is considered to be a well
           performing asset. The ranges and source material is listed below:"),
                                
                                tags$p("Lighting: The light levels within a learning space should fall between 300lx - 2000lx during school hours - ", 
                                       tags$a(href= "https://www.education.govt.nz/assets/Documents/Primary-Secondary/Property/Design/Flexible-learning-spaces/DQLS-Lighting.pdf", 
                                              "DQLS 2020 Lighting, pg. 13")),
                                
                                tags$p("Acoustics: The sound levels within an occupied learning space should not exceed 70dB LAeq - ",
                                       tags$a(href = "https://www.education.govt.nz/assets/Documents/Primary-Secondary/Property/Design/Flexible-learning-spaces/DQLS-Acoustics.pdf",
                                              "DQLS 2020 Acoustics, pg.29")),
                                
                                tags$p("Thermal Comfort: The temperature of a learning space should fall between 18°C - 25°C - ",
                                       tags$a(href = "https://www.education.govt.nz/assets/Documents/Primary-Secondary/Property/Design/Flexible-learning-spaces/DQLSIndoorAirQualityThermalComfortV1-0.pdf",
                                              "DQLS 2017 Indoor Air Quality and Thermal Comfort, pg. 5")),
                                
                                tags$p("Air Quality: The CO₂ content in a learning space should not exceed 1500ppm - ", 
                                       tags$a(href = "https://www.education.govt.nz/assets/Documents/Primary-Secondary/Property/Design/Flexible-learning-spaces/DQLSIndoorAirQualityThermalComfortV1-0.pdf",
                                              "DQLS 2017 Indoor Air Quality and Thermal Comfort, pg. 8")),
                                
                                tags$p("The comfort score for each variable is defined as the percentage of time spent within the acceptable range during school hours. An average comfort score
                                       for each school is calculated from these values. Risk groups are then created based on the average score. An average score of 75% or greater is considered 
                                       good. A score between 50% - 75% is considered fair and a score below 50% is considered poor."),
                                
                                
                                h2("Privacy"),
                                
                                tags$p("Given the sensitivity of the information contained in this application, several steps have been taken to transform and annonomise the data. There 
           are four locations that contain portions of real data logger output in this application. Those being listed under Ladbrooks School, Otamatea High School, Purua School and St Matthew's Primary School (Hastings). All other information 
           was created by transforming the output from Ladbrooks School. Moreover, the school names information is stored under do not correspond to the true location the 
           information was recorded at. In addition to the above, several dates were changed for real data logger output, and new data was imputed where no data existed 
           to both further anonomise and make the process of data analysis better replicate its real world application.")),
                         
                         
                         column(4, offset = 1,       # Image code
                                br(),
                                h3("Image of Data Logger"),
                                hr(),
                                tags$img(src = 'sensor.png', 
                                         height = 500,
                                         width = 450
                                )))
                
        ),
        
        
        
        tabItem(tabName = "map",        # Mapping panel
                fluidRow(
                    valueBoxOutput("activeDevices"),  # Value boxes
                    valueBoxOutput("districts"),
                    valueBoxOutput("highRisk"),
                ),
                
                box(width = 4, 
                    tags$h2("Mapping"),    # Slider box 
                    hr(),
                    tags$h4("Select a date range of interest using the slider 
                            below. The visual display will update once the confirm 
                            button has been clicked."),
                    hr(),
                    sliderInput(inputId = "mapslider",
                                label = NULL,
                                min = min(dataLog$Date),
                                max = max(dataLog$Date),
                                value =  c(min(dataLog$Date), max(dataLog$Date))),
                    actionButton(inputId = "click",
                                 label = "Confirm")
                ),
                box(width = 8,                                                    # map box
                    leafletOutput("bopmap", height = 600))),
        
        
        
        
        
        tabItem(tabName = "tables",    # Table tab
                box(width = 3,
                    sliderInput(inputId = "tabslider",            # Reactive date slider
                                label = "Select a Date Range",
                                min = min(dataLog$Date),
                                max = max(dataLog$Date),
                                value =  c(min(dataLog$Date), max(dataLog$Date))),
                    
                    checkboxGroupInput(inputId = "checkboxfilters", label = "Performance Level",     # Reactive checkboxes
                                       choices = levels(map.framework$groups), 
                                       selected = levels(map.framework$groups)),
                    checkboxGroupInput("regionfilters", label = "Regions",
                                       choices = unique(map.framework$school.region), 
                                       selected = unique(map.framework$school.region)),
                    actionButton(inputId = "tabclick",
                                 label = "Confirm")),
                box(width = 9,
                    dataTableOutput("dataTable"))),
        
        
        
        
        
        
        tabItem(tabName = "graphics",             # Plotting tab
                fluidRow(
                    box(width = 4, 
                        tags$h2("Time Series Analysis"),        # Information box 
                        hr(),
                        h4("Select the desired time frame and location from the control box on the right."), 
                        h4("The horizontal bars reprsent the minimum and maximum comfort ranges for each variable. 
                           Where a minimum in non-applicable (Acoustics and CO2) only the maximum is displayed.")),
                    box(width = 8, 
                        title = "Controls",
                        sliderInput(inputId = "slider",            # Reactive slider
                                    label = "Select a Date Range",
                                    min = min(dataLog$Date),
                                    max = max(dataLog$Date),
                                    value =  c(min(dataLog$Date) + days(3), min(dataLog$Date) + days(5))),
                        
                        selectInput(inputId = "school",           # Reactive school selector
                                    label = "Select a Location:",
                                    choices = map.framework$SchoolName,
                                    selected = "Ladbrooks School",
                                    multiple = TRUE))
                    ),
                
                fluidRow(box(width = 6,            # Creating space for plots
                             plotOutput("temp")),
                         
                         box(width = 6,
                             plotOutput("acoustics")),
                         
                         box(width = 6,
                             plotOutput("carbon")),
                         
                         box(width = 6,
                             plotOutput("light")))
                    
                    
            
        )
    )
    )
)



server <- function(input, output){
    
    
    #### Mapping page
    
    
    
    ## Creating reactive summarized dataset
    performance <- reactive({as.data.frame(filter(dataLog, schoolHours == TRUE, Date >= input$mapslider[1], Date <= input$mapslider[2]) %>%
                                               select(2:10) %>% group_by(SchoolName, Longitude, Latitude, school.region, school.type) %>%
                                               summarise(tempPer = round(sum(roll.temp >= 18 & roll.temp <=25)/length(roll.temp), digits = 2)*100,
                                                         lightPer = round(sum(roll.light >= 300 & roll.light <=2000)/length(roll.light), digits = 2)* 100,
                                                         soundPer = round(sum(roll.sound >= 0 & roll.sound <=70)/length(roll.sound), digits = 2)*100,
                                                         iaqPer = round(sum(roll.c02 >= 0 & roll.c02 <=1500)/length(roll.c02), digits = 2)* 100) %>%
                                               mutate(risk.group = (tempPer + lightPer + soundPer + iaqPer)/4) %>%
                                               mutate(groups = cut(risk.group,
                                                                   breaks = c(0,49,74,101),
                                                                   labels = c("poor","fair", "good"))))
    })
    
    
    
    
    
    
    ######## value boxes
    
    
    # Data for Value boxes
    
    deviceData <- eventReactive(input$click, {
        nrow(performance())
    }, ignoreNULL = FALSE)
    
    
    districtData <- eventReactive(input$click, {
        length(unique(performance()$school.region))},
        ignoreNULL = FALSE)
    
    
    riskData <- eventReactive(input$click, {
        nrow(performance()[performance()$groups == "poor",])},
        ignoreNULL = FALSE)
    
    
    
    # value boxes 
    
    output$activeDevices <- renderValueBox({
        valueBox(
            paste0(deviceData()), h4("Active Devices"),
            icon = icon("list"),
            color = "purple"
        )
    })
    
    output$districts <- renderValueBox({
        valueBox(
            paste0(districtData()), h4("Districts"),
            icon = icon("map-marker-alt"),
            color = "blue")
    })
    
    
    output$highRisk <- renderValueBox({
        valueBox(
            paste0(riskData()), h4("Poor Performing Spaces"),
            icon = icon("bell"),
            color = "red"
        )
        
    })
    
    
    
    ## Map
    output$bopmap <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng = 174.335676, lat = -39.141953,
                    zoom = 7) %>%
            addCircles(map.framework$Longitude, map.framework$Latitude, radius = 3000,
                       stroke = FALSE, fillOpacity = 0.9, fillColor = pal(map.framework$groups),
                       layerId = map.framework$SchoolName, label = map.framework$SchoolName) %>%
            addLegend("bottomright", pal = palLeg, values = legend)
    })
    
    ## create proxy for circles on top of original map
    observeEvent(input$click, {
        observeEvent(input$click, {
            leafletProxy("bopmap") %>%
                clearShapes() %>%
                addCircles(performance()$Longitude, performance()$Latitude, layerId = performance()$SchoolName,
                           radius = 3000, stroke = FALSE, fillOpacity = 1, fillColor = pal(performance()$groups),
                           label = performance()$SchoolName) 
            
        })
        
        
        
        ## Creating output for building summary. Shows when map circle is clicked
        ShowSchoolPerformance <- function(schoolname, lat, lng){
            selectedSchool <- performance()[performance()$SchoolName == schoolname,]
            content <- as.character(tagList(
                tags$strong(schoolname), tags$br(),
                sprintf("Light: %g%%", selectedSchool$lightPer), tags$br(),
                sprintf("Temp: %g%%", selectedSchool$tempPer), tags$br(),
                sprintf("Acoustics: %g%%", selectedSchool$soundPer),tags$br(),
                sprintf("Air Quality: %g%%", selectedSchool$iaqPer )
            ))
            leafletProxy("bopmap") %>% addPopups(lng, lat, content, layerId = schoolname)
            
        }
        
        # Recalculates popups after date slider changes on proxy map 
        observe( {
            leaflet("bopmap") %>% clearPopups()
            event <-input$bopmap_shape_click
            if(is.null(event))
                return()
            
            isolate({
                ShowSchoolPerformance(event$id, event$lat, event$lng)
            })
        })
        
        observeEvent(input$click, {
            leafletProxy("bopmap") %>% clearPopups()
        })
    }, ignoreNULL = FALSE)
    
    
    
    
    
    
    
    
    
    
    ## Table page
    
    ## Creates reactive dataset for table page inputs
    mytable <- eventReactive(input$tabclick, {as.data.frame(filter(dataLog, schoolHours == TRUE, Date >= input$tabslider[1], Date <= input$tabslider[2]) %>%
                                                                select(2:10) %>% group_by(SchoolName, school.region) %>%
                                                                summarise(Temperature = round(sum(roll.temp >= 18 & roll.temp <=25)/length(roll.temp), digits = 2)*100,
                                                                          Light = round(sum(roll.light >= 300 & roll.light <=2000)/length(roll.light), digits = 2)* 100,
                                                                          Acoustic = round(sum(roll.sound >= 0 & roll.sound <=70)/length(roll.sound), digits = 2)*100,
                                                                          AirQuality = round(sum(roll.c02 >= 0 & roll.c02 <=1500)/length(roll.c02), digits = 2)* 100) %>%
                                                                mutate(average.score = (Temperature + Light + Acoustic + AirQuality)/4) %>%
                                                                mutate(groups = cut(average.score,
                                                                                    breaks = c(0,49,74,101),
                                                                                    labels = c("poor","fair", "good"))) %>%
                                                                filter(groups %in% input$checkboxfilters, school.region %in% input$regionfilters))
    }, ignoreNULL = FALSE)
    
    
    
    
    ## outputs table
    output$dataTable <- renderDataTable(mytable(),
                                        options = list(
                                            pageLength = 10
                                        ))
    
    
    
    
    
    
    
    
    
    
    ## Graphing page
    
    ## Reactive dataset for plots
    subsetLog <- reactive({
        filter(dataLog, SchoolName %in% input$school, Date >= input$slider[1], Date <= input$slider[2])
    })
    
    # Temperature Graph
    output$temp <- renderPlot({
        ggplot(subsetLog())+
            geom_line(aes(x = DateTime, y = roll.temp, col = SchoolName))+
            geom_rect(data = schoolHours, mapping = aes(xmin = start, xmax = end,
                                                        ymin = -Inf, ymax = Inf, fill = "Out of School Hours"), 
                      colour = NA, alpha = 0.1)+
            geom_hline(yintercept = 25, linetype = 5, col = "red")+
            geom_hline(yintercept = 18, linetype = 5, col = "red")+
            labs(title = "Classroom Temperature",
                 x = "Date and Time",
                 y = "Temperature °C")+
            theme_fivethirtyeight()+
            theme(axis.title = element_text())+
            coord_cartesian(xlim = c(min(subsetLog()$DateTime), max(subsetLog()$DateTime)))+
            scale_fill_manual('Shading', values = "blue", 
                              guide = guide_legend(override.aes = list(alpha = 0.1)))
    })
    
    #Co2 Graph
    output$carbon <- renderPlot({
        ggplot(subsetLog())+
            geom_line(aes(x = DateTime, y = roll.c02, col = SchoolName))+
            geom_rect(data = schoolHours, mapping = aes(xmin = start, xmax = end,
                                                        ymin = -Inf, ymax = Inf, fill = "Out of School Hours"), 
                       colour = NA, alpha = 0.1)+
            geom_hline(yintercept = 1500, linetype = 5, col = "red")+
            labs(title = "Classroom CO2 Levels",
                 x = "Date and Time",
                 y = "CO2")+
            theme(axis.title = element_text())+
            coord_cartesian(xlim = c(min(subsetLog()$DateTime), max(subsetLog()$DateTime)))+
            scale_fill_manual('Shading', values = "blue", 
                              guide = guide_legend(override.aes = list(alpha = 0.1)))+
            theme_fivethirtyeight()
    })
    
    # Acoustics Graph
    output$acoustics <- renderPlot({
        ggplot(subsetLog())+
            geom_line(aes(x = DateTime, y = roll.sound, col = SchoolName))+
            geom_rect(data = schoolHours, mapping = aes(xmin = start, xmax = end,
                                                        ymin = -Inf, ymax = Inf, fill = "Out of School Hours"), 
                      colour = NA, alpha = 0.1)+
            geom_hline(yintercept = 70, linetype = 5, col = "red")+
            labs(title = "Classroom Acoustics",
                 x = "Date and Time",
                 y = "Sound (dB)")+
            theme_fivethirtyeight()+
            theme(axis.title = element_text())+
            coord_cartesian(xlim = c(min(subsetLog()$DateTime), max(subsetLog()$DateTime)))+
            scale_fill_manual('Shading', values = "blue", 
                              guide = guide_legend(override.aes = list(alpha = 0.1)))
    })
    
    # Lighting Graph 
    output$light <- renderPlot({
        ggplot(subsetLog())+
            geom_line(aes(x = DateTime, y = roll.light, col = SchoolName))+
            geom_rect(data = schoolHours, mapping = aes(xmin = start, xmax = end,
                                                        ymin = -Inf, ymax = Inf, fill = "Out of School Hours"), 
                      colour = NA, alpha = 0.1)+
            geom_hline(yintercept = 2000, linetype = 5, col = "red")+
            geom_hline(yintercept = 300, linetype = 5, col = "red")+
            labs(title = "Classroom Light",
                 x = "Date and Time",
                 y = "Sound (dB)")+
            theme_fivethirtyeight()+
            theme(axis.title = element_text())+
            coord_cartesian(xlim = c(min(subsetLog()$DateTime), max(subsetLog()$DateTime)))+
            scale_fill_manual('Shading', values = "blue", 
                              guide = guide_legend(override.aes = list(alpha = 0.1)))
    })
    
}

shinyApp(ui = ui, server = server)

