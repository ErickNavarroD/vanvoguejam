#### Load packages ####
options(shiny.autoreload = FALSE) 
library(shiny)
library(shinythemes)
library(dplyr)
library(shinyWidgets)
library(lubridate)
library(tidyr)
library(readxl)
library(here)
library(DT)

#### Load data ####
data_balls <- read_excel("data.xlsx") %>% 
    drop_na() %>% 
    mutate(full_name = paste(Participant, House, sep = " "),
           Date = lubridate::dmy(Date))

#### Define helper functions ####
#Function to identify the members of each house
ident_house_mem = function(data){
    house_members = list()
    for (house in unique(data$House)){
        members = data %>% 
            filter(House == house) %>%
            pull(full_name) %>% 
            unique()
        house_members[[house]] = unique(members)
    }
    return(house_members)
}

#Function to group the categories into broader categories
parent_categories = c("Runway", "Performance", "Face", "Realness", "Body", "Best Dressed", "Sex Siren")
categories_list = function(data){
    categories = list()
    for (parent_category in parent_categories){
        categories[[parent_category]] = data %>% 
            filter(grepl(parent_category, Category, ignore.case = TRUE)) %>%
            pull(Category) %>% 
            unique()
    }
    #Code exceptions 
    categories[["Performance"]] = c(categories[["Performance"]], "Pop dip and spin", "Realness with a twist")
    # Add Other categories
    categories[["Other"]] = data %>% 
        filter(!grepl(paste(parent_categories, collapse = "|"), Category, ignore.case = TRUE)) %>%
        filter(!grepl("Pop dip and spin|Realness with a twist", Category, ignore.case = TRUE)) %>%
        pull(Category) %>% 
        unique()
    return(categories)
}

#### Define UI ####
navbarPage("VanBAE",
           theme = shinytheme("lumen"),
    
    # GP visualization
    
    tabPanel(title ="About", #New feature number 3: added an "About" panel to introduce the app
             fluidRow(column(width = 10, 
                             div(style = "height:20px; font-size:35px;", 
                                 "Vancouver's Ballroom Archive Explorator (VanBAE)")),
                      column(width = 2, align = "center",
                             img(src = "logo.png", height = 60, width = 60,))),
             tags$hr(),
             p("Welcome to VanBAE!"),
             p("This web tool will allow you to explore the data in the Vancouver Ballroom Archive."),
             p("Here you can easily visualize:"),
             tags$ul(
                 tags$li("Individual GPs: Here you can explore the cumulative Grand Prizes that 
                         have been awarded to each person in the scene."),
                 tags$li("House GPs: Here you can explore the cumulative Grand Prizes that 
                         have been awarded to each kiki house in the scene."),
                 tags$li("Balls details: Here you visualize data about each ball that has been celebrated in 
                         the scene, such as name, judges, commentator, host, DJ and venue.")
             ),
             br(),
             "If you want to learn more about Ballroom, please visit",tags$a(href="https://www.vanvoguejam.com/ballroom-history", "Van Vogue Jam."),
             br(), br(),
             fluidRow(column(width = 4, align = "right",
                             img(src = "pic1.jpg", 
                                 height = 300, width = 450)),
                      column(width = 4, align = "right",
                             img(src = "pic2.jpg", 
                                 height = 300, width = 450)),
                      column(width = 4, align = "right",
                             img(src = "pic3.jpg", 
                                 height = 300, width = 450))),
             br(),br(),
             tags$i("This app was developed by Sci Gvasalia (Erick Navarro-Delgado). If you have any questions or suggestions please contact ericknadel98@hotmail.com.")
    ),
    
    #### Individual GPs ####
    tabPanel("Individual GPs",
             # Sidebar with a slider input for number of bins
             div(id = "app_info", class = "collapse out", 
                 p("This tab allows you to explore the Grand Prizes (GPs) awarded to individuals in the Vancouver Ballroom scene.
                   The data is based on VKBA data. "),
                 p(""),
                 p("First, there is a Plot tab, here you can select a plot type, either a bar or line plot. The bar plot shows the total number of GPs awarded to each person, 
                   whereas the line plot shows the cumulative number of GPs awarded over time. You can also select the categories of GPs to display, 
                   the people, the number of top individuals, and the time window.  
                   You can also filter the data to show only GPs that have been approved by Vancouver Kiki Ballroom Alliance as Of The Year (OTY) eligible.
                   Whithin the plots, people with darker color represent a higher number of cumulative GPs."),
                 p("Additionally, if you click on the Full Table tab, you can visualize and explore a table with the data of all participants and categories selected whithin the time window (i.e. without the Top Participants number restriction).")
                 
             ),
             
             
             
             HTML("<button type='button' class='btn' data-toggle='collapse' style='float:left' data-target='#app_info'><span class='glyphicon glyphicon-collapse-down'></span> More Information</button>"),
             
             br(),  br(), 
             sidebarLayout(
                 sidebarPanel(
                     radioGroupButtons(
                         inputId = "individualgp_plot_choice",
                         label = "Choose a plot:", 
                         choices = c(`<i class='fa fa-bar-chart'></i>` = "bar", 
                                     `<i class='fa fa-line-chart'></i>` = "line"),
                         justified = TRUE,
                         selected = "bar"
                         ),
                     virtualSelectInput(
                         inputId = "individualgp_category",
                         label = "Choose one or more categories:", 
                         choices = categories_list(data_balls),
                         #selected = unlist(categories_list(data_balls)),
                         multiple = TRUE,
                         width = "100%",
                         search = TRUE,
                         hideClearButton = FALSE, 
                         optionsCount = 5,
                         dropboxWrapper = "body"
                         ),
                     virtualSelectInput(
                         inputId = "individualgp_person",
                         label = "Choose one or more people:", 
                         choices = ident_house_mem(data_balls),
                         multiple = TRUE,
                         selected = unlist(ident_house_mem(data_balls)),
                         width = "100%",
                         search = TRUE,
                         hideClearButton = FALSE, 
                         optionsCount = 5,
                         dropboxWrapper = "body"
                         ),
                     sliderTextInput(
                         inputId = "individualgp_n_people",
                         label = "Top participants:", 
                         choices = seq(from = 1,
                                       to = 10,
                                       by = 1),
                         selected = 10,
                         grid = TRUE
                     ),
                     airMonthpickerInput(inputId = "individualgp_time",
                                         label = "Time window :", 
                                         value = c(max(data_balls$Date - 365),max(data_balls$Date)),
                                         range = TRUE,
                                         minDate = min(data_balls$Date),
                                         maxDate = max(data_balls$Date+31),
                                         highlightedDates = unique(data_balls$Date),
                                         clearButton = FALSE,
                                         autoClose = TRUE,
                                         dateFormat = "MMMM yyyy",
                                         width = "100%"
                                         ),
                     checkboxGroupButtons(
                         inputId = "individualgp_OTY_approved",
                         label = "OTY approved balls:",
                         choices = c("Yes", "No"),
                         selected = c("Yes"),
                         justified = TRUE,
                         checkIcon = list(
                             yes = icon("ok", 
                                        lib = "glyphicon"))
                     )
            ## Deprecated: having the dates as a range
            # sliderInput("DatesMerge",
            #             "Dates:",
            #             min = as.Date("2018-01-22","%Y-%m-%d"),
            #             max = as.Date("2020-04-04","%Y-%m-%d"),
            #             value= c(as.Date("2018-01-22","%Y-%m-%d"),as.Date("2020-04-04","%Y-%m-%d")),
            #             timeFormat="%Y-%m-%d"
            # ),
            ),
            
            mainPanel(
                tabsetPanel(
                    tabPanel("Plot", plotOutput("individualgp_plot")), 
                    tabPanel("Full table",DTOutput("individualgp_table"))
                )
            )
            )),
            
    #### House GPs ####
    # tabPanel("House GPs",# Sidebar with a slider input for number of bins
    #          div(id = "app_info", class = "collapse out", 
    #              p("This tab allows you to explore the Grand Prizes (GPs) awarded to individuals in the Vancouver Ballroom scene.
    #                The data is based on the Vancouver Ballroom Archive, which is a collection of data on the balls 
    #                celebrated in the scene. "),
    #              p(""),
    #              p("First, there is a Plot tab, here you can select a plot type, either a bar or line plot. The bar plot shows the total number of GPs awarded to each person, 
    #                whereas the line plot shows the cumulative number of GPs awarded over time. You can also select the categories of GPs to display, 
    #                the people, the number of top individuals, and the time window.  
    #                You can also filter the data to show only GPs that have been approved by Vancouver Kiki Ballroom Alliance as Of The Year (OTY) eligible.
    #                Whithin the plots, people with darker color represent a higher number of cumulative GPs."),
    #              p("Additionally, if you click on the Full Table tab, you can visualize and explore a table with the data of all participants and categories selected whithin the time window (i.e. without the Top Participants number restriction).")
    #              
    #          ),
    #          
    #          
    #          
    #          HTML("<button type='button' class='btn' data-toggle='collapse' style='float:left' data-target='#app_info'><span class='glyphicon glyphicon-collapse-down'></span> More Information</button>"),
    #          
    #          br(),  br(), 
    #          sidebarLayout(
    #              sidebarPanel(
    #                  radioGroupButtons(
    #                      inputId = "individualgp_plot_choice",
    #                      label = "Choose a plot:", 
    #                      choices = c(`<i class='fa fa-bar-chart'></i>` = "bar", 
    #                                  `<i class='fa fa-line-chart'></i>` = "line"),
    #                      justified = TRUE,
    #                      selected = "bar"
    #                  ),
    #                  virtualSelectInput(
    #                      inputId = "individualgp_category",
    #                      label = "Choose one or more categories:", 
    #                      choices = categories_list(data_balls),
    #                      #selected = unlist(categories_list(data_balls)),
    #                      multiple = TRUE,
    #                      width = "100%",
    #                      search = TRUE,
    #                      hideClearButton = FALSE, 
    #                      optionsCount = 5,
    #                      dropboxWrapper = "body"
    #                  ),
    #                  checkboxGroupButtons(
    #                      inputId = "housegp_house",
    #                      label = "Choose one or more houses",
    #                      choices = unique(data_balls$House),
    #                      direction = "vertical"
    #                  ),
    #                  sliderTextInput(
    #                      inputId = "individualgp_n_people",
    #                      label = "Top participants:", 
    #                      choices = seq(from = 1,
    #                                    to = 10,
    #                                    by = 1),
    #                      selected = 10,
    #                      grid = TRUE
    #                  ),
    #                  airMonthpickerInput(inputId = "individualgp_time",
    #                                      label = "Time window :", 
    #                                      value = c(max(data_balls$Date - 365),max(data_balls$Date)),
    #                                      range = TRUE,
    #                                      minDate = min(data_balls$Date),
    #                                      maxDate = max(data_balls$Date+31),
    #                                      highlightedDates = unique(data_balls$Date),
    #                                      clearButton = FALSE,
    #                                      autoClose = TRUE,
    #                                      dateFormat = "MMMM yyyy",
    #                                      width = "100%"
    #                  ),
    #                  checkboxGroupButtons(
    #                      inputId = "individualgp_OTY_approved",
    #                      label = "OTY approved balls:",
    #                      choices = c("Yes", "No"),
    #                      selected = c("Yes"),
    #                      justified = TRUE,
    #                      checkIcon = list(
    #                          yes = icon("ok", 
    #                                     lib = "glyphicon"))
    #                  )
    #                  ## Deprecated: having the dates as a range
    #                  # sliderInput("DatesMerge",
    #                  #             "Dates:",
    #                  #             min = as.Date("2018-01-22","%Y-%m-%d"),
    #                  #             max = as.Date("2020-04-04","%Y-%m-%d"),
    #                  #             value= c(as.Date("2018-01-22","%Y-%m-%d"),as.Date("2020-04-04","%Y-%m-%d")),
    #                  #             timeFormat="%Y-%m-%d"
    #                  # ),
    #              ),
    #              
    #              mainPanel(
    #                  tabsetPanel(
    #                      tabPanel("Plot", plotOutput("individualgp_plot")), 
    #                      tabPanel("Full table",DTOutput("individualgp_table"))
    #                  )
    #              )
    #          )
    # ),
    #### Balls ####
    tabPanel("Balls",
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
    )
)
