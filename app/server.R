#### Load packages ####
library(shiny)
library(here)
library(readxl)
library(dplyr)
library(lubridate)
library(cowplot)
library(forcats)
library(rcartocolor)
library(ggplot2)
library(DT)
library(ggrepel)

#### Load ballroom data ####
data_balls <- read_excel("data.xlsx") %>% 
    drop_na() %>% 
    mutate(full_name = paste(Participant, House, sep = " "),
           Date = lubridate::dmy(Date))

#### Define server ####
function(input, output, session) {
    #### Category GP visualization ####
    # Get the filtered data based on the user input
    filtered_categories_GPs <- reactive({
        data_balls %>% 
            filter(Category %in% input$individualgp_category,
                   full_name %in% input$individualgp_person,
                   Date >= input$individualgp_time[1] & Date <= input$individualgp_time[2],
                   OTY_approved %in% input$individualgp_OTY_approved) %>% 
            group_by(full_name) %>% 
            summarize(GPs = n(),
                      Balls = paste(Ball, collapse = ", "),
                      Categories = paste(Category, collapse = ", ")) %>%
            rename(Name = full_name) %>%
            mutate(Name = factor(Name, unique(Name))) %>% 
            arrange(desc(GPs)) %>% #Order by number of Grand Prizes
            mutate(Name = fct_reorder(Name, GPs))
    })
    
    filtered_categories_cumulative <- reactive({
        data_balls %>% 
            filter(Category %in% input$individualgp_category,
                   full_name %in% input$individualgp_person,
                   Date >= input$individualgp_time[1] & Date <= input$individualgp_time[2],
                   OTY_approved %in% input$individualgp_OTY_approved) %>% 
            rename(Name = full_name) %>% 
            select(Name, Date) %>% 
            mutate(gps = 1) %>% 
            add_row("Name" = input$individualgp_person, # Adding this will make sure that in the final plot all the participants have a line until the current date
                    "Date" =input$individualgp_time[2], 
                    "gps" = 0)
    })
    
    # Create a plot with the selected data for the top 10 participants
    output$individualgp_plot <- renderPlot({
        if (input$individualgp_plot_choice %in% "bar") {
            filtered_categories_GPs() %>% 
                slice_head(n = input$individualgp_n_people) %>% 
                ggplot(aes(x = Name, y = GPs, fill =  GPs)) +
                geom_col() +
                cowplot::theme_cowplot()+
                xlab("Person") +
                ylab("Number of Grand Prizes") +
                coord_flip() + 
                guides(fill = "none") +
                scale_fill_carto_c(palette = "Mint") +
                labs(title = paste("Top", input$individualgp_n_people, "people with the most Grand Prizes"),
                     subtitle = paste("Time period:", input$individualgp_time[1], "to", input$individualgp_time[2]),
                     caption = "Data source: Vancouver Kiki Ballroom Alliance")
        } else {
            filtered_categories_cumulative() %>% 
                arrange(Date) %>% 
                filter(Name %in% (filtered_categories_GPs()  %>% 
                           slice_head(n = input$individualgp_n_people) %>% 
                           pull(Name))) %>%
                group_by(Name, Date) %>% 
                summarise(total_gps_date = sum(gps)) %>%
                mutate(cumulative_gps = cumsum(total_gps_date),
                       label = if_else(Date == max(Date), as.character(Name), NA_character_)) %>% 
                ggplot(aes(x =Date, y = cumulative_gps, color = Name, group=Name)) + 
                geom_point() + 
                geom_line() +
                geom_label_repel(aes(label = label),
                                 nudge_x = 1,
                                 max.overlaps = 20,
                                 na.rm = TRUE) +
                scale_color_carto_d(palette = "Earth") +
                scale_y_continuous(limits = c(0, NA)) +
                cowplot::theme_cowplot()+
                #cowplot::background_grid() +
                xlab("Date") +
                ylab("Number of Grand Prizes") +
                guides(color = "none") +
                labs(title = paste("Top", input$individualgp_n_people, "people with the most Grand Prizes"),
                     subtitle = paste("Time period:", input$individualgp_time[1], "to", input$individualgp_time[2]),
                     caption = "Data source: Vancouver Kiki Ballroom Alliance")
        }
    })
    
    #Send outputs
    output$individualgp_table = renderDT(filtered_categories_GPs())

}


