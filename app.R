library(shiny)
library(shinythemes)
library(rio)
library(tidyverse)
library(janitor)
library(colorblindr)
library(stringr)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("lumen"),

    # Application title
    titlePanel("KickStarter Data: Exploring Campaigns from 2013 by State"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(h4("Graph Customization Options:  "),
            selectInput("state", 
                        label = "Please select a state:",
                        choices = c("Alabama" = "9",
                                    "Texas" = "1",
                                    "California" = "2", 
                                    "Michigan" = "3",
                                    "South Carolina" = "4",
                                    "Ohio" = "5",
                                    "New Mexico" = "6",
                                    "Colorado" = "7",
                                    "New York" = "8",
                                    "Illinois" = "10",
                                    "Pennsylvania" = "11",
                                    "Georgia" = "12",
                                    "Florida" = "13",
                                    "Utah" = "14",
                                    "Iowa" = "15",
                                    "Indiana" = "16",
                                    "Virginia" = "17",
                                    "Connecticut" = "18",
                                    "North Carolina" = "19",
                                    "Wisconsin" = "20",
                                    "Oklahoma" = "21",
                                    "New Jersey" = "22",
                                    "Massachusetts" = "23",
                                    "Arizona" = "24",
                                    "Missouri" = "25",
                                    "Maryland" = "26",
                                    "New Hampshire" = "27",
                                    "Vermont" = "28",
                                    "Rhode Island" = "29",
                                    "Louisiana" = "30",
                                    "Oregon" = "31",
                                    "West Virginia" = "32",
                                    "Washington" = "33",
                                    "Minnesota" = "34",
                                    "Arkansas" = "35",
                                    "Maine" = "36",
                                    "Montana" = "37",
                                    "North Dakota" = "38",
                                    "Kentucky" = "39",
                                    "Mississippi" = "40",
                                    "South Dakota" = "41",
                                    "Idaho" = "42",
                                    "Nevada" = "43",
                                    "Wyoming" = "44",
                                    "Tennessee" = "45",
                                    "Kansas" = "46",
                                    "Delaware" = "47",
                                    "Nebraska" = "48",
                                    "District of Columbia" = "49"), 
                        selected = "31"),
            radioButtons("facet",
                        label = "Group to facet by:",
                        choices = c("Status" = "status",
                                    "Spotlight" = "spot_light",
                                    "Staff Pick" = "staff_pick"),
                        selected = "status"),
            submitButton("Apply changes", icon = icon("refresh"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ggdistPlot"),
           tableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    d <- import("./MasterKickstarter.csv", setclass = "tbl_df") %>% 
        clean_names()
    
    lower48 <- d %>% 
        select(-1:-3) %>% 
        filter(launched_at_y == 13 & 
                   country == "USA" & 
                   county != "Non-USA" & 
                   state != "Non-USA" &
                   status != "canceled") %>% 
        mutate(categories = as.factor(categories))
    
    levels(lower48$categories) <- sub("film%20&%20video", "film", levels(lower48$categories))
    
    lower48 <- data.frame(lapply(lower48, function(lower48) {
        if (is.character(lower48)) return(tolower(lower48))
        else return(lower48)
    }))

    output$ggdistPlot <- renderPlot({
        
        lower48_nest <- lower48 %>%
            group_by(state) %>%
            nest() %>%
            mutate(plot = map2(data, state, ~ggplot(.x, aes(backers_count, log(pledged))) +
                                   geom_point(aes(color = categories)) +
                                   geom_smooth(se = FALSE) +
                                   facet_wrap(input$facet) +
                                   labs(x = "Number of Backers", y = "Amount Pledged ($)", 
                                        color = "Categories", 
                                        title = "Number of campaign backers and money pledged", 
                                        subtitle = glue::glue("Kickstarter data for the state of {.y}")) +
                                   scale_color_OkabeIto() +
                                   theme_minimal() +
                                   theme(plot.title = element_text(face = "bold", hjust = 0.5), 
                                         plot.subtitle = element_text(hjust = 0.5),
                                         legend.position = "bottom",
                                         legend.title = element_text(face = "bold"),
                                         axis.title = element_text(face = "bold"))))
        lower48_nest[[3]][as.numeric(input$state)]
        })
    
    output$table <- renderDataTable({
        check_args <- function(data, 
                               group_var, 
                               sum_var
        ) {
            if(!is.data.frame(data)) {
                stop("Data supplied must be of type data frame.  Data supplied is not a data frame.")
            }
            if(!is.numeric(pull(data, !!enquo(sum_var)))) {
                stop("The variable to summarize must be numeric. The variable supplied is not numeric.")
            }
            if(is.numeric(pull(data, !!enquo(group_var)))) {
                warning("Warning: the grouping variable supplied is numeric, not categorical.")
            }
        }
        
        stat_calc <- function(data, 
                              group_var, 
                              outcome_var, 
                              .funs = list(n = ~length(.),
                                           n_valid = ~sum(!is.na(.)),
                                           n_miss = ~sum(is.na(.)),
                                           mean = ~mean(., na.rm = TRUE),
                                           sd = ~sd(., na.rm = TRUE),
                                           min = ~min(., na.rm = TRUE),
                                           max = ~max(., na.rm = TRUE))){
            
            check_args(data, !!enquo(group_var), !!enquo(outcome_var))
            
            data %>%
                group_by(!!enquo(group_var)) %>%
                summarize_at(vars(!!enquo(outcome_var)),
                             .funs)
        }
        
        as.data.frame(lower48_nest[[2]][as.numeric(input$state)])%>%
            stat_calc(., input$facet, backers_count) %>%
            datatable()
    })  #so we are really confused at this point: we want to put in a table at the bottom of the page
    # that will give summary statistics for the state the user selects, but we are getting tons of
    # errors that it is unable to find the object "backers_count", even when using the !!sym(), etc.
    # notations... help, please!! 

}

# Run the application 
shinyApp(ui = ui, server = server)
