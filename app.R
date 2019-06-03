library(shiny)
#install.packages("shinythemes")
library(shinythemes)
library(rio)
library(tidyverse)
library(janitor)
library(colorblindr)
library(stringr)
library(DT)

# Custom functions
# Summary function
stat_calc <- function(data, group_var, outcome_var, .funs = list( n = ~ length(.),
                                                                  n_valid = ~ sum(!is.na(.)),
                                                                  n_miss = ~ sum(is.na(.)),
                                                                  mean = ~ mean(., na.rm = TRUE),
                                                                  sd = ~ sd(., na.rm = TRUE),
                                                                  min = ~ min(., na.rm = TRUE),
                                                                  max = ~ max(., na.rm = TRUE))) {
  group <- enquo(group_var)
  outcome <- enquo(outcome_var)
  
  if (!is.data.frame(data)) {
    stop("Data supplied must be of type data frame.  Data supplied is not a data frame.")
  }
  if (is.numeric(pull(data, !!group))) {
    warning("The grouping variable supplied is numeric, not categorical.")
  }
  if (!is.numeric(pull(data, !!outcome))) {
    stop("The variable to summarize must be numeric. The variable supplied is not numeric.")
  }
  else{
    
    data %>%
      group_by(!!group) %>%
      summarize_at(vars(!!outcome),
                   .funs)
  }
}


# Function for list extraction
extract_col <- function(l, col) {
  l$data[[col]]
}



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
                                    
                                    "Arizona" = "24",
                                    
                                    "Arkansas" = "35",
                                    
                                    "California" = "2",
                                    
                                    "Colorado" = "7",
                                    
                                    "Connecticut" = "18",
                                    
                                    "Delaware" = "47",
                                    
                                    "District of Columbia" = "49",
                                    
                                    "Florida" = "13",
                                    
                                    "Georgia" = "12",
                                    
                                    "Idaho" = "42",
                                    
                                    "Illinois" = "10",
                                    
                                    "Indiana" = "16",
                                    
                                    "Iowa" = "15",
                                    
                                    "Kansas" = "46",
                                    
                                    "Kentucky" = "39",
                                    
                                    "Louisiana" = "30",
                                    
                                    "Maine" = "36",
                                    
                                    "Maryland" = "26",
                                    
                                    "Massachusetts" = "23",
                                    
                                    "Michigan" = "3",
                                    
                                    "Minnesota" = "34",
                                    
                                    "Mississippi" = "40",
                                    
                                    "Missouri" = "25",
                                    
                                    "Montana" = "37",
                                    
                                    "Nebraska" = "48",
                                    
                                    "Nevada" = "43",
                                    
                                    "New Hampshire" = "27",
                                    
                                    "New Jersey" = "22",
                                    
                                    "New Mexico" = "6",
                                    
                                    "New York" = "8",
                                    
                                    "North Carolina" = "19",
                                    
                                    "North Dakota" = "38",
                                    
                                    "Ohio" = "5",
                                    
                                    "Oklahoma" = "21",
                                    
                                    "Oregon" = "31",
                                    
                                    "Pennsylvania" = "11",
                                    
                                    "Rhode Island" = "29",
                                    
                                    "South Carolina" = "4",
                                    
                                    "South Dakota" = "41",
                                    
                                    "Tennessee" = "45",
                                    
                                    "Texas" = "1",
                                    
                                    "Utah" = "14",
                                    
                                    "Vermont" = "28",
                                    
                                    "Virginia" = "17",
                                    
                                    "Washington" = "33",
                                    
                                    "West Virginia" = "32",
                                    
                                    "Wisconsin" = "20",
                                    
                                    "Wyoming" = "44"),
                    
                         selected = "22"),
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
           DTOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    d <- import(here::here("/MasterKickstarter.csv"), setclass = "tbl_df") %>%  #changed path to use here() so this will work on other computers
        clean_names() 
    
    lower48 <- d %>% 
        select(-1:-3) %>% 
        filter(launched_at_y == 13 & 
                   country == "USA" & 
                   county != "Non-USA" & 
                   state != "Non-USA" &
                   status != "canceled" & 
                   status != "suspended") %>% 
        mutate(categories = as.factor(categories)) 
        
    
    levels(lower48$categories) <- sub("film%20&%20video", "film", levels(lower48$categories))
    
    lower48 <- data.frame(map(lower48, function(lower48) {
        if (is.character(lower48)) return(tolower(lower48))
        else return(lower48)
    }))

    lower48$status <- map_chr(lower48$status, stringr::str_to_title )
    
    lower48$categories <- map_chr(lower48$categories, stringr::str_to_title )
    
    lower48$spot_light <- map_chr(lower48$spot_light, stringr::str_to_title )
    
    lower48$staff_pick <- map_chr(lower48$staff_pick, stringr::str_to_title )
    
    output$ggdistPlot <- renderPlot({
        
        lower48_nest <- lower48 %>%
            group_by(state) %>%
            nest() %>%
            mutate(plot = map2(data, stringr::str_to_title(state), ~ggplot(.x, aes(log(backers_count), log(pledged))) +
                                   geom_smooth(se = FALSE, color = "grey70", size = .5) +
                                   geom_point(aes(color = categories), na.rm = TRUE, alpha = .6) +  # Remove missing values
                                   facet_wrap(input$facet) +
                                   labs(x = "Logged Number of Backers", y = "Logged Amount Pledged ($)", 
                                        color = "Categories", 
                                        title = "Number of campaign backers and money pledged", 
                                        subtitle = glue::glue("Kickstarter data for the state of {.y}")) +
                                   scale_color_viridis_d() +
                                   theme_minimal() +
                                   theme(plot.title = element_text(face = "bold", hjust = 0.5), 
                                         plot.subtitle = element_text(hjust = 0.5),
                                         legend.position = "bottom",
                                         legend.title = element_text(face = "bold"),
                                         axis.title = element_text(face = "bold"))))
        lower48_nest[[3]][as.numeric(input$state)]
        })
    
    output$table <- renderDT({
      
      lower48 %>%
        group_by(state) %>%
        nest() %>% 
        extract_col(., as.numeric(input$state)) %>% 
        stat_calc(., !!sym(input$facet), backers_count) %>%
        datatable(colnames = c("Status", "N", "Valid cases", "Missing cases", 
                               "Mean", "SD", "Minimum", "Maximum")) %>%
        formatRound(columns = c("mean", "sd") , digits = 2)
        
    })
    # })  #so we are really confused at this point: we want to put in a table at the bottom of the page
    # that will give summary statistics for the state the user selects, but we are getting tons of
    # errors that it is unable to find the object "backers_count", even when using the !!sym(), etc.
    # notations... help, please!! 

}

# Run the application 
shinyApp(ui = ui, server = server)
