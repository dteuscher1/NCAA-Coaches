## Author: David Teuscher
## Last Edited: 04.12.22
## This script provides the code for the Shiny app presenting the results
## of this analysis
##############################################################################

# Load packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(gt)
library(cfbplotR)
library(tidyverse)

# Run the script that is needed for the app
source("global.R")

# Set up UI
ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        # Create a menu with 4 tabs; 
        sidebarMenu(
            menuItem("Overview", tabName = "view", icon = icon("info")),
            menuItem("Coach Information", tabName = "table", icon = icon("table")),
            menuItem("Expected vs. Actual", tabName = 'expect', icon = icon("chart-line")),
            menuItem("Sortable Table", tabName = 'sort', icon = icon("table"))
        )
    ),
    dashboardBody(
        tabItems(
            # Create a tab that contains text explaning the background of the project
            # and the layout of the app
            tabItem(tabName = "view",
                    fluidRow(
                        column(1),
                        box(
                            width = 10,
                            h1(strong("Analyzing NCAA Coaches on Plays out of Timeouts")),
                            p("This app shows the results of analyzing NCAA Men's Basketball coaches on plays out of a timeout (ATO). Play by play data was taken beginning with the 2013-2014 season through part of the 2021-2022 college basketball season and each play immediately following a timeout was extracted. Since there are over 300 Division I men's college basketball teams, only teams from the ACC, Big 12, Big 10, Big East, SEC, Pac-12, and WCC are included. The expected points per possession were modeled using a random forest, using the score differential, time remaining, and location of the shot. The expected points per possession are compared with the actual points per possession for each coach and team. The code for the analysis can be found at the GitHub repository ", a("here.", href = "https://github.com/dteuscher1/NCAA-Coaches"), "If there are any questions about this project, feel free to contact David Teuscher. Contact information can be found on my GitHub profile."),
                            h3(strong("Coach and Team Information")),
                            p("A table showing the coach, the team they coached for, expected and actual points per possession, and the difference between expected and actual is provided for exploration. Options are provided to look at results by year as well as teams, regardless of coach"),
                            h3(strong("Expected vs. Actual")),
                            p("This tab shows a plot of the expected points against the actual points per possession for each coach. There are options to select a single year or over all of the years as well as an option to limit the results to a specific conference."),
                            h3(strong("Sortable Table")),
                            p("This tab is similar to the Coach and Team information tab, but the tables provide the option to be sorted by any of the variables. The information from this table can also be downloaded.")
                        ))
            ),
            # Display a table with the top teams above their expected points per possession
            tabItem(tabName = "table",
                    fluidRow(
                        box(width = 6,
                            column(width = 6,
                                   # Provide option to select the minimum number of plays a team/coach had
                                   selectizeInput('team2', 'Choose number of minimum plays', 1:100, 100),
                                   selectizeInput('group', "Chose variables to group by", c("Team", "Coach", "Season"), selected = c("Team"), multiple = TRUE),
                                   actionButton('update2', 'Update')
                            )
                        )
                    ),
                    # Display the table output
                    fluidRow(
                        box(width = 12,
                            gt_output("selected")
                        )
                    )
            ),
            # Tab that shows a plot of expected against actual
            tabItem(tabName = "expect",
                    fluidRow(
                        box(
                            selectizeInput("team_chord", "Choose a team", c("BYU")),
                            actionButton('update1', 'Update')
                        )    
                    ),
                    # Display the plot
                    fluidRow(
                        box(height = 20, width = 12,
                            plotOutput('residual')
                        )    
                    )
            ),
            tabItem(tabName = "sort",
                    fluidRow(
                        box(width = 6,
                            column(width = 6,
                                   # Provide option to select the minimum number of plays a team/coach had
                                   selectizeInput('plays', 'Choose number of minimum plays', 1:100, 100),
                                   selectizeInput('group2', "Chose variables to group by", c("Team", "Coach", "Season"), selected = c("Team"), multiple = TRUE),
                                   actionButton('update3', 'Update'), 
                                   downloadButton("downloadData", "Download")
                            )
                        )
                    ),
                    # Display the table output
                    fluidRow(
                        box(width = 12,
                            dataTableOutput("sorted")
                        )
                    )
            )
        )    
    )
)

# Define server
server <- function(input, output, session){
    # Reactive function to create table
    rplot_selected <- eventReactive(input$update2, {
        # per possession and the number of plays out of a timeout
        attempt <- plays %>% 
            group_by_at(input$group) %>%
            summarize(expected = round(mean(RF), digits = 3),
                      actual = round(mean(possession_score), digits = 3), 
                      diff = round(actual - expected, digits = 3), 
                      n = n()) %>%
            arrange(desc(diff))
        
        # Join the data with the team data and change some teams and conferences to match the 
        # strings in the cfbplotR package
        attempt2 <- attempt %>% inner_join(team_info, by = c("Team" = "market")) %>%
            mutate(Team = ifelse(Team == "North Carolina State", "NC State", Team),
                   Team = ifelse(Team == "Miami (FL)", "Miami", Team)) %>%
            dplyr::select(-X, -id, -alias, -conf_alias) %>% 
            mutate(Conference_logo = ifelse(conference == "West Coast", "NCAA", conference),
                   Conference_logo = ifelse(Conference_logo == "Big East", "NCAA", Conference_logo),
                   Conference_logo = ifelse(Conference_logo == "Southeastern", "SEC", Conference_logo),
                   Conference_logo = ifelse(Conference_logo == "Atlantic Coast", "ACC", Conference_logo),
                   Conference_logo = ifelse(Conference_logo == "Pacific 12", "Pac-12", Conference_logo),
                   Team_logo = Team) %>%
            dplyr::select_at(c(input$group, "name", "Team_logo", "expected", "actual", "diff", "n", "conference", "Conference_logo"))
        
        # Rename the columns of the data frame
        names(attempt2) <- c(c(input$group), "Team Name", "Logo", "Expected Points", "Actual Points", "Difference", "Number of Plays", "Conference", "Conference Logo")
        # Filter table by the number of plays
        displayTable <- attempt2 %>% 
            filter(`Number of Plays` > as.numeric(input$team2)) %>%
            gt() %>% 
            # Format the team and conference logo
            gt_fmt_cfb_logo(columns = c(`Conference Logo`, `Logo`)) #%>%
            #gt_merge_stack_team_color(Team,`Team Name`,Team)
        displayTable
    })
    # Reactive function to draw acutal vs. expected plot
    rplot_resid <- eventReactive(input$update1, {
        
        plot <- ggplot(attempt2, aes(x = `Expected Points`, y = `Actual Points`)) +
            geom_median_lines(aes(v_var = `Expected Points`, h_var = `Actual Points`)) +
            geom_cfb_logos(aes(team = Team), width = 0.025) +
            labs(y = "Actual Points per Possession",x = "Expected Points Per Possession") +
            theme_bw()
        plot
    })
    
    rsort <- eventReactive(input$update3, {
        attempt <- plays %>% 
            group_by_at(input$group2) %>%
            summarize(expected = round(mean(RF), digits = 3),
                      actual = round(mean(possession_score), digits = 3), 
                      diff = round(actual - expected, digits = 3), 
                      n = n()) %>%
            filter(n > as.numeric(input$plays)) %>%
            arrange(desc(diff))
        names(attempt) <- c(input$group2, "Expected Points", "Actual Points", "Difference", "Number of Plays")
        DisplayTable <- attempt
    })
    # Define the outputs
    output$selected <- render_gt({
        expr = rplot_selected()
    })
    output$residual <- renderPlot(rplot_resid())
    output$sorted <- renderDataTable(rsort())
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("ncaa_coach_data.csv", sep="")
        },
        content = function(file) {
            write.csv({rsort()}, file, row.names = FALSE)
        }
    )
}

# Deploy the app
shinyApp(ui = ui, server = server)
