library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(gt)
library(cfbplotR)
source("global.R")

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "view", icon = icon("info")),
            menuItem("Coach Information", tabName = "table", icon = icon("table")),
            menuItem("Expected vs. Actual", tabName = 'expect', icon = icon("chart-line")),
            menuItem("Location Results", tabName = 'loc', icon = icon("map-pin"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "view",
                    fluidRow(
                        column(1),
                        box(
                            width = 10,
                            h1(strong("Analyzing NCAA Coaches on Plays out of Timeouts")),
                            p("This app shows the results of analyzing NCAA Men's Basketball coaches on plays out of a timeout (ATO). Play by play data was taken beginning with the 2013-2014 season through part of the 2021-2022 college basketball season and each play immediately following a timeout was extracted. Since there are over 300 Division I men's college basketball teams, only teams from the ACC, Big 12, Big 10, Big East, SEC, Pac-12, and WCC are included. The expected points per possession were modeled using a random forest, using the score differential, time remaining, and location of the shot. The expected points per possession are compared with the actual points per possession for each coach and team. The code for the analysis can be found at the GitHub repository ", a("here.", href = "https://github.com/dteuscher1/NCAA-Coaches"), "If there are any questions about this project, feel free to contact David Teuscher. Contact information can be found on my GitHub profile."),
                            h3(strong("Coach and Team Information:")),
                            p("A table showing the coach, the team they coached for, expected and actual points per possession, and the difference between expected and actual is provided for exploration. Options are provided to look at results by year as well as teams, regardless of coach"),
                            h3(strong("Expected vs. Actual")),
                            p("This tab shows a plot of the expected points against the actual points per possession for each coach. There are options to select a single year or over all of the years as well as an option to limit the results to a specific conference."),
                            h3(strong("Location Results:")),
                            p("The location results tab shows the expected points per possession across the court, which illustrates the ideal locations to get shots that will maximize expected points per possession.")
                        ))
            ),
            # First tab content
            tabItem(tabName = "table",
                    fluidRow(
                        box(width = 6,
                            column(width = 6,
                                   selectizeInput('team2', 'Choose number of minimum plays', 1:100, 100),
                                   actionButton('update2', 'Update'),
                                   downloadButton("downloadData", "Download")
                            )
                        )
                    ),
                    fluidRow(
                        box(width = 12,
                            gt_output("selected")
                        )
                    )
            ),
            tabItem(tabName = "expect",
                    fluidRow(
                        box(
                            selectizeInput("team_chord", "Choose a team", c("BYU")),
                            actionButton('update1', 'Update')
                        )    
                    ),
                    fluidRow(
                        box(height = 20, width = 12,
                            plotOutput('residual')
                        )    
                    )
            ),
            tabItem(tabName = "loc"
            )
        )    
    )
)


server <- function(input, output, session){
    rplot_selected <- eventReactive(input$update2, {
        displayTable <- attempt2 %>% 
            filter(`Number of Plays` > as.numeric(input$team2)) %>%
            gt() %>% 
            gt_fmt_cfb_logo(columns = c(`Conference Logo`, `Logo`)) %>%
            gt_merge_stack_team_color(Team,`Team Name`,Team)
        displayTable
    })
    rplot_resid <- eventReactive(input$update1, {
        plot <- ggplot(attempt2, aes(x = `Expected Points`, y = `Actual Points`)) +
            geom_median_lines(aes(v_var = `Expected Points`, h_var = `Actual Points`)) +
            geom_cfb_logos(aes(team = Team), width = 0.025) +
            labs(y = "Actual Points per Possession",x = "Expected Points Per Possession") +
            theme_bw()
        plot
    })
    output$selected <- render_gt({
        expr = rplot_selected()
    })
    output$residual <- renderPlot(rplot_resid())
}

shinyApp(ui = ui, server = server)
