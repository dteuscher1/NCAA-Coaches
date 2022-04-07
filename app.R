library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)


ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "view", icon = icon("info")),
            menuItem("Coach Information", tabName = "table", icon = icon("table")),
            menuItem("Expected vs. Actual Points", tabName = 'expect', icon = icon("chart-line")),
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
                            p("This app takes the text of address from the General Conference of the Church of Jesus Christ of  Latter-Day Saints from April 1971 through April 2021. There are a few features of this app that can be explored, which are explained in detail below. The purpose of this app is to take analysis and visualization of text data from the General Conference addresses and presenting in an interactive and interesting way. If there are any questions or feedback, please contact Skyler Gray, David Teuscher, or Daniel Garrett"),
                            h3(strong("Coach and Team Information:")),
                            p("The speakers tab allows a user to explore the most frequently used words by a speaker throughout all of the talks they have given between April 1971 and April 2021. The top words for each speaker is displayed in a bar chart. The user also has the option to explore the topics that a speaker has talked about most frequently. The topics were obtained from tags that were given to the talks by the Church of Jesus Christ of Latter-Saints and was not done by any of us. "),
                            h3(strong("Expected vs. Actual")),
                            p("The trends tab allows the user to explore the frequency of a word over time between conferences session. The option is available to explore the frequency of topics over time as well"),
                            h3(strong("Location Results:")),
                            p("A word cloud can be created for the most frequent words during a conference session from April 1971 until April 2021")
                        ))
            ),
            # First tab content
            tabItem(tabName = "table"
            ),
            tabItem(tabName = "expect"
            ),
            tabItem(tabName = "loc"
            )
        )    
    )
)


server <- function(input, output, session){
}

shinyApp(ui = ui, server = server)
