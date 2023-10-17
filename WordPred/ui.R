# ui.R

library(shiny)
library(shinydashboard)

header <- dashboardHeader()

sidebar <- dashboardSidebar(
        sidebarMenu(
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("How to Use", tabName = "how_to_use", icon = icon("info")),
                menuItem("Text Prediction Methods", tabName = "text_prediction", icon = icon("book"))
        )
)

body <- dashboardBody(
        tabItems(
                # Home tab
                tabItem(
                        tabName = "home",
                        fluidPage(
                                titlePanel("Text Prediction App"),
                                textInput("user_input", label = "Enter your text:"),
                                actionButton("predict_button", "Predict"),
                                actionButton("clear_button", "Clear Input"),
                                htmlOutput("prediction_output")
                        )
                )
        )
)

dashboardPage(header, sidebar, body)
