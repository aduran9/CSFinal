# title: 	"Capstone Final Project: Shiny User Interface"
# author: 	"aduran9"
# date: 	"15/9/2021"

suppressWarnings(library(shiny))
suppressWarnings(library(markdown))
shinyUI(navbarPage("Data Science Capstone",
                   tabPanel("NextWord",
                            HTML("<strong>NLP Application</strong>"),
                            br(),
                            # Sidebar
                            sidebarLayout(
                                sidebarPanel(
                                    helpText("Write a short sentence to begin the NextWord prediction"),
                                    textInput("inputString", "Type here: ",value = ""),
                                    br(),
                                    br(),
                                    br(),
                                    br()
                                ),
                                mainPanel(
                                    h2("Predicted NextWord"),
                                    verbatimTextOutput("prediction"),
                                    strong("Sentence Input:"),
                                    tags$style(type='text/css', '#text1 {background-color: rgba(255,255,0,0.40); color: blue;}'), 
                                    textOutput('text1'),
                                    br(),
                                    strong("Note:"),
                                    tags$style(type='text/css', '#text2 {background-color: rgba(255,255,0,0.40); color: black;}'),
                                    textOutput('text2')
                                )
                            )
                            
                   ),
                   tabPanel("About",
                            mainPanel(
                                includeMarkdown("about.md")
                            )
                   )
)
)