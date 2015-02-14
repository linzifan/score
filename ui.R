library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Group Stage Ranking Calculator"),
  sidebarPanel(
    p("This tiny app could automatically update team name and current ranking after you
      input such information. It will also give a summary of current situation.", style="color:grey"),
    h3("Ranking Results",img(src = "soccer.png", width=24, height=24)),
    verbatimTextOutput("result"),
    verbatimTextOutput("summary"),
    h4('Enter each team\'s name'),
    textInput('name1','',"team1"),
    textInput('name2','',"team2"),
    textInput('name3','',"team3"),
    textInput('name4','',"team4"),
    br(),
    actionButton("namerefresh", "Refresh Names"),
    br(),
    br(),
    strong('Note:'),
    p("The sample schedule is taken from 2015 Asian Cup. Different Games may have diffrent schedule.")
    ),
  mainPanel(
    h4('Enter each game\'s result'),
    p('Please pay attention to the player order in each game.'),
    actionButton("gamerefresh", "Update"),
    h5('Round 1'),
    verbatimTextOutput("GAB"),
    fluidRow(
      column(2, numericInput('A1','',NA,min=0)), 
      column(2,offset=3, numericInput('B1','',NA,min=0))
      ),
    verbatimTextOutput("GCD"),
    fluidRow(
      column(2, numericInput('C1','',NA,min=0)), 
      column(2,offset=3, numericInput('D1','',NA,min=0))
    ),
    h5('Round 2'),
    verbatimTextOutput("GBC"),
    fluidRow(
      column(2, numericInput('B2','',NA,min=0)), 
      column(2,offset=3, numericInput('C2','',NA,min=0))
    ),
    verbatimTextOutput("GDA"),
    fluidRow(
      column(2, numericInput('D2','',NA,min=0)), 
      column(2,offset=3, numericInput('A2','',NA,min=0))
    ),
    h5('Round 3'),
    verbatimTextOutput("GAC"),
    fluidRow(
      column(2, numericInput('A3','',NA,min=0)), 
      column(2,offset=3, numericInput('C3','',NA,min=0))
    ),
    verbatimTextOutput("GDB"),
    fluidRow(
      column(2, numericInput('D3','',NA,min=0)), 
      column(2,offset=3, numericInput('B3','',NA,min=0))
    )
   )       
  )
)
