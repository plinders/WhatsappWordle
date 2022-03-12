#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(rwhatsapp)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Wordle Statistics from Whatsapp"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("whatsappFile", "Choose Whatsapp Export as .zip or .txt", accept = ".zip|.txt")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), plotOutput("scoreDist"), plotOutput("scoreTime"))
            ),
            fluidRow(
                tableOutput("authorStats"), 
                tableOutput("authorFail"),
                tableOutput("gameStats")
            )
         )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    chat <- reactive({
        req(input$whatsappFile)
        showNotification("Loading Whatsapp Chat...", type = "message")
        chat <- rwa_read(input$whatsappFile$datapath)
        chat <- chat %>% filter(!is.na(author))
        chat <- chat %>% select(time, author, text, source)
        
        df <- chat %>% filter(str_detect(text, "Wordle \\d"))
        df <- df %>% mutate(WordleGame = str_extract(text, "(?!Wordle )(\\d+)[^/\\ ]"),
                            Guess = str_extract(text, "(X|\\d)\\/\\d"))
        df <- df %>% mutate(Score = case_when(
            Guess == "1/6" ~ 1,
            Guess == "2/6" ~ 2,
            Guess == "3/6" ~ 3,
            Guess == "4/6" ~ 4,
            Guess == "5/6" ~ 5,
            Guess == "6/6" ~ 6,
            Guess == "X/6" ~ NA_real_
        ))
        df$WordleGame <- factor(df$WordleGame)
        return(df)
    })
    
    output$scoreDist <- renderPlot({
        ggplot(chat(), aes(x = Score, color = author, fill = author)) + 
            geom_density() +
            scale_color_brewer(palette = "Dark2") +
            scale_fill_brewer(palette = "Dark2") +
            facet_wrap(~author, labeller = labeller(groupwrap = label_wrap_gen(width = 10, multi_line = TRUE))) +
            theme_minimal() +
            theme(legend.position = "none")
    })
    
    output$scoreTime <- renderPlot({
        ggplot(chat(), aes(x = time, y = Score, color = str_wrap(author, 15), fill = str_wrap(author, 15))) +
            geom_point(alpha = 0.3) +
            geom_smooth(se = FALSE, alpha = 0.8) +
            scale_color_brewer(palette = "Dark2") +
            scale_fill_brewer(palette = "Dark2") +
            scale_y_reverse() +
            theme_minimal() +
            theme(legend.position = "right")
    })
    
    output$authorStats <- renderTable({
        chat() %>% 
            replace(is.na(.), 10) %>% 
            group_by(author) %>%
            summarise(Average = mean(Score, na.rm = TRUE)) %>%
            arrange(Average)
    })
    
    output$authorFail <- renderTable({
        chat() %>% 
            group_by(author) %>% 
            summarise(n = n(), fail = sum(is.na(Score))) %>% 
            arrange(fail) %>% 
            mutate(Percent_win = (1-fail / n) * 100)
    })
    
    output$gameStats <- renderTable({
        chat() %>% 
            replace(is.na(.), 10) %>% 
            group_by(WordleGame) %>%
            summarise(Average = mean(Score, na.rm = TRUE)) %>%
            arrange(Average)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
