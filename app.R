#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(ggplot2)
library(reshape2)
library(dplyr)

songs <- read.csv("spotify_songs.txt")

spotify_genres <- c("edm","latin","pop","r&b","rap","rock")
variables <- c("track_popularity","danceability","energy", "key","loudness",
               "speechiness","acousticness", "instrumentalness",
               "liveness","valence","tempo","duration_ms")
saba_colors <- c("goldenrod3", "darkgreen", "aquamarine4","#11005e","darkred","black")

means_across_genres <- aggregate(select_if(songs,is.numeric), list(songs$playlist_genre), mean)
means_2 <- aggregate(select_if(songs,is.numeric), list(songs$playlist_genre, songs$playlist_subgenre), mean)
goodtext <- element_text(face="bold",size=14)

ui <- fluidPage(
    tabsetPanel(
    tabPanel("Histograms",
         titlePanel("Visualizing Spotify genres using histograms"),
    sidebarLayout(
            sidebarPanel(
                radioButtons("x","Variable to visualize",choices= variables),
                checkboxGroupInput("genres","Genres to be compared",
                        choices = spotify_genres,selected = spotify_genres)
            ),
            mainPanel(
               plotOutput("histPlot")
            )
        )),
    tabPanel("Scatter plots",
             titlePanel("Visualizing Spotify genres using scatter plots"),
             sidebarLayout(
                 sidebarPanel(
                     radioButtons("x_s","Variable for x", choices=variables, selected="valence"),
                     radioButtons("y_s", "Variable for y", choices=variables, selected="energy"),
                     checkboxGroupInput("genres_s","Genres to be compared",
                            choices = spotify_genres,selected = c("pop","rap"))
                 ),
                 mainPanel(
                     plotOutput("scatter")
                 )
        
            )),
    tabPanel("Means across genres",
             titlePanel("Visualizing means across Spotify genres"),
             sidebarLayout(
                 sidebarPanel(
                     radioButtons("x_m", "Variable to visualize", choices=variables)
                 ),
                 mainPanel(
                     plotOutput("means")
                 )
             )),
    tabPanel("Means across subgenres",
             titlePanel("Visualizing means across Spotify subgenres"),
             sidebarLayout(
                 sidebarPanel(
                     radioButtons("gen_for_subgen", "Genre to visualize", choices=spotify_genres),
                     radioButtons("y_sub","Variable to visualize", choices=variables)
                 ),
                 mainPanel(
                     plotOutput("subgenre_means")
                 )
             ))
))

server <- function(input, output) {
    plotData <- reactive({
        filter(songs, playlist_genre %in% input$genres)
    })
    
    plotData_s <- reactive({ 
        filter(songs, playlist_genre %in% input$genres_s)
    })
    
    means_for_gen <- reactive({
        filter(means_2, Group.1 == input$gen_for_subgen)
    })
    
    output$histPlot <- renderPlot({
        bins = 45
        if(input$x=="key"){
            bins = 12
        }
        ggplot(plotData(), aes_string(x=input$x, fill="playlist_genre")) + 
            geom_histogram(bins = bins) + theme_minimal() + theme(axis.title.x=goodtext,
                                                                  legend.text=goodtext,
                                                                  legend.title=goodtext) +
            scale_fill_manual(drop=FALSE,
                labels = spotify_genres,
                values = saba_colors)
    })
    
    output$scatter <- renderPlot({
        ggplot(plotData_s(), aes_string(x=input$x_s, y=input$y_s, color="playlist_genre")) + 
            geom_point(alpha=0.2) + theme_minimal()  + theme(axis.title.x=goodtext,
                                                             axis.title.y=goodtext,
                                                             legend.text=goodtext,
                                                             legend.title=goodtext) +
            scale_color_manual(drop=FALSE,labels = spotify_genres,
                               values = saba_colors)
    })
    output$means <- renderPlot({
        ggplot(means_across_genres, aes_string(x = "Group.1", y=input$x_m, fill="Group.1")) +
            geom_bar(stat="identity")+ theme_minimal() + theme(axis.title.x = element_blank(),
                                                               axis.text.x=goodtext,
                                                               axis.title.y=goodtext,
                                                               legend.position = "none") +
            scale_fill_manual(drop=FALSE,labels = spotify_genres,
                              values = saba_colors)
    })
    
    output$subgenre_means <- renderPlot({
        ggplot(means_for_gen(), aes_string("Group.2" , y=input$y_sub, fill="Group.2")) +
            geom_bar(stat="identity")+ theme_minimal() + theme(axis.title.x = element_blank(),
                                                               axis.text.x=goodtext,
                                                               axis.title.y=goodtext,
                                                               legend.position = "none")
    })
}

shinyApp(ui = ui, server = server)
