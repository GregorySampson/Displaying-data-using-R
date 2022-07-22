library("tidyverse")
library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(ggridges)
library(shinydashboard)

# data----
df = read.csv("netflix_titles.csv")
df <- df%>%add_column(year_added = gsub(".*,","",df$date_added))
df %>% select(country) %>% mutate( gsub(".*,","",df$country))
mov = df%>% filter(type == "Movie")
tv = df%>% filter(type == "TV Show")



#Combined_histogram-movie/TV----

his <- ggplot(df, aes(x=year_added, fill = type)) + 
  geom_histogram(alpha = 0.6, stat="count") + 
  labs(title="Content added over the years", 
       subtitle="") + 
  scale_fill_manual(name="group",values=c("red","blue"),labels=c("a","b"))

his_p <- ggplotly(his)
his_p



#Bar_Genre----

#~~~~~~~~~~~~~~~~~~~~~Count Data~~~~~~~~~~~~~~~~~~~~~~~~~~~
genreM<-count(mov %>% mutate(listed_in = strsplit(as.character(listed_in), ", ")) %>% unnest(listed_in), listed_in)
genreM$count <- genreM$n
genreM<- genreM[order(genreM$n,decreasing = TRUE),]

genreT<-count(tv %>% mutate(listed_in = strsplit(as.character(listed_in), ", ")) %>% unnest(listed_in), listed_in)
genreT$count <- genreT$n
genreT<- genreT[order(genreT$n,decreasing = TRUE),]

#~~~~~~~~~~~~~~~~~~~~~Count Plots~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Movies

barm <- ggplot(genreM,aes(x=reorder(listed_in,count), y=count )) +
  geom_bar(color='lightskyblue4', fill='lightskyblue', stat="identity")+
  labs(title="Content added over the years - Movies ")+
  xlab("Genre")+
  ylab("Number of Releases")+
  coord_flip()


bar_m <- ggplotly(barm)
bar_m

#TV Shows
bart <- ggplot(genreT,aes(x=reorder(listed_in,count), y=count )) +
  geom_bar(color='mediumorchid4', fill='mediumorchid', stat="identity")+
  labs(title="Content added over the years - Movies ")+
  xlab("Genre")+
  ylab("Number of Releases")+
  coord_flip()


bar_t <- ggplotly(bart)
bar_t




#release_Density----
p <- ggplot(df, aes(release_year, fill = type)) + geom_density(alpha = 0.4)

fig <- ggplotly(p)

fig

#lollipop_country ----

#data

country<-count(df%>% mutate(country = strsplit(as.character(country), ", ")) %>% unnest(country), country)
country$country <- as.character(gsub(",","",country$country))
country <- aggregate(n ~ country, data = country, FUN = sum)
country<- country[order(country$n,decreasing = TRUE),]


#Graph
lol <- ggplot(country %>% top_n(30), aes(x=country, y=n, colour=country)) + 
  geom_point(size=3) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  geom_segment(aes(x=country, 
                   xend=country, 
                   y=0, 
                   yend=n)) + 
  ylab("Number of Movies")+
  labs(title="Number of releases per country", 
       subtitle="Number of movies in a country") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


lollipop <- ggplotly(lol)
lollipop


#shiny ----


ui <- dashboardPage(
  dashboardHeader(title = "Interactive Graphs"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Histogram", tabName = "his", icon = icon("bar-chart-o")),
      menuItem("Bar Plot", tabName = "bar", icon = icon("bar-chart-o")),
      menuItem("Lollipop Plot", tabName = "lol", icon = icon("bar-chart-o")),
      menuItem("density Plot", tabName = "den", icon = icon("bar-chart-o")),
      menuItem("Data", tabName = "dat", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "his",
              fluidRow(
                box(
                  selectInput(inputId="xhis",label="Choose X axis",
                              choices = c("year_added",
                                          "release_year"), 
                              selected = "year_added")),
                
                box(
                  selectInput(
                  inputId="colour1",label="Choose colour",choices = c("Red"="Red","Blue"="Blue","purple"="purple","orange"="orange"),
                  selected = "Blue",multiple = F),
                  
                  radioButtons(inputId = "border1",label = "Select Border",choices = c("Black"="#000000","White"="#ffffff")),
                ),
                
                plotOutput("histPlot")
                
              )),

      tabItem(tabName = "bar",
              fluidRow(
                box(selectInput(
                  inputId="colour2",label="Choose colour",choices = c("Red"="Red","Blue"="Blue","purple"="purple","orange"="orange"),
                  selected = "Blue",multiple = F)),
                
                box(selectInput(
                  inputId="flip",label="flip graph?",choices = c("flip"="flip","noflip"="noflip"),
                  selected = "noflip",multiple = F)),
                
                box(
                  selectInput(
                  inputId="data_bar",label="Choose dataset",choices = c("Movies"="Movies","TV"="TV"),
                  selected = "TV",multiple = F)),
                
                  box(
                  sliderInput("height", "height", min = 200, max = 800, value = 300),
                  sliderInput("width", "width", min = 200, max = 800, value = 300)
                  ),
                
                plotOutput("barPlot", width = 300, height = 300)
                
                
              )),
          tabItem(tabName = "lol",
                  fluidRow( plotlyOutput("lolPlot", width = 700, height = 600))),
          
          tabItem(tabName = "den",
                  fluidRow( plotlyOutput("denPlot", width = 600, height = 600))),
      
      tabItem(tabName = "dat",
              fluidRow(DT::dataTableOutput("mytable")))
      
    ) 
  )
)



server <- function(input, output) {
  

  output$histPlot <- renderPlot({
  
    if(input$xhis=="year_added"){
      hisx = 13
    }else if (input$xhis=="release_year"){
      hisx = 8
    }
    
    
    if(input$colour1=="Red"){
      scolour = "#ff3300"
    }else if(input$colour1=="Blue"){
      scolour = "#3399ff"
    }else if(input$colour1=="purple"){
      scolour = "#D01C8B"
    }else if(input$colour1=="orange"){
      scolour = "#E66101"
    }
  
    his1 = ggplot(df, aes(df[,hisx])) + 
      geom_histogram(fill = scolour, col = input$border1, stat="count") + 
      labs(title="Content added over the years")+
      xlab(input$xhis)
        
    plot(his1)
  })
  

  output$barPlot <- renderPlot(
    width = function() input$width,
    height = function() input$height,
    {
    

    
    if(input$colour2=="Red"){
      scolour2 = "#ff3300"
    }else if(input$colour2=="Blue"){
      scolour2 = "#3399ff"
    }else if(input$colour2=="purple"){
      scolour2 = "#D01C8B"
    }else if(input$colour2=="orange"){
      scolour2 = "#E66101"
    }
      if(input$data_bar == "Movies"){
        data2 <- genreM
      }else if(input$data_bar == "TV"){
        data2 <- genreT
      }
        
    if(input$flip == "flip"){
      bar1 <- ggplot(data2,aes(x=reorder(listed_in,count), y=count )) +
        geom_bar(fill= scolour2, stat="identity")+
        labs(title="Content added over the years -  Movies ")+
        xlab("Genre")+
        ylab("Number of Releases")+
        coord_flip()
    }else if(input$flip == "noflip"){
      bar1 <- ggplot(data2,aes(x=reorder(listed_in,count), y=count )) +
        geom_bar(fill= scolour2, stat="identity")+
        labs(title="Content added over the years -  Movies ")+
        xlab("Genre")+
        ylab("Number of Releases")+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }
    

    plot(bar1)
    
  })
 
    output$lolPlot <- renderPlotly({
    lollipop
    
  })

    output$denPlot <- renderPlotly({
      fig
      
    })

  output$mytable = DT::renderDataTable({
   df
  })
  
}

shinyApp(ui = ui, server = server)

