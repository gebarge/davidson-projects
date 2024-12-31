#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

server <- function(input, output) {
    race <- readRDS("join_left_race.RDS")
    life<- readRDS("join_left_race.RDS")
    income<- readRDS("join_left_income.RDS")
    facet <- readRDS("join_left_race.RDS")
    health <- readRDS("join_left_insurance.RDS")
    
    
    output$race <- renderTmap({
        st_sf(race) %>%
            filter(variable == input$map) %>%
            tm_shape() +
            tm_polygons("pct", title = "Percent of Total Tract Population")})
    
    output$life <- renderTmap({
        st_sf(life) %>%
            tm_shape() +
            tm_polygons("life_expectancy", title = "Life Expectancy")})
    
    output$income <- renderTmap({
        st_sf(income) %>%
            tm_shape() +
            tm_polygons("summary_est", title = "Median Income", breaks = c(0, 25000, 35000, 50000, 75000, 100000, 150000, 200000))})
    
    output$facet_plot <- renderPlot({
        facet %>%
            filter(variable %in% input$scatterplot) %>% 
            ggplot(aes(x=pct,
                       y = life_expectancy, color = variable))+
            geom_point() + 
            labs(x = "Percent of Total Tract Population", y = "Life Expectancy", color = "Race")})
    
    output$plotgraphs <- renderPlot({   
        if (input$graphs =="Income"){
            income %>%
                filter(!is.na(life_expectancy)) %>% 
                ggplot(aes(y =  cut_number(life_expectancy, n = 10),
                           x = estimate))+
                geom_boxplot()+
                labs(x = "Income", y = "Life Expectancy")}
        else if(input$graphs == "Health Insurance"){
            health %>%
                ggplot(aes(y = life_expectancy,
                           x = pct)) + 
                geom_point()+ 
                labs(x = "Percent of People with Health Insurance", y = "Life Expectancy")}
    }) 
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
