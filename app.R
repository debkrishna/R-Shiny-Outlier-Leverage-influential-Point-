#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a plot
ui <- fluidPage(

    # Application title
    titlePanel("Regression Analysis(Outlier,Leverage point,Influential point)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("p",
                        "Input X value(Speed):",
                        min = -10,
                        max = 50,
                        value = 30),
            sliderInput("q",
                        "Input y value(Distance)",
                        min = -30,
                        max = 200,
                        value = 30)
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

server <- function(input, output) {

    output$distPlot <- renderPlot({
        H=cars$speed
        V=cars$dist
        x=append(H,input$p)
        y=append(V,input$q)
        lg=lm(V~H)
        lg_o=lm(y~x)
        y_est=lg$coefficients[1]+(H*lg$coefficients[2])
        y_est_o=lg_o$coefficients[1]+(x*lg_o$coefficients[2])
        plot(x,y_est_o,type = 'b',col="RED",xlim = c(-10,50),ylim=c(-30,160),ylab = "Distance",xlab = "Speed",main="Effect of Outlier,Leverage and Influential point",pch=17)
        points(H,y_est,type = 'b',col="BLUE",pch=17)
        points(H,V,col="GREEN",pch=16)
        points(input$p,input$q,col="YELLOW",pch=16)
        legend(-10,150,legend=c("Observed values","Regression line", "Regression line(With one extra point)"),col=c("green","blue","red"), lty=1, cex=0.8)
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
