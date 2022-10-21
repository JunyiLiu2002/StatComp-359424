#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Number of n(sample size):",
                        min = 0,
                        max = 100,
                        value = 10),
      
        
        sliderInput("shape1",
                    "Number of Shape1:",
                    min = 0,
                    max = 10,
                    value = 5),
        
        sliderInput("shape2",
                    "Number of Shape2:",
                    min = 0,
                    max = 10,
                    value = 5),
        sliderInput("x",
                    "location is:",
                    min = 0,
                    max = 1,
                    step = 0.05,
                    value = 0.5),
        
        
    ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

derivative2 <-function(x){
  -(sin(1/(x/3 + 0.1)) * (1/3/(x/3 + 0.1)^2) * (1/3/(x/3 + 0.1)^2) - 
      cos(1/(x/3 + 0.1)) * (1/3 * (2 * (1/3 * (x/3 + 0.1)))/((x/3 + 
                                                                0.1)^2)^2))
}

gaussian1 <- function(x){
  (1/sqrt(2*pi)*exp(-x^2/2))^2
}
gaussian2 <- function(x){
  x^2*1/sqrt(2*pi)*exp(-x^2/2)
}

hopt <- function(x_single,n,shape1,shape2,bw =5){
  sum1 <-integrate(gaussian1,-Inf,+Inf)$value
  #ddf <- D(D(expression(sin(1/(x/3+0.1))),'x'),'x')
  sum2 <- integrate(gaussian2,-Inf,+Inf)$value
  #fx <-density(x,kernel = 'gaussian',bw=bw)
  result <- n^(-1/5)*(sum1/((sum2*derivative2(x_single))^2*dbeta(x_single,shape1,shape2)))^(1/5)
  return(result)
}

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        n <-input$n
        shape1<-input$shape1
        shape2<-input$shape2
        x_single <- input$x
        x <- rbeta(n,shape1,shape2)
        x_grid = seq(0,1,.01)
        #fx <-density(x,kernel = 'gaussian',bw=bw)
        y_pre <- sin(1/(x/3+0.1))
        n <-input$n
        e <- rnorm(n,0,1)
        y <- y_pre +e
        result = hopt(x_single,n,shape1,shape2)
        
        
        plot(x_grid,sin(1/(x_grid/3+0.1)))
        abline(v =x_single ,col = "blue")
        abline(v =x_single-result, col="yellow")
        abline(v = x_single+result,col = "yellow")
        
        
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white',
         #    xlab = 'Waiting time to next eruption (in mins)',
          #   main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
