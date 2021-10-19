library("MASS")
library("psych")

library("shiny")
library("shinydashboard")

ui <- dashboardPage(
  dashboardHeader(title = "MM*Stat"),
  dashboardSidebar(
    tags$div(HTML("<h3>Multicollinearity<h3>"), HTML("<h4>y=x<sub>1</sub>+x<sub>2</sub>+err</h4>"), align="center"),
    sliderInput("cor", "Correlation(X1,X2)", 0, 9, 2, step=0.01),
    checkboxInput("std", "Standardized regression coefficients", FALSE),
    sliderInput("n", "Sample size (logarithmic)", 1, 3, 2, step=0.01),
    sliderInput("B", "Samples (logarithmic)", 1, 3, 2, step=0.01)
    #sliderInput("ey", "SD of errors (logarithmic)", -9, 1, -1, step=0.01)
  ),
  dashboardBody(
    tags$head(tags$style("#plot1{height:80vh !important;}")),
    plotOutput("plot1", width="100%")
  )
)

relative <- function(p, x=c(0,1)) {
  min(x, na.rm=TRUE)+p*(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
}

lm.beta <- function (MOD)  {
  b  <- MOD$coefficients
  sd <- apply(MOD$model, 2, sd)
  beta <- b[-1]*sd[-1]/sd[1] 
  return(beta)
}

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    r  <- 1-10^(-input$cor)
    n  <- as.integer(10^input$n)
    #browser()
    #ey <- 10^input$ey
    ey <- 0.1
    B  <- 10^input$B
    b  <- matrix(NA, ncol=3, nrow=B)
    bs <- matrix(NA, ncol=2, nrow=B)
    t <- runif(n)
    for (i in 1:B) {
      x <- mvrnorm(n, mu=c(0,0), Sigma=matrix(c(1,r,r,1), nrow=2))
      #  pca <- principal(x)
      y <- x[,1]+x[,2]+rnorm(n, 0, ey)
      lmi <- lm(y~x)
      b[i,]  <- lmi$coefficients
      bs[i,] <- lm.beta(lmi)
    }  
    if (input$std) {
      xy   <- data.frame(x=bs[,1], y=bs[,2])
      main <- "Estimated standardized regression coefficients"
      xlab <- expression(paste(b[1]^"*"))
      ylab <- expression(paste(b[2]^"*"))
    } else {
      xy   <- data.frame(x=b[,2], y=b[,3])
      main <- "Estimated regression coefficients"
      xlab <- expression(b[1]~~(beta[1]==1))
      ylab <- expression(b[2]~~(beta[2]==1))
    }
    par(mar=c(5.1, 5.1, 4.1, 2.1))
    xlim <- range(c(0,2,xy[,1]))
    ylim <- range(c(0,2,xy[,2]))
    plot(xy, asp=TRUE, main=main, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim,
         sub=sprintf("Sample size = %.0f, Samples = %.0f", n, B),
         pch=19, cex=0.5
    )
    usr <- par("usr")
    pcb <- prcomp(b[,2:3])
    vm  <- 1/(1-r^2)
    vm  <- if (vm>100000) sprintf("VIF=%e", vm) else sprintf("VIF=%.1f", vm)
    if (((pcb$rotation[1,1]<0) &&  (pcb$rotation[2,1]>0)) || ((pcb$rotation[1,1]>0) &&  (pcb$rotation[2,1]<0))) {
      text(relative(0, usr[1:2]), relative(0.1, usr[3:4]), sprintf("cor(x)=%.10f", r), pos=4)
      text(relative(1, usr[1:2]), relative(0.9, usr[3:4]), vm, pos=2)
    } else {
      text(relative(0, usr[1:2]), relative(0.9, usr[3:4]), sprintf("cor(x)=%.10f", r), pos=4)
      text(relative(1, usr[1:2]), relative(0.1, usr[3:4]), vm, pos=2)
    }
    if (!input$std) points(1,1,col="red", pch=19)
  })
}

shinyApp(ui, server)
