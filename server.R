library(shiny)
library(shinyAce)
library(psych)
library(ltm)


shinyServer(function(input, output) {


    options(warn=-1)
    
    
    #get.text <- reactive({
    #if (input$show) {
    #      input$text
    #   }else{
    #       return(cat("a"))
    #   }
    #})



    bs <- reactive({
        x <- read.table(text=input$text, sep="\t")
        x <- as.matrix(x)
        total <- rowSums(x, na.rm=T)
        result1 <- describe(total)[2:13]

        y <- rowMeans(x, na.rm=T)
        result2 <- describe(y)[2:13]
        
        row.names(result1) <- "Total   "
        row.names(result2) <- "Average "
        return(list(result1, result2))
    })
    
    
    alpha.result <- reactive({
        x <- read.table(text=input$text, sep="\t")
        x <- as.matrix(x)

        if (input$colname) {
            z <- input$colname.text
            colnames(x) <- unlist(strsplit(z, "[\n, \t]"))
            }

        result1 <- cronbach.alpha(x)
        result2 <- alpha(x)
        list(result1, result2)
    })
    
    
    output$distPlot <- renderPlot({
        x <- read.table(text=input$text, sep="\t")
        x <- as.matrix(x)
        x <- rowMeans(x, na.rm=T)

        simple.bincount <- function(x, breaks) {
            nx <- length(x)
            nbreaks <- length(breaks)
            counts <- integer(nbreaks - 1)
            for (i in 1:nx) {
                lo <- 1
                hi <- nbreaks
                if (breaks[lo] <= x[i] && x[i] <= breaks[hi]) {
                    while (hi - lo >= 2) {
                        new <- (hi + lo) %/% 2
                        if(x[i] > breaks[new])
                        lo <- new
                        else
                        hi <- new
                    }
                    counts[lo] <- counts[lo] + 1
                }
            }
            return(counts)
        }
        
        nclass <- nclass.FD(x)
        breaks <- pretty(x, nclass)
        counts <- simple.bincount(x, breaks)
        counts.max <- max(counts)
        
        h <- hist(x, na.rm= T, las=1, breaks="FD", xlab= "Red vertical line shows the mean.",
        ylim=c(0, counts.max*1.2), main="", col = "cyan")
        rug(x)
        abline(v = mean(x, na.rm=T), col = "red", lwd = 2)
        xfit <- seq(min(x, na.rm=T), max(x, na.rm=T))
        yfit <- dnorm(xfit, mean = mean(x, na.rm=T), sd = sd(x, na.rm=T))
        yfit <- yfit * diff(h$mids[1:2]) * length(x)
        lines(xfit, yfit, col = "blue", lwd = 2)
    })
    
    
    output$boxPlot <- renderPlot({
        x <- read.table(text=input$text, sep="\t")
        x <- as.matrix(x)
        x <- rowMeans(x, na.rm=T)
        boxplot(x, horizontal=TRUE, xlab= "Mean and +/-1 SD are displayed in red.")
        stripchart(x, pch = 16, add = TRUE)
        points(mean(x, na.rm=T), 0.9, pch = 18, col = "red", cex = 2)
        arrows(mean(x, na.rm=T), 0.9, mean(x, na.rm=T) + sd(x, na.rm=T), length = 0.1, angle = 45, col = "red")
        arrows(mean(x, na.rm=T), 0.9, mean(x, na.rm=T) - sd(x, na.rm=T), length = 0.1, angle = 45, col = "red")
    })
    
    
    testnorm <- reactive({
        x <- read.table(text=input$text, sep="\t")
        x <- as.matrix(x)
        x <- rowMeans(x, na.rm=T)

        list(ks.test(scale(x), "pnorm"), shapiro.test(x))
    })
    
    
    output$qqPlot <- renderPlot({
        x <- read.table(text=input$text, sep="\t")
        x <- as.matrix(x)
        x <- rowMeans(x, na.rm=T)

        qqnorm(x, las=1)
        qqline(x, col=2)
    })




#output$dataframe <- renderTable({
#   read.table(text=get.text())
#})

    output$textarea.out <- renderPrint({
        bs()
    })
    
    output$alpha.result.out <- renderPrint({
        alpha.result()
    })
    
    output$testnorm.out <- renderPrint({
        testnorm()
    })
    

})
