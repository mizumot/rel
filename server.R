library(shiny)
library(shinyAce)
library(psych)
library(ltm)


shinyServer(function(input, output) {


    options(warn=-1)
    
    
    bs <- reactive({
        if (input$colname == 0) {
            x <- read.table(text=input$text, sep="\t")
            x <- as.matrix(x)
            total <- rowSums(x, na.rm=T)
            result1 <- describe(total)[2:13]

            y <- rowMeans(x, na.rm=T)
            result2 <- describe(y)[2:13]
        
            row.names(result1) <- "Total   "
            row.names(result2) <- "Average "
            return(list(result2, result1))
        
        } else {
            
            x <- read.csv(text=input$text, sep="\t")
            total <- rowSums(x, na.rm=T)
            result1 <- describe(total)[2:13]
            
            y <- rowMeans(x, na.rm=T)
            result2 <- describe(y)[2:13]
            
            row.names(result1) <- "Total   "
            row.names(result2) <- "Average "
            return(list(result2, result1))
        
        }
    })
    
    
    alpha.result <- reactive({
        if (input$colname == 0) {
            x <- read.table(text=input$text, sep="\t")
            x <- as.matrix(x)
            result1 <- cronbach.alpha(x)
            result2 <- alpha(x, check.keys=F)
            list(result1, result2)
        
        } else {
            
            x <- read.csv(text=input$text, sep="\t")
            result1 <- cronbach.alpha(x)
            result2 <- alpha(x, check.keys=F)
            list(result1, result2)
        
        }
    })
    
    
    
    
    
    
    makedistPlot <- function(){
        if (input$colname == 0) {
            x <- read.table(text=input$text, sep="\t")
            x <- as.matrix(x)
            
            if (input$meantotal1 == "mean1") {
                x <- rowMeans(x, na.rm=T)
            } else {
                x <- rowSums(x, na.rm=T)
            }
            
        } else {
            x <- read.csv(text=input$text, sep="\t")
            
            if (input$meantotal1 == "mean1") {
                x <- rowMeans(x, na.rm=T)
            } else {
                x <- rowSums(x, na.rm=T)
            }
            
        }
        
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
    }
    
    output$distPlot <- renderPlot({
        print(makedistPlot())
    })
    
    
    
    
    
    makeboxPlot <- function(){
        if (input$colname == 0) {
            x <- read.table(text=input$text, sep="\t")
            x <- as.matrix(x)
            
            if (input$meantotal2 == "mean2") {
                x <- rowMeans(x, na.rm=T)
            } else {
                x <- rowSums(x, na.rm=T)
            }
            
        } else {
            x <- read.csv(text=input$text, sep="\t")
            
            if (input$meantotal2 == "mean2") {
                x <- rowMeans(x, na.rm=T)
            } else {
                x <- rowSums(x, na.rm=T)
            }
        }
        
        boxplot(x, horizontal=TRUE, xlab= "Mean and +/-1 SD are displayed in red.")
        stripchart(x, pch = 16, add = TRUE)
        points(mean(x, na.rm=T), 0.9, pch = 18, col = "red", cex = 2)
        arrows(mean(x, na.rm=T), 0.9, mean(x, na.rm=T) + sd(x, na.rm=T), length = 0.1, angle = 45, col = "red")
        arrows(mean(x, na.rm=T), 0.9, mean(x, na.rm=T) - sd(x, na.rm=T), length = 0.1, angle = 45, col = "red")
    }

    output$boxPlot <- renderPlot({
        print(makeboxPlot()) # 上の function を参照する指定
    })
    
    
    
    
    
    testnorm <- reactive({
        if (input$colname == 0) {

            x <- read.table(text=input$text, sep="\t")
            x <- as.matrix(x)
            x <- rowMeans(x, na.rm=T)

        } else {
            x <- read.csv(text=input$text, sep="\t")
            x <- rowMeans(x, na.rm=T)
        }

        list(ks.test(scale(x), "pnorm"), shapiro.test(x))
    })
    
    
    
    
    
    
    makeqqPlot <- function(){
        if (input$colname == 0) {
            
            x <- read.table(text=input$text, sep="\t")
            x <- as.matrix(x)
            x <- rowMeans(x, na.rm=T)
            
        } else {
            x <- read.csv(text=input$text, sep="\t")
            x <- rowMeans(x, na.rm=T)
        }
        
        qqnorm(x, las=1)
        qqline(x, col=2)
    }
    
    output$qqPlot <- renderPlot({
        print(makeqqPlot())
    })





    info <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")# バージョン情報
        info2 <- paste("It was executed on ", date(), ".", sep = "")# 実行日時
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })





    output$textarea.out <- renderPrint({
        bs()
    })
    
    output$alpha.result.out <- renderPrint({
        alpha.result()
    })
    
    output$testnorm.out <- renderPrint({
        testnorm()
    })
    
    output$info.out <- renderPrint({
        info()
    })
    
    output$downloadDistPlot <- downloadHandler(
    filename = function() {
        paste('Distribution-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makedistPlot())
		dev.off()
	})
    
    output$downloadBoxPlot <- downloadHandler(
    filename = function() {
        paste('Boxplot-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makeboxPlot())
		dev.off()
	})
    
    output$downloadQQPlot <- downloadHandler(
    filename = function() {
        paste('QQplot-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makeqqPlot())
		dev.off()
	})

})
