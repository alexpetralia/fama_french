library(shiny)
library(ggplot2)
library(data.table)
library(scales)
library(stockPortfolio)
library(plyr)
library(RQuantLib)
library(quantmod)
library(e1071)
library(gridExtra)

shinyServer(function(input, output) {
  
  ##########################
  #       DEBUGGING        #
  ##########################
  
#   setwd("C:/Users/apetralia/Desktop/app")
#   beg <- "1990-01-01"
#   end <- "2012-12-31"
#   fname <- "FF_monthly.CSV"

  #####################################
  #       SERVER-SIDE (LINUX EC2)     #
  #####################################

#   setwd("/var/shiny-server/www/fama_french/")
  
  ##########################
  #       PROCESSING       #
  ##########################
  
  # CHANGE FREQUENCY OF FF DATA
  FF <- reactive({
    fname <- paste("FF_", input$freq, ".CSV", sep="")
    data <- read.csv(fname)
    
    if (input$freq == "monthly") { # ascribe end of month trading date to Monthly data
      data$Date <- as.Date(paste(as.character(data$Date), 01, sep=""), format = "%Y%m%d")
      month_ends <- sapply(data$Date, getEndOfMonth, calendar = "UnitedStates/NYSE")
      data$Date <- as.character(as.Date(month_ends, origin = "1970-01-01")) # drop timestamps
      data$Date <- as.POSIXct(data$Date, format = "%Y-%m-%d")
    } else {
      data$Date <- as.POSIXct(as.character(data$Date), format = "%Y%m%d") }
    
    data[,-1] <- data[,-1] / 100 # convert from percent to decimal
    return(data)
  })
  
#   # CHANGE REAL/NOMINAL OF DATA
#   FF_real <- reactive({
#     infl <- input$inflation
#   })
  
  # PULL STOCK DATA
  stock <- reactive({
    beg <- paste(as.character(input$period[[1]]), '-01-01', sep = "")
    end <- paste(as.character(input$period[[2]]), '-12-31', sep = "")
    stock <- getReturns(input$ticker, freq="day", get="overlapOnly", start=beg, end=end)
    close <- stock$full[[1]][,c(1, 7)]
    close$Date <- as.POSIXct(close$Date)
    close <- close[order(close$Date), ]
    
    return(close)
  })
  
  # COMBINE FF AND STOCK INTO MASTER DF
  combined <- reactive({
    df <- merge(x = FF(), y = stock(), by = "Date", all.x = TRUE) # LEFT OUTER JOIN
    
    beg <- as.POSIXct(paste(as.character(input$period[[1]]), '0101'), format = "%Y%m%d")
    end <- as.POSIXct(paste(as.character(input$period[[2]]), '1231'), format = "%Y%m%d")
    data <- df[df$Date %between% c(beg, end), ] # to subset FF data
    data$Date <- format(data$Date, "%Y-%m-%d")
      
    data[, "Adj.Close"] <- Delt(data[, "Adj.Close"], type="arithmetic") # convert to % returns
    data <- na.omit(data)
    names(data)[names(data) == "Adj.Close"] <- "stock"
    data$stock <- data$stock[1:length(data$stock)]
    
    data['Mkt.Excess'] = data['Mkt.RF'] - data['RF']
    data['stock.Excess'] = data['stock'] - data['RF']
    
    return(data)
  })

  regression <- reactive({
    data <- combined()
    
    mkt_regr <- lm(data[,"stock.Excess"] ~ data[,"Mkt.Excess"], na.action=na.omit)
    mkt_alpha <- mkt_regr$coef[1]
    mkt_beta <- mkt_regr$coef[2]
    mkt_r2 <- summary(mkt_regr)$r.squared
    
    smb_regr <- lm(data[,"stock.Excess"] ~ data[,"SMB"], na.action=na.omit)
    smb_alpha <- smb_regr$coef[1]
    smb_beta <- smb_regr$coef[2]
    smb_r2 <- summary(smb_regr)$r.squared
    
    hml_regr <- lm(data[,"stock.Excess"] ~ data[,"HML"], na.action=na.omit)
    hml_alpha <- hml_regr$coef[1]
    hml_beta <- hml_regr$coef[2]
    hml_r2 <- summary(hml_regr)$r.squared
    
    mkt_factor_prem <- mean(data[, "Mkt.Excess"])
    smb_factor_prem <- mean(data[, "SMB"])
    hml_factor_prem <- mean(data[, "HML"])
    
    rf_avg <- mean(data[, "RF"]) # THIS SHOULD BE CURRENT RF VALUE, NOT AN AVERAGE
    
    ff3_ret <- rf_avg + (mkt_beta*mkt_factor_prem) + (smb_beta*smb_factor_prem) + (hml_beta*hml_factor_prem)
    capm_ret <- rf_avg + (mkt_beta*mkt_factor_prem)
    
    stats <- list("ff3_ret" = ff3_ret*100, "capm_ret" = capm_ret*100, "rf" = rf_avg, "mkt_beta" = mkt_beta, "smb_beta" = smb_beta, "hml_beta" = hml_beta, "mkt_alpha" = mkt_alpha, "mkt_r2" = mkt_r2, "smb_alpha" = smb_alpha, "smb_r2" = smb_r2, "hml_alpha" = hml_alpha, "hml_r2" = hml_r2) # convert to percentages    
    stats_rd <- lapply(stats, function(x){ format(round(x, 2), nsmall = 2) })
    
    return(stats_rd)
    
    # a measure for momentum? google it. google other important financial ratios //////////////////////////////
    # cokurtosis? coskewness?
    # model efficacy for fama-french? r^2?
    # add real/nominal functionality  //////////////////////////////
  })

  combined_disp <- reactive({
    data <- combined()
    data$stock.Excess <- NULL
    data$Mkt.Excess <- NULL
    
    names(data)[names(data) == "stock"] <- input$ticker
    names(data)[names(data) == "Mkt.RF"] <- "Market"
    names(data)[names(data) == "RF"] <- "Risk-free"
    
    data[,2:6] <- sapply(data[,2:6], function(x) { 
      paste(format(round(x * 100, 2), nsmall=2), "%", sep="") })
    
    len = length(data[ ,1])
    if (len > 200) {
      head <- head(data, 100)
      tail <- tail(data, 100)
    } else if (len > 100 & len <= 200) {
      head <- head(data, 100)
      tail <- tail(data, len-100)
    } else {
      head <- head(data, len) 
      tail <- tail(data, 0) }
    
    both <- list("head" = head, "tail" = tail)
    return(both)    
  })

  ######################
  #      THEMES        #
  ######################

  ggplot_theme <- theme( # https://github.com/jrnold/ggthemes
    panel.background = element_rect(fill = '#F3ECE2'), 
    plot.background = element_rect(fill = '#F3ECE2'), 
    panel.grid.major = element_line(color = "#DFDDDA"), 
    panel.grid.minor = element_line(color = "#DFDDDA"),
    axis.title.x = element_text(color = "#B2B0AE"),
    axis.title.y = element_text(color = "#B2B0AE") )

  ######################
  #      OUTPUTS       #
  ######################
  
  output$histogram <- renderPlot({
    p <- ggplot(combined(), aes(x = stock)) + 
      geom_histogram(binwidth=.005, fill = "#383837") + 
      geom_density(color="blue", fill="white", alpha=.03) + 
      scale_x_continuous(labels=percent) + 
      scale_y_discrete(breaks=pretty_breaks()) + 
      labs(x = paste(input$ticker, " returns"), y = "observations") + 
      ggplot_theme
    print(p)
  })

  output$scatterplot <- renderPlot({
    regr <- regression()
    
    mkt_plot <- ggplot(combined(), aes(x = Mkt.Excess, y = stock.Excess)) + geom_point(color = "#383837") +
      geom_smooth(method = 'lm', formula=y~x, alpha = 0, size = 1) +
      scale_x_continuous(labels=percent) + 
      scale_y_continuous(labels=percent) + 
      labs(x = paste("excess market returns\n\n", "model: y = ", regr$mkt_alpha, " + ", regr$mkt_beta, "x + (e) | R^2 = ", regr$mkt_r2, sep =""), 
           y = paste("excess", input$ticker, "returns")) + 
      ggplot_theme
    
    smb_plot <- ggplot(combined(), aes(x = SMB, y = stock.Excess)) + geom_point(color = "#383837") +
      geom_smooth(method = 'lm', formula=y~x, alpha = 0, size = 1) + 
      scale_x_continuous(labels=percent) + 
      scale_y_continuous(labels=percent) + 
      labs(x = paste("SMB returns\n\n", "model: y = ", regr$smb_alpha, " + ", regr$smb_beta, "x + (e) | R^2 = ", regr$smb_r2, sep =""), 
           y = paste("excess", input$ticker, "returns")) + 
      ggplot_theme
    
    hml_plot <- ggplot(combined(), aes(x = HML, y = stock.Excess)) + geom_point(color = "#383837") +
      geom_smooth(method = 'lm', formula=y~x, alpha = 0, size = 1) + 
      scale_x_continuous(labels=percent) + 
      scale_y_continuous(labels=percent) + 
      labs(x = paste("HML returns\n\n", "model: y = ", regr$hml_alpha, " + ", regr$hml_beta, "x + (e) | R^2 = ", regr$hml_r2, sep =""), 
           y = paste("excess", input$ticker, "returns")) + 
      ggplot_theme
    
    grid.arrange(mkt_plot, smb_plot, hml_plot, ncol=3)
  })

  output$metrics <- renderUI({
    regr <- regression()
        
    stock <- combined()$stock
    stats <- c("mean" = mean(stock)*100, "median" = median(stock)*100, "stdev" = sd(stock)*100, "skewness" = skewness(stock), "kurtosis" = kurtosis(stock)-3) # convert to percentages
    stats_rd <- lapply(stats, function(x){ format(round(x, 2), nsmall = 2) })
  
    div(   
      strong(p("Measures of central tendency")),
      
      tags$table(
        tags$tbody(
          tags$tr(
            tags$td(paste("Mean (", input$freq, "): ", sep="")),
            tags$td(paste(stats_rd$mean, "%", sep="")) ),
          tags$tr(
            tags$td(paste("Median (", input$freq, "): ", sep="")),
            tags$td(paste(stats_rd$median, "%", sep="")) ),
          tags$tr(
            tags$td(paste("Standard deviation (", input$freq, "): ",  sep="")),
            tags$td(paste(stats_rd$stdev, "%", sep="")) ),
          tags$tr(
            tags$td(paste("Skewness (", input$freq, "): ",  sep="")),
            tags$td(stats_rd$skewness) ),
          tags$tr(
            tags$td(paste("Excess kurtosis (", input$freq, "): ",  sep="")),
            tags$td(stats_rd$kurtosis) )
          )
        ),
      
      br(),
      
      strong(p("Financial metrics")),
      
      tags$table(
        tags$tbody(
          tags$tr(
            tags$td(paste("CAPM required return (", input$freq, "): ", sep="")),
            tags$td(paste(regr$capm_ret, "%", sep="")) ),
          tags$tr(
            tags$td(paste("Fama-French 3 Factor required return (", input$freq, "): ", sep="")),
            tags$td(paste(regr$ff3_ret, "%", sep="")) ),
          tags$tr(
            tags$td(paste("Sharpe ratio (", input$freq, "): ",  sep="")),
            tags$td(p("N/A", style="display: inline")) ),
          tags$tr(
            tags$td(paste("Sortino ratio (", input$freq, "): ",  sep="")),
            tags$td("N/A") ),
          tags$tr(
            tags$td(paste("Market beta (", input$freq, "): ",  sep="")),
            tags$td(regr$mkt_beta) ), 
          tags$tr(
            tags$td(paste("SMB beta (", input$freq, "): ",  sep="")),
            tags$td(regr$smb_beta) ), 
          tags$tr(
            tags$td(paste("HML beta (", input$freq, "): ",  sep="")),
            tags$td(regr$hml_beta) )
        ) ) # end table
    ) # end div
  }) # end renderUI

  output$table_head <- renderTable({
    combined_disp()$head    
  },
  include.rownames = FALSE)
  
  output$table_tail <- renderTable({
    if (length(combined_disp()) == 2) {# ie. tail table exists
      combined_disp()$tail 
    }
  },
  include.rownames = FALSE,
  include.colnames = FALSE)
})