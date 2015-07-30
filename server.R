library(shiny)
library(ggplot2)
library(data.table)
library(scales)
library(stockPortfolio)
library(plyr)
library(RQuantLib)
library(quantmod)
library(e1071)

shinyServer(function(input, output) {
  
  ##########################
  #       DEBUGGING        #
  ##########################
  
  setwd("C:/Users/apetralia/Desktop/app")
  beg <- "1990-01-01"
  end <- "2012-12-31"
  fname <- "FF_monthly.csv"
  
  ##########################
  #       PROCESSING       #
  ##########################
  
  # CHANGE FREQUENCY OF FF DATA
  FF <- reactive({
    fname <- paste("FF_", input$freq, ".csv", sep="")
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
    
    return(data)
  })

  regression <- reactive({
    data <- combined()
    
    mkt_regr <- lm(data[,"stock"] ~ data[,"Mkt.RF"], na.action=na.omit)
    mkt_beta <- mkt_regr$coef[2]
    mkt_r2 <- summary(mkt_regr)$r.squared
    
    smb_regr <- lm(data[,"stock"] ~ data[,"SMB"], na.action=na.omit)
    smb_beta <- smb_regr$coef[2]
    smb_r2 <- summary(smb_regr)$r.squared
    
    hml_regr <- lm(data[,"stock"] ~ data[,"HML"], na.action=na.omit)
    hml_beta <- hml_regr$coef[2]
    hml_r2 <- summary(hml_regr)$r.squared
    
    mkt_factor_prem <- mean(data[, "Mkt.RF"])
    smb_factor_prem <- mean(data[, "SMB"])
    hml_factor_prem <- mean(data[, "HML"])
    
    rf_avg <- mean(data[, "RF"])
    
    ff3_ret <- rf_avg + (mkt_beta*mkt_factor_prem) + (smb_beta*smb_factor_prem) + (hml_beta*hml_factor_prem)
    capm_ret <- rf_avg + (mkt_beta*mkt_factor_prem) # mkt_factor_prem should be EXCESS ret
    
    betas <- c("mkt_beta" = mkt_beta, "smb_beta" = smb_beta, "hml_beta" = hml_beta)
    
    required_returns <- list("ff3_ret" = ff3_ret*100, "capm_ret" = capm_ret*100, "rf" = rf_avg, "betas" = betas) # convert to percentages
    return(required_returns)
    
    # scatterplot + R^2 overlay //////////////////////////////
    # a measure for momentum? google it. //////////////////////////////
  })

  combined_disp <- reactive({
    data <- combined()
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
  #      OUTPUTS       #
  ######################
  
  output$plot <- renderPlot({
    p <- ggplot(combined(), aes(x = stock)) + geom_histogram(binwidth=.01) + geom_density(color="red", fill="white", alpha=.03) + theme(axis.title.x = element_blank())
    print(p)
  })
  
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

  output$metrics <- renderUI({
    regr <- regression()
    regr <- c(regr, unlist(regr$betas))
    regr_rd <- lapply(regr, function(x){ format(round(x, 2), nsmall = 2) })
    
    stock <- combined()$stock
    stats <- c("mean" = mean(stock)*100, "median" = median(stock)*100, "stdev" = sd(stock)*100, "skewness" = skewness(stock), "kurtosis" = kurtosis(stock)-3, "sharpe" = (mean(stock) - regr$rf)/sd(stock)) # convert to percentages
    stats_rd <- lapply(stats, function(x){ format(round(x, 2), nsmall = 2) })
    
    div(
      strong(p("Measures of central tendency")),
      p(paste("Mean (", input$freq, "): ", stats_rd$mean, "%", sep="")),
      p(paste("Median (", input$freq, "): ", stats_rd$median, "%", sep="")),
      p(paste("Standard deviation: ", stats_rd$stdev, "%", sep="")),
      p(paste("Skewness: ", stats_rd$skewness, sep="")),
      p(paste("Excess kurtosis: ", stats_rd$kurtosis, sep="")),
      br(),
      strong(p("Financial metrics")),
      p(paste("CAPM required return (", input$freq, "): ", regr_rd$capm_ret, "%", sep="")),
      p(paste("Fama-French 3 Factor required return (", input$freq, "): ", regr_rd$ff3_ret, "%", sep="")),
      p(paste("Sharpe ratio: ", stats_rd$sharpe, sep=""), span(" INCORRECT b/c must annualize return", style="color:red")),
      p(paste("Sortino ratio: ", "N/A", sep="")),
      p(paste("Market beta: ", regr_rd$mkt_beta, "; ", "SMB beta: ", regr_rd$smb_beta, "; ", "HML beta: ", regr_rd$hml_beta, sep=""))
    )
  })
  
  # How can I fix the HTML output? # names(tags) -> fix links, emphasis //////////////////////  
    
    # Can you describe skewness? kurtosis? what do they mean for securities? What's good and what's bad?
    # Can you explain R^2?
})