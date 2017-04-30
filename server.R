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
library(lubridate)
library(magrittr)
  
  ##########################
  #       DEBUGGING        #
  ##########################

#   beg <- "1990"
#   beta_beg <- "2010"
#   rp_beg <- "1995"
#   end <- "2015"

  ####################
  #     OVERHEAD     #
  ####################

  body(getReturns)[[2]] <- substitute(startURL <- "https://ichart.finance.yahoo.com/table.csv?s=") # BUG, yahoo.finance API changed to use https -> need to change underlying library

  truncate <- function(df, x, y) {
    if (!is(df[,1], "POSIXct")) { # if annualized, convert from numeric to POSIXct
      df[,1] <- sapply(df[,1], function (x) { paste(as.character(x), '0102', sep="") })
      df[,1] <- as.POSIXct(df[,1], format = "%Y%m%d") }
    beg_in <- as.POSIXct(paste(as.character(x), '0101', sep=""), format = "%Y%m%d")
    end_in <- as.POSIXct(paste(as.character(y), '1231', sep=""), format = "%Y%m%d")
    df <- df[df$Date %between% c(beg_in, end_in), ]
    df$Date <- format(df$Date, "%Y-%m-%d") # convert back to string
    return(df)
  }

  product = function(x, na.rm=TRUE){ prod(x+1)-1 }
  
  ggplot_theme <- theme( 
    panel.background = element_rect(fill = '#F3ECE2'), 
    plot.background = element_rect(fill = '#F3ECE2'), 
    panel.grid.major = element_line(color = "#DFDDDA"), 
    panel.grid.minor = element_line(color = "#DFDDDA"),
    axis.title.x = element_text(color = "#B2B0AE"),
    axis.title.y = element_text(color = "#B2B0AE"),
    title = element_text(color = "#606060") )
  
  ##########################
  #       PROCESSING       #
  ##########################

daily <-  read.csv("FF_daily.CSV")
monthly <- read.csv("FF_monthly.CSV")
snp_tickers <- read.csv("snp_tickers.CSV")

shinyServer(function(input, output) {
  
  # CHANGE FREQUENCY OF FF DATA
  FF <- reactive({
    data <- get(input$freq)
    
    if (input$freq == "monthly") { # ascribe end of month trading date to Monthly data
      data$Date <- as.Date(paste(as.character(data$Date), 01, sep=""), format = "%Y%m%d")
      month_ends <- sapply(data$Date, getEndOfMonth, calendar = "UnitedStates/NYSE")
      data$Date <- as.character(as.Date(month_ends, origin = "1970-01-01")) # drop timestamps
      data$Date <- as.POSIXct(data$Date, format = "%Y-%m-%d")
    } else { data$Date <- as.POSIXct(as.character(data$Date), format = "%Y%m%d") }
    
    data[,-1] <- data[,-1] / 100 # convert from percent to decimal
    return(data)
  })
  
  # PULL STOCK DATA
  stock <- reactive({
    stock <- getReturns(input$ticker, freq="day", get="overlapOnly", start="1920-01-01")
    close <- stock$full[[1]][,c(1, 7)]
    close$Date <- as.POSIXct(close$Date)
    close <- close[order(close$Date), ]
    return(close)
  })
  
  # COMBINE FF AND STOCK INTO MASTER DF
  unannualized <- reactive({
    data <- merge(x = FF(), y = stock(), by = "Date", all.x = TRUE) # LEFT OUTER JOIN
    
    names(data)[names(data) == "Adj.Close"] <- "stock"
    data[, "stock"] <- Delt(data[, "stock"], type="arithmetic") # convert to % returns
    data$stock <- data$stock[1:length(data$stock)] # Delt.1.arithmetic is trapped inside an object
    data <- na.omit(data)
    
    data['stock.Excess'] = data['stock'] - data['RF']
    return(data)
  })

  unannualized_subset <- reactive({
    return( truncate(unannualized(), input$ret_period[[1]], input$ret_period[[2]]) ) })

  annualized <- reactive({
    data <- unannualized()
    
    # data.table uses references, not copies by default. must explicitly copy.
    df <- setDT(copy(data))[, lapply(.SD, product), by=.(year(Date))] %>% as.data.frame()
    setnames(df, "year", "Date")
    
    df['stock.Excess'] = df['stock'] - df['RF']
    return(df)
  })
  
  annualized_FF <- reactive({
    df <- setDT(copy(FF()))[, lapply(.SD, product), by=.(year(Date))] %>% as.data.frame()
    setnames(df, "year", "Date")
    return(df)
  })

  dates_out <- reactive({ # for histogram title
    data <- truncate(unannualized(), input$ret_period[[1]], input$ret_period[[2]])
    return(list( year(data[1,"Date"]) , year(data[nrow(data),"Date"]) )) })  

  regression <- reactive({ # for unannualized scatterplots and beta calculations
    data <- unannualized()
    data <- truncate(data, input$beta_period[[1]], input$beta_period[[2]])
    
    mkt_regr <- lm(data[,"stock.Excess"] ~ data[,"Mkt.RF"], na.action=na.omit)
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
    
    stats <- list("mkt_beta" = mkt_beta, "smb_beta" = smb_beta, "hml_beta" = hml_beta, "mkt_alpha" = mkt_alpha, "mkt_r2" = mkt_r2, "smb_alpha" = smb_alpha, "smb_r2" = smb_r2, "hml_alpha" = hml_alpha, "hml_r2" = hml_r2)
    return(list(data, stats))    
  })

  regression_annualized <- reactive({ # for annualized financial table
    betas <- regression()[[2]] # stats variable
    df <- annualized_FF() # risk premia should be based on original dataframe, not a merged one subset by stock period
    
    # risk premia time period #
    df <- truncate(df, input$rp_period[[1]], input$rp_period[[2]])
    mkt_factor_prem <- mean(df[, "Mkt.RF"])
    smb_factor_prem <- mean(df[, "SMB"])
    hml_factor_prem <- mean(df[, "HML"])
    rf_t <- df[nrow(df), "RF"]
    
    # calculate #
    ff3_ret <- rf_t+(betas$mkt_beta*mkt_factor_prem)+(betas$smb_beta*smb_factor_prem)+(betas$hml_beta*hml_factor_prem)
    capm_ret <- rf_t+(betas$mkt_beta*mkt_factor_prem)
    
    metrics <- list("ff3_ret" = ff3_ret, "capm_ret" = capm_ret, "rf" = rf_t)
    return(metrics)
  })

  calculate_unannualized <- reactive({ # mean, median and stdev should be converted to percentages
    df <- unannualized_subset()
    stock_stats <- c("mean" = mean(df$stock)*100, 
                     "median" = median(df$stock)*100, 
                     "stdev" = sd(df$stock)*100, 
                     "skewness" = skewness(df$stock), 
                     "e.kurtosis" = kurtosis(df$stock),
                     "sharpe" = mean(df$stock.Excess) / sd(df$stock),
                     "sortino" = mean(df$stock.Excess) / sd(df$stock[df$stock < 0]) )
    return(stock_stats)
  })

  calculate_annualized <- reactive({ 
    # SELECTED STOCK #    
    df <- truncate(annualized(), input$ret_period[[1]], input$ret_period[[2]])
    stock_stats <- c("mean" = mean(df$stock)*100, 
               "median" = median(df$stock)*100, 
               "stdev" = sd(df$stock)*100, 
               "skewness" = skewness(df$stock), 
               "e.kurtosis" = kurtosis(df$stock),
               "sharpe" = mean(df$stock.Excess) / sd(df$stock),
               "sortino" = mean(df$stock.Excess) / sd(df$stock[df$stock < 0]) )
    
    # S&P 500 #
    data <- snp_tickers
    
    idx_mean <- sapply(data[2:length(data)], mean, na.rm = TRUE)*100 # yields 500 means of annualized stock returns
    idx_median <- sapply(data[2:length(data)], median, na.rm = TRUE)*100
    idx_stdev <- sapply(data[2:length(data)], sd, na.rm = TRUE)*100
    idx_skewness <- sapply(data[2:length(data)], skewness, na.rm = TRUE)
    idx_e.kurtosis <- sapply(data[2:length(data)], kurtosis, na.rm = TRUE)
    
    # PERCENTILES #
    perc.rank <- function(x, y) { length(x[x <= y])/length(x)*100 } 
    percentile <- c("mean" = perc.rank(idx_mean, stock_stats['mean']),
      "median" = perc.rank(idx_median, stock_stats['median']),
      "stdev" = perc.rank(idx_stdev, stock_stats['stdev']),
      "skewness" = perc.rank(idx_skewness, stock_stats['skewness']),
      "e.kurtosis" = perc.rank(idx_e.kurtosis, stock_stats['e.kurtosis']) )
    
    return(list("stock" = stock_stats, "percentile" = percentile))
  })

  ######################
  #      OUTPUTS       #
  ######################

  # PLOTS DISPLAY #
  
  output$histogram <- renderPlot({ 
    p <- ggplot(unannualized_subset(), aes(x = stock)) + 
      geom_histogram(binwidth=.005, fill = "#383837") + 
      geom_density(color="blue", fill="white", alpha=.03) + 
      scale_x_continuous(labels=percent) + 
      scale_y_discrete(breaks=pretty_breaks()) + 
      labs(x = paste(toupper(input$ticker), " returns"), y = "observations",
           title = paste(toupper(input$ticker), " ", input$freq, " returns distribution (", dates_out()[[1]], " to ", dates_out()[[2]], ")\n", sep="")) + 
      ggplot_theme
    print(p)
  })

  output$scatterplot <- renderPlot({
cat('got here', file=stderr())
    regr <- regression()[[2]]
    regr_rd <- lapply(regr, function(x){ format(round(x, 2), nsmall = 2) })
    ticker <- toupper(input$ticker)
    
    mkt_plot <- ggplot(regression()[[1]], aes(x = Mkt.RF, y = stock.Excess)) + geom_point(color = "#383837", alpha=.8) +
      geom_smooth(method = 'lm', formula=y~x, alpha = 0, size = 1) +
      scale_x_continuous(labels=percent) + 
      scale_y_continuous(labels=percent) + 
      labs(title = paste(ticker, " vs. market returns (", input$freq, ")\n", sep=""),
           x = paste("excess market returns\n\n", "model: y = ", regr_rd$mkt_alpha, " + ", regr_rd$mkt_beta, "x + (e) | R^2 = ", regr_rd$mkt_r2, sep =""), 
           y = paste("excess", ticker, "returns")) + 
      ggplot_theme
    
    smb_plot <- ggplot(regression()[[1]], aes(x = SMB, y = stock.Excess)) + geom_point(color = "#383837", alpha=.8) +
      geom_smooth(method = 'lm', formula=y~x, alpha = 0, size = 1) + 
      scale_x_continuous(labels=percent) + 
      scale_y_continuous(labels=percent) + 
      labs(title = paste(ticker, " vs. SMB returns (", input$freq, ")\n", sep=""),
           x = paste("SMB returns\n\n", "model: y = ", regr_rd$smb_alpha, " + ", regr_rd$smb_beta, "x + (e) | R^2 = ", regr_rd$smb_r2, sep =""), 
           y = paste("excess", ticker, "returns")) + 
      ggplot_theme
    
    hml_plot <- ggplot(regression()[[1]], aes(x = HML, y = stock.Excess)) + geom_point(color = "#383837", alpha=.8) +
      geom_smooth(method = 'lm', formula=y~x, alpha = 0, size = 1) + 
      scale_x_continuous(labels=percent) + 
      scale_y_continuous(labels=percent) + 
      labs(title = paste(ticker, " vs. HML returns (", input$freq, ")\n", sep=""),
           x = paste("HML returns\n\n", "model: y = ", regr_rd$hml_alpha, " + ", regr_rd$hml_beta, "x + (e) | R^2 = ", regr_rd$hml_r2, sep =""), 
           y = paste("excess", ticker, "returns")) + 
      ggplot_theme
    
    grid.arrange(mkt_plot, smb_plot, hml_plot, ncol=3)
  })

  # METRICS DISPLAY #

  output$metrics_stats <- renderUI({
    stock_unannualized <- calculate_unannualized()
    stock_annualized <- calculate_annualized()$stock
    percentile <- calculate_annualized()$percentile
    
    stock_unannualized_rd <- lapply(stock_unannualized, function(x) { format(round(x, 2), nsmall = 2) })
    stock_annualized_rd <- lapply(stock_annualized, function(x) { format(round(x, 2), nsmall = 2) })
    percentile_rd <- lapply(percentile, function(x) { format(round(x, 1), nsmall = 1) })
    
    div(         
      strong(h3("Measures of central tendency and dispersion")),
      
      tags$table(
        tags$thead(
          tags$tr(
            tags$td(""),
            tags$td(HTML(paste(strong(toupper(input$ticker)),  " (", input$freq, ")", sep=""))),
            tags$td(HTML(paste(strong(toupper(input$ticker)),  "(annualized)"))), 
            tags$td(HTML(paste(strong("Percentile among",br(),"current S&P 500 stocks"),"(annualized)")) ))
        ),
        tags$tbody(
          tags$tr(
            tags$td("Mean"),
            tags$td(paste(stock_unannualized_rd['mean'], "%", sep="")),
            tags$td(paste(stock_annualized_rd['mean'], "%", sep="")),
            tags$td(paste(percentile_rd['mean'], "%", sep="")) ),
          tags$tr(
            tags$td("Median"),
            tags$td(paste(stock_unannualized_rd['median'], "%", sep="")),
            tags$td(paste(stock_annualized_rd['median'], "%", sep="")),
            tags$td(paste(percentile_rd['median'], "%", sep="")) ),
          tags$tr(
            tags$td("Standard deviation"),
            tags$td(paste(stock_unannualized_rd['stdev'], "%", sep="")),
            tags$td(paste(stock_annualized_rd['stdev'], "%", sep="")),
            tags$td(paste(percentile_rd['stdev'], "%", sep="")) ),
          tags$tr(
            tags$td("Skewness"),
            tags$td(stock_unannualized_rd['skewness']),
            tags$td(stock_annualized_rd['skewness']),
            tags$td(paste(percentile_rd['skewness'], "%", sep="")) ),
          tags$tr(
            tags$td("Excess kurtosis"),
            tags$td(stock_unannualized_rd['e.kurtosis']),
            tags$td(stock_annualized_rd['e.kurtosis']),
            tags$td(paste(percentile_rd['e.kurtosis'], "%", sep="")) )
          ) ) # end table
    ) # end div
  }) # end renderUI

  output$metrics_finance <- renderUI({
    stock <- calculate_annualized()$stock
    stock_rd <- lapply(stock, function(x) { format(round(x, 2), nsmall = 2) })
    
    regr <- regression_annualized()
    regr$capm_ret <- regr$capm_ret*100; regr$ff3_ret <- regr$ff3_ret*100 # turn to %
    regr_rd <- lapply(regr, function(x) { format(round(x, 2), nsmall = 2) })
    
    regr_unannualized <- regression()[[2]]
    regr_un_rd <- lapply(regr_unannualized, function(x){ format(round(x, 2), nsmall = 2) })
    
    div(
      strong(h3("Financial metrics (annualized)")),
      
      tags$table(
        tags$tbody(
          tags$tr(
            tags$td("CAPM required return"),
            tags$td(paste(regr_rd$capm_ret, "%", sep="")) ),
          tags$tr(
            tags$td("Fama-French 3 Factor required return"),
            tags$td(paste(regr_rd$ff3_ret, "%", sep="")) ),
          tags$tr(
            tags$td("Sharpe ratio"),
            tags$td(stock_rd$sharpe) ),
          tags$tr(
            tags$td("Sortino ratio"),
            tags$td(stock_rd$sortino) )
        ) ), # end table
      
      withMathJax(), # to display LaTeX-like formulas
      
      strong(h3(paste("Regression results (", input$freq, ")", sep=""))),
      
      tags$table(
        tags$thead(
          tags$tr(
            tags$td(""),
            tags$td(strong("Beta")),
            tags$td(strong("R\\(^2\\)")) )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Market factor"),
            tags$td(regr_un_rd$mkt_beta),
            tags$td(regr_un_rd$mkt_r2) ),
          tags$tr(
            tags$td("SMB factor"),
            tags$td(regr_un_rd$smb_beta),
            tags$td(regr_un_rd$smb_r2) ),
          tags$tr(
            tags$td("HML factor "),
            tags$td(regr_un_rd$hml_beta),
            tags$td(regr_un_rd$hml_r2) )
        ) ), # end table
      br()
    ) # end div
  }) # end renderUI

  # TABLES DISPLAY #

  unannualized_disp <- reactive({
    data <- unannualized_subset()
    data$stock.Excess <- NULL
    
    setnames(data, "stock", toupper(input$ticker))
    setnames(data, "Mkt.RF", "Market")
    setnames(data, "RF", "Risk-free")
    
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

  output$table_head <- renderTable({
    unannualized_disp()$head    
  },
  include.rownames = FALSE)
  
  output$table_tail <- renderTable({
    if (length(unannualized_disp()) == 2) { # ie. tail table exists
      unannualized_disp()$tail 
    }
  },
  include.rownames = FALSE,
  include.colnames = FALSE)

  annualized_disp <- reactive({
    data <- annualized()
    data <- truncate(data, input$ret_period[[1]], input$ret_period[[2]])
    data$Date <- sapply(strsplit(data$Date, "-"), '[', 1)
    data$stock.Excess <- NULL
    setnames(data, "stock", toupper(input$ticker))
    setnames(data, "Mkt.RF", "Market")
    setnames(data, "RF", "Risk-free")
    data[,1] <- sapply(data[,1], function(x) { format(x, nsmall=0) })
    data[,2:6] <- sapply(data[,2:6], function(x) { paste(format(round(x * 100, 2), nsmall=2), "%", sep="") })
    return(data)
  })

  output$annualized_table <- renderTable({
    annualized_disp()
  },
  include.rownames = FALSE)
})
