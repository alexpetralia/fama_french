shinyUI(fluidPage(
  
  titlePanel("A Dynamic Approach to Security Analysis"),
  
  sidebarLayout(sidebarPanel(
        
    textInput("ticker", h3("What's your security's ticker?"), value="AAPL"),
    
    radioButtons("freq", h3("What frequency?"),
                   c("Daily" = "daily",
                   "Weekly" = "weekly",
                   "Monthly" = "monthly"),
                 inline = TRUE,
                 selected = "monthly"),
    
    sliderInput("period", h3("What time period?"),
                min = 1920,
                max = 2020,
                step = 1,
                value = c(1920,2020),
                sep = ""),
    
    br(),
    
    checkboxInput("inflation", "Inflation-adjusted? (ie. real returns)", TRUE),
    
    width = 3
    
  ),
  
  mainPanel(
    
    tabsetPanel(position=c("right"),
      tabPanel(strong("Plot"), 
               plotOutput("plot"),
               htmlOutput("metrics")),
      
      tabPanel(strong("Table"), 
               tableOutput("table_head"),
               tableOutput("table_tail"),
               code("Displays no more than 200 rows.")),
      
      tabPanel("Description",
               br(),
               p("Why is skewness relevant? Why is kurtosis relevant? Can you talk about the Fama-French model? Sortino ratio? CAPM?"),
               p("The annualized return is computed as: [1 + e(r)]^12"),
               p("Skewness describes the symmetry of a distribution. A negative skew (left-tailed) implies that negative returns are more severe than positive returns; similarly, a positive skew (right-tailed) implies that positive returns have a larger magnitude than negative returns. In other words, for a right-skewed distribution, the probability of large positive returns is greater than the probability of large negative returns. When it comes to financial securities, a securities with large negative skew are often avoided because they imply large downside risk. Although interpretations of skewness are arbitrary, it is often thought that a skewness greater than 1 or less than -1 imply significant skewness. All else equal, securities whose distributions are less than -1 are riskier than those above it."),
               p("Kurtosis describes the peakedness of a distribution. Because a Gaussian (normal) distrubtion has a kurtosis of 3, excess kurtosis is often a more informative metric. A distribution that is platykurtic (excess kurtosis < 3) has a low peak and thin tails; one that is leptokurtic (excess kurtosis > 3) has a high peak and fat tails. A distribution that is leptokurtic will exhibit fluctuations of larger magnitute than one platykurtic distribution, rendering the security more risky."),
               p("The skewness and kurtosis metrics should be computed relative to their standard errors. In other words, are the distributions /significantly/ skewed or leptokurtic/platykurtic?"),
               p("The Sortino ratio was created for X reasons.")
      ),
      
      tabPanel("Methodology and Sources",
               br(),
               h2("Methodology"),
               p("This project was created entirely using R for data analysis and R Shiny for a dynamic webapp."),
               p(paste("The ", strong("Fama-French 3 Factor model data "), "comes from Kenneth French's data library, located here: http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html. I downloaded a static dataset on June 30, 2015. French's data comes in 3 forms: daily, weekly, or monthly. I downloaded each of these - whenever a specific periodocity is selected in the lefthand Input bar, the app updates and reads the appropriate Fama-French dataset.")),
               p("The data is converted from percentages to decimal values (ie. divided by 100)"),
               p(paste("The ", strong("stock data "), "is pulled from Yahoo Finance. In R, there is a package called \"stock portfolio\" which automates this process. Whenever the ticker is changed, the webapp repulls daily data from Yahoo Finance using this module.")),
               p("The app then performs a left outer join of the stock (daily) on the Fama-French dataset (daily, weekly or monthly) on the \"Date\" column. It converts the stock prices to stock returns, then subsets this data by the period specified in the Input tab."),
               p("The app then regresses stock returns (not excess) on market returns, SMB and HML returns to find market beta, SMB beta and HML beta, respectively."),
               p("The market, SMB and HML factor risk premiums are determined as means over the specified time period of their respective returns. The risk-free rate is the average risk-free rate over the specified time period. Note, should these be in EXCESS of rf?"),
               p("The CAPM formula used is: CAPM r = r_f + mkt_beta * mkt_RP"),
               p("The Fama-French 3 factor model formula used is: FF3 r = r_f + mkt_beta * mkt_RP + SMB_beta * SMB_RP + HML_beta * HML_RP"),
               p("The Sharpe ratio is determined by: [ E(R) - rf ] / std(R)"),
               h2("Sources")
        ),
      
      tabPanel("Background",
               br(),
               p("The motivation for this project was to visualize and calculate important financial metrics for a security where the input assumptions can be flexibly changed."),
               p("Common metrics for analyzing stock returns are betas, mean returns, and standard deviation of returns. While these metrics important, they only provide a partial picture of stock returns. Indeed, more comprehensive metrics are readily available."),
               p("For example, standard deviation is widely criticized as being an incomplete metric for a stock's \"risk\". Often, \"fat tails\" or skewness is overlooked. These are simple calculations however and this model can calculate them."),
               p("In another example, the CAPM is widely used as the de facto model for calculating a security's required return, largely because of the simplicity in impementing it. However, it too provides an incomplete picture of a security's returns. While the CAPM empirically explains about 70% of a security's realized returns, the Fama-French 3 Factor model explains about 95% of a security's return variability."),
               p("Using R, it is easy to calculate these metrics. Using R Shiny, it is easy to insert flexible inputs whereby assumptions to this model can be changed. For example, time period and data frequency are important assumptions for any financial model - rather than being static, they should be dynamic."),
               p("This tool is especially useful in an academic context. Students can visualize how changing important assumptions about a security can change the financial metrics. Likewise, they can calculate useful information about a security in seconds (eg. the Sortino ratio), as opposed to downloading the data in Excel, formatting it, running a regression and calculating the metrics.")
      )
    )
  ))
))