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
               p("TO REWRITE..."),
               p("Why is skewness relevant? Why is kurtosis relevant? Can you talk about the Fama-French model? Sortino ratio? CAPM?"),
               p("The annualized return is computed as: [1 + e(r)]^12"),
               p("Skewness describes the symmetry of a distribution. A negative skew (left-tailed) implies that negative returns are more severe than positive returns; similarly, a positive skew (right-tailed) implies that positive returns have a larger magnitude than negative returns. In other words, for a right-skewed distribution, the probability of large positive returns is greater than the probability of large negative returns. When it comes to financial securities, a securities with large negative skew are often avoided because they imply large downside risk. Although interpretations of skewness are arbitrary, it is often thought that a skewness greater than 1 or less than -1 imply significant skewness. All else equal, securities whose distributions are less than -1 are riskier than those above it."),
               p("Kurtosis describes the peakedness of a distribution. Because a Gaussian (normal) distrubtion has a kurtosis of 3, excess kurtosis is often a more informative metric. A distribution that is platykurtic (excess kurtosis < 3) has a low peak and thin tails; one that is leptokurtic (excess kurtosis > 3) has a high peak and fat tails. A distribution that is leptokurtic will exhibit fluctuations of larger magnitute than one platykurtic distribution, rendering the security more risky."),
               p("The skewness and kurtosis metrics should be computed relative to their standard errors. In other words, are the distributions /significantly/ skewed or leptokurtic/platykurtic?"),
               p("The Sortino ratio was created for X reasons.")
               
               
               # Can you describe skewness? kurtosis? what do they mean for securities? What's good and what's bad?
               # Can you explain R^2?
      ),
      
      tabPanel("Methodology and Sources",
               br(),
               h4(strong("Overview")),
               p("This program joins a given's stock returns to the Fama-French 3 Factor portfolios' returns, then runs multiple regressions in order to determine the stocks betas. Univariate descriptive statistics and financial calculations are performed using the stock's returns while the regressions are used to compute CAPM and Fama-French required returns."),
               h4(strong("Methodology")),
               p(strong(span("Downloading the data...", style="color:grey"))),
               p("This project was developed entirely using R (data analysis) and R Shiny (dynamic webapp)."),
               p("The ", strong("Fama-French 3 Factor model data "), "comes from Kenneth French's data library, located ", a("here", href="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html"), ". I downloaded all data on June 30, 2015. French's data comes in 3 periodicities: daily, weekly, or monthly. I downloaded each of these. Whenever a specific periodocity is selected in the ", span("Input menu", style="color:grey"), ", the app reloads the appropriate Fama-French dataset and reruns all calculations."),
               p("The ", strong("stock data "), "is downloaded from Yahoo Finance. In R, there is a package called ", code("stockPortfolio"), "which automates this process. Whenever the ticker is modified in the ", span("Input menu", style="color:grey"), ", the webapp redownloads daily data."),
               p("The program then performs a left outer join of the daily stock prices to the Fama-French dataset (daily, weekly or monthly) on the \"Date\" column. It converts the stock prices to stock returns. It then subsets this data by the period specified in the ", span("Input menu", style="color:grey")),
               p(strong(span("Performing the calculations...", style="color:grey"))),
               p("Calculations of the mathematical moments are performed automatically by the R package", code("e1071"), span(" TO CHECK", style="color:red")),
               p("Using the combined \"master\" dataframe, the program regresses stock returns (not excess) on market, SMB and HML returns to find market, SMB and HML betas, respectively."),
               p("The risk-free rate, market, SMB and HML factor risk premiums are calculated as simple means over the specified time period.", span("Note: should these be in EXCESS of rf Should the regressions use EXCESS returns?", style="color:red")),
               p("The CAPM formula used is:", "CAPM r = r_f + mkt_beta * mkt_RP"),
               p("The Fama-French 3 factor model formula used is:", "FF3 r = r_f + mkt_beta * mkt_RP + SMB_beta * SMB_RP + HML_beta * HML_RP"),
               p("The Sharpe ratio calculation used is:", "[ E(R) - rf ] / std(R)"),
               p("The Sortino ratio calculation used is:", "[ E(R) - rf ] / std(negative R only)"),
               h4(strong("Sources consulted")),
               a("Fama-French website", href="/"), br(),
               a("Description of skewness", href="/"), br(),
               a("Description of kurtosis", href="/"), br(),
               a("Calculating CAPM", href="/"), br(),
               a("Calculating Fama-French 3", href="/"), br(),
               a("CAPM vs Fama-French models in explaining stock market variability", href="/"), br(),
               a("Calculating Sortino ratio", href="/"), br(),
               a("R Shiny documentation", href="/"), br(),
               # p("Professor Sebastien Betermier, McGill University (need to ask permission)")
               
               # is there a method for HTML LaTEX markup for mathematical calculations? Can I insert <beta> symbol and super/subscripts?
        ),
      
      tabPanel("Background",
               br(),
               h4(strong("Overview")),
               p("The motivation for this project was to calculate and visualize important financial metrics for a security where the input assumptions can be flexibly changed."),
               p("When analyzing stock returns, only certain financial metrics and ratios are readily available online, such as betas, historical realized returns and standard deviations (sometimes). While important, these numbers provide an incomplete picture of a stock returns. More importantly, other useful statistics are simple to calculate."),
               p("For example, despite being widely interpreted as a measure of risk, standard deviation provides an extremely one-dimensional view of a stock's variability. In practice, \"fat tails\" and skewness are often overlooked. However, these values are easy to calculate, so this program does."),
               p("In another example, the CAPM is widely used as the de facto model for calculating a security's required return, largely because of its simplicity in implementation. However, while the CAPM empirically explains about 70% stock market realized returns, the Fama-French 3 factor model explains about 95%. While it is slightly more difficult to calculate, its empirical validity implies that it too should be readily available in security analysis"),
               h4(strong("Applications")),
               tags$li("Academic"),
               p("This tool is especially useful in an academic context. Students can visualize how changing important assumptions about a security can change the financial metrics. Likewise, they can calculate useful information about a security in seconds (eg. the Sortino ratio), as opposed to downloading the data in Excel, formatting it, running a regression and calculating the metrics."),
               tags$li("Rapid-testing"),
               p("Are you in the ballpark about your calculation?"),
               p("How do changes in assumptions change the calculations?"),
               p("Is there an 'elasticity' of calculation in response to changes in underlying?"),
               tags$li("Testing the CAPM and FF3"), # eg. how do CAPM/FF3 required returns compare to realized?
               tags$li("Portfolio Management"),
               HTML(paste0('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;test')),
               p("With statistics readily available, portfolio FF3 returns can be readily computed. All of these statistics on an individual basis can be aggregated to the portfolio level.")
              
      )
    )
  ))
))