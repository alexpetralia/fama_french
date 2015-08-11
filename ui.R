shinyUI(fluidPage(
  
  withMathJax(), # to display LaTeX-like formulas
  
  tags$head( # CSS insert
    tags$style(HTML("      
      td, th, tr {
        padding-right: 10px;
        padding-left: 10px;
        border: 1px solid #d3d3d3;
      }
      border-color {
        d3d3d3
      }
    "))
  ),
  
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
      tabPanel(strong("Plots"), 
               br(),
               plotOutput("histogram"),
               plotOutput("scatterplot"),
               br(),
               htmlOutput("metrics")),
      
      tabPanel(strong("Table"), 
               br(),
               tableOutput("table_head"),
               p("..."),
               br(),
               tableOutput("table_tail"),
               code("Displays no more than 200 rows.")),
      
      tabPanel("Description",
               br(),
               h4(strong("TO REWRITE...")),
               p("Why is skewness relevant? Why is kurtosis relevant? Can you talk about the Fama-French model? Sortino ratio? CAPM? Return distributions for financial securities are often non-normal. High excess kurtosis coupled with high skewness makes for an especially toxic investment. p. 19 of Damodar."),
               p("Skewness describes the symmetry of a distribution. A negative skew (left-tailed) implies that negative returns are more severe than positive returns; similarly, a positive skew (right-tailed) implies that positive returns have a larger magnitude than negative returns. In other words, for a right-skewed distribution, the probability of large positive returns is greater than the probability of large negative returns. When it comes to financial securities, a securities with large negative skew are often avoided because they imply large downside risk. Although interpretations of skewness are arbitrary, it is often thought that a skewness greater than 1 or less than -1 imply significant skewness. All else equal, securities whose distributions are less than -1 are riskier than those above it."),
               p("Kurtosis describes the peakedness of a distribution. Because a Gaussian (normal) distrubtion has a kurtosis of 3, excess kurtosis is often a more informative metric. A distribution that is platykurtic (excess kurtosis < 3) has a low peak and thin tails; one that is leptokurtic (excess kurtosis > 3) has a high peak and fat tails. A distribution that is leptokurtic will exhibit fluctuations of larger magnitute than one platykurtic distribution, rendering the security more risky."),
               p("The skewness and kurtosis metrics should be computed relative to their standard errors. In other words, are the distributions /significantly/ skewed or leptokurtic/platykurtic?"),
               p("The Sortino ratio was created for X reasons."),
               br()
               
               
               # Can you describe skewness? kurtosis? what do they mean for securities? What's good and what's bad?
               # Can you explain R^2?
      ),
      
      tabPanel("Methodology and Sources",
               br(),
               h4(strong("Methodology")),
               p(strong("Downloading the data")),
               p("This project was developed using R to perform data analysis and R Shiny to produce the dynamic webapp."),
               p(span("Fama-French 3 Factor model data ", style="color:#990000"), "was downloaded from ", a("Kenneth French's data library", href="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html"), "on June 30, 2015. French's data comes in 3 frequencies: daily, weekly, or monthly. When a frequency is selected in the ", em("Input Menu,"), "the app reloads the appropriate Fama-French dataset and reruns all calculations."),
               p(span("Stock data ", style="color:#990000"), "is downloaded from Yahoo! Finance using R package ", code("stockPortfolio."), "When the ticker is modified in the ", em("Input Menu,"), "the app downloads the stock's daily data."),
               p("The program then performs a left outer join of the daily stock prices to the Fama-French dataset (daily, weekly or monthly) on the \"Date\" column, converts the stock prices to stock returns, then subsets this data by the period specified in the ", em("Input Menu."), "The data table produced is shown in the ", em("Table"), "tab."),
               p(strong("Performing the calculations")),
               p("The mathematical moments are automatically calculated by the R package", code("e1071.")),
               p("Market, SMB and HML betas are calculated by regressing excess stock returns on excess market returns, SMB portfolio returns and HML portfolio returns, respectively. Because the SMB and HML factors are long-short portfolios, the risk-free rate is implicitly removed - excess returns need not be used."),
               p("Market, SMB and HML factor risk premia are calculated as simple means over the time period as specified in the ", em("Input menu.")),
               p("CAPM return is calculated by: \\(r_{CAPM} = r_{f, today} + [\\beta_{market} * (r_{market} - r_{f})]\\)"), # $$...$$ new line vs. \\(...\\) inline
               p("Fama-French 3 factor return is calculated by: \\(r_{FF3} = r_{f, today} + (\\beta_{market} * RP_{market}) + (\\beta_{SMB} * RP_{SMB}) + (\\beta_{HML} * RP_{HML})\\)"),
               p("The annualized return is calculated by: \\((1 + r_{i})^{12}\\)"),               
               p("The annualized Sharpe ratio is calculated by: \\( (r_{i} - r_{f}) / std(r_{i})\\)"),
               p("The annualized Sortino ratio is calculated by: \\( (r_{i} - r_{f}) / std(r_{i}^{negaive\\_only})\\)"),
               
               br(),
               h4(strong("Sources consulted")),
               p("Fama-French 3 factor model data.", a("Kenneth R. French - Data Library.", href="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html")),
               p("Skewness and kurtosis for financial investments.", a("\"Chapter 4: How do we measure risk?\" Damodaran Online.", href="http://people.stern.nyu.edu/adamodar/pdfiles/valrisk/ch4.pdf")),
               p("CAPM calculation and R\\(^2\\) explanation.", a("\"Lecture 14: Implementing CAPM.\" M. Spiegel and R. Stanton, U.C. Berkeley, 2000.", href="http://faculty.som.yale.edu/zhiwuchen/finance-core/slides/l14-new.pdf")),
               p("Fama-French 3 factor model calculation.", a("\"Common risk factors in the returns on stocks and bonds.\" Eugene F. Fama and Kenneth R. French, 1992.", href="http://rady.ucsd.edu/faculty/directory/valkanov/pub/classes/mfe/docs/fama_french_jfe_1993.pdf")),
               p("CAPM vs. Fama-French models in explaining stock market variability", a("\"Testing the performance of asset pricing models in different economic and interest rate regimes using individual stock returns.\"Anne Marie Hibbert and Edward R. Lawrence, 2010.", href="http://epublications.bond.edu.au/cgi/viewcontent.cgi?article=1021&context=ijbf")),
               p("Calculating the Sharpe and Sortino ratios.", a("\"Sortino ratio: A better measure of risk.
\" Thomas Rollinger and Scott Hoffman, Futures Magazine, 2013.", href="http://www.futuresmag.com/2013/02/01/sortino-ratio-a-better-measure-of-risk")),
               p("R Shiny documentation.", a("Shiny by RStudio.", href="http://shiny.rstudio.com/reference/shiny/latest/")),
               
               br(),
               h4(strong("Thanks to")),
               br()
               # p("Professor Sebastien Betermier, McGill University") # need to ask permission
               
               
               # http://www.mcubeit.com/download/research/AnnualizingDailyReturns.pdf
        ),
      
      tabPanel("Background",
               br(),
               h4(strong("Background")),
               p("The motivation for this project was to calculate and visualize important financial metrics for any publicly traded security. Traditionally, these calculations are performed in Excel. This process is time-consuming, error-prone and produces static output. To my knowledge, there is no freely available online application that allows various input assumptions to be flexibly changed. Leveraging R to automatically run these calculations yields useful financial information extremely quickly."),
               p("Although some important financial statistics are readily available online, such as betas and realized returns, they provide an incomplete picture of stock returns. For example, despite being widely interpreted as a measure of risk, standard deviation provides a narrow, one-dimensional view of a stock's variability. In reality, stock returns are rarely normal, so variables like skewness and kurtosis cannot be assumed away."),
               p("Similarly, the CAPM is widely used as the de facto model for calculating required return, largely due to its simplicity. But while the CAPM empirically explains about 70% of portfolio returns, the Fama-French 3 factor model explains about 90%. Despite this, CAPM calculators online are abundant while Fama-Frnech ones are virtually nonexistent."),
               p("Notably, these calculations are not difficult to perform. Heavily tested and widely used programming libraries perform them automatically."),
               p("For these reasons, this webapp offers a more comprehensive view of stock returns. Valuable information like those calculated here should not be difficult to find."),
               br(),
               
               h4(strong("Applications")),
               tags$li("Academic"),
               p("Students can visualize how changing important assumptions about a security can change the financial metrics. For example, how is calculating beta sensitive to the frequency or time period chosen? What does R\\(^2\\) mean in relation to beta? What do fat tails look like and what are examples of non-normal distributions?"),
               p("Additionally, students can focus more on the concepts rather than the implementation of these calculations. For instance, understanding why the Sortino ratio is important may be a better use of time than calculating it."),
               tags$li("Quality control"),
               p("Using this tool, more thorough calculations can be sanity-checked throughout the production process. Checking comparable companies or changing important assumptions can be rapidly executed on this webapp"),
               tags$li("Testing the CAPM and Fama-French models"), 
               p("Because CAPM/Fama-French required returns can be easily computed using this tool, testing these models becomes less cumbersome. For example, how have portfolio realized returns fared against Fama-French required returns? Here, a regression can be run to determine the efficacy of these models."),
               tags$li("Portfolio Management"),
               p("This tool offers a nuanced view of portfolio returns. Risks are more easily seen and quantified. All of the statistics calculated here on an individual basis can be aggregated to the portfolio level."),
               tags$li("Elasticity"),
               p("Is there an 'elasticity' of required returns in response to changes in assumptions? How do changes in assumptions change the results?"),       

               br(),
               h4(strong("About me")),
               p("I am an analyst at the economic consulting firm Charles River Associates. I am passionate about data science and data visualization. More information about me can be found", a("at my personal website.", href="www.alexpetralia.com")),
              
               br()
      )
    )
  ))
))