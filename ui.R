shinyUI(fluidPage(

  shinyjs::useShinyjs(), # debugging
  withMathJax(), # to display LaTeX-like formulas
  
  tags$head( # CSS insert
    tags$style(HTML("      
      td, th, tr {
        padding-right: 10px;
        padding-left: 10px;
        border: 1px solid #d3d3d3;
      }
    "))
  ),
  
  titlePanel("A Dynamic Approach to Security Analysis"),
  
  br(),
  
  sidebarLayout(sidebarPanel(
        
    textInput("ticker", h3("Security ticker"), value="SPY"),
    
    br(),
    
    radioButtons("freq", h3("Frequency"),
                   c("Daily" = "daily",
                   "Monthly" = "monthly"),
                 inline = TRUE,
                 selected = "monthly"),
    
    br(),
    
    h3("Time period for the..."),
    
    sliderInput("ret_period", h4("returns distribution"),
                min = 1920,
                max = 2020,
                step = 1,
                value = c(1900,2015),
                sep = ""),
    
    sliderInput("beta_period", h4("beta calculations"),
                min = 1920,
                max = 2020,
                step = 1,
                value = c(2010,2015),
                sep = ""),
    
    code("recommended: 5 year interval"),
    
    sliderInput("rp_period", h4("risk premia calculations"),
                min = 1920,
                max = 2020,
                step = 1,
                value = c(1995,2015),
                sep = ""),
    
    code("recommended: 20 year interval"),
    
    br(),
    width = 3
  ),
  
  mainPanel(
    
    tabsetPanel(position=c("right"),
      tabPanel(strong("Plots"), 
               br(),
               plotOutput("histogram"),
               plotOutput("scatterplot"),
               htmlOutput("metrics_stats"),
               htmlOutput("metrics_finance"),
               br()),
      
      tabPanel(strong("Data"), 
               br(),
               tableOutput("table_head"),
               p("..."),
               br(),
               tableOutput("table_tail"),
               code("Displays no more than 200 rows."),
               br()),
      
      tabPanel(strong("Annualized data"),
               br(),
               tableOutput("annualized_table"),
               br()),
      
      tabPanel("Description",
               br(),
               h4(strong("Overview")),
               p(strong("Mean return"), "is the average realized stock return."),
               p(strong("Median return"), "is the return at which half of returns are above and half are below. The median differs from the mean when distributions are not normal."),
               p(strong("Standard deviation"), "describes the dispersion of stock returns. If the distribution is normal, stock returns will fall above or below the mean return by one standard deviation about 68% of the time. About 95% of the time, stock returns will fall within two standard deviations of the mean return."),
               p(strong("Skewness"), "describes the symmetry of a distribution. A negative skew (left-tailed) implies that negative returns (relative to the mode) are less common but more extreme than positive returns; likewise, a positive skew (right-tailed) implies that positive returns (relative to the mode) are less common but more extreme than negative returns. In other words, for a right-skewed distribution, the likelihood of extreme positive returns is greater than that of extreme negative returns. In finance, securities that exhibit large negative skewness are often avoided because they imply large downside risk."),
               p(strong("Kurtosis"), "describes the peakedness of a distribution. Because a normal distribution has a kurtosis of 3, excess kurtosis is often used instead (ie. kurtosis less 3). A distribution that is platykurtic (excess kurtosis < 0) has a low peak and thin tails; one that is leptokurtic (excess kurtosis > 0) has a high peak and fat tails. A distribution that is leptokurtic will exhibit fluctuations of larger magnitude than one platykurtic distribution, rendering the security more risky."),
               p(span("From the above, it's clear that distributions which exhibit high standard deviations, negative skewness and high kurtosis are the riskiest investments", style="color:#990000")),
               p("The", strong("Capital Asset Pricing Model (CAPM)"), "is a commonly used financial pricing model. It calculates a required rate of return for a stock by adding the risk-free rate to the product of the stock's beta and the market risk premium. Unlike many other models, it assumes there is only one type of risk: market risk."),
               p("The", strong("Fama-French 3 factor model"), "is a financial pricing model. It expands on the CAPM model by adding two other factors to market risk: size risk and value risk. It uses two portfolios to proxy these risks: SMB and HML, respectively. Because the Fama-French model empirically explains the variability of market returns better than the CAPM, it is believed this is the evidence that investors require size and value risk premia (or, at least, whatever these portfolios truly proxy). The SMB (small minus big market cap) and HML (high minus low book-to-value) are long-short portfolios that are long the top third of small/high stocks and short the bottom third."),
               p("Both these regression models produce \\(R^2\\) results. The \\(R^2\\) value measures to what degree the independent variable (eg. factor returns) explains the variability of the dependent variable (eg. stock returns). It is a proxy for how \"useful\" the alpha and beta values are. For example, a beta value that is high while the \\(R^2\\) is low implies that the beta value is unreliable. As a result, using this beta in a financial pricing model may not produce robust results."),
               p("The", strong("Sharpe ratio"), "is a commonly used metric to evaluate stock or portfolio performance. It is defined as mean return less the risk free rate, divided by the standard deviation. Put simply, it is how much the stock returns given each additional unit of risk."),
               p("The", strong("Sortino ratio"), "expands on the Sharpe ratio by recognizing that standard deviation of returns also includes positive returns. Typically, investors are only concerned with downside risk, not upside. As a result, Sortino updates the Sharpe ratio to use not the standard deviation of all returns, but only negative returns."),
               br()
      ),
      
      tabPanel("Methodology and Sources",
               br(),
               h4(strong("Methodology")),
               p(strong("Downloading the data")),
               p("This project was developed using R to perform data analysis and R Shiny to produce the dynamic webapp."),
               p(span("Fama-French 3 Factor model data ", style="color:#990000"), "was downloaded from ", a("Kenneth French's data library", href="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html"), "on June 30, 2015. French's data comes in 3 frequencies: daily, weekly, or monthly. Weekly was omitted because it produced inconsistent results. When a frequency is selected in the ", em("Input Menu,"), "the app reloads the appropriate Fama-French dataset and reruns all calculations."),
               p(span("Stock data ", style="color:#990000"), "is downloaded from Yahoo! Finance using R package ", code("stockPortfolio."), "When the ticker is modified in the ", em("Input Menu,"), "the app downloads the stock's daily data."),
               p("The program performs a left outer join of the daily stock prices to the Fama-French dataset (daily or monthly) on the \"Date\" column, converts the stock prices to stock returns, then subsets this data by the period specified in the ", em("Input Menu."), "The data table produced is shown in the ", em("Table"), "tab."),
               p("The annualized table is calculated by taking each return of the daily or monthly tables, \\(r_{i}\\), then performing for each row in the year \\(( \\prod_{i=1}^n (1+r_{i}) )-1\\)."),
               p(strong("Performing the calculations")),
               p("All calculations that are \"annualized\" are produced using the", em("Annualized data"), "table and all unannualized calculations are produced using the", em("Data"), "table."),
               p("The mathematical moments are automatically calculated by the R package", code("e1071.")),
               p("Unannualized market, SMB and HML betas are calculated by regressing excess stock returns on excess market returns, SMB portfolio returns and HML portfolio returns, respectively, over the beta time period as specified in the", em("Input menu."), "Because the SMB and HML factors are long-short portfolios, the risk-free rate is implicitly removed - excess returns need not be used."),
               p("Annualized market, SMB and HML factor risk premia are calculated as simple means over the risk premia time period as specified in the ", em("Input menu.")),
               p("CAPM return is calculated by: \\(r_{CAPM} = r_{f, today} + [\\beta_{market} * (r_{market} - r_{f})]\\). \"Today\" implies the risk-free rate at the end of the risk premia time period."), # $$...$$ new line vs. \\(...\\) inline
               p("Fama-French 3 factor return is calculated by: \\(r_{FF3} = r_{f, today} + (\\beta_{market} * RP_{market}) + (\\beta_{SMB} * RP_{SMB}) + (\\beta_{HML} * RP_{HML})\\)"),       
               p("The annualized Sharpe ratio is calculated by: \\( (r_{i} - r_{f}) / std(r_{i})\\)"),
               p("The annualized Sortino ratio is calculated by: \\( (r_{i} - r_{f}) / std(r_{i}^{negative\\_only})\\)"),
               
               br(),
               h4(strong("Sources consulted")),
               p("Annualizing returns data.", a("\"Annualizing Daily Returns - A Twist and a Solution\". Arun Muralidhar.", href="http://www.mcubeit.com/download/research/AnnualizingDailyReturns.pdf")),
               p("Fama-French 3 factor model data.", a("Kenneth R. French - Data Library.", href="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html")),
               p("Skewness and kurtosis for financial investments.", a("\"Chapter 4: How do we measure risk?\" Damodaran Online.", href="http://people.stern.nyu.edu/adamodar/pdfiles/valrisk/ch4.pdf")),
               p("CAPM calculation and R\\(^2\\) explanation.", a("\"Lecture 14: Implementing CAPM.\" M. Spiegel and R. Stanton, U.C. Berkeley, 2000.", href="http://faculty.som.yale.edu/zhiwuchen/finance-core/slides/l14-new.pdf")),
               p("Fama-French 3 factor model calculation.", a("\"Common risk factors in the returns on stocks and bonds.\" Eugene F. Fama and Kenneth R. French, 1992.", href="http://rady.ucsd.edu/faculty/directory/valkanov/pub/classes/mfe/docs/fama_french_jfe_1993.pdf")),
               p("CAPM vs. Fama-French models in explaining stock market variability", a("\"Testing the performance of asset pricing models in different economic and interest rate regimes using individual stock returns.\" Anne Marie Hibbert and Edward R. Lawrence, 2010.", href="http://epublications.bond.edu.au/cgi/viewcontent.cgi?article=1021&context=ijbf")),
               p("Calculating the Sharpe and Sortino ratios.", a("\"Sortino ratio: A better measure of risk.
\" Thomas Rollinger and Scott Hoffman, Futures Magazine, 2013.", href="http://www.futuresmag.com/2013/02/01/sortino-ratio-a-better-measure-of-risk")),
               p("R Shiny documentation.", a("Shiny by RStudio.", href="http://shiny.rstudio.com/reference/shiny/latest/")),
               
               br(),
               h4(strong("Thanks to")),
               p(strong("Professor Sebastien Betermier"), " (McGill University) for his", em("Investment Management"), "course and helpful comments."),
               br()
        ),
      
      tabPanel("Background",
               br(),
               h4(strong("Background")),
               p("The motivation for this project was to calculate and visualize important financial metrics for any publicly traded security. Traditionally, these calculations are performed in Excel. This process is time-consuming, error-prone and produces static output. To my knowledge, there is no freely available online application that allows various input assumptions to be flexibly changed. Leveraging R to automatically run these calculations yields useful financial information extremely quickly."),
               p("Although some important financial statistics are readily available online, such as betas and realized returns, they provide an incomplete picture of stock returns. For example, despite being widely interpreted as a measure of risk, standard deviation provides a narrow, one-dimensional view of a stock's variability. In reality, stock returns are rarely normal, so variables like skewness and kurtosis cannot be assumed away."),
               p("Similarly, the CAPM is widely used as the de facto model for calculating required return, largely due to its simplicity. But while the CAPM empirically explains about 70% of variability in market returns, the Fama-French 3 factor model explains over 90%. Despite this, CAPM calculators online are abundant while Fama-French ones are virtually nonexistent."),
               p("Notably, these calculations are not difficult to perform. Heavily tested and widely used programming libraries perform them automatically. For these reasons, this webapp offers a more comprehensive view of stock returns."),
               p(span("All errors in this project are solely my own.", style="color:#990000")),
               br(),
               
               h4(strong("Applications")),
               strong("Academic"),
               p("Students can visualize how changing their input assumptions affects their results. For example, how is calculating beta sensitive to the frequency or time period chosen? What does R\\(^2\\) mean in relation to beta? What do fat tails look like and what are examples of non-normal distributions?"),
               strong("Quality control"),
               p("Using this tool, more thorough Excel-based models can be sanity-checked throughout the production process. Checking comparable companies or changing important assumptions can be rapidly executed using this webapp."),
               strong("Portfolio Management"),
               p("For portfolio managers, risk may be more easily seen and quantified. All of the statistics calculated here on an individual basis can be aggregated to the portfolio level."),

               br(),
               h4(strong("About me")),
               p("I am an analyst at the economic consulting firm Charles River Associates. More information about me can be found", a("at my personal website.", href="http://www.alexpetralia.com")),
              
               br()
      )
    )
  ))
))
