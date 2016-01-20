##A Dynamic Approach to Security Analysis

####**<a href="http://52.2.13.97:3838/fama_french/" target="_blank">You can view this project live here.</a>**

The motivation for this project was to calculate and visualize important financial metrics for any publicly traded security. Traditionally, these calculations are performed in Excel. This process is time-consuming, error-prone and produces static output. To my knowledge, there is no freely available online application that allows various input assumptions to be flexibly changed. Leveraging R to automatically run these calculations yields useful financial information extremely quickly.

Although some important financial statistics are readily available online, such as betas and realized returns, they provide an incomplete picture of stock returns. For example, despite being widely interpreted as a measure of risk, standard deviation provides a narrow, one-dimensional view of a stock's variability. In reality, stock returns are rarely normal, so variables like skewness and kurtosis cannot be assumed away.

Similarly, the CAPM is widely used as the de facto model for calculating required return, largely due to its simplicity. But while the CAPM empirically explains about 70% of variability in market returns, the Fama-French 3 factor model explains over 90%. Despite this, CAPM calculators online are abundant while Fama-French ones are virtually nonexistent.

Notably, these calculations are not difficult to perform. Heavily tested and widely used programming libraries perform them automatically. For these reasons, this webapp offers a more comprehensive view of stock returns.