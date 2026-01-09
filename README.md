Key Objectives:

The primary objective of this project was to analyze historical 4×100m relay performance data to understand long-term trends in elite sprint relay events. A comparative analysis 
was conducted to examine how performance evolution differs between men’s and women’s categories. In addition, time-series models were developed to forecast future relay timings 
and assess potential performance improvements. The project also aimed to visually explore medal outcomes and competitiveness over time to better understand performance gaps.

Data Cleaning & Preprocessing:

The dataset was first cleaned to handle missing values and remove inconsistent or incomplete event records. 
The analysis was restricted to senior-level men’s and women’s 4×100m relay events to ensure comparability across seasons. 
Yearly best relay times were then aggregated to create a consistent time series suitable for trend analysis and forecasting.

Exploratory Data Analysis (EDA):

Exploratory analysis was performed to study year-wise trends in best relay performances and identify long-term improvement patterns.
Three-year moving averages were applied to smooth short-term fluctuations and highlight underlying trends. 
A direct comparison between men’s and women’s performance evolution was carried out to assess differences in improvement rates and variability over time.

Time Series Forecasting:

To predict future relay performance, ARIMA and ETS models were built and compared using historical performance data. 
Forecasts were generated for up to eight future seasons to evaluate long-term performance trends. 
A train–test split approach was used, and model accuracy was assessed using standard error metrics such as MAE and RMSE to ensure reliable comparisons.

Sports Performance Visualisation:

Multiple visualizations were created to communicate performance insights effectively. 
Medal-wise trends for Gold, Silver, and Bronze positions were analyzed to study competitiveness across seasons. 
Country-level medal dominance was examined to identify consistently strong relay nations. 
Additionally, time-gap analysis between Gold and Silver medal performances was conducted to assess how competitive margins have evolved.
