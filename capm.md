# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

\newpage
## Instructions
### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo",
                       from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo",
                        from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED",
                      from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))

# Accessing the first column of rf_data
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```


#### Data Processing

```{r data}
colSums(is.na(df))

# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

\newpage
### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

\[ E(R_i) = R_f + \beta_i (E(R_m) - R_f) \]

Where:

- \( E(R_i) \) is the expected return on the capital asset,
- \( R_f \) is the risk-free rate,
- \( \beta_i \) is the beta of the security, which represents the systematic risk of the security,
- \( E(R_m) \) is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$
  
```{r return}
# Calculate the Daily Returns
df <- df %>%
  mutate(AMD_DR = AMD/lag(AMD) - 1,
         GSPC_DR = GSPC/lag(GSPC) - 1)
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```{r riskfree}
# Calculate the Daily Risk Free Rate (RF)
df <- df %>%
  mutate(Daily_RF = (1 + RF/100) ^ (1/360) - 1)
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}
# Calculate the Excess Returns (ER)
df <- df %>%
  mutate(AMD_ER = AMD_DR - Daily_RF,
         GSPC_ER = GSPC_DR - Daily_RF)
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}
# Perform linear regression
AMD_GSPC <- lm(df$AMD_ER ~ df$GSPC_ER)

# Summary of model
summary(AMD_GSPC)

# Find the Beta
beta <- coef(AMD_GSPC)[2]

# Print the beta
cat("Beta:", beta)
```

\newpage
#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
A \(\beta\) of a stock, in this case AMD, is its volatility in comparison to the market, more specifically here, the S&P 500 (GSPC), which represents the top 500 companies in the stock market. 

A \(\beta\) of 1 indicates that the stock is as volatile as the market,
a \(\beta\) less than 1 indicates less volatility,
and a \(\beta\) greater than 1 indicates greater volatility.

Since the \(\beta\) of the linear model of AMD relative to the S&P 500 is **1.5701012**, AMD is **more volatile** than the S&P 500, which is representing the broader market. More specifically, it is (approx.) 57.01% more volatile than the S&P 500.

#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}
# Creating a scatter plot
ggplot(aes(x=GSPC_ER, y=AMD_ER), data = df[-1,]) +
geom_point(size = 0.8, colour = 'blue') +

# Creating the CAPM regresssion line
geom_smooth(method = "lm", se = FALSE, colour = 'red',formula = y ~ x) +
  
# Plot labels and theme
labs(x = "S&P 500 Excess Returns", y = "AMD Excess Returns",
     title = "AMD vs S&P 500 CAPM Regression") +
theme_classic()
```

\newpage

### Step 3: Predictions Interval

Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.

*Hint: Calculate the daily standard error of the forecast ($s_f$), and assume that the annual standard error for prediction is $s_f \times \sqrt{252}$. Use the simple return average method to convert daily stock returns to annual returns if needed.*


**Answer:**

```{r pi}
# Set value of assumed Expected Return (aER) for GSPC and Risk Free Rate (aRFR).
aRFR <- 0.05
aER_GSPC <- 0.133

# Calculate the daily standard error of the forecast (sf)
daily_sf <- sd(resid(AMD_GSPC))

# Calculate the annual standard error for prediction
annual_sf <- daily_sf * sqrt(252)

# Calculate the expected return (ER) of AMD
ER_AMD <- aRFR + beta * (aER_GSPC - aRFR)

# Calculate the prediction interval
lower <- ER_AMD - qt(0.95, df=nrow(df)-2) * annual_sf
upper <- ER_AMD + qt(0.95, df=nrow(df)-2) * annual_sf

# The 90% prediction interval for AMD's annual expected return
cat("90% prediction interval for AMD's annual expected return:",
    "[", lower, ",", upper, "]")
```

