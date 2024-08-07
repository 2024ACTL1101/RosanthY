## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a data frame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.

\newpage
## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a data frame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```


## Plotting the Data
Plot the closing prices over time to visualize the price movement.
```{r plot}
plot(amd_df$date, amd_df$close,'l')
```

\newpage
## Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the data frame to execute trades based on the set conditions.

- Change the algorithm by modifying the loop to include the cost and proceeds metrics for Buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one: **(1)** If the previous price = 0, set 'trade_type' to 'Buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable. **(2)** Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'Buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100. **(3)** You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price. **(4)** If this is the last day of trading, set the 'trade_type' to 'Sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```{r trading}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# For loop to run through each day from the first day to the second last day
for (i in 1:(nrow(amd_df) - 1)) {
  
  # Buy if the close price of the current day is less than that of the previous day
  if (i == 1 || amd_df$close[i] < previous_price) {
    amd_df$trade_type[i] <- 'Buy'
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    amd_df$accumulated_shares[i] <- accumulated_shares + share_size
  } else {
    
    # If the share price is higher, no shares are bought
    amd_df$trade_type[i] <- 'Hold'
    amd_df$costs_proceeds[i] <- 0
    amd_df$accumulated_shares[i] <- accumulated_shares
  }
  
  # Update Variables
  accumulated_shares <- amd_df$accumulated_shares[i]
  previous_price <- amd_df$close[i]
}

# Sell on the last day in the given data frame
amd_df$trade_type[nrow(amd_df)] <- 'Sell'
amd_df$accumulated_shares[nrow(amd_df)] <- amd_df$accumulated_shares[nrow(amd_df) - 1]
amd_df$costs_proceeds[nrow(amd_df)] <- amd_df$accumulated_shares[nrow(amd_df)] *
                                          amd_df$close[nrow(amd_df)]
```

\newpage
## Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```{r period}
# Create data frame for the chosen period (2023-01-31 to 2023-11-02)
amd_df_custom <- amd_df[933:1124, ]
rownames(amd_df_custom) <- 1:nrow(amd_df_custom) # Rename rows from 1 onwards

# Initialize columns for trade type, cost/proceeds, and accumulated shares
amd_df_custom$trade_type <- NA
amd_df_custom$costs_proceeds <- NA
amd_df_custom$accumulated_shares <- 0

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Implementation of trading algorithm from step 2
# For loop to run through each day from the first day to the second last day
for (i in 1:(nrow(amd_df_custom) - 1)) {
  
  # Buy if the close price of the current day is less than that of the previous day
  if (i == 1 || amd_df_custom$close[i] < previous_price) {
    amd_df_custom$trade_type[i] <- 'Buy'
    amd_df_custom$costs_proceeds[i] <- -amd_df_custom$close[i] * share_size
    amd_df_custom$accumulated_shares[i] <- accumulated_shares + share_size
  } else {
    
    # If the share price is higher, no shares are bought
    amd_df_custom$trade_type[i] <- 'Hold'
    amd_df_custom$costs_proceeds[i] <- 0
    amd_df_custom$accumulated_shares[i] <- accumulated_shares
  }
  
  # Update Variables
  accumulated_shares <- amd_df_custom$accumulated_shares[i]
  previous_price <- amd_df_custom$close[i]
}

# Sell on the last day in the given data frame
amd_df_custom$trade_type[nrow(amd_df_custom)] <- 'Sell'
amd_df_custom$accumulated_shares[nrow(amd_df_custom)] <-
                            amd_df_custom$accumulated_shares[nrow(amd_df_custom) - 1]
amd_df_custom$costs_proceeds[nrow(amd_df_custom)] <-
                            amd_df_custom$accumulated_shares[nrow(amd_df_custom)] *   
                            amd_df_custom$close[nrow(amd_df_custom)]
```

\newpage
## Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your data frame. This column records the financial impact of each trade, reflecting money spent on Buys as negative values and money gained from Sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'Buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}
# Total Profit / Loss Calculation
Profit_Loss = sum(amd_df_custom$costs_proceeds)

# Invested Capital Calculation
Invested_Capital = -sum(amd_df_custom$costs_proceeds[amd_df_custom$costs_proceeds < 0])

# Return on Investment (ROI) Calculation
ROI = Profit_Loss / Invested_Capital * 100

# Print the Variables above
cat("     Profit/Loss:", Profit_Loss, "$",  # Print profit or loss
    "\nInvested Capital:", "#", Invested_Capital,  # Print the invested capital
    "\n             ROI:", ROI, "%")  # Print the ROI
```

\newpage
## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Option 1)

```{r option}
# Initialize columns for trade type, cost/proceeds, and accumulated shares
amd_df_custom$trade_type <- NA
amd_df_custom$costs_proceeds <- NA
amd_df_custom$accumulated_shares <- 0

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
average_price <- 0

# For loop to run through each day from the first day to the second last day
for(i in 1:(nrow(amd_df_custom) - 1)) {
  
  # Buy if the close price of the current day is less than that of the previous day
  if(i == 1 || amd_df_custom$close[i] < previous_price) {
    accumulated_shares <- accumulated_shares + share_size
    amd_df_custom$trade_type[i] <- 'Buy'
    amd_df_custom$costs_proceeds[i] <- -amd_df_custom$close[i] * share_size
    amd_df_custom$accumulated_shares[i] <- accumulated_shares
    
    # Update Average Purchase Price
    average_price <- ((average_price * (accumulated_shares - share_size)) +
                    (share_size * amd_df_custom$close[i])) / accumulated_shares
    
    # If price has increased 20% from the average purchase price, Sell half the shares
  } else if(accumulated_shares > 0 && amd_df_custom$close[i] > 1.2 * average_price) {
    accumulated_shares <- accumulated_shares / 2
    amd_df_custom$trade_type[i] <- 'Sell'
    amd_df_custom$costs_proceeds[i] <- accumulated_shares * amd_df_custom$close[i]
    amd_df_custom$accumulated_shares[i] <- accumulated_shares
    
    # If none of the above are satisfied, no shares are bought
  } else {
    amd_df_custom$trade_type[i] <- 'Hold'
    amd_df_custom$costs_proceeds[i] <- 0
    amd_df_custom$accumulated_shares[i] <- accumulated_shares
  }
  
  # Update Variables
  accumulated_shares <- amd_df_custom$accumulated_shares[i]
  previous_price <- amd_df_custom$close[i]
}

# Sell on the last day in the given data frame
amd_df_custom$trade_type[nrow(amd_df_custom)] <- 'Sell'
amd_df_custom$accumulated_shares[nrow(amd_df_custom)] <-
                            amd_df_custom$accumulated_shares[nrow(amd_df_custom) - 1]
amd_df_custom$costs_proceeds[nrow(amd_df_custom)] <-
                            amd_df_custom$accumulated_shares[nrow(amd_df_custom)] *   
                            amd_df_custom$close[nrow(amd_df_custom)]
```

\newpage
## Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```{r}
# Total Profit / Loss Calculation
Profit_Loss = sum(amd_df_custom$costs_proceeds)

# Invested Capital Calculation
Invested_Capital = -sum(amd_df_custom$costs_proceeds[amd_df_custom$costs_proceeds < 0])

# Return on Investment (ROI) Calculation
ROI = Profit_Loss / Invested_Capital * 100

# Print the Variables above
cat("     Profit/Loss:", Profit_Loss, "$",  # Print profit or loss
    "\nInvested Capital:", "#", Invested_Capital,  # Print the invested capital
    "\n             ROI:", ROI, "%")  # Print the ROI
```

**Output from previous strategy (Steps 3-4)**

- \qquad Profit/Loss: 76109.02 $
- Invested Capital: # 948276
- \qquad \quad \quad \quad ROI: 8.026041 %

**Discusion**

The 'Profit-Taking' strategy (Step 5) resulted in $6373.76 less Profit than the 'Excel' Strategy (Step 3), with the same amount of Invested Capital. This resulted in the return on investment for the 'Profit-Taking' strategy to be lower by 0.6667051%.  Hence, the Profit/Loss and ROI did not improve with the implementation of the 'Profit-Taking' strategy over the time period, 2023-01-31 to 2023-11-02.

Looking into market events during this time period by referring to the past archives of AMD Newsroom, it is evident that May was an eventful month for AMD when they: 

- Released their 2023 Q1 financial results on May 3^rd^
- Announced their plans to showcase a Data Center and AI Technology on May 9^th^
- Released the Ryzen 7020 C-series on 23^rd^ May
- Released the AMD Radeon RX 7600 on 24^th^ May

This is further evident in their growth in May, with consecutive 'Sell' signals outputted by the 'Profit-Taking' strategy towards the end of the month, during the release of their products. In other words, this suggests a rise in 20% or greater over the average purchase price for these consecutive days.

Hence we can conclude that the events listed above have had a positively significant impact on the perception of AMD’s value by stakeholders and investors, leading to an increased demand for AMD shares and a consequent rise in their value.

\newpage
## Custom Trading Period Data Frame
```{r}
amd_df_custom
```

\newpage
## Original Data Frame
```{r}
amd_df
```
