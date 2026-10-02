validate_portfolio <- function(portfolio) {
  
  required_columns <- c(
    "Ticker",
    "Quantity",
    "PurchasePrice",
    "AssetClass"
  )
  
  supported_asset_classes <- c(
    "Equity",
    "ETF"
  )
  
  errors <- character(0)
  if (nrow(portfolio) == 0) errors <- c(errors, "Portfolio must contain at least one holding.")
  if (nrow(portfolio) > 50) errors <- c(errors, "Please limit the portfolio to 50 holdings.")
  
  
  # -----------------------------
  # 1. Check required columns
  # -----------------------------
  
  missing_columns <- setdiff(
    required_columns,
    names(portfolio)
  )
  
  if (length(missing_columns) > 0) {
    
    errors <- c(
      errors,
      paste(
        "Missing required column(s):",
        paste(missing_columns, collapse = ", ")
      )
    )
    
    return(
      list(
        valid = FALSE,
        data = portfolio,
        errors = errors
      )
    )
  }
  
  
  # -----------------------------
  # 2. Clean basic text fields
  # -----------------------------
  
  portfolio$Ticker <- toupper(
    trimws(portfolio$Ticker)
  )
  
  portfolio$AssetClass <- trimws(
    portfolio$AssetClass
  )
  
  
  # -----------------------------
  # 3. Check missing values
  # -----------------------------
  
  if (any(is.na(portfolio))) {
    
    errors <- c(
      errors,
      "Portfolio contains missing values."
    )
  }
  
  if (any(portfolio$Ticker == "", na.rm = TRUE)) {
    
    errors <- c(
      errors,
      "Ticker cannot be empty."
    )
  }
  
  if (any(portfolio$AssetClass == "", na.rm = TRUE)) {
    
    errors <- c(
      errors,
      "AssetClass cannot be empty."
    )
  }
  
  
  # -----------------------------
  # 4. Check duplicate tickers
  # -----------------------------
  
  duplicate_tickers <- unique(
    portfolio$Ticker[
      duplicated(portfolio$Ticker)
    ]
  )
  
  if (length(duplicate_tickers) > 0) {
    
    errors <- c(
      errors,
      paste(
        "Duplicate ticker(s):",
        paste(duplicate_tickers, collapse = ", ")
      )
    )
  }
  
  
  # -----------------------------
  # 5. Validate Quantity
  # -----------------------------
  
  if (!is.numeric(portfolio$Quantity)) {
    
    errors <- c(
      errors,
      "Quantity must contain numeric values."
    )
    
  } else if (any(
    !is.finite(portfolio$Quantity) | portfolio$Quantity <= 0,
    na.rm = TRUE
  )) {
    
    errors <- c(
      errors,
      "Quantity must be greater than zero."
    )
  }
  
  
  # -----------------------------
  # 6. Validate PurchasePrice
  # -----------------------------
  
  if (!is.numeric(portfolio$PurchasePrice)) {
    
    errors <- c(
      errors,
      "PurchasePrice must contain numeric values."
    )
    
  } else if (any(
    !is.finite(portfolio$PurchasePrice) | portfolio$PurchasePrice <= 0,
    na.rm = TRUE
  )) {
    
    errors <- c(
      errors,
      "PurchasePrice must be greater than zero."
    )
  }
  
  
  # -----------------------------
  # 7. Validate AssetClass
  # -----------------------------
  
  invalid_asset_classes <- setdiff(
    unique(portfolio$AssetClass),
    supported_asset_classes
  )
  
  invalid_asset_classes <- invalid_asset_classes[
    !is.na(invalid_asset_classes)
  ]
  
  if (length(invalid_asset_classes) > 0) {
    
    errors <- c(
      errors,
      paste(
        "Unsupported asset class(es):",
        paste(invalid_asset_classes, collapse = ", ")
      )
    )
  }
  
  
  # -----------------------------
  # Return validation result
  # -----------------------------
  
  list(
    valid = length(errors) == 0,
    data = portfolio,
    errors = errors
  )
  
}

calculate_portfolio_values <- function(portfolio, market_data = list()) {
  portfolio$ValuationPrice <- portfolio$PurchasePrice
  portfolio$PriceSource <- "Cost basis fallback"
  portfolio$PriceDate <- as.Date(NA)
  for (i in seq_len(nrow(portfolio))) {
    prices <- market_data[[portfolio$Ticker[i]]]
    if (is.null(prices)) next
    close <- quantmod::Cl(prices)
    good <- which(is.finite(as.numeric(close)) & as.numeric(close) > 0)
    if (!length(good)) next
    j <- tail(good, 1)
    portfolio$ValuationPrice[i] <- as.numeric(close[j])
    portfolio$PriceDate[i] <- as.Date(zoo::index(close)[j])
    portfolio$PriceSource[i] <- "Latest close in selected range"
  }
  
  portfolio$PositionValue <-
    portfolio$Quantity * portfolio$ValuationPrice
  
  portfolio_value <- sum(
    portfolio$PositionValue
  )
  
  portfolio$Weight <-
    portfolio$PositionValue / portfolio_value
  
  list(
    data = portfolio,
    portfolio_value = portfolio_value
  )
  
}

calculate_portfolio_summary <- function(portfolio) {
  
  number_of_holdings <- nrow(portfolio)
  
  largest_position_index <- which.max(
    portfolio$PositionValue
  )
  
  largest_position_ticker <-
    portfolio$Ticker[largest_position_index]
  
  largest_position_value <-
    portfolio$PositionValue[largest_position_index]
  
  largest_position_weight <-
    portfolio$Weight[largest_position_index]
  
  list(
    number_of_holdings = number_of_holdings,
    largest_position_ticker = largest_position_ticker,
    largest_position_value = largest_position_value,
    largest_position_weight = largest_position_weight
  )
  
}

