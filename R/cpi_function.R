

# cpi <- read_csv("./Data/Canadian_CPI.csv") %>% 
#   rename(YEAR=Year) %>% 
#   mutate(base_year=2002)
# 
# cpi_base <- cpi %>% select(-InflationRate)
# base_year <- 2015
inflation_adjust <- function(cpi_base, base_year=NA){  
  require(dplyr)
  #cpi_base must have the following columns in this order: year, cpi, base_year 
  colnames(cpi_base) <- c("year", "cpi", "base_year")
  if (nchar(base_year) == 4){
    # Formula for calculating inflation example: $1.00 * (1980 CPI/ 2014 CPI) = 1980 price
    cf <- data_frame(adj_value = as.numeric(cpi_base$cpi[which(cpi_base$year==base_year)])/ cpi_base$cpi, year=cpi_base$year)
    dat <- left_join(cpi_base, cf)
    # Xts object to data frame
    dat$base_year <- as.character(base_year)
    # Reorder cols in a more usable fashion
    dat <- dat %>% select(year, cpi, base_year, adj_value)
    return(dat)
  }
  else {(message(
    "***************************************************************************************
    Please input a valid four digit year without quotes. For example: 2015.
    ***************************************************************************************", appendLF = TRUE))
  }
}
# cpi_2010 <- inflation_adjust(cpi_base = cpi_base, base_year=10)
