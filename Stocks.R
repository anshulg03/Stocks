# Stock Market exercise

stocks <- read_csv("data.csv")

#Distinct Stocks
stocks %>% group_by(Symbol) %>% summarise()

#BAC 2016 opening prices
BAC_2016 <- stocks %>% filter(Symbol == "BAC", year(Date) == 2016) %>% select(1:3)

#Lowest and Highest Closing Price and respective Date
stocks %>% group_by(Symbol) %>% 
  summarise(Max_Price = max(Close),Max_Date = Date[Close == max(Close)], Min_Price = min(Close), Min_Date = Date[Close == min(Close)])

#Avg Price of a stock for a given Time Period
AvgPrice <- function(Sym, stdate, endate){
  stocks %>% filter(Symbol == Sym, Date >= stdate , Date <= endate) %>% 
    summarise(Avg_Price = mean(Close))
}

AvgPrice("X","2016-01-01","2016-01-31")


# Weighted Avg Price
WavgPrice <- function(Sym, stdate, endate){
  stocks %>% filter(Symbol == Sym, Date >= stdate , Date <= endate) %>% 
    summarise(Wavg_Price = sum(as.numeric(Close * Volume))/sum(as.numeric(Volume)))
}

WavgPrice("GE","2016-01-01","2016-12-31")

# Daily Closing Price of each Stock
Daily_Close <- stocks %>% select(Symbol, Date, Close) %>% spread(Symbol, value = Close)

#Daily return for each stock
freturn <- function(X){(X - lag(X, default = 0))/lag(X)}
Drtrn <- apply(Daily_Close[,2:6], 2, freturn)
Drtrn <- as_tibble(Drtrn)
Drtrn <- Drtrn %>% mutate(Date = Daily_Close$Date) %>% select(Date, 1:5)

#Avg Daily Return 
map_dbl(Drtrn,mean, na.rm = T)

#Highest Avg Risk
map_dbl(Drtrn,sd, na.rm = T)


#Lowest Risk in a period
LowestRisk <- function(stdate, endate){
       dt <- Drtrn %>% filter(Date >= stdate, Date <= endate)
       names(which.min(map_dbl(dt,sd, na.rm = T)))
}

LowestRisk("2016-04-01","2016-04-30")


# Corealtion Matrix
cor(Drtrn[,-1], use="pairwise.complete.obs")



