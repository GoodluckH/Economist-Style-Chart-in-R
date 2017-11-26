# Import libs.
library(ggthemes)
library(ggplot2)
library(extrafont)

# Import and read csv files. Extract close prices from SP500 and APPLE and save them into a new dataframe.
SP500 <- read.csv('/Your/File/Name/R/SP500.csv')
APPLE <- read.csv('/Your/File/Name/R/AAPL.csv')
Tbill <- read.csv('/Your/File/Name/R/TB3MS.csv')
price <- data.frame(SP500$Close, APPLE$Close)
colnames(price) <- c('SP500', 'APPLE') 

# Calculate the monthly return by dividing the earning by the cost.
gain <- price[2:dim(price)[1],]
cost <- price[1:dim(price)[1]-1,]
monthly.return <- (gain-cost)/cost

# Calculate monthly return for US 3-Month Treasury Bill. 
Tbill.monthly.return <- (1+Tbill$TB3MS)^(1/12)-1

# Calculate monthly premium for SP500 and APPLE by subtracting their monthly return by the risk-free (treasury bill) monthly return.
monthly.risk.premium <- data.frame(monthly.return$SP500 - Tbill.monthly.return, monthly.return$APPLE - Tbill.monthly.return)
colnames(monthly.risk.premium) <- colnames(price)

# Use lm() to derive y-intercept and slope. 
index.model.APPLE <- lm(APPLE~SP500, data = monthly.risk.premium)

# Visualise the data using ggplot2 
p <- (
(ggplot(monthly.risk.premium, aes(SP500, APPLE))+
  geom_point(col='#014d64')+
  labs(title = 'A monolithic bite.',
       subtitle= "Apple's beta is less than 1.\nRisk premium regression model.",
       caption='Source: US Federal Reserve and Yahoo Finance.'
       )+
  labs(x='S&P 500', y='APPL')+
  theme_economist()+
  geom_abline(intercept=index.model.APPLE$coefficients[1], slope=index.model.APPLE$coefficients[2],
              lty=2,lwd=1.2,color='#56B4E9', xlim(-0.2,0.1))
    )+
theme(plot.title = element_text(family = 'Avenir Next Condensed',size=18,margin=margin(-7,0,10,0)),
    plot.subtitle = element_text(family = 'Avenir Next Condensed',size=12, margin=margin(-2,0,3,0)),
    plot.caption = element_text(family = 'Avenir Next Condensed', size=9, hjust = 0,margin=margin(34,0,-45,0)),  
    axis.text = element_text(face='bold'),
    axis.title.x = element_text(family = 'Avenir Next Condensed',face = 'bold', margin=margin(12,0,-12,0)),
    axis.title.y = element_text(family = 'Avenir Next Condensed',face = 'bold', margin=margin(0,12,0,0)),
    plot.margin = margin(1,1.5,2,1, 'cm'))
  +ylim(-0.5,0.35)
)
p<-p+geom_segment(aes(x=0,xend=0,y=-Inf,yend=0.1975),color='#e5001f',lwd=0.4)+
  geom_segment(aes(x=0,xend=0,y=-0.1975,yend=0.1975),color='#e5001f',lwd=1.1)

# Save the graph to designated file path. 
svg('/Your/File/Name/your.svg',width = 6, height = 5)
p
# Add the iconic Economist red rectangle to the graph!
grid.rect(x=0.02, y=0.9,hjust = 1,vjust=0,gp=gpar(fill='#e5001c',lwd=0))
dev.off()
