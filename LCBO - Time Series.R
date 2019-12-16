#Packages
library(readxl)
library(sqldf)
library(tidyr)

#Working Directory
setwd("C:/RAHIM/RESUME/LCBO")

# Read Data
df_full <- read_excel("201904 Category Analyst Dataset Power BI.xlsx")

# Subset time and sales only for time series
df <- df_full[df_full$`Set DESC` == "PREMIXED COCKTAILS",]
df <- subset(df,select=c('Promotion Turn DESC', 'Sales $ Net'))


separate(data = df, col = df$`Promotion Turn DESC`, into = c("Month", "Season"), sep = " ")
