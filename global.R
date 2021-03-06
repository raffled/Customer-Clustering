#### global.R

### packages
require(shiny)
require(shinydashboard)
require(dplyr)
require(tidyr)
require(cluster)
require(d3heatmap)
require(plotly)
require(amap)
require(DT)
require(gplots)
require(RColorBrewer)
require(scales)

#### load and clean data
prices <- read.csv("./data/Customer_Prices.csv")

############ Process data for clustering & plotting
#### get summary stats for customers

#### grab medians
cust.num <- prices %>%
    filter(complete.cases(Net.WAC.Percent)) %>%
    aggregate(cbind(Rebates, Net.WAC.Percent) ~ Customer.Group,
              data = .,
              FUN = median, na.rm = TRUE)

#### grab high level info from summaries
cust.totals <- read.csv("./data/Customer_Summary.csv")

#### merge data sets

customer.df <- merge(cust.num,
                     cust.totals,
                     by = "Customer.Group",
                     all.x = TRUE) %>%
    filter(complete.cases(Percent.GM))

#### filter out Top 7 and Oncology Supply (outlier)
cust.sub <- customer.df %>%
    filter(Percent.GM < 0.03) %>%
    filter(Customer.Group != "Shawn")
customer.vec <- unique(as.character(customer.df$Customer.Group))

#### get matrix suitable for clustering
customer.mat <- cust.sub %>%
    select(Net.WAC.Percent, Rebates, Percent.Doses, Percent.GM) %>%
    as.matrix
rownames(customer.mat) <- cust.sub[, 1]


xyz.choices <- list("List %" = "Net.WAC.Percent",
                    "Rebate %" = "Rebates",
                    "% of Vol." = "Percent.Doses",
                    "% of Gross" = "Percent.GM")

#### now subset to just top 7
cust.sub <- customer.df %>%
    filter(Percent.GM < 0.03) %>%
    filter(Customer.Group != "Shawn")

#### get matrix suitable for clustering
top7.sub <- customer.df %>%
    filter(Percent.GM > 0.03)
top7.mat <- top7.sub %>%
    select(Net.WAC.Percent, Rebates, Percent.Doses, Percent.GM) %>%
    as.matrix
rownames(top7.mat) <- top7.sub[, 1]

col.labs <- c("List %", "Rebate %", "% of Volume", "% of Revenue")
names(col.labs) <- colnames(customer.mat)

#### Grab more product info from summaries, POAM, etc
## get product-family level summaries and merge w/ customer level info we
## started with.  Also includes some IMS info.
prod.df <- read.csv("./data/Product_Info.csv")
prices$Product.Family <- as.character(prices$Product.Family)
prod.df$Product.Family <- as.character(prod.df$Product.Family)
prices.agg <- prices %>%
    filter(Rebates < 1) %>%
    aggregate(cbind(Rebates, Net.WAC.Percent) ~ Customer.Group + Product.Family,
                    data = ., FUN = median)
product.df <- merge(x = prices.agg,
                    y = select(prod.df, Product.Family,
                               Therapy.Area, Exclusivity, Simplified.Form,
                               Gx.Competitors, Mylan.MS, Mkt.Size),
                    by = "Product.Family",
                    all.x = TRUE) %>%
    mutate(Mylan.Doses = round(Mylan.MS * Mkt.Size))


## grab more detailed customer/family level info (doses, GM)
cust.prod.df <- read.csv("./data/Customer_Product.csv", stringsAsFactors = FALSE)
product.df <- merge(x = product.df,
                    y = cust.prod.df,
                    by = c("Customer.Group", "Product.Family"),
                    all.x = TRUE)

#### summarise by product family for plotting
## means where it makes sense.  For family level values, keeps the same.
product.medians <- product.df %>%
    aggregate(cbind(Rebates, Net.WAC.Percent,
                    Mylan.MS, Mkt.Size, Mylan.Doses) ~
                  Product.Family + Therapy.Area + Exclusivity,
              data = ., FUN = median)
## sum of doses & GM to calculate total GM % and % of GM
product.totals <- product.df %>%
    aggregate(cbind(Net.Sales, Gross.Margin) ~ Product.Family + Therapy.Area + Exclusivity,
              data = ., FUN = sum)
product.summary <- merge(product.medians,
                         product.totals,
                         by = c("Product.Family", "Therapy.Area", "Exclusivity"))

## compute some extra vars
lev <- rev(c("Exclusive", "Semi-Exclusive", "Commodity"))
product.summary <- product.summary %>%
    mutate(GM.Percent = Gross.Margin / Net.Sales,
           Percent.GM = Gross.Margin / sum(Gross.Margin),
           Exclusivity = factor(Exclusivity,
                                levels = lev,
                                labels = lev,
                                ordered = TRUE))
prod.col.labs <- c("Product Family", "Product Class", "Exclusivity", "Rebate",
                   "List %", "Market Share", "Size", "Volume",
                   "Net $", "Gross $", "Gross%", "% of Gross")
names(prod.col.labs) <- colnames(product.summary)

xyz.prod <- as.list(colnames(product.summary))
names(xyz.prod) <- prod.col.labs

get.paired <- function(){
    pal <- brewer.pal(12, "Paired")#[c(seq(1, 11, 2), seq(2, 12, 2))]
    pal[11] <- col2hex("gold1")
    pal
}

product.vec <- unique(as.character(product.df$Product.Family))
