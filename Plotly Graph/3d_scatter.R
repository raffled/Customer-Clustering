######## Script to create 3d scatter to host on plotly so I can have it as a
######## stand-alone sample on my portfolio
#### libraries
library(dplyr)
library(tidyr)
library(plotly)
library(cluster)
library(plotly)
library(amap)

#### load and clean data
prices <- read.csv("../data/Customer_Prices.csv")

############ Process data for clustering & plotting
#### get summary stats for customers

#### grab medians
cust.num <- prices %>%
    filter(complete.cases(Net.WAC.Percent)) %>%
    aggregate(cbind(Rebates, Net.WAC.Percent) ~ Customer.Group,
              data = .,
              FUN = median, na.rm = TRUE)

#### grab high level info from summaries
cust.totals <- read.csv("../data/Customer_Summary.csv")

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

head(customer.mat)

#### cluster
input <- list(k = 6)
dendro <- hclust(daisy(customer.mat, metric = "manhattan"))
dendro.cut <- cutree(dendro, input$k)
km.obj <- customer.mat %>%
    as.data.frame %>%
    mutate(Initial.Group = dendro.cut) %>%
    aggregate(cbind(Net.WAC.Percent,
                    Rebates,
                    Percent.Doses,
                    Percent.GM) ~
                  Initial.Group,
              data = ., FUN = median) %>%
    select(-Initial.Group) %>%
    Kmeans(customer.mat, centers = ., method = "manhattan")

#### little bit of kludging to re-use as much shiny code as possible
xyz.choices <- list("List %" = "Net.WAC.Percent",
                    "Rebate %" = "Rebates",
                    "% of Vol." = "Percent.Doses",
                    "% of Gross" = "Percent.GM")

input <- append(
    input,
    list(
        x = "Net.WAC.Percent",
        y = "Percent.GM",
        z = "Rebates"
    )
)

plot.df <- data.frame(x = customer.mat[, input$x]*100,
                      y = customer.mat[, input$y]*100,
                      z = customer.mat[, input$z]*100,
                      Group = factor(km.obj$cluster))

p <- plot_ly(
    plot.df,
    x = ~x,
    y = ~y,
    z = ~z,
    color = ~Group,
    ## color = pal[km$cluster],
    mode = "markers",
    text = rownames(customer.mat),
    type = "scatter3d",
    opacity = .7,
    source = "small_scatter"
) %>%
    layout(
        scene = list(
            xaxis = list(title = col.labs[input$x]),
            yaxis = list(title = col.labs[input$y]),
            zaxis = list(title = col.labs[input$z]),
            aspectratio = list(x = 1, y = 1, z = 1)
        )
    )
p
#### Plotly authentication code (values not shown)
## Sys.setenv("plotly_username"="")
## Sys.setenv("plotly_api_key"="")
plotly_POST(p, filename = "Customer-3d-Scatter")
