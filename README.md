# Customer Clustering
This Shiny app is designed to simulate how a hypothetical company might analyze
its customers based on their similarities.

You can see the live app
[On my ShinyApp.io account](https://raffled.shinyapps.io/customer_clustering/).
Refer to the Shiny documentation for help with running the application.

## Background
This app is designed to show how clustering techniques can be used by an
organization to examine similarity across their customers. The primary
motivation is to group customers by behavior to streamline initial pricing
processes, but it also includes tools to examine purchasing patterns of
customers based on the types of products they buy (Product Class) and the
organization's market position in terms of competition.

## Context
In this scenario, the organization has several dozen customers, each buying some
assortment of their hundreds of products. The products each belong to one of
12 classes  Product pricing is primarily driven by customer size in terms of a
discount off of the list price and rebates based on their throughput.  Since the
top 7 customers make up over 75% of the organization's profits, they are
interested in grouping their smaller customers to streamline pricing processes.

Also of interest is a tool to allow the organization to compare customers in
terms of the Classes and Exclusivity of the products they buy, potentially
identifying areas of risk and opportunity for each of the customers.

## Components/Tools
### All Customers
The All Customers section provides a high-level overview of all of the
organizations customers, including:

- A heatmap of all customers, allowing for a quick identification of customer
    grouping using Hierarchical Clustering on the main four customer attributes
- A 3D Scatterplot showing the distribution of the customers according to how
    they cluster using K-Medians clustering (with the Top 7 grouped separately),
    along with a summary table of the groups
- Sections for comparing customers to  each other and the organizations overall
    distribution of Product Classes and Exclusivity.  These sections include a
    visuals of the organizations aggregate distribution, heatmaps of the
    customers distributions, and a tool for comparing each customer to the
    aggregate distribution.

### Small Customers
Focusing on the smaller customers, this section provides:

- A similar heatmap to the one in All Customers
- A 3D Scatterplot of just the smaller customers, including bloxplots detailing
    the differences in the clusters, group summaries, and details of the top
    customers and products within each group.

### Product Views
A 3D Scatterplot of the organizations portfolio, grouped by Therapy Area. A wide
range of variables are available for the axes.
