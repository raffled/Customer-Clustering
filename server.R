#### server.R
shinyServer(
    function(input, output, session){
        ## make heatmap
        this.dist <- function(x) daisy(x, metric = "manhattan")
        hm.pal <- rev(brewer.pal(11, "Spectral"))[1:10]
        output$cust.heatmap <- renderD3heatmap(
            d3heatmap(
                t(customer.mat)*100,
                scale = "none",
                k_col = input$k,
                anim_duration = 500,
                cexCol = 1,
                cexRow = 1,
                xaxis_height = 150,
                yaxis_width = 110,
                dendrogram = "col",
                labRow = col.labs,
                distfun = this.dist,
                colors = hm.pal
            )
        )
        ## reactive fxns for fitting k-means
        ## fit k-means
        km.obj <- reactive({
            dendro <- hclust(daisy(customer.mat, metric = "manhattan"))
            dendro.cut <- cutree(dendro, input$k)
            customer.mat %>%
                as.data.frame %>%
                mutate(Initial.Group = dendro.cut) %>%
                aggregate(cbind(Net.WAC.Percent, Rebates, Percent.Doses, Percent.GM) ~
                              Initial.Group,
                          data = ., FUN = median) %>%
                select(-Initial.Group) %>%
                Kmeans(customer.mat, centers = ., method = "manhattan")
        })


        mediod.df <- reactive(
            data.frame(
                Group = 1:input$k,
                x = km.obj()$centers[, input$x]*100,
                y = km.obj()$centers[, input$y]*100,
                z = km.obj()$centers[, input$z]*100
            )
        )



        ## reactively create data.frame to plot
        plot.df <- reactive({
            data.frame(x = customer.mat[, input$x]*100,
                       y = customer.mat[, input$y]*100,
                       z = customer.mat[, input$z]*100,
                       Group = factor(km.obj()$cluster))
        })


        ## make scatterplot
        output$small.scatter <- renderPlotly({
            plot_ly(
                plot.df(),
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
        })

        ## create combined DF for all customers
        all.scatter.df <- reactive({
            small.df <- data.frame(Customer.Group = rownames(customer.mat),
                Group = km.obj()$cluster,
                x = customer.mat[, input$all.x]*100,
                y = customer.mat[, input$all.y]*100,
                z = customer.mat[, input$all.z]*100
            )

            rownames(small.df) <- NULL
            top7.df <- top7.sub[, c("Customer.Group",
                                    input$all.x,
                                    input$all.y,
                                    input$all.z)] %>%
                mutate(Group = "Top 7")
            top7.df[2:4] <- top7.df[2:4]*100
            colnames(top7.df)[2:4] <- letters[24:26]
            top7.df <- top7.df %>%
                select(Customer.Group, Group, x, y, z)
            rbind(small.df, top7.df)
        })

        ## create heatmap for all customers
        ## color function
        output$all.heatmap <- renderD3heatmap({
            df <- customer.df %>%
                select(Customer.Group,
                       Net.WAC.Percent,
                       Rebates,
                       Percent.Doses,
                       Percent.GM)
            rownames(df) <- df$Customer.Group
            df <- df[, -(which(colnames(df) == "Customer.Group"))]
            d3heatmap(
                t(df) * 100,
                scale = "none",
                k_col = input$k + 1,
                anim_duration = 500,
                cexCol = 0.8,
                cexRow = 0.8,
                xaxis_height = 150,
                yaxis_width = 110,
                dendrogram = "col",
                labRow = col.labs,
                distfun = this.dist,
                colors = hm.pal
            )
        })

        myl.ta.agg.df <- product.df %>%
            aggregate(cbind(Doses, Gross.Margin) ~ Therapy.Area,
                      data = ., FUN = sum) %>%
            merge(x = .,
                  y = product.df %>%
                      aggregate(Product.Family ~ Therapy.Area,
                                data = .,
                                FUN = function(x) length(unique(as.character(x)))),
                  by = "Therapy.Area") %>%
            mutate(Therapy.Area = factor(Therapy.Area)) %>%
            arrange(desc(Therapy.Area)) %>%
            mutate(Color = get.paired())

        output$all.ta.bubble <- renderPlotly({
            plot_ly(
                ## quick hack because plotly breaks w/ single value color groups.
                rbind(myl.ta.agg.df, myl.ta.agg.df),
                x = ~Product.Family,
                y = ~Doses,
                color = ~Therapy.Area,
                text = ~Therapy.Area,
                type = "scatter",
                mode = "markers",
                size = ~Gross.Margin,
                opacity = 0.75,
                marker = list(symbol = 'circle', sizemode = 'diameter',
                              line = list(width = 2, color = '#FFFFFF'))
            ) %>%
                layout(
                    xaxis = list(title = "Number of Product Families"),
                    yaxis = list(title = "Total Volume"),
                    title = "Corporation Product Class Bubblechart",
                    markers = list(colors = ~Color),
                    showlegend = FALSE
                )
        })

        output$all.ta.bars <- output$mylan.ta.bars <- renderPlotly({
            agg.percent <- myl.ta.agg.df %>%
                mutate(Doses = Doses / sum(Doses),
                       GM = Gross.Margin / sum(Gross.Margin),
                       N.Products = Product.Family / sum(Product.Family)) %>%
                select(-Gross.Margin, -Product.Family) %>%
                gather(Metric, Percent, Doses, GM, N.Products) %>%
                mutate(Metric = gsub("N.Products", "N. Prods", Metric)) %>%
                mutate(Metric = gsub("GM", "Revenue", Metric)) %>%
                mutate(Metric = gsub("Doses", "Volume", Metric)) %>%
                mutate(Percent = Percent * 100)
            plot_ly(
                agg.percent,
                x = ~Metric,
                y = ~Percent,
                type = "bar",
                color = ~Therapy.Area,
                opacity = 0.75
            ) %>%
                layout(
                    markers = list(colors = ~Color),
                    title = "Corporation Product Class Distributions"
                )
        })

        col.lookup <- myl.ta.agg.df[, c("Therapy.Area", "Color")]
        ## subset and aggregate product df to selected customers on TA tabs
        ta.selected.df <- reactive({
            if(!is.null(input$ta.cust.select)){
                ta.selected.df <- product.df %>%
                    filter(Customer.Group %in% input$ta.cust.select)
            } else {
                ta.selected.df <- product.df
            }
            ta.selected.df <- ta.selected.df %>%
                aggregate(cbind(Doses, Gross.Margin) ~ Therapy.Area,
                          data = ., FUN = sum) %>%
                merge(x = .,
                      y = ta.selected.df %>%
                          aggregate(Product.Family ~ Therapy.Area,
                                    data = .,
                                    FUN = function(x) length(unique(as.character(x)))),
                      by = "Therapy.Area")
            df <- myl.ta.agg.df %>%
                select(-Doses, - Gross.Margin, -Product.Family) %>%
                merge(x = ., y = ta.selected.df, by = "Therapy.Area",
                      all.x = TRUE) %>%
            arrange(desc(Therapy.Area))
            df[is.na(df)] <- 0
            df
        })

        output$selected.ta.bars <- renderPlotly({
            agg.percent <- ta.selected.df() %>%
                mutate(Doses = Doses / sum(Doses),
                       GM = Gross.Margin / sum(Gross.Margin),
                       N.Products = Product.Family / sum(Product.Family)) %>%
                select(-Gross.Margin, -Product.Family) %>%
                gather(Metric, Percent, Doses, GM, N.Products) %>%
                mutate(Metric = gsub("N.Products", "N. Prods", Metric)) %>%
                mutate(Metric = gsub("GM", "Revenue", Metric)) %>%
                mutate(Metric = gsub("Doses", "Volume", Metric)) %>%
                mutate(Percent = Percent * 100)
            plot_ly(
                agg.percent,
                x = ~Metric,
                y = ~Percent,
                type = "bar",
                color = ~Therapy.Area,
                opacity = 0.75
            ) %>%
                layout(
                    markers = list(colors = ~Color),
                    title = "Selected Customer(s) Product Distributions"
                )
        })

        ## put together table of top TA prods for compared customer
        cust.compare.ta.df <- reactive({
            if(!is.null(input$ta.cust.select)){
                ta.selected.df <- product.df %>%
                    filter(Customer.Group %in% input$ta.cust.select)
            } else {
                ta.selected.df <- product.df
            }
            ta.selected.df <- ta.selected.df %>%
                aggregate(cbind(Doses, Gross.Margin) ~ Therapy.Area + Product.Family,
                          data = ., FUN = sum) %>%
                merge(x = .,
                      y = ta.selected.df %>%
                          aggregate(Net.WAC.Percent ~ Product.Family,
                                    data = ., FUN = median),
                      all.x = TRUE) %>%
                merge(x = .,
                      y = product.df %>%
                          select(Product.Family, Exclusivity) %>%
                          distinct(),
                      all.x = TRUE)
            df <- ta.selected.df %>%
                mutate(Percent.Doses = round(Doses / sum(Doses) * 100, 2),
                       Percent.GM = round(Gross.Margin / sum(Gross.Margin) * 100, 2),
                       Doses = round(Doses * 1e-6, 2),
                       Gross.Margin = round(Gross.Margin * 1e-6, 2),
                       Net.WAC.Percent = round(Net.WAC.Percent * 100, 2)) %>%
                arrange(desc(Gross.Margin)) %>%
                select(Product.Family, Therapy.Area, Exclusivity, Doses, Percent.Doses,
                       Gross.Margin, Percent.GM, Net.WAC.Percent)
            colnames(df) <- c("Product", "Class", "Exclusivity", "Vol. (mm)",
                              "% Vol.", "Rev. (mm)", "% Rev.", "Price (% List)")
            df
        })

        output$cust.compare.ta.dt <- renderDataTable({
            datatable(
                cust.compare.ta.df()
            )
        })

        cust.compare.ta.summary.df <- reactive({
            c.names <- colnames(cust.compare.ta.df())
            cust.compare.ta.df <- data.frame(cust.compare.ta.df())
            df <- cust.compare.ta.df %>%
                aggregate(cbind(Vol...mm., X..Vol., Rev...mm., X..Rev.)  ~ Class,
                          data = ., FUN = sum) %>%
                arrange(desc(Class))
            colnames(df) <- c.names[c(2, 4:7)]
            df
        })

        output$cust.compare.ta.summary.dt <- renderDataTable({
            datatable(
                cust.compare.ta.summary.df(),
                rownames = FALSE,
                options = list(
                    dom = "t"
                )
            )
        })

        metric.list <- c("Doses", "GM", "Product.Family")
        names(metric.list) <- c("Doses", "Gross.Margin", "Product.Family")
        product.df$Doses <- as.numeric(product.df$Doses)
        render.cust.ta.heatmap <- function(metric){
            titl <- metric.list[[metric]]
            df <- product.df[, c("Customer.Group", "Therapy.Area", metric)]
            colnames(df)[3] <- "metric"
            cust.vec <- unique(as.character(df$Customer.Group))
            if(metric == "Product.Family"){
                df <- df %>%
                    aggregate(metric ~ Customer.Group + Therapy.Area,
                              data = ., FUN = function(x)
                                  length(unique(as.character(x))))
                df <- do.call(rbind, lapply(cust.vec, function(cust){
                    df %>%
                        filter(Customer.Group == cust) %>%
                        mutate(metric = (metric / sum(metric, na.rm =
                                                                  TRUE))*100)
                }))
            } else {
                df <- do.call(rbind, lapply(cust.vec, function(cust){
                    df %>%
                        filter(Customer.Group == cust) %>%
                        aggregate(metric ~ Customer.Group + Therapy.Area,
                                  data = ., FUN = sum) %>%
                        mutate(metric = (metric / sum(metric, na.rm = TRUE)) * 100)
                }))

            }
            ta.df <- df %>%
                spread(Therapy.Area, metric)
            rownames(ta.df) <- ta.df$Customer.Group
            ta.mat <- as.matrix(ta.df %>% select(-Customer.Group))
            ta.mat[is.na(ta.mat)] <- 0
            ta.mat[ta.mat < 0] <- 0
            d3heatmap(
                t(ta.mat),
                scale = "none",
                k_col = 9,
                anim_duration = 500,
                cexCol = 0.8,
                cexRow = 0.8,
                xaxis_height = 150,
                yaxis_width = 150,
                dendrogram = "col",
                distfun = this.dist,
                colors = hm.pal
            )
        }
        output$all.ta.doses.hm <- renderD3heatmap(render.cust.ta.heatmap("Doses"))
        output$all.ta.gm.hm <- renderD3heatmap(render.cust.ta.heatmap("Gross.Margin"))
        output$all.ta.prod.hm <- renderD3heatmap(render.cust.ta.heatmap("Product.Family"))

        ## look at exclusivity overall
        myl.excl.agg.df <- product.df %>%
            aggregate(cbind(Doses, Gross.Margin) ~ Exclusivity,
                      data = ., FUN = sum) %>%
            merge(x = .,
                  y = product.df %>%
                      aggregate(Product.Family ~ Exclusivity,
                                data = .,
                                FUN = function(x) length(unique(as.character(x)))),
                  by = "Exclusivity") %>%
            mutate(Exclusivity = factor(Exclusivity, levels = lev)) %>%
            '['(c(2, 3, 1), ) %>%
            mutate(Color = brewer.pal(3, "Set2"))

        output$all.excl.bars <- output$mylan.excl.bars <- renderPlotly({
            agg.percent <- myl.excl.agg.df %>%
                mutate(Doses = Doses / sum(Doses),
                       GM = Gross.Margin / sum(Gross.Margin),
                       N.Products = Product.Family / sum(Product.Family)) %>%
                select(-Gross.Margin, -Product.Family) %>%
                gather(Metric, Percent, Doses, GM, N.Products) %>%
                mutate(Metric = gsub("N.Products", "N. Prods", Metric)) %>%
                mutate(Metric = gsub("Doses", "Vol.", Metric)) %>%
                mutate(Metric = gsub("GM", "Revenue", Metric)) %>%
                mutate(Percent = Percent * 100)
            plot_ly(
                agg.percent,
                x = ~Metric,
                y = ~Percent,
                type = "bar",
                color = ~Exclusivity,
                opacity = 0.75
            ) %>%
                layout(
                    markers = list(colors = ~Color),
                    title = "Corporation Exclusivity Distributions"
                )
        })
        output$excl.summary.df <- renderDataTable({
            df <- myl.excl.agg.df %>%
                select(-Color) %>%
                mutate(Doses = round(Doses / 1e6, 2),
                       Gross.Margin = round(Gross.Margin / 1e6, 2))
            colnames(df)[2:4] <- c("Volume", "Revenue", "Products")
            datatable(
                df,
                rownames = FALSE,
                options = list(
                    dom = "t"
                )
            )
        })

        ## exclusivity heatmaps
        render.cust.excl.heatmap <- function(metric){
            titl <- metric.list[[metric]]
            df <- product.df[, c("Customer.Group", "Exclusivity", metric)]
            colnames(df)[3] <- "metric"
            cust.vec <- unique(as.character(df$Customer.Group))
            if(metric == "Product.Family"){
                df <- df %>%
                    aggregate(metric ~ Customer.Group + Exclusivity,
                              data = ., FUN = function(x)
                                  length(unique(as.character(x))))
                df <- do.call(rbind, lapply(cust.vec, function(cust){
                    df %>%
                        filter(Customer.Group == cust) %>%
                        mutate(metric = (metric / sum(metric, na.rm = TRUE))*100)
                }))
            } else {
                df <- do.call(rbind, lapply(cust.vec, function(cust){
                    df %>%
                        filter(Customer.Group == cust) %>%
                        aggregate(metric ~ Customer.Group + Exclusivity,
                                  data = ., FUN = sum) %>%
                        mutate(metric = (metric / sum(metric, na.rm = TRUE)) * 100)
                }))

            }
            excl.df <- df %>%
                spread(Exclusivity, metric) %>%
                select(1, 2, 4, 3)
            rownames(excl.df) <- excl.df$Customer.Group
            excl.mat <- as.matrix(excl.df %>% select(-Customer.Group))
            excl.mat[is.na(excl.mat)] <- 0
            d3heatmap(
                t(excl.mat),
                scale = "none",
                anim_duration = 500,
                cexCol = 0.8,
                cexRow = 0.8,
                xaxis_height = 150,
                yaxis_width = 150,
                dendrogram = "col",
                distfun = this.dist,
                colors = hm.pal
            )
        }
        output$all.excl.doses.hm <- renderD3heatmap(render.cust.excl.heatmap("Doses"))
        output$all.excl.gm.hm <- renderD3heatmap(render.cust.excl.heatmap("Gross.Margin"))
        output$all.excl.prod.hm <-
            renderD3heatmap(render.cust.excl.heatmap("Product.Family"))

        ## subset and aggregate product df to selected customers on TA tabs
        excl.selected.df <- reactive({
            if(!is.null(input$exclusivity.cust.select)){
                excl.selected.df <- product.df %>%
                    filter(Customer.Group %in% input$exclusivity.cust.select)
            } else {
                excl.selected.df <- product.df
            }
            excl.selected.df <- excl.selected.df %>%
                aggregate(cbind(Doses, Gross.Margin) ~ Exclusivity,
                          data = ., FUN = sum) %>%
                merge(x = .,
                      y = excl.selected.df %>%
                          aggregate(Product.Family ~ Exclusivity,
                                    data = .,
                                    FUN = function(x) length(unique(as.character(x)))),
                      by = "Exclusivity")
            df <- myl.excl.agg.df %>%
                select(-Doses, - Gross.Margin, -Product.Family) %>%
                merge(x = ., y = excl.selected.df, by = "Exclusivity",
                      all.x = TRUE)
            rownames(df) <- df$Exclusivity
            df[is.na(df)] <- 0
            df[c("Exclusive", "Semi-Exclusive", "Commodity"),]
        })

        output$selected.excl.bars <- renderPlotly({
            agg.percent <- excl.selected.df() %>%
                mutate(Doses = Doses / sum(Doses),
                       GM = Gross.Margin / sum(Gross.Margin),
                       N.Products = Product.Family / sum(Product.Family)) %>%
                select(-Gross.Margin, -Product.Family) %>%
                gather(Metric, Percent, Doses, GM, N.Products) %>%
                mutate(Metric = gsub("N.Products", "N. Prods", Metric)) %>%
                mutate(Metric = gsub("Doses", "Vol.", Metric)) %>%
                mutate(Metric = gsub("GM", "Revenue", Metric)) %>%
                mutate(Percent = Percent * 100)
            plot_ly(
                agg.percent,
                x = ~Metric,
                y = ~Percent,
                type = "bar",
                color = ~Exclusivity,
                opacity = 0.75
            ) %>%
                layout(
                    markers = list(colors = ~Color),
                    title = "Selected Corporation(s) Exclusivity Distributions"
                )
        })

        cust.compare.excl.df <- reactive({
            if(!is.null(input$exclusivity.cust.select)){
                excl.selected.df <- product.df %>%
                    filter(Customer.Group %in% input$exclusivity.cust.select)
            } else {
                excl.selected.df <- product.df
            }
            excl.selected.df <- excl.selected.df %>%
                aggregate(cbind(Doses, Gross.Margin) ~ Therapy.Area + Product.Family,
                          data = ., FUN = sum) %>%
                merge(x = .,
                      y = excl.selected.df %>%
                          aggregate(Net.WAC.Percent ~ Product.Family,
                                    data = ., FUN = median),
                      all.x = TRUE) %>%
                merge(x = .,
                      y = product.df %>%
                          select(Product.Family, Exclusivity) %>%
                          distinct(),
                      all.x = TRUE)
            df <- excl.selected.df %>%
                mutate(Percent.Doses = round(Doses / sum(Doses) * 100, 2),
                       Percent.GM = round(Gross.Margin / sum(Gross.Margin) * 100, 2),
                       Doses = round(Doses * 1e-6, 2),
                       Gross.Margin = round(Gross.Margin * 1e-6, 2),
                       Net.WAC.Percent = round(Net.WAC.Percent * 100, 2)) %>%
                arrange(desc(Gross.Margin)) %>%
                select(Product.Family, Therapy.Area, Exclusivity, Doses, Percent.Doses,
                       Gross.Margin, Percent.GM, Net.WAC.Percent)
            colnames(df) <- c("Product Family", "Class", "Exclusivity", "Vol. (mm)",
                              "% Vol.", "Rev. (mm)", "% Rev.", "Price (% List)")
            df
        })

        output$cust.compare.excl.dt <- renderDataTable({
            datatable(
                cust.compare.excl.df(),
                rownames = FALSE
            )
        })

        cust.compare.excl.summary.df <- reactive({
            c.names <- colnames(cust.compare.excl.df())
            cust.compare.excl.df <- data.frame(cust.compare.excl.df())
            df <- cust.compare.excl.df %>%
                aggregate(cbind(Vol...mm., X..Vol., Rev...mm., X..Rev.)  ~ Exclusivity,
                          data = ., FUN = sum)
            colnames(df)[-1] <- c.names[4:7]
            df
        })
        output$cust.compare.excl.summary.dt <- renderDataTable({
            datatable(
                cust.compare.excl.summary.df(),
                rownames = FALSE,
                options = list(
                    dom = "t"
                )
            )
        })


        ## plot all customers on same plot
        output$all.scatter <- renderPlotly({
            plot_ly(
                all.scatter.df(),
                x = ~x,
                y = ~y,
                z = ~z,
                color = ~Group,
                mode = "markers",
                text = ~Customer.Group,
                type = "scatter3d",
                opacity = .7
            ) %>%
                layout(
                    scene = list(
                        xaxis = list(title = col.labs[input$all.x]),
                        yaxis = list(title = col.labs[input$all.y]),
                        zaxis = list(title = col.labs[input$all.z]),
                        aspectratio = list(x = 1, y = 1, z = 1)
                    )
                )
        })

        ## create product plot df and scatter
        prod.plot.df <- reactive({
            dat <- data.frame(x = product.summary[, input$prod.x],
                              y = product.summary[, input$prod.y],
                              z = product.summary[, input$prod.z],
                              Therapy.Area = product.summary$Therapy.Area,
                              Product.Family = product.summary$Product.Family) %>%
                mutate(Text = paste(Product.Family, Therapy.Area, sep = "<br>"))
            args <- c(input$prod.x, input$prod.y, input$prod.z)
            if("Exclusivity" %in% args){
                dat[, which(args == "Exclusivity")] <- factor(
                    as.character(dat[, which(args == "Exclusivity")]),
                    levels = lev,
                    labels = 1:3,
                    ordered = TRUE
                )
                dat[, which(args != "Exclusivity")] <- dat[, which(args != "Exclusivity")]*100
            } else {
                dat <- dat*100
            }
            dat
        })

        output$prod.scatter <- renderPlotly({
            plot_ly(
                prod.plot.df(),
                x = ~x,
                y = ~y,
                z = ~z,
                color = ~Therapy.Area,
                mode = "markers",
                text = ~Text,
                type = "scatter3d",
                opacity = .7,
                colors = get.paired(),
                source = "prod_scatter"
            ) %>%
                layout(
                    scene = list(
                        xaxis = list(title = prod.col.labs[input$prod.x]),
                        yaxis = list(title = prod.col.labs[input$prod.y]),
                        zaxis = list(title = prod.col.labs[input$prod.z]),
                        aspectratio = list(x = 1, y = 1, z = 1)
                    )
                )
        })

        ## data frame of customer values w/ the group they're in
        cust.df <- reactive({
            cbind(as.data.frame(customer.mat*100),
                  data.frame(Group = factor(km.obj()$cluster)))
        })

        ## get summary of all customers by group
        all.summary <- reactive({
            small.df <- cbind(cust.sub,
                              data.frame(Group = km.obj()$cluster)) %>%
                mutate(Customer = Customer.Group) %>%
                select(Group, Customer,
                       Percent.Doses, Percent.GM,
                       Rebates, Net.WAC.Percent,
                       Doses, GM)

            top7.df <- cbind(top7.sub,
                             data.frame(Group = "Top 7")) %>%
                mutate(Customer = Customer.Group,
                       Group = "Top 7") %>%
                select(Group, Customer,
                       Percent.Doses, Percent.GM,
                       Rebates, Net.WAC.Percent,
                       Doses, GM)
            all.df <- rbind(small.df, top7.df)
            all.percent <- all.df %>%
                aggregate(cbind(Rebates, Net.WAC.Percent) ~ Group,
                          data = ., FUN = median)
            all.sums <- all.df %>%
                aggregate(cbind(Percent.Doses, Percent.GM,
                                Doses, GM) ~ Group,
                          data = ., FUN = sum)
            all.summary <- merge(all.percent,
                                 all.sums,
                                 by = "Group")
            all.summary[2:5] <- round(all.summary[2:5]*100, 1)
            all.summary[6:7] <- round(all.summary[6:7]/1e6, 1)
            colnames(all.summary)[2:7] <- c("Rebate",
                                            "List%",
                                            "%Vol.",
                                            "%Rev.",
                                            "Vol.",
                                            "Rev.")
            all.summary

        })
        output$all.sum.dt <- renderDataTable(
            datatable(
                all.summary(),
                rownames = FALSE,
                options = list(
                    dom = "t"
                )
            )
        )
        ## make boxplots
        output$small.netwac.box <- renderPlotly(
            plot_ly(cust.df(),
                    x = ~Group,
                    color = ~Group,
                    y = ~Net.WAC.Percent,
                    type = "box",
                    showlegend = FALSE) %>%
            layout(xaxis = list(title = "Group"),
                   yaxis = list(title = "List %"))
        )
        output$small.rebate.box <- renderPlotly(
            plot_ly(cust.df(),
                    x = ~Group,
                    color = ~Group,
                    y = ~Rebates,
                    type = "box",
                    showlegend = FALSE) %>%
            layout(xaxis = list(title = "Group"),
                   yaxis = list(title = "Med. Rebate %"))
        )
        output$small.dose.box <- renderPlotly(
            plot_ly(cust.df(),
                    x = ~Group,
                    color = ~Group,
                    y = ~Percent.Doses,
                    type = "box",
                    showlegend = FALSE) %>%
            layout(xaxis = list(title = "Group"),
                   yaxis = list(title = "Percent Doses"))
        )
        output$small.gm.box <- renderPlotly(
            plot_ly(cust.df(),
                    x = ~Group,
                    color = ~Group,
                    y = ~Percent.GM,
                    type = "box",
                    showlegend = FALSE) %>%
            layout(xaxis = list(title = "Group"),
                   yaxis = list(title = "Percent GM"))
        )

        ## pretty tables for tables tab

        ## set up an observer to track number of groups so we can subset the
        ## data set, also add a listener for the clear button
        observe({
            updateSelectizeInput(
                session,
                "pred.selection",
                choices = 1:input$k,
                selected = NULL
            )
            updateSelectizeInput(
                session,
                "all.selection",
                choices = 1:input$k,
                selected = NULL
            )
            updateSelectizeInput(
                session,
                "small.sum.selection",
                choices = 1:input$k,
                selected = NULL
            )
            updateSelectizeInput(
                session,
                "group.prod.select",
                choices = 1:input$k,
                selected = NULL
            )
        })
        clear.cust <- reactive({
            if(!is.null(input$group.prod.select)){
                updateSelectizeInput(
                    session,
                    "cust.prod.select",
                    choices = customer.vec,
                    selected = NULL
                )
            }
        })
        observe(clear.cust())
        clear.prod <- reactive({
            if(!is.null(input$cust.prod.select)){
                updateSelectizeInput(
                    session,
                    "group.prod.select",
                    choices = 1:input$k,
                    selected = NULL
                )
            }
        })
        observe(clear.prod())

        click.cust.list <- reactive({
            click <- event_data("plotly_click",
                                source = "small_scatter")
            if(!is.null(click)){
                cust <- (plot.df() %>%
                    mutate(cust = rownames(plot.df())) %>%
                    filter(round(x, 3) == round(as.numeric(click[["x"]]), 3),
                           round(y, 3) == round(as.numeric(click[["y"]]), 3),
                           round(z, 3) == round(as.numeric(click[["z"]]), 3)) %>%
                    select(cust))[1,1]
                updateSelectizeInput(
                    session,
                    "cust.prod.select",
                    choices = customer.vec,
                    selected = cust
                )
                click <- NULL
            }
        })
        observe(click.cust.list())

        click.prod.list <- reactive({
            click <- event_data("plotly_click",
                                source = "prod_scatter")
            if(!is.null(click)){
                prod <- (prod.plot.df() %>%
                    filter(round(as.numeric(x), 3) == round(as.numeric(click[["x"]]), 3),
                           round(as.numeric(y), 3) == round(as.numeric(click[["y"]]), 3),
                           round(as.numeric(z), 3) == round(as.numeric(click[["z"]]), 3)) %>%
                    select(Product.Family))[,1]
                updateSelectizeInput(
                    session,
                    "prod.select",
                    choices = product.vec,
                    selected = prod
                )
                click <- NULL
            }
        })
        observe(click.prod.list())


        ## data set for customers/group product level summaries on small
        ## customer page.
        group.lookup <- reactive({
            cust.df() %>%
                mutate(Customer.Group = rownames(cust.df())) %>%
                select(Customer.Group, Group)
        })

        cust.select.df <- reactive({
            cust.select.df <- merge(product.df,
                             group.lookup(),
                             by = "Customer.Group",
                             all.x = TRUE) %>%
                mutate(Percent.GM = Gross.Margin / sum(Gross.Margin, na.rm = TRUE),
                       Percent.Doses = Doses / sum(as.numeric(Doses), na.rm = TRUE))
            if(!is.null(input$cust.prod.select)) {
                cust.select.df %>%
                    filter(Customer.Group %in% input$cust.prod.select)

            } else if(!is.null(input$group.prod.select)) {
                cust.select.df %>%
                    filter(Group %in% input$group.prod.select)
            } else {
                cust.select.df
            }
        })

        cust.select.sum.df <- reactive({
            cust.sum <- cust.select.df() %>%
                select(Gross.Margin, Percent.GM, Doses, Percent.Doses) %>%
                colSums
            n.prods <- length(unique(cust.select.df()$Product.Family))
            df <- t(data.frame(c(dollar(cust.sum[1]/1e6),
                                 percent(round(cust.sum[2], 4)),
                                 comma(round(cust.sum[3]/1e6, 2)),
                                 percent(round(cust.sum[4], 4)),
                                 n.prods)))
            colnames(df) <- c("Rev.",
                              "%Rev.",
                              "Vol.",
                              "%Vol.",
                              "Prods")
            rownames(df) <- NULL
            df
        })

        ## render dt for cust.select.sum.df
        output$cust.select.sum.dt <- renderDataTable({
            datatable(
                cust.select.sum.df(),
                options = list(
                    rownames = FALSE,
                    dom = "t",
                    columnDefs = list(
                        list(
                            targets = 0:4,
                            class = "dt-right"
                        )
                    )
                )
            )
        })

        ## summarize TA's for selected group/ customer on small scatter tab
        selected.ta.df <- reactive({
            if(!is.null(input$cust.prod.select)){
                ta.selected.df <- product.df %>%
                    filter(Customer.Group %in% input$cust.prod.select)
            } else if(!is.null(input$group.prod.select)){
                ta.selected.df <- product.df %>%
                    merge(.,
                          group.lookup(),
                          by = "Customer.Group",
                          all.x = TRUE) %>%
                    filter(Group %in% input$group.prod.select)
            } else {
                ta.selected.df <- product.df
            }
            ta.selected.df <- ta.selected.df %>%
                aggregate(cbind(Doses, Gross.Margin) ~ Therapy.Area,
                          data = ., FUN = sum) %>%
                merge(x = .,
                      y = ta.selected.df %>%
                          aggregate(
                              Product.Family ~ Therapy.Area,
                              data = .,
                              FUN = function(x){
                                  length(unique(as.character(x)))
                              }
                          ),
                      by = "Therapy.Area") %>%
                mutate(Gross.Margin = round(Gross.Margin / 1e6, 2),
                       Doses = round(Doses / 1e6, 2)) %>%
                arrange(desc(Gross.Margin))
            colnames(ta.selected.df) <- c("Class", "Vol.", "Rev.", "Prods")
            ta.selected.df
        })
        output$selected.ta.dt <- renderDataTable({
            datatable(
                selected.ta.df(),
                rownames = FALSE,
                options = list(
                    dom = "t"
                )
            )
        })

        ## selected cutomer(s) exclusivity summary
        selected.excl.df <- reactive({
            if(!is.null(input$cust.prod.select)){
                selected.df <- product.df %>%
                    filter(Customer.Group %in% input$cust.prod.select)
            } else if(!is.null(input$group.prod.select)){
                selected.df <- product.df %>%
                    merge(.,
                          group.lookup(),
                          by = "Customer.Group",
                          all.x = TRUE) %>%
                    filter(Group %in% input$group.prod.select)
            } else {
                selected.df <- product.df
            }
            selected.df <- selected.df %>%
                aggregate(cbind(Doses, Gross.Margin) ~ Exclusivity,
                          data = ., FUN = sum) %>%
                merge(x = .,
                      y = selected.df %>%
                          aggregate(
                              Product.Family ~ Exclusivity,
                              data = .,
                              FUN = function(x){
                                  length(unique(as.character(x)))
                              }
                          ),
                      by = "Exclusivity") %>%
                mutate(Gross.Margin = round(Gross.Margin / 1e6, 2),
                       Doses = round(Doses / 1e6, 2)) %>%
                arrange(desc(Gross.Margin))
            colnames(selected.df) <- c("Class", "Vol.", "Rev.", "Prods")
            selected.df
        })
        output$selected.excl.dt <- renderDataTable({
            datatable(
                selected.excl.df(),
                rownames = FALSE,
                options = list(
                    dom = "t"
                )
            )
        })

        ## aggregate products w/in selected groups or customers
        selected.prod.df <- reactive({
            df <- cust.select.df() %>%
                aggregate(cbind(Gross.Margin, Doses) ~
                              Product.Family + Therapy.Area + Exclusivity,
                          data = ., FUN = function(x) sum(x, na.rm = TRUE)) %>%
                select(Product.Family, Gross.Margin, Doses) %>%
                mutate(Gross.Margin = round(Gross.Margin/1e6, 2),
                       Doses = round(Doses/1e6, 2))
            price <- cust.select.df() %>%
                select(Product.Family, Net.WAC.Percent) %>%
                aggregate(Net.WAC.Percent ~ Product.Family,
                          data = ., FUN = median) %>%
                mutate(Net.WAC.Percent = round(Net.WAC.Percent, 2)*100)
            df <- merge(x = df, y = price, by = "Product.Family", all.x = TRUE) %>%
                arrange(desc(Gross.Margin)) %>%
                distinct()
            colnames(df) <- c("Product", "Rev.", "Vol.", "List %")
            rownames(df) <- NULL
            df
        })
        output$selected.prod.dt <- renderDataTable({
            datatable(
                selected.prod.df(),
                rownames = FALSE,
                options = list(
                    pageLength = 5,
                    dom = "t"
                )
            )

        })

        ## product summary info for product view tab
        prod.select.df <- reactive({
            prod.select.df <- product.df %>%
                mutate(Percent.GM = Gross.Margin / sum(Gross.Margin, na.rm = TRUE),
                       Percent.Doses = Doses / sum(as.numeric(Doses), na.rm = TRUE))
            if(!is.null(input$prod.select)) {
                prod.select.df %>%
                    filter(Product.Family %in% input$prod.select)
            } else {
                prod.select.df
            }
        })

        prod.select.sum.df <- reactive({
            prod.sum <- prod.select.df() %>%
                select(Gross.Margin, Percent.GM, Doses, Percent.Doses) %>%
                colSums
            n.cust <- length(unique(prod.select.df()$Customer.Group))
            df <- t(data.frame(c(dollar(prod.sum[1]/1e6),
                                 percent(round(prod.sum[2], 4)),
                                 comma(round(prod.sum[3]/1e6, 2)),
                                 percent(round(prod.sum[4], 4)),
                                 n.cust)))
            df[1,] <- gsub("NaN", "0", df[1,])
            colnames(df) <- c("Rev.",
                              "%Rev.",
                              "Vol.",
                              "%Vol.",
                              "Customers")
            rownames(df) <- NULL
            df
        })

        ## render dt for cust.select.sum.df
        output$prod.select.sum.dt <- renderDataTable({
            datatable(
                prod.select.sum.df(),
                options = list(
                    rownames = FALSE,
                    dom = "t",
                    columnDefs = list(
                        list(
                            targets = 0:4,
                            class = "dt-right"
                        )
                    )
                )
            )
        })

        prod.select.detail.df <- reactive({
            prod.select.detail.df <- prod.select.df() %>%
                aggregate(cbind(Gross.Margin, Doses) ~ Customer.Group,
                          data = ., FUN = sum) %>%
                merge(x = .,
                      y = prod.select.df() %>%
                          aggregate(Net.WAC.Percent ~ Customer.Group,
                                    data = ., FUN = median),
                      by = "Customer.Group",
                      all.x = TRUE) %>%
                select(Customer.Group, Gross.Margin, Doses, Net.WAC.Percent) %>%
                arrange(desc(Gross.Margin)) %>%
                mutate(Gross.Margin = round(Gross.Margin / 1e6, 2),
                       Doses = round(Doses / 1e6, 2),
                       Net.WAC.Percent = round(Net.WAC.Percent*100))
            colnames(prod.select.detail.df) <- c("Customer", "GM", "Doses",
                                                 "%List")
            prod.select.detail.df
        })
        output$prod.select.detail.dt <- renderDataTable({
            datatable(
                prod.select.detail.df(),
                rownames = FALSE,
                options = list(
                    dom = "t"
                )
            )
        })



        ## get selected group summary
        selected.summary.df <- reactive({
            small.df <- cbind(cust.sub,
                              data.frame(Group = km.obj()$cluster)) %>%
                mutate(Customer = Customer.Group) %>%
                select(Group, Customer,
                       Percent.Doses, Percent.GM,
                       Rebates, Net.WAC.Percent,
                       Doses, GM)
            if(!is.null(input$small.sum.selection)){
                small.df <- small.df %>%
                    filter(Group %in% input$small.sum.selection)
            }


            small.percent <- small.df %>%
                aggregate(cbind(Rebates, Net.WAC.Percent) ~ Group,
                          data = ., FUN = median)
            small.sums <- small.df %>%
                aggregate(cbind(Percent.Doses, Percent.GM,
                                Doses, GM) ~ Group,
                          data = ., FUN = sum)
            n.cust <- aggregate(Customer ~ Group,
                                data = small.df,
                                FUN = length)
            this.summary <- merge(small.percent,
                                 small.sums,
                                 by = "Group") %>%
                merge(x = .,
                      y = n.cust,
                      by = "Group")

            this.summary[2:5] <- round(this.summary[2:5]*100, 1)
            this.summary[6:7] <- round(this.summary[6:7]/1e6, 1)
            colnames(this.summary)[2:8] <- c("Rebate %",
                                            "List %",
                                            "% Vol.",
                                            "% Rev.",
                                            "Vol.",
                                            "Rev.",
                                            "N. Cust.")
            if(!is.null(input$small.sum.selection)){
                rownames(this.summary) <- input$small.sum.selection
            } else {
                rownames(this.summary) <- 1:nrow(this.summary)
            }
            rownames(this.summary) <- paste0("G", rownames(this.summary))
            this.summary %>% select(-Group) %>% t
        })
        output$selected.summary.dt <- renderDataTable({
            datatable(
                selected.summary.df(),
                options = list(
                    dom = "t"
                )
            )
        })


    } ## close server fxn
) ## close server
