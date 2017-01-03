#### ui.R
dashboardPage(
    ## make a pretty header
    dashboardHeader(
        title = "Customer Clustering"
    ),
    ## make sidebar
    dashboardSidebar(
        ## width = 180,
        hr(),
        sidebarMenu(
            menuItem(
                "All Customers",
                icon = icon("square"),
                menuSubItem(
                    "Heatmap",
                    tabName = "all_heatmap",
                    icon = icon("th")
                ),
                menuSubItem(
                    "3D Scatter Plot",
                    tabName = "all_scatter",
                    icon = icon("codepen")
                ),
                menuSubItem(
                    "Product Classes",
                    tabName = "all_ta",
                    icon = icon("bullseye")
                ),
                menuSubItem(
                    "Competition",
                    tabName = "all_exclusivity",
                    icon = icon("money")
                )
            ),
            menuItem(
                "Small Customers",
                icon = icon("square"),
                menuSubItem(
                    "Heatmap",
                    tabName = "small_heatmap",
                    icon = icon("th")
                ),
                menuSubItem(
                    "3D Scatter Plot",
                    tabName = "small_cluster",
                    icon = icon("codepen"),
                    selected = TRUE
                )
            ),
            menuItem(
                "Product Views",
                icon = icon("square"),
                menuSubItem(
                    "3D Scatter Plot",
                    tabName = "product_cluster",
                    icon = icon("codepen")
                )
            ),
            menuItem(
                "Readme",
                icon = icon("file-text-o"),
                tabName = "overview"
            ),
            hr(),
            numericInput(
                "k",
                label = "Number of Groups",
                value = 6
            )
        ) ## close sidebarmenu
    ), ## close sidebar
    dashboardBody(
        fluidPage(
            tabItems(
                tabItem(
                    tabName = "overview",
                    fluidRow(
                        column(
                            width = 6,
                            box(
                                width = NULL,
                                title = "Motivation",
                                strong("The Application"),
                                p("This app is designed to show how clustering",
                                  "techniques can be used by an organization",
                                  "to examine similarity across their customers.",
                                  "The primary motivation is to group customers",
                                  "by behavior to streamline initial pricing",
                                  "processes, but it also includes tools to",
                                  "examine purchasing patterns of customers",
                                  "based on the types of products they buy",
                                  "(Product Class) and the",
                                  "organization\'s market position in terms",
                                  "of competition."),
                                strong("Context"),
                                p("In this scenario, the organization has",
                                  "several dozen customers, each buying",
                                  "some assortment of their hundreds of",
                                  "products.",
                                  "The products each belong to one of 12 classes",
                                  "Product pricing is primarily driven",
                                  "by customer size in terms of a discount off",
                                  "of the list price and rebates based on",
                                  "their throughput.  Since the top 7 customers",
                                  "make up over 75% of the organization\'s",
                                  "profits,",
                                  "they are interested in grouping their smaller",
                                  "customers to streamline pricing processes."),
                                p("Also of interest is a tool to allow the",
                                  "organization to compare customers in terms",
                                  "of the Classes and Exclusivity of",
                                  "the products they buy, potentially",
                                  "identifying areas of risk and opportunity",
                                  "for each of the customers."),
                                strong("Variables"),
                                p("Customer behavior and attributes are",
                                  "primarily defined in terms of:"),
                                p("- Median Price (% of List Price)"),
                                p("- Median Rebate %"),
                                p("- % of Volume"),
                                p("- % of Revenue")
                            )
                        ),
                        column(
                            width = 6,
                            box(
                                width = NULL,
                                title = "Components/Tools",
                                strong("All Customers"),
                                p("The All Customers section provides a",
                                  "high-level overview of all of the",
                                  "organizations customers, including:"),
                                p("- A heatmap of all customers, allowing for",
                                  "a quick identification of customer grouping",
                                  "using Hierarchical Clustering on the main",
                                  "four customer attributes"),
                                p("- A 3D Scatterplot showing the distribution",
                                  "of the customers according to how they",
                                  "cluster using K-Medians clustering",
                                  "(with the Top 7 grouped separately),",
                                  "along with a summary table of the groups"),
                                p("- Sections for comparing customers to ",
                                  "each other and the organizations overall",
                                  "distribution of Product Classes and",
                                  "Excusivity.  These sections include a ",
                                  "visuals of the organizations aggregate",
                                  "distribution, heatmaps of the customers",
                                  "distributions, and a tool for comparing each",
                                  "customer to the aggregate distribution."),
                                br(),
                                strong("Small Customers"),
                                p("Focusing on the smaller customers, this",
                                  "section provides:"),
                                p("- A similar heatmap to the one in All",
                                  "Customers"),
                                p("- A 3D Scatterplot of just the smaller",
                                  "customers, including bloxplots detailing",
                                  "the differences in the clusters, group",
                                  "summaries, and details of the top",
                                  "customers and products within each group."),
                                br(),
                                strong("Product Views"),
                                p("A 3D Scatterplot of the organizations",
                                  "portfolio, grouped by Therapy Area.",
                                  "A wide range of variables are available",
                                  "for the axes.")
                            )
                        )
                    )
                ),
                tabItem(
                    tabName = "all_heatmap",
                    d3heatmapOutput("all.heatmap",
                                    width = "100%",
                                    height = "700px")
                ),
                tabItem(
                    tabName = "all_scatter",
                    column(
                        width = 6,
                        fluidRow(
                            box(
                                title = tagList(
                                    icon("codepen"),
                                    "3D Scatter Plot"
                                ),
                                width = NULL,
                                plotlyOutput("all.scatter", height = "565px")
                            )
                        )
                    ),
                    column(
                        width = 6,
                        box(
                            title = tagList(
                                icon("gear"),
                                "Plot Options"
                            ),
                            width = NULL,
                            fluidRow(
                                column(
                                    width = 4,
                                    selectInput(
                                        "all.x",
                                        label = "X Variable",
                                        choices = xyz.choices,
                                        selected = "Net.WAC.Percent",
                                        width = NULL
                                    )
                                ),
                                column(
                                    width = 4,
                                    selectInput(
                                        "all.y",
                                        label = "Y Variable",
                                        choices = xyz.choices,
                                        selected = "Percent.GM",
                                        width = NULL
                                    )
                                ),
                                column(
                                    width = 4,
                                    selectInput(
                                        "all.z",
                                        label = "Z Variable",
                                        choices = xyz.choices,
                                        selected = "Rebates",
                                        width = NULL
                                    )
                                )
                            )
                        ), ## end var select box
                        tabBox(
                            "Cluster Values",
                            id = "all.boxplots",
                            width = NULL,
                            tabPanel(
                                "Group Summary",
                                dataTableOutput("all.sum.dt"),
                                hr(),
                                "Rebate and price as percent of list price, summarized as median for the group.",
                                "%Volume and %Revenue measured represent group total as % of Corp Total.",
                                "Doses and GM columns measure group totals in millions"
                            )
                        ) ## close boxplot tabBox
                    ) ## close col 2
                ), ## end all scatter tab
                tabItem(
                    tabName = "all_ta",
                    tabBox(
                        width = NULL,
                        id = "all_ta",
                        tabPanel(
                            "Corporate Summary",
                            fluidRow(
                                column(
                                    width = 12,
                                    plotlyOutput("all.ta.bubble",
                                                 height = "300px"),
                                    HTML("<br><br>")
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    plotlyOutput("all.ta.bars",
                                                 height = "300px")
                                )
                            )
                        ),
                        tabPanel(
                            "Customer Overview",
                            fluidRow(
                                column(
                                    width = 12,
                                    tabBox(
                                        width = NULL,
                                        tabPanel(
                                            "Doses",
                                            d3heatmapOutput("all.ta.doses.hm",
                                                            height = "600px")
                                        ),
                                        tabPanel(
                                            "Revenue",
                                            d3heatmapOutput("all.ta.gm.hm",
                                                            height = "600px")
                                        ),
                                        tabPanel(
                                            "Products",
                                            d3heatmapOutput("all.ta.prod.hm",
                                                            height = "600px")
                                        )
                                    )
                                )
                            )
                        ),
                        tabPanel(
                            "Customer Detail",
                            fluidRow(
                                column(
                                    width = 12,
                                    selectizeInput(
                                        "ta.cust.select",
                                        "Select Customer(s)",
                                        choices = customer.vec,
                                        selected = NULL,
                                        multiple = TRUE,
                                        options = list(
                                            plugins = list(
                                                "remove_button",
                                                "restore_on_backspace"
                                            )
                                        )
                                    )
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    plotlyOutput("mylan.ta.bars",
                                                 height = "300px")
                                )
                            ),
                            HTML("<br>"),
                            fluidRow(
                                column(
                                    width = 12,
                                    plotlyOutput("selected.ta.bars",
                                                 height = "300px"),
                                    HTML("<br>")
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    tabBox(
                                        width = NULL,
                                        tabPanel(
                                            "Selected Customer(s) Product Class Summary",
                                            dataTableOutput("cust.compare.ta.summary.dt")
                                        ),
                                        tabPanel(
                                            "Selected Customer(s) Top Products",
                                            dataTableOutput("cust.compare.ta.dt")
                                        )
                                    )
                                )
                            )
                        )
                    )
                ), ## close all TA breakdown tab
                tabItem(
                    tabName = "all_exclusivity",
                    tabBox(
                        width = NULL,
                        id = "all_exlc",
                        tabPanel(
                            "Corporate Summary",
                            fluidRow(
                                column(
                                    width = 12,
                                    plotlyOutput("all.excl.bars",
                                                 height = "300px"),
                                    HTML("<br><br>")
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    h3("Corporate Summary Table"),
                                    dataTableOutput("excl.summary.df",
                                                 height = "300px")
                                )
                            )
                        ),
                        tabPanel(
                            "Customer Overview",
                            fluidRow(
                                column(
                                    width = 12,
                                    tabBox(
                                        width = NULL,
                                        tabPanel(
                                            "Doses",
                                            d3heatmapOutput("all.excl.doses.hm",
                                                            height = "600px")
                                        ),
                                        tabPanel(
                                            "GM",
                                            d3heatmapOutput("all.excl.gm.hm",
                                                            height = "600px")
                                        ),
                                        tabPanel(
                                            "Product Families",
                                            d3heatmapOutput("all.excl.prod.hm",
                                                            height = "600px")
                                        )
                                    )
                                )
                            )
                        ),
                        tabPanel(
                            "Customer Detail",
                            fluidRow(
                                column(
                                    width = 12,
                                    selectizeInput(
                                        "exclusivity.cust.select",
                                        "Select Customer(s)",
                                        choices = customer.vec,
                                        selected = NULL,
                                        multiple = TRUE,
                                        options = list(
                                            plugins = list(
                                                "remove_button",
                                                "restore_on_backspace"
                                            )
                                        )
                                    )
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    plotlyOutput("mylan.excl.bars",
                                                 height = "300px"),
                                    HTML("<br><br>")
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    plotlyOutput("selected.excl.bars",
                                                 height = "300px"),
                                    HTML("<br><hr><br>")
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 12,
                                    tabBox(
                                        width = NULL,
                                        tabPanel(
                                            "Selected Customer(s) Exclusivity Totals",
                                            dataTableOutput("cust.compare.excl.summary.dt")
                                        ),
                                        tabPanel(
                                            "Selected Customer(s) Top Products",
                                            dataTableOutput("cust.compare.excl.dt")
                                        )

                                    )
                                )
                            )
                        )
                    )
                ), ## close all exclusivity breakdown tab
                tabItem(
                    tabName = "small_heatmap",
                    d3heatmapOutput("cust.heatmap",
                                    width = "100%",
                                    height = "700px")
                ), ## close small cust heatmap tab
                tabItem(
                    tabName = "small_cluster",
                    fluidRow(
                        ## col for plot & centriods
                        column(
                            width = 7,
                            fluidRow(
                                box(
                                    title = tagList(
                                        icon("codepen"),
                                        "3D Scatter Plot"
                                    ),
                                    width = NULL,
                                    plotlyOutput("small.scatter", height = "600px")
                                )
                            )
                        ), ## close plot & col
                        ## col 2
                        column(
                            width = 5,
                            box(
                                title = tagList(
                                    icon("gear"),
                                    "Plot Options"
                                ),
                                width = NULL,
                                fluidRow(
                                    column(
                                        width = 4,
                                        selectInput(
                                            "x",
                                            label = "X Variable",
                                            choices = xyz.choices,
                                            selected = "Net.WAC.Percent",
                                            width = NULL
                                        )
                                    ),
                                    column(
                                        width = 4,
                                        selectInput(
                                            "y",
                                            label = "Y Variable",
                                            choices = xyz.choices,
                                            selected = "Percent.GM",
                                            width = NULL
                                        )
                                    ),
                                    column(
                                        width = 4,
                                        selectInput(
                                            "z",
                                            label = "Z Variable",
                                            choices = xyz.choices,
                                            selected = "Rebates",
                                            width = NULL
                                        )
                                    )
                                )
                            ), ## end var select box
                            tabBox(
                                "Cluster Values",
                                id = "small_sidebar",
                                width = NULL,
                                side = "right",
                                tabPanel(
                                    "Boxplots",
                                    tabBox(
                                        width = NULL,
                                        id = "small_boxplots",
                                        tabPanel(
                                            "List %",
                                            plotlyOutput("small.netwac.box",
                                                         height = "352px")
                                        ),
                                        tabPanel(
                                            "Rebate %",
                                            plotlyOutput("small.rebate.box",
                                                         height = "352px")
                                        ),
                                        tabPanel(
                                            "% Volume",
                                            plotlyOutput("small.dose.box",
                                                         height = "352px")
                                        ),
                                        tabPanel(
                                            "% Revenue",
                                            plotlyOutput("small.gm.box",
                                                         height = "352px")
                                        )
                                    ) ## close boxplot tabBox
                                ), ## close boxplot tab panel
                                tabPanel(
                                    "Group Summaries",
                                    selectizeInput(
                                        "small.sum.selection",
                                        "Filter Groups",
                                        choices = 1:6,
                                        selected = NULL,
                                        multiple = TRUE,
                                        options = list(
                                            plugins = list(
                                                "remove_button",
                                                "restore_on_backspace"
                                            )
                                        )
                                    ),
                                    dataTableOutput("selected.summary.dt"),
                                    "Revenue and Volume measured in millions.  List % is median net price as",
                                    " percent of list price. List % and Rebate % are median each group",
                                    " member's median values, all others are",
                                    "sums."
                                ),
                                tabPanel(
                                    "Customer/Product Info",
                                    fluidRow(
                                        column(
                                            width = 6,
                                            selectizeInput(
                                                "cust.prod.select",
                                                "Search for/Click Customer(s)",
                                                choices = customer.vec,
                                                selected = NULL,
                                                multiple = TRUE,
                                                options = list(
                                                    plugins = list(
                                                        "remove_button",
                                                        "restore_on_backspace"
                                                    )
                                                )
                                            )
                                        ),
                                        column(
                                            width = 6,
                                            selectizeInput(
                                                "group.prod.select",
                                                "Or select group(s)",
                                                choices = 1:6,
                                                selected = NULL,
                                                multiple = TRUE,
                                                options = list(
                                                    plugins = list(
                                                        "remove_button",
                                                        "restore_on_backspace"
                                                    )
                                                )
                                            )
                                        )
                                    ),
                                    tabBox(
                                        width = NULL,
                                        tabPanel(
                                            "Overview",
                                            dataTableOutput("cust.select.sum.dt")
                                        ),
                                        tabPanel(
                                            "Top Products",
                                            dataTableOutput("selected.prod.dt")
                                        ),
                                        tabPanel(
                                            "Classes",
                                            dataTableOutput("selected.ta.dt")
                                        ),
                                        tabPanel(
                                            "Exclusivity",
                                            dataTableOutput("selected.excl.dt")
                                        )
                                    ),
                                    "Products aggregated within the seleced ",
                                    "customer(s) or group(s).",
                                    "Rev. and Vol. measured in millions.",
                                    " Price measured as net price as % of list price."
                                )
                            ) ## close col2 tabBox
                        ) ## close col 2
                    ) ## close row
                ), ## close small cust cluster tab
                tabItem(
                    tabName = "product_cluster",
                    column(
                        width = 7,
                        fluidRow(
                            box(
                                title = tagList(
                                    icon("codepen"),
                                    "3D Scatter Plot"
                                ),
                                width = NULL,
                                plotlyOutput("prod.scatter", height = "600px")
                            )
                        )
                    ),
                    column(
                        width = 5,
                        box(
                            title = tagList(
                                icon("gear"),
                                "Plot Options"
                            ),
                            width = NULL,
                            fluidRow(
                                column(
                                    width = 4,
                                    selectInput(
                                        "prod.x",
                                        label = "X Variable",
                                        choices = xyz.prod,
                                        selected = "Net.WAC.Percent",
                                        width = NULL
                                    )
                                ),
                                column(
                                    width = 4,
                                    selectInput(
                                        "prod.y",
                                        label = "Y Variable",
                                        choices = xyz.prod,
                                        selected = "Percent.GM",
                                        width = NULL
                                    )
                                ),
                                column(
                                    width = 4,
                                    selectInput(
                                        "prod.z",
                                        label = "Z Variable",
                                        choices = xyz.prod,
                                        selected = "Exclusivity",
                                        width = NULL
                                    )
                                )
                            )
                        ), ## end var select box
                        box(
                            title = tagList(
                                icon("th-list"),
                                "Product Details"
                            ),
                            width = NULL,
                            fluidRow(
                                column(
                                    width = 12,
                                    selectizeInput(
                                        "prod.select",
                                        "Search for or Click Product(s)",
                                        choices = product.vec,
                                        selected = NULL,
                                        multiple = TRUE,
                                        options = list(
                                            plugins = list(
                                                "remove_button",
                                                "restore_on_backspace"
                                            )
                                        )
                                    )
                                )
                            ), ## close product select row
                            h4("Product Totals"),
                            dataTableOutput("prod.select.sum.dt"),
                            h4("Customer Breakdown"),
                            dataTableOutput("prod.select.detail.dt"),
                            "Customers aggregated within the seleced ",
                            "product(s).",
                            "GM and Doses measured in millions.",
                            " Price measured as net price as % of list"
                        )
                    ) ## close product col 2
                ) ## close product scatter tab
            ) ## close tab structure
        ) ## close page
    ) ## close body
) ## close UI
