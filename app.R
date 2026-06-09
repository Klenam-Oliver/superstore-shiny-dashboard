library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(scales)

# ============================================
# UI
# ============================================
ui <- dashboardPage(
  dashboardHeader(title = "Superstore Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Sales Analysis", tabName = "sales", icon = icon("chart-bar")),
      menuItem("Profit Analysis", tabName = "profit", icon = icon("dollar-sign")),
      menuItem("Data Table", tabName = "data", icon = icon("table"))
    ),
    
    selectInput("region", "Select Region:",
                choices = c("All", unique(superstore$region)),
                selected = "All"),
    
    selectInput("category", "Select Category:",
                choices = c("All", unique(superstore$category)),
                selected = "All"),
    
    selectInput("segment", "Select Segment:",
                choices = c("All", unique(superstore$segment)),
                selected = "All"),
    
    sliderInput("discount", "Max Discount:",
                min = 0, max = 0.8,
                value = 0.8, step = 0.1)
  ),
  
  dashboardBody(
    tabItems(
      
      # ============================================
      # TAB 1 - OVERVIEW
      # ============================================
      tabItem(tabName = "overview",
              
              fluidRow(
                valueBoxOutput("total_sales"),
                valueBoxOutput("total_profit"),
                valueBoxOutput("total_orders")
              ),
              
              fluidRow(
                valueBoxOutput("avg_discount"),
                valueBoxOutput("avg_ship_days"),
                valueBoxOutput("profit_margin")
              ),
              
              fluidRow(
                box(title = "Sales by Region",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    plotlyOutput("region_sales")),
                
                box(title = "Sales by Category",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    plotlyOutput("category_sales"))
              ),
              
              fluidRow(
                box(title = "Sales vs Profit by Sub-Category (Bubble Chart)",
                    status = "info",
                    solidHeader = TRUE,
                    width = 12,
                    plotlyOutput("sales_trend"))
              )
      ),
      
      # ============================================
      # TAB 2 - SALES ANALYSIS
      # ============================================
      tabItem(tabName = "sales",
              
              fluidRow(
                box(title = "Top 10 Products by Sales",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    plotlyOutput("top_products")),
                
                box(title = "Sales by Segment",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    plotlyOutput("segment_sales"))
              ),
              
              fluidRow(
                box(title = "Sales by Sub-Category",
                    status = "info",
                    solidHeader = TRUE,
                    width = 12,
                    plotlyOutput("subcategory_sales"))
              )
      ),
      
      # ============================================
      # TAB 3 - PROFIT ANALYSIS
      # ============================================
      tabItem(tabName = "profit",
              
              fluidRow(
                box(title = "Discount vs Profit",
                    status = "warning",
                    solidHeader = TRUE,
                    width = 6,
                    plotlyOutput("discount_profit")),
                
                box(title = "Profit by State (Top & Bottom 10)",
                    status = "warning",
                    solidHeader = TRUE,
                    width = 6,
                    plotlyOutput("state_profit"))
              ),
              
              fluidRow(
                box(title = "Profit by Sub-Category",
                    status = "danger",
                    solidHeader = TRUE,
                    width = 12,
                    plotlyOutput("subcategory_profit"))
              )
      ),
      
      # ============================================
      # TAB 4 - DATA TABLE
      # ============================================
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Superstore Data",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    DTOutput("data_table"))
              )
      )
    )
  )
)

# ============================================
# SERVER
# ============================================
server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- superstore
    
    if (input$region != "All")
      data <- data %>% filter(region == input$region)
    
    if (input$category != "All")
      data <- data %>% filter(category == input$category)
    
    if (input$segment != "All")
      data <- data %>% filter(segment == input$segment)
    
    data <- data %>% filter(discount <= input$discount)
    
    return(data)
  })
  
  # ============================================
  # KPI BOXES
  # ============================================
  output$total_sales <- renderValueBox({
    valueBox(
      dollar(sum(filtered_data()$sales)),
      "Total Sales",
      icon = icon("dollar-sign"),
      color = "blue"
    )
  })
  
  output$total_profit <- renderValueBox({
    valueBox(
      dollar(sum(filtered_data()$profit)),
      "Total Profit",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$total_orders <- renderValueBox({
    valueBox(
      comma(nrow(filtered_data())),
      "Total Orders",
      icon = icon("shopping-cart"),
      color = "purple"
    )
  })
  
  output$avg_discount <- renderValueBox({
    valueBox(
      percent(mean(filtered_data()$discount)),
      "Avg Discount",
      icon = icon("tag"),
      color = "orange"
    )
  })
  
  output$avg_ship_days <- renderValueBox({
    valueBox(
      round(mean(filtered_data()$ship_days), 1),
      "Avg Ship Days",
      icon = icon("truck"),
      color = "red"
    )
  })
  
  output$profit_margin <- renderValueBox({
    valueBox(
      percent(sum(filtered_data()$profit) / sum(filtered_data()$sales)),
      "Profit Margin",
      icon = icon("percent"),
      color = "yellow"
    )
  })
  
  # ============================================
  # OVERVIEW CHARTS
  # ============================================
  output$region_sales <- renderPlotly({
    filtered_data() %>%
      group_by(region) %>%
      summarise(total_sales = sum(sales)) %>%
      plot_ly(x = ~reorder(region, -total_sales),
              y = ~total_sales,
              type = "bar",
              color = ~region) %>%
      layout(xaxis = list(title = "Region"),
             yaxis = list(title = "Total Sales"))
  })
  
  output$category_sales <- renderPlotly({
    filtered_data() %>%
      group_by(category) %>%
      summarise(total_sales = sum(sales)) %>%
      plot_ly(labels = ~category,
              values = ~total_sales,
              type = "pie") %>%
      layout(title = "")
  })
  
  output$sales_trend <- renderPlotly({
    filtered_data() %>%
      group_by(sub_category) %>%
      summarise(
        total_sales = sum(sales),
        total_profit = sum(profit),
        total_orders = n()
      ) %>%
      plot_ly(x = ~total_sales,
              y = ~total_profit,
              size = ~total_orders,
              text = ~paste("Sub-Category:", sub_category,
                            "<br>Sales:", dollar(total_sales),
                            "<br>Profit:", dollar(total_profit),
                            "<br>Orders:", total_orders),
              hoverinfo = "text",
              type = "scatter",
              mode = "markers",
              marker = list(
                sizemode = "diameter",
                opacity = 0.7,
                sizeref = 1.5,
                line = list(width = 1, color = "white")
              )) %>%
      add_annotations(
        x = ~total_sales,
        y = ~total_profit,
        text = ~sub_category,
        showarrow = FALSE,
        font = list(size = 9),
        yshift = 10
      ) %>%
      layout(
        xaxis = list(title = "Total Sales"),
        yaxis = list(title = "Total Profit"),
        showlegend = FALSE
      )
  })
  
  # ============================================
  # SALES ANALYSIS CHARTS
  # ============================================
  output$top_products <- renderPlotly({
    filtered_data() %>%
      group_by(product_name) %>%
      summarise(total_sales = sum(sales)) %>%
      top_n(10, total_sales) %>%
      plot_ly(x = ~total_sales,
              y = ~reorder(product_name, total_sales),
              type = "bar",
              orientation = "h") %>%
      layout(xaxis = list(title = "Total Sales"),
             yaxis = list(title = ""))
  })
  
  output$segment_sales <- renderPlotly({
    filtered_data() %>%
      group_by(segment) %>%
      summarise(total_sales = sum(sales)) %>%
      plot_ly(labels = ~segment,
              values = ~total_sales,
              type = "pie")
  })
  
  output$subcategory_sales <- renderPlotly({
    filtered_data() %>%
      group_by(sub_category) %>%
      summarise(total_sales = sum(sales)) %>%
      plot_ly(x = ~reorder(sub_category, -total_sales),
              y = ~total_sales,
              type = "bar",
              color = ~sub_category) %>%
      layout(xaxis = list(title = "Sub-Category"),
             yaxis = list(title = "Total Sales"))
  })
  
  # ============================================
  # PROFIT ANALYSIS CHARTS
  # ============================================
  output$discount_profit <- renderPlotly({
    plot_ly(filtered_data(),
            x = ~discount,
            y = ~profit,
            color = ~category,
            type = "scatter",
            mode = "markers",
            alpha = 0.5) %>%
      layout(xaxis = list(title = "Discount"),
             yaxis = list(title = "Profit"))
  })
  
  output$state_profit <- renderPlotly({
    state_data <- filtered_data() %>%
      group_by(state) %>%
      summarise(total_profit = sum(profit)) %>%
      arrange(total_profit) %>%
      slice(c(1:10, (n()-9):n()))
    
    plot_ly(state_data,
            x = ~total_profit,
            y = ~reorder(state, total_profit),
            type = "bar",
            orientation = "h",
            color = ~total_profit > 0,
            colors = c("red", "steelblue")) %>%
      layout(xaxis = list(title = "Total Profit"),
             yaxis = list(title = ""))
  })
  
  output$subcategory_profit <- renderPlotly({
    filtered_data() %>%
      group_by(sub_category) %>%
      summarise(total_profit = sum(profit)) %>%
      plot_ly(x = ~reorder(sub_category, -total_profit),
              y = ~total_profit,
              type = "bar",
              color = ~total_profit > 0,
              colors = c("red", "steelblue")) %>%
      layout(xaxis = list(title = "Sub-Category"),
             yaxis = list(title = "Total Profit"))
  })
  
  # ============================================
  # DATA TABLE
  # ============================================
  output$data_table <- renderDT({
    filtered_data() %>%
      select(order_date, region, category, sub_category,
             product_name, sales, profit, discount, ship_mode) %>%
      datatable(options = list(pageLength = 15,
                               scrollX = TRUE))
  })
}

# ============================================
# RUN APP
# ============================================
shinyApp(ui = ui, server = server)