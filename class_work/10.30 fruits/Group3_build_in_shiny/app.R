library(shiny)
library(tidyverse)
library(DT)
library(plotly)
library(RColorBrewer) 

# [优化 1: 将 FRUIT_LIST 设为全局变量]
# [修改] 替换为您的10种水果列表
FRUIT_LIST <- c("Peach", "Apple", "Orange", "Mango", "Pineapple", 
                "Grape", "Blueberry", "Cherry", "Strawberry", "Kiwi")

# [优化 2: 动态创建 choiceNames]
# (此逻辑不变, 自动更新为10种水果)
fruitChoiceNames <- lapply(FRUIT_LIST, function(fruit) {
  div(
    class = "fruit-choice-label",
    span(fruit),
    img(
      src = paste0("icons/", fruit, ".png"), 
      height = "20px", 
      width = "20px",
      style = "margin-left: 8px;" # 增加一点左边距
    )
  )
})


# UI部分
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      /* ... (CSS 保持不变) ... */
      
      .title-panel h2 {
        font-size: 24px; 
        font-weight: bold;
        color: #333; 
      }
      body {
        font-family: 'Open Sans', sans-serif; 
        background-color: #f8f9fa; /* 浅灰色背景 */
      }
      .wellPanel {
        border-radius: 8px; 
        border-color: #e0e0e0; 
        box-shadow: 2px 2px 5px rgba(0,0,0,0.05); 
      }
      h4 {
        color: #0056b3; 
        font-weight: bold;
      }
      
      .col-sm-9 { 
        background-color: #ffffff; 
        border-radius: 8px; 
        box-shadow: 2px 2px 5px rgba(0,0,0,0.05); 
        padding: 25px; 
      }
      
      .value-box-custom {
        border: 2px solid; 
        border-radius: 8px;
        padding: 15px;
        margin-top: 0px; 
        text-align: center;
        box-shadow: 3px 3px 8px rgba(0,0,0,0.08);
        color: #FFFFFF; 
      }
      .value-box-custom .value-box-title {
        font-size: 16px;
        font-weight: bold;
        color: #FFFFFF; 
      }
      .value-box-custom .value-box-value {
        font-size: 32px;
        font-weight: bold;
        margin: 10px 0;
        color: #FFFFFF; 
      }
      .value-box-custom .value-box-subtitle {
        font-size: 12px;
        color: #FFFFFFCC; 
      }
      
      .radio label span {
         width: 95%; 
      }
      .fruit-choice-label {
         display: flex;
         justify-content: space-between; 
         align-items: center;
      }
      
    "))
  ),
  
  titlePanel(div(class="title-panel", "Group3: Zhenbei Guo, Yuecheng Hong, Minyu Zhang")), 
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      wellPanel(
        h4("✨ Dashboard Overview", style = "margin-top: 0;"), 
        p("This interactive dashboard presents a comprehensive analysis of surplus fruit data from various farmers. Leverage the filters to dive deep into specific insights."),
        p(strong("📊 Left Chart (Global View):"), "Displays the total amount of 'Flash-Frozen & Ready' fruit available in the market, across all fruit types and farms."),
        p(strong("📈 Right Chart (Filtered Details):"), "Visualizes the selected fruit's amount by farmer and its freezing status, ordered by total quantity.")
      ),
      
      # [优化 3: radioButtons 会自动使用新的 FRUIT_LIST]
      wellPanel(
        h4("🍓 Select Fruit", style = "margin-top: 0;"),
        radioButtons(
          "fruit_filter",
          label = NULL,
          choiceNames = fruitChoiceNames,
          choiceValues = FRUIT_LIST, 
          selected = "Strawberry" # 默认选项 "Strawberry" 仍然有效
        )
      ),
      
      wellPanel(
        h4("👨‍🌾 Filter by Farmer", style = "margin-top: 0;"),
        uiOutput("farmer_checkbox")
      ),
      
      wellPanel(
        h4("❄️ Freezing Status", style = "margin-top: 0;"),
        checkboxGroupInput(
          "freezing_filter",
          label = NULL,
          choices = c("Awaiting Pickup", "At Freezing Facility", "Flash-Frozen & Ready"),
          selected = c("Awaiting Pickup", "At Freezing Facility", "Flash-Frozen & Ready")
        )
      ),
      
      wellPanel(
        h4("💲 Price Range (USD/kg)", style = "margin-top: 0;"),
        sliderInput(
          "price_range",
          label = NULL,
          min = 0,
          max = 15,
          value = c(0, 15),
          step = 0.1,
          post = " USD/kg"
        )
      )
    ),
    
    mainPanel(
      width = 9,
      
      fluidRow(
        column(
          6, 
          plotlyOutput("market_plot", height = "400px") 
        ),
        column(
          6, 
          plotlyOutput("farmer_plot", height = "400px")
        )
      ),
      
      br(), 
      
      fluidRow(
        column(
          4, 
          offset = 4, 
          uiOutput("sellable_value_box") 
        )
      ),
      
      br(), 
      
      fluidRow(
        column(
          12,
          h4("Table: General data information"),
          DTOutput("data_table")
        )
      )
    )
  )
)

# Server部分
server <- function(input, output, session) {
  
  # [优化 4: 替换为您的特定颜色映射]
  FRUIT_COLOR_MAP <- c(
    Peach = "#FFDAB9",
    Apple = "#FF6347",
    Orange = "#FFA500",
    Mango = "#9ACD32",
    Pineapple = "#FFFF00",
    Grape = "#800080",
    Blueberry = "#4682B4",
    Cherry = "#8B0000",
    Strawberry = "#FF69B4",
    Kiwi = "#8B4513"
  )
  
  # [删除] 旧的 colorRampPalette/setNames 逻辑已被删除
  
  
  # 读取数据
  fruit_data <- reactive({
    df <- read.csv("single_farmer_500_records.csv", stringsAsFactors = FALSE)
    df$Freezing_Status <- factor(df$Freezing_Status, 
                                 levels = c("Awaiting Pickup", "At Freezing Facility", "Flash-Frozen & Ready"))
    return(df)
  })
  
  # 动态生成农场选择器
  output$farmer_checkbox <- renderUI({
    data <- fruit_data()
    farmers <- unique(data$Farmer_Name)
    checkboxGroupInput(
      "farmer_filter",
      label = NULL,
      choices = farmers,
      selected = farmers
    )
  })
  
  # 为 Value Box 创建的 reactive
  sellable_data <- reactive({
    req(input$farmer_filter, input$fruit_filter) 
    
    fruit_data() %>%
      filter(
        Fruit_Type == input$fruit_filter, 
        Farmer_Name %in% input$farmer_filter,
        Freezing_Status == "Flash-Frozen & Ready" 
      ) %>%
      summarise(Total_Sellable = sum(Surplus_Amount)) %>%
      pull(Total_Sellable)
  })
  
  # 渲染 Value Box UI
  output$sellable_value_box <- renderUI({
    value <- sellable_data()
    fruit_name <- req(input$fruit_filter)
    
    formatted_value <- scales::comma(value, accuracy = 1)
    
    # [自动] 查找新的颜色 (例如 "Apple" -> "#FF6347")
    current_color <- FRUIT_COLOR_MAP[fruit_name]
    
    box_style <- paste0(
      "background-color: ", current_color, ";", 
      "border-color: ", current_color, ";",     
      "box-shadow: 3px 3px 12px ", current_color, "66;" 
    )
    
    wellPanel(
      class = "value-box-custom",
      style = box_style, 
      div(class = "value-box-title", paste(fruit_name, "Sellable Stock")),
      div(class = "value-box-value", paste0(formatted_value, " kg")),
      div(class = "value-box-subtitle", "(From selected farmers)")
    )
  })
  
  
  # 筛选后的数据（只用于右图和表格）
  filtered_data <- reactive({
    req(input$farmer_filter, input$fruit_filter) 
    
    data <- fruit_data()
    
    data %>%
      filter(
        Fruit_Type == input$fruit_filter, 
        Farmer_Name %in% input$farmer_filter,
        Freezing_Status %in% input$freezing_filter,
        Price >= input$price_range[1],
        Price <= input$price_range[2]
      )
  })
  
  # [自动] 修改 market_plot 以使用新的 FRUIT_COLOR_MAP
  output$market_plot <- renderPlotly({
    data <- fruit_data()
    
    ready_data <- data %>%
      filter(Freezing_Status == "Flash-Frozen & Ready")
    
    ready_quantity <- ready_data %>%
      group_by(Fruit_Type) %>%
      summarise(Total_Ready_Quantity = sum(Surplus_Amount), .groups = "drop") %>%
      arrange(desc(Total_Ready_Quantity)) %>%
      mutate(Fruit_Type = factor(Fruit_Type, levels = Fruit_Type))
    
    if (nrow(ready_quantity) == 0) {
      return(plotly_empty() %>% layout(title = list(text = "No 'Flash-Frozen & Ready' data available in total.", y = 0.5)))
    }
    
    p1 <- ggplot(ready_quantity, 
                 aes(x = Fruit_Type, 
                     y = Total_Ready_Quantity, 
                     fill = Fruit_Type,
                     text = paste("Fruit Type: ", Fruit_Type, 
                                  "<br>Total Ready Amount: ", scales::comma(Total_Ready_Quantity, accuracy = 1), " kg"))) +
      geom_col(alpha = 0.9) + 
      # [自动] 使用新的 FRUIT_COLOR_MAP
      scale_fill_manual(values = FRUIT_COLOR_MAP) + 
      labs(
        title = NULL, 
        x = "Fruit Type",
        y = "Amount (kg)"
      ) +
      theme_minimal(base_size = 12) + 
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "#555"),
        axis.title = element_text(size = 13, color = "#333", face = "bold"),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA) 
      )
    
    ggplotly(p1, tooltip = "text") %>%
      layout(
        title = list(text = "<b>Total 'Flash-Frozen & Ready' Fruit (All Farms)</b>", x = 0.5,
                     font = list(size = 18, color = "#333")), 
        xaxis = list(title = list(standoff = 20)), 
        yaxis = list(title = list(standoff = 20)), 
        margin = list(t = 50, b = 100, l = 70, r = 30) 
      )
  })
  
  # 图2: 动态图 (保持不变)
  output$farmer_plot <- renderPlotly({
    data <- filtered_data()
    
    fruit_data_grouped <- data %>%
      group_by(Farmer_Name, Freezing_Status) %>%
      summarise(Amount = sum(Surplus_Amount), .groups = "drop")
    
    if(nrow(fruit_data_grouped) == 0) {
      return(plotly_empty() %>% 
               layout(title = list(text = paste0("No ", input$fruit_filter, " data for selected filters."), y = 0.5)))
    }
    
    plot_title <- paste0("<b>", input$fruit_filter, " Amount by Farmer and Freezing Status</b>")
    
    fruit_data_grouped$Freezing_Status <- factor(fruit_data_grouped$Freezing_Status, 
                                                 levels = c("Awaiting Pickup", "At Freezing Facility", "Flash-Frozen & Ready"))
    
    freezing_colors <- c(
      "Awaiting Pickup" = "#FC8D62",      
      "At Freezing Facility" = "#66C2A5", 
      "Flash-Frozen & Ready" = "#8DA0CB"  
    )
    
    p2 <- ggplot(fruit_data_grouped, 
                 aes(x = fct_reorder(Farmer_Name, Amount, .fun = sum, .desc = TRUE), 
                     y = Amount, 
                     fill = Freezing_Status,
                     text = paste("Farmer: ", Farmer_Name, 
                                  "<br>Status: ", Freezing_Status,
                                  "<br>Amount: ", scales::comma(Amount, accuracy = 1), " kg"))) + 
      geom_col(position = "stack", alpha = 0.9, width = 0.7) + 
      scale_fill_manual(values = freezing_colors) + 
      labs(
        title = NULL, 
        x = NULL, 
        y = "Amount (kg)",
        fill = NULL 
      ) +
      theme_minimal(base_size = 12) + 
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "#555"),
        axis.title = element_text(size = 13, color = "#333", face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(), 
        legend.text = element_text(size = 10, color = "#555"),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA) 
      )
    
    ggplotly(p2, tooltip = "text") %>%
      layout(
        title = list(text = plot_title, x = 0.5,
                     font = list(size = 18, color = "#333")), 
        xaxis = list(title = list(standoff = 20)), 
        yaxis = list(title = list(standoff = 20)),
        margin = list(t = 50, b = 120, l = 70, r = 30), 
        legend = list(
          orientation = "h", x = 0.5, xanchor = "center", y = -0.4, 
          bgcolor = "rgba(255, 255, 255, 0.7)", bordercolor = "#e0e0e0", borderwidth = 1,
          title = list(text = "<b>Freezing Status:</b>", 
                       font = list(size = 12, color = "#333"),
                       side = "top") 
        )
      )
  })
  
  # 数据表格
  output$data_table <- renderDT({
    data <- filtered_data() %>%
      select(
        `Farmer Name` = Farmer_Name,
        `Fruit Type` = Fruit_Type,
        `Surplus Amount` = Surplus_Amount,
        `Price (USD/kg)` = Price,
        Quality,
        `Freezing Status` = Freezing_Status
      ) %>%
      arrange(`Farmer Name`) 
    
    datatable(
      data,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = 'frtip'
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("Surplus Amount", "Price (USD/kg)"), digits = 1)
  })
}

# 运行应用
shinyApp(ui = ui, server = server)