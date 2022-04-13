## Create Date: 2022/3/09
## Author: Erjie Zhao
## Purpose: server of box plot

library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(tibble)
library(magrittr)
library(stringr)
library(purrr)
library(tidyr)
library(readr)
library(readxl)
library(DT)
library(shinyjqui)##图片拖拽大小
library(rlang)

server <- function(input, output) {
  
  ## setting
  options(shiny.maxRequestSize=30*1024^2)
  jqui_bookmarking()
  
  # 保存交互参数
  values <- reactiveValues(data = NULL, group = NULL, plot_data = NULL,
                           plot_setting = NULL, plot = NULL)
  
  ## 绘图数据 ---------------------------------
  observeEvent(input$file, {
    if (!is.null(input$file)) {
      inFile <- input$file
      if ((str_detect(inFile$name,pattern = '.xlsx$')) | (str_detect(inFile$name,pattern = '.xls$'))) {
        sheets <- excel_sheets(path = inFile$datapath)
        if (length(sheets) > 1) {
          showModal(
            modalDialog(
              p("There are multiple sheets in the excel file, select first sheet for analysis!"),
              easyClose = TRUE,
              ooter = tagList(
                actionButton(inputId = "intro", label = "OK", icon = icon("info-circle")))
            )
          )
        } else {
          data_up <- read_excel(inFile$datapath, sheet = 1)
          values$data <- data_up
        }
      } else if (str_detect(inFile$name,pattern = '.csv$')) {
        values$data <- read_csv(file = inFile$datapath)
      } else {
        values$data <- read_delim(file = inFile$datapath, delim = '\t')
      }
    } else {
      values$data <- NULL
    }
  })
  
  ## 分组数据 ---------------------------------
  observe({
    if (!is.null(input$group)) {
      groupFile <- input$group
      if ((str_detect(groupFile$name,pattern = '.xlsx$')) | (str_detect(groupFile$name,pattern = '.xls$'))) {
        sheets <- excel_sheets(path = groupFile$datapath)
        if (length(sheets) > 1) {
          # 弹窗提醒
          showModal(
            modalDialog(
              p('There are multiple sheets in the excel file, select first sheet for analysis!'),
              easyClose = TRUE,
              footer = tagList(
                actionButton(inputId = "intro", label = "OK", icon = icon("info-circle")))
            )
          )
        } else {
          group <- read_excel(groupFile$datapath, sheet = 1)
          values$group <- group
        }
      } else if (str_detect(groupFile$name,pattern = '.csv$')) {
        values$group <- read_csv(file = groupFile$datapath)
      } else {
        values$group <- read_delim(file = groupFile$datapath, delim = '\t')
      }
    } else {
      values$group <- NULL
    }
  })
  
    
  ## 展示绘图数据-----------------
  output$data_output <- renderDT({
    req(!is.null(values$data))
    coln = 5
    if (coln > ncol(values$data)) {
      coln <- ncol(values$data)
    }
    rown = 5
    if (rown > nrow(values$data)) {
      rown <- nrow(values$data)
    }
    DT::datatable(values$data[1:rown,1:coln], extensions = 'Buttons', class = "row-border hover",### nowrap
                  options=list(pageLength = 10,lengthMenu = c(10, 20, 50, 100,-1),dom = 'rt', scrollX=TRUE))
  })
  
  ## 展示分组数据-----------------
  output$group_output <- renderDT({
    req(!is.null(values$group))
    coln = 5
    if (coln > ncol(values$group)) {
      coln <- ncol(values$group)
    }
    rown = 5
    if (rown > nrow(values$group)) {
      rown <- nrow(values$group)
    }
    DT::datatable(values$group[1:rown,1:coln], extensions = 'Buttons', class = "row-border hover",### nowrap
                  options=list(pageLength = 10,lengthMenu = c(10, 20, 50, 100,-1),dom = 'rt', scrollX=TRUE))
  })
  
  ## 整理画图数据 -------
  observe({
    req(!is.null(values$group))
    req(!is.null(values$data))
    colnames(values$group) <- c('ID', 'Group')
    values$plot_data <- values$data %>% 
      column_to_rownames(var = colnames(values$data)[1]) %>% 
      as.matrix() %>% 
      data_convert(group = values$group)
  })
  
  ## 更改图例位置UI
  # observe({
  #   if (input$legend_in == '坐标系外') {
  #     output$legend_position <- renderUI({
  #       selectInput(inputId = 'legend_position', label = i18n('图例位置'), c('上', '下', '左', '右'), selected = '右')
  #     })
  #     values$plot_setting <- list(
  #       legend_in = FALSE,
  #       legend_position = ifelse(input$legend_position == '右', 'right',
  #                                ifelse(input$legend_position == '左', 'left',
  #                                       ifelse(input$legend_position == '上', 'top', 'bottom'))),
  #       legend_x = NULL, legend_y = NULL
  #     )
  #   } else {
  #     output$legend_position <- renderUI({
  #       fluidRow(
  #         numericInput('legend_x', '图例x轴位置', min = 0, max = 1, value = 0.8),
  #         numericInput('legend_y', '图例y轴位置', min = 0, max = 1, value = 0.8)
  #       )
  #     })
  #     values$plot_setting <- list(
  #       legend_in = TRUE,
  #       legend_position = ifelse(input$legend_position == '右', 'right',
  #                                ifelse(input$legend_position == '左', 'left',
  #                                       ifelse(input$legend_position == '上', 'top', 'bottom'))),
  #       legend_x = input$legend_x, legend_y = input$legend_y
  #     )
  #   }
  # })
  
  ## 获取UI输入，绘制图片 ---------------------
  observe({
    ## set range
    if (isTRUE(!anyNA(input$ylim))) {
      y_lim <- input$ylim
    } else {
      y_lim <- NULL
    }
    values$plot_setting <- append(values$plot_setting, list(
      log = input$log10,
      plot_by_group = ifelse(input$plot_by_group == 'TRUE', TRUE, FALSE),
      show_outliers = input$show_outliers,
      show_errorbar = input$show_errorbar,
      theme = input$theme,
      main = input$title,
      xlab = input$xlab,
      ylab = input$ylab,
      legend_title = input$legend_title,
      axis_x_font_angle = input$rect,
      y_min = input$min,
      y_max = input$max,
      show_point = ifelse(input$type == 'none', FALSE, TRUE),
      point_jitter = ifelse(input$type == 'jitter', TRUE, FALSE),
      show_legend = ifelse(input$show_legend == '显示', TRUE, FALSE),
      title_size = input$title_size,
      legend_title_size = input$legend_title_size,
      axis_font_size = input$axis_font_size,
      labs_title_size = input$labs_title_size,
      y_lim = c(NA, NA)
    ))
  })
  
  output$options <- renderDT({
    req(!is.null(values$plot_setting))
    dat <- tibble(options = names(values$plot_setting)[c(1:10, 13:19)],
                  value = unlist(values$plot_setting)[c(1:10, 13:19)])
    DT::datatable(dat)
  })  
  

  

  ## 绘制图片
  observe({
    req(!is.null(values$plot_data))
    values$plot <- plot_box(values$plot_data,
                            log = values$plot_setting$log,
                            plot_by_group = values$plot_setting$plot_by_group,
                            show_point = values$plot_setting$show_point,
                            point_jitter = values$plot_setting$point_jitter,
                            show_outliers = values$plot_setting$show_outliers,
                            show_errorbar = values$plot_setting$show_errorbar,
                            xlab = values$plot_setting$xlab,
                            ylab = values$plot_setting$ylab,
                            main = values$plot_setting$main,
                            # theme = values$plot_setting$theme,
                            legend_title = values$plot_setting$legend_title,
                            show_legend = values$plot_setting$show_legend,
                            legend_in = values$plot_setting$legend_in,
                            legend_position = values$plot_setting$legend_position,
                            legend_x = values$plot_setting$legend_x,
                            legend_y = values$plot_setting$legend_y,
                            title_size = values$plot_setting$title_size,
                            axis_font_size = values$plot_setting$axis_font_size,
                            legend_title_size = values$plot_setting$legend_title_size,
                            axis_x_font_angle = values$plot_setting$axis_x_font_angle,
                            labs_title_size = values$plot_setting$labs_title_size,
                            y_lim = values$plot_setting$y_lim)
  })

  
  output$boxplot <- renderPlot({
    req(!is.null(values$plot))
    print(values$plot)
  })
  
  
  
  ## 输出图片
  # values$boxplot <- reactive({
  #   # plot_box(plot_data, log = plot_setting$log, plot_by_group = plot_by_group,
  #   #          show_point = plot_setting$show_point, point_jitter = FALSE,
  #   #          show_outliers = plot_setting$show_outliers, show_errorbar = TRUE,
  #   #          xlab = plot_setting$xlab, ylab = plot_setting$ylab,
  #   #          main = plot_setting$main, theme = plot_setting$theme,
  #   #          legend_title = plot_setting$legend_title,
  #   #          show_legend = plot_setting$show_legend,
  #   #          legend_in = plot_setting$legend_in,
  #   #          legend_position = plot_setting$legend_position,
  #   #          legend_x = plot_setting$legend_x,
  #   #          legend_y = plot_setting$legend_y,
  #   #          title_size = plot_setting$title_size,
  #   #          axis_font_size = plot_setting$axis_font_size,
  #   #          legend_title_size = plot_setting$legend_title_size,
  #   #          axis_x_font_angle = plot_setting$axis_x_font_angle)
  #  plot_box(values$plot_data, log = TRUE, plot_by_group = FALSE,
  #           show_point = FALSE, point_jitter = FALSE,
  #           show_outliers = TRUE, show_errorbar = TRUE,
  #           xlab = 'Sample', ylab = 'log10(Value+1)',
  #           main = 'Boxplot', theme = 'theme_classic()',
  #           legend_title = 'Group', show_legend = TRUE,
  #           legend_in = FALSE, legend_position = 'right',
  #           legend_x = NULL, legend_y = NULL,
  #           title_size = 20, axis_font_size = 9,
  #           legend_title_size = 10, axis_x_font_angle = 0)
  # })
  # output$boxplot <- renderPlot({
  #   req(values$boxplot)
  #   print(values$boxplot)
  # })
}