## Create Date: 2022/3/09
## Author: Erjie Zhao
## Purpose: server of box plot

library(RColorBrewer)
library(ggplot2)
library(ggsignif)
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
library(ggthemes)
library(hrbrthemes)

server <- function(input, output) {
  ## seeting 
  options(shiny.maxRequestSize=30*1024^2)
  jqui_bookmarking() ## 图片可以动态拖拽
  original_dir <- getwd()
  
  # 保存全局交互参数
  values <- reactiveValues(data = NULL, group = NULL, plot_data = NULL,
                           plot_setting = NULL, colors = NULL, plot = NULL)
  

  ## 展示分析方法内容 -------------------------
  methods <- readxl::read_excel(file.path(original_dir, 'inst', 'methods.xlsx'), sheet = 1)
  output$methods <- renderTable(
    expr = methods,
    bordered = TRUE
  )
  
  ## Demo 数据下载 ----------------------------
  demo_data <- readxl::read_excel(file.path(original_dir, 'demo', 'Demo_Boxplot.xlsx'), sheet = 1)
  demo_group <- readxl::read_excel(file.path(original_dir, 'demo', 'Demo_Group.xlsx'), sheet = 1)
  output$downloaddemo <- downloadHandler(
    filename = 'Demo_Boxplot.xlsx',
    content = function(file) {
      openxlsx::write.xlsx(demo_data, file = file)
    }
  )
  output$downloadgroup <- downloadHandler(
    filename = 'Demo_Group.xlsx',
    content = function(file) {
      openxlsx::write.xlsx(demo_group, file = file)
    }
  )
  
  
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
  
  ## 观察绘图数据
  output$plot_data <- renderDT({
    req(!is.null(values$plot_data))
    DT::datatable(values$plot_data)
  })
  
  ## 动态UI，设置图例位置信息、比较组信息、颜色信息 --------------
  output$legend_position <- renderUI({
    if (input$legend_in == '坐标系外') {
      selectInput(inputId = 'legend_position', label = i18n('图例位置'), c('上' = 'top', '下' = 'bottom', '左' = 'left', '右' = 'right'), selected = 'right')
    } else {
      fluidRow(column(width = 6, numericInput('legend_x', '图例x轴位置', min = 0, max = 1, value = 0.8)),
               column(width = 6, numericInput('legend_y', '图例y轴位置', min = 0, max = 1, value = 0.8))
      )
    }
  })
  output$multiple_compare <- renderUI({
    if (input$multiple) {
      fluidRow(column(width = 4, selectInput('multiple_method', i18n('计算方法:多组比较'), c('anova', 'kruskal.test'))),
               column(width = 4, numericInput('multiple_x', '标签横向位置', value = NA, min = 0, max = 10)),
               column(width = 4, numericInput('multiple_y', '标签纵向位置', value = NA, min = 0, max = 10)))
    }
  })
  output$two_compare <- renderUI({
    tagList(
      tags$div(
        if (input$two_compare) {
        fluidRow(column(width = 6, selectInput('two_method', i18n('计算方法:两两比较'), selected = 'wilcox.test', c('wilcox.test', 't.test'))),
                 column(width = 6, selectInput('signf_method', i18n('两两比较显示效果较'), selected = 'p', c('p值' = 'p', '符号' = 'star'))))
        }
      ),
      tags$div(
        if (input$two_compare && !is.null(values$plot_data)) {
          ## get all compare pairs
          print(input$plot_by_group)
          if (input$plot_by_group) {
            all_compares <- combn(unique(values$plot_data$Group), 2) %>%
              t() %>% apply(1, function(x) {paste(x[1], 'vs', x[2])})
          } else {
            all_compares <- combn(unique(values$plot_data$name), 2) %>%
              t() %>% apply(1, function(x) {paste(x[1], 'vs', x[2])})
          }
          selectizeInput(inputId = 'compare_pairs',
                         label = '选取显示的比较组:',
                         choices = all_compares,# as.list(all_compares),
                         selected = NULL,
                         multiple = TRUE,
                         options = list(plugins = list("remove_button"))
          )
        }
      )
    )
  })
  # observe({
  #   print(class(input$compare_pairs))
  # })
  
  ## 获取输入颜色
  observe({
    req(!is.null(values$plot_data))
    all_samples <- values$plot_data$Group
    palette_server(id = 'colors', all_samples = all_samples)
    colors_r_d <- debounce(palette_server(id = 'colors', all_samples = all_samples), millis = 1000)
    
    observe({
      values$colors <- colors_r_d()
    })
    
  })
  

  
  ## 设置绘图默认参数 -------------------------
  values$plot_setting <- list()
  
  ## 获取UI输入，改变默认参数 ---------------------
  observe({
    ## get legend position
    if (input$legend_in == '坐标系外') {
      legend_position <- input$legend_position
      legend_x <- legend_y <- NULL
    } else {
      legend_position <- 'right'
      legend_x <- input$legend_x
      legend_y <- input$legend_y
    }
    ## get multiple compare argument
    if (input$multiple == FALSE) {
      multiple_method <- 'anova'
      multiple_x <- multiple_y <- NA
    } else {
      # print('多比较位置:')
      # print(input$multiple_x)
      multiple_method <- input$multiple_method
      if (is.null(input$multiple_x)) {
        if (is.null(input$multiple_y)) {
          multiple_x <- multiple_y <- NA
        } else {
          multiple_x <- NA
          multiple_y <- input$multiple_y
        }
      } else {
        if (is.null(input$multiple_y)) {
          multiple_x <- input$multiple_x
          multiple_y <- NA
        } else {
          multiple_x <- input$multiple_x
          multiple_y <- input$multiple_y
        }
      }
    }
    ## get two compare argument
    if (input$two_compare == FALSE) {
      two_method <- 'wilcox.test'
      signf_method <- 'p'
    } else {
      two_method <- input$two_method
      signf_method <- input$signf_method
    }
    values$plot_setting$log = input$log10
    values$plot_setting$plot_by_group = ifelse(input$plot_by_group == 'TRUE', TRUE, FALSE)
    values$plot_setting$show_outliers = input$show_outliers
    values$plot_setting$show_errorbar = input$show_errorbar
    values$plot_setting$theme = input$theme
    values$plot_setting$main = input$title
    values$plot_setting$xlab = input$xlab
    values$plot_setting$ylab = input$ylab
    values$plot_setting$legend_title = input$legend_title
    values$plot_setting$legend_x = legend_x
    values$plot_setting$legend_y = legend_y
    values$plot_setting$legend_position = legend_position
    values$plot_setting$axis_x_font_angle = input$rect
    values$plot_setting$y_lim = c(input$y_min, input$y_max)
    # print(values$plot_setting$y_lim)
    values$plot_setting$show_point = ifelse(input$type == 'none', FALSE, TRUE)
    values$plot_setting$point_jitter = ifelse(input$type == 'jitter', TRUE, FALSE)
    values$plot_setting$show_legend = ifelse(input$show_legend == '显示', TRUE, FALSE)
    values$plot_setting$legend_in = ifelse(input$legend_in == '坐标系外', FALSE, TRUE)
    values$plot_setting$title_size = input$title_size
    values$plot_setting$legend_title_size = input$legend_title_size
    values$plot_setting$axis_font_size = input$axis_font_size
    values$plot_setting$labs_title_size = input$labs_title_size
    
    values$plot_setting$multiple = input$multiple
    values$plot_setting$multiple_method = multiple_method
    values$plot_setting$multiple_x <- multiple_x
    values$plot_setting$multiple_y <- multiple_y
    
    values$plot_setting$two_compare = input$two_compare
    values$plot_setting$two_method = two_method
    values$plot_setting$signf_method = signf_method
    # print(values$plot_setting$two_compare)
    # print(values$plot_setting$two_method)
    # print(values$plot_setting$signf_method)
  })

  
  ## 观察参数设置是否完成 
  output$options <- renderPrint({
    req(!is.null(values$plot_setting))
    unlist(values$plot_setting)
  })  
  
  observe({
    req(!is.null(values$plot_data))
    compare_pairs <- input$compare_pairs %>% 
      str_split(pattern = ' vs ')
    
    if (is.null(values$colors)) {
      which_pal_scale_obj <- NULL
    } else {
      if (values$colors$scale == 'palette') {
        which_pal_scale_obj <- esquisse::which_pal_scale(mapping = aes(fill = Group),
                                                         data = values$plot_data,
                                                         palette = values$colors$colors,
                                                         fill_type = 'discrete',
                                                         reverse = values$colors$reverse)      
      } else if (values$colors$scale == 'manual') {
        which_pal_scale_obj <- esquisse::which_pal_scale(mapping = aes(fill = Group),
                                                         data = values$plot_data,
                                                         palette = values$colors$colors,
                                                         fill_type = values$colors$type,
                                                         reverse = FALSE) 
      }
    }
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
                            theme = values$plot_setting$theme,
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
                            y_lim = values$plot_setting$y_lim,
                            multiple = values$plot_setting$multiple,
                            multiple_method = values$plot_setting$multiple_method,
                            multiple_x = values$plot_setting$multiple_x,
                            multiple_y = values$plot_setting$multiple_y,
                            two_compare = values$plot_setting$two_compare,
                            two_method = values$plot_setting$two_method,
                            signf_method = values$plot_setting$signf_method,
                            compare_pairs = compare_pairs,
                            which_pal_scale_obj = which_pal_scale_obj
    )

  })
  
  ## 图片展示 -----------------------
  output$boxplot <- renderPlot({
    req(!is.null(values$plot))
    print(values$plot)
  })
  
  ## 下载结果图片 -----------------------
  output$download_label <- renderUI({
    req(!is.null(values$plot))
    tagList(
      tags$div(
        align = 'center',
        tags$br(),
        downloadBttn(
          outputId = "download_png",
          label = 'PNG',
          style = "bordered",
          color = "primary",
          no_outline = TRUE,
          icon = ph('image'),
          size = 'xs'
        ),
        downloadBttn(
          outputId = "download_pdf",
          label = 'PDF',
          style = "bordered",
          color = "primary",
          no_outline = TRUE,
          icon = ph('file-pdf'),
          size = 'xs'
        ),
        downloadBttn(
          outputId = "download_svg",
          label = 'SVG',
          style = "bordered",
          color = "primary",
          no_outline = TRUE,
          icon = ph('browsers'),
          size = 'xs'
        ),
        downloadBttn(
          outputId = "download_jpeg",
          label = 'JEPG',
          style = "bordered",
          color = "primary",
          no_outline = TRUE,
          icon = ph('image'),
          size = 'xs'
        ),
        downloadBttn(
          outputId = "download_pptx",
          label = 'PPTX',
          style = "bordered",
          color = "primary",
          no_outline = TRUE,
          icon = ph('projector-screen'),
          size = 'xs'
        )
      )
    )
  })
  
  output$download_png <- downloadHandler(
    filename = paste0('Boxplot-', Sys.Date(), '.png'),
    content = function(file) {
      ggsave(filename = file, plot = values$plot,
             width = input$boxplot_size$width/80,
             height = input$boxplot_size$height/80)
    }
  )
  output$download_pdf <- downloadHandler(
    filename = paste0('Boxplot-', Sys.Date(), '.pdf'),
    content = function(file) {
      ggsave(filename = file, plot = values$plot,
             width = input$boxplot_size$width/80,
             height = input$boxplot_size$height/80)
    },
    contentType = 'pdf'
  )
  output$download_svg <- downloadHandler(
    filename = paste0('Boxplot-', Sys.Date(), '.svg'),
    content = function(file) {
      ggsave(filename = file, plot = values$plot,
             width = input$boxplot_size$width/80,
             height = input$boxplot_size$height/80)
    }
  )
  output$download_jpeg <- downloadHandler(
    filename = paste0('Boxplot-', Sys.Date(), '.jpeg'),
    content = function(file) {
      ggsave(filename = file, plot = values$plot,
             width = input$boxplot_size$width/80,
             height = input$boxplot_size$height/80)
    },
    contentType = 'jpeg'
  )
  output$download_pptx <- downloadHandler(
    filename = paste0('Boxplot-', Sys.Date(), '.pptx'),
    content = function(file) {
      editable_graph <- rvg::dml(ggobj = values$plot)
      ppt <- officer::read_pptx() %>%
        officer::add_slide(layout = "Title and Content", master = "Office Theme") %>% 
        officer::ph_with(value = editable_graph,
                         location = officer::ph_location_type(type = "body")) %>% 
        print(target = file)
    }
  )
}