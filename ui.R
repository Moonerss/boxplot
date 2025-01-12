## Create Date: 2022/2/25
## Author: Erjie Zhao
## Purpose: UI of box plot

library(shiny)
library(esquisse)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(shinyjqui)##图片拖拽大小
library(datamods)
library(htmltools)

# Header ----------------------------------
header <- dashboardHeader(title = 'Boxplot', titleWidth = 200, disable = F)


# sidebar ---------------------------------
sidebar <- dashboardSidebar(width = 200, disable = F, collapsed = F,
                            sidebarMenu(
                              menuItem('Boxplot', tabName = 'Boxplot', icon = icon("meteor"))
                            )
)

# left panel tab 1 ---------------------------------
left_panel_tab1 <- tabPanel(title = '输入文件',
                            # br(),
                            # strong('载入示例数据'),
                            # br(),
                            # br(),
                            # actionButton(inputId = 'demo', label = 'Demo',
                            #               style="color:#fff;background-color:#337ab7;border-color:#2e6da4"),
                            br(),
                            fluidRow(column(width = 8, fileInput("file", '输入绘图数据', buttonLabel = "上传文件", multiple = FALSE,
                                                                 placeholder = 'Demo_Boxplot.xlsx',
                                                                 accept = c(".txt",".csv",".xls",".xlsx"))),
                                     column(width = 4, style='padding: 24px;float:left;', downloadButton("downloaddemo","下载示例文件"))),
                            hr(),
                            # strong('设置分组信息'),
                            # br(),
                            # # downloadButton("downloaddemo","分组文件下载"),
                            # p('1.分组文件：根据您的文字自动生成'),
                            # p('2.下载后在【组名】列修改信息'),
                            # p('3.上传即可自动按分组信息进行配色'),
                            # p('*** 图例中分组的顺序与您所填分组信息顺序一致'),
                            fluidRow(column(width = 8, fileInput("group", '输入分组信息', buttonLabel = "上传分组信息", placeholder = 'Group.xlsx')),
                                     column(width = 4, style='padding: 24px;float:left;', downloadButton("downloadgroup","下载分组文件"))),
                            hr(),
                            strong('数据处理'),
                            checkboxInput('log10', '对数据做log10转换', value = T),
                            p('默认对所有数据加1再做log10处理，以避免计算log10(0)'),
                            radioButtons("plot_by_group", "绘图选项", c('按组配色' = 'FALSE', '按组绘制' = 'TRUE'), inline = T)
)


# left panel tab 2 -----------------------------
left_panel_tab2 <- tabPanel(title = 'p值计算',
                            br(),
                            #checkboxInput('log10', '是否为配对样本?', value = F),
                            #hr(),
                            checkboxInput('multiple', strong('是否进行多组比较?'), value = F),
                            uiOutput(outputId = 'multiple_compare'),
                            hr(),
                            checkboxInput('two_compare', strong('是否进行两两比较?'), value = F),
                            uiOutput(outputId = 'two_compare'),
                            # hr(),
                            # strong('设置比较组信息'),
                            # # downloadButton("downloaddemo","下载比较组示例文件"),
                            # p('1.比较组文件：根据您的文字自动生成'),
                            # p('2.下载后自行修改比较组，前后位置可以任意'),
                            # p('3.上传即可自动按比较组信息增加组件p值信息'),
                            # fileInput("file", '输入文件', buttonLabel = "上传比较组文件", placeholder = 'compare_group.xlsx')
)


# left panel tab 1 -------------
left_panel_tab3 <- tabPanel(title = '主要参数',
                            br(),
                            fluidRow(column(width = 4, selectInput(inputId = 'theme', label = i18n('背景主题'), choices = esquisse:::get_themes(),
                                                                   selected = "theme_classic")),
                                     #options = list(size = 10, container = "body"), width = "100%")),
                                     column(width = 4, selectInput(inputId = 'type', label = i18n('图形效果'), c('none', 'dotplot', 'jitter'), selected = 'none')),
                                     column(width = 4, fluidPage(checkboxInput('show_outliers', strong('绘制离群点'), value = T),
                                                                 checkboxInput('show_errorbar', strong('绘制误差棒'), value = T)))
                                     # style='padding: 24px;float:left;')
                            ),
                            fluidRow(column(width = 3, textInput("title", "图片主标题", value = 'Boxplot')),
                                     column(width = 3, textInput("xlab", "x轴标题", value = 'Sample')),
                                     column(width = 3, textInput("ylab", "y轴标题", value = 'Expression')),
                                     column(width = 3, textInput("legend_title", "填充色标题", value = 'Group'))),
                            br(),
                            fluidRow(column(width = 4, sliderInput('rect', 'x标签的角度', value = 45, min = 0, max = 90, step = 15)),
                                     #column(width = 8, numericRangeInput(inputId = 'ylim', label = 'Y轴区间:', value = c(NA, NA)))),
                                     column(width = 4, numericInput('y_min', 'y轴最小值', value = NA, min = 0, max = 100000)),
                                     column(width = 4, numericInput('y_max', 'y轴最大值', value = NA, min = 0, max = 100000))),
                            fluidRow(column(width = 4, radioButtons('show_legend', '图例', c('显示', '隐藏'), inline = T, selected = '显示')),
                                     column(width = 3, selectInput(inputId = 'legend_in', label = i18n('图例位置'), c('坐标系外', '坐标系内'), selected = '坐标系外')),
                                     column(width = 5, uiOutput('legend_position'))
                            ),
                            #column(width = 4, selectInput(inputId = 'legend_position', label = i18n('坐标系外 选项：'), c('上', '下', '左', '右'), selected = '右'))),
                            fluidRow(column(width = 3, numericInput('title_size', '主标题大小', min = 0, max = 100, value = 20)),
                                     column(width = 3, numericInput('labs_title_size', '坐标轴标题大小', min = 0, max = 100, value = 15)),
                                     column(width = 3, numericInput('legend_title_size', '图例度标题大小', min = 0, max = 100, value = 15)),
                                     column(width = 3, numericInput('axis_font_size', '刻度文本大小', min = 0, max = 100, value = 10))),
                            hr(),
                            #h5(strong('设置颜色 参考： 颜色值（十六进制） ; 颜色名称')),
                            h5(strong('颜色设置')),
                            palette_ui('colors')
)

# middle panel tab 2 --------------------------------
# middle_panel_tab2 <- tabPanel('参考文献',
#                               h5(strong('anova')),
#                               p('1. Chambers, J. M. and Hastie, T. J. (1992) Statistical Models in S, Wadsworth & Brooks/Cole.'),
#                               h5(strong('wilcox.test')),
#                               p('1. David F. Bauer (1972). Constructing confidence sets using rank statistics. Journal of the American Statistical Association 67, 687–690. doi: 10.1080/01621459.1972.10481279.'),
#                               p('2. Myles Hollander and Douglas A. Wolfe (1973). Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 27–33 (one-sample), 68–75 (two-sample).'),
#                               h5(strong('kruskal.test')),
#                               p('1. Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.')
# )

# middle panel tab 3 --------------------------------
middle_panel_tab2 <- tabPanel('分析方法',
                              tags$div(
                                align = 'center',
                                tags$br(),
                                tableOutput(outputId = 'methods')
                              )
                              )

# middle panel tab 1 --------------------------------
middle_panel_tab1 <- tabPanel('图片&下载',
                              # tags$div(
                              #   align = 'center',
                              #   tags$br(),
                              #   textInputIcon(
                              #     inputId = "filename",
                              #     label = NULL,
                              #     value = "export-plot",
                              #     placeholder = i18n("Filename"),
                              #     icon = list(i18n("下载文件名")),
                              #     width = "60%"
                              #   )
                              # ),
                              ## 使下载标签在有图片的时候显现
                              uiOutput(outputId = 'download_label'),
                              tags$div(
                                align = 'center',
                                tags$br(),
                                jqui_resizable(plotOutput(outputId = "boxplot", width = 400, height = 400))
                              )
)

# right panel tab 1 --------------------------------
right_panel_tab1 <- tabPanel('输入数据',
                             h4('绘图数据', align = 'center'),
                             DTOutput(outputId = 'data_output'),
                             h4('分组数据', align = 'center'),
                             DTOutput(outputId = 'group_output')
)

# right panel tab 2 --------------------------------
right_panel_tab2 <- tabPanel('使用教程',
                             div(
                               style="margin:0px 12px;max-height: 800px; overflow-y: auto;",
                               h4(align="center",tags$b("使用教程")),
                               tags$div(
                                 h4(strong('简介：')),
                                 p('本工具适用于绘制不同样本或不同组别之间蛋白表达量或其他连续数据之间数值分布的箱线图比较。'),
                                 h4(strong('适用范围：')),
                                 p('适用于蛋白组、修饰组、转录组、代谢组、16S、实验结果采集等所有给出二维数字矩阵的生物学数据。'),
                                 h4(strong('输入：')),
                                 p('输入文件主要有两个：(1) ', strong('样本的表达量数据'), '；(2) ', strong('样本的分组数据')),
                                 p('输入的文件格式包括txt(制表符分隔)文本文件、csv(逗号分隔)文本文件、以及Excel专用的xlsx格式。'),
                                 h5(strong('表达量数据')),
                                 p('表达量数据需要包含以下信息：'),
                                 div(align="center", tags$img(src = "data.png", width = "100%", height = "100%")),
                                 tags$br(),
                                 p('可以下载Demo数据文件进行修改，重新上传'),
                                 h5(strong('分组数据')),
                                 p('分组数据需要包含以下信息：'),
                                 div(align="center", tags$img(src = "group.png", width = "50%", height = "50%")),
                                 tags$br(),
                                 p('可以下载Demo数据文件进行修改，重新上传'),
                                 h4(strong('下载：')),
                                 p('可拖拽以改变图片大小和比例'),
                                 div(align="center", tags$img(src = "pic.png", width = "80%", height = "80%")),
                                 tags$br(),
                                 p('点击上部的不同下载按钮，可以下载不同格式的图片文件'),
                                 div(align="center", tags$img(src = "download.png", width = "80%", height = "80%")),
                                 h4(strong('已传数据预览：')),
                                 p('可以在右侧的输入数据栏中查看已上传的数据，默认都只展示前五行的信息。'),
                                 tags$br(),
                                 tags$br()
                               )
                             )
)

# right panel tab 3 --------------------------------
right_panel_tab3 <- tabPanel('版本说明',
                             h4('v1.0.0'),
                             p('1. 该版本实现主要的绘图功能和比较组差异计算功能')
)

# body ------------------------------------
body <- dashboardBody(
  theme = shinytheme(theme="flatly"),
  style="background-color:white; font-size: 90%; ",##将全局默认背景改为白色
  tabItems(
    tabItem('Boxplot',
            fluidRow(column(width = 4, h4('参数'), tabsetPanel(left_panel_tab1, left_panel_tab2, left_panel_tab3)),
                     column(width = 5, h4('结果与方法'), tabsetPanel(middle_panel_tab1, middle_panel_tab2)),
                     column(width = 3, h4('说明'), tabsetPanel(right_panel_tab1, right_panel_tab2, right_panel_tab3)),
                     style = "height:849px"
            )
    )
  )
)

# dashboard ---------------------------------------------------------------
ui <- dashboardPage(title = 'Boxplot', skin = "blue", header = header, sidebar = sidebar, body = body)