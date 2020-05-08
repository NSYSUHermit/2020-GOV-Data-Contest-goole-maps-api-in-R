#options(shiny.usecairo = FALSE)

#options(encoding = "UTF-8")
# font_home <- function(path = '') file.path('~', '.fonts', path)
# if (Sys.info()[['sysname']] == 'Linux' &&
#     system('locate wqy-zenhei.ttc') != 0 &&
#     !file.exists(font_home('wqy-zenhei.ttc'))) {
#     if (!file.exists('wqy-zenhei.ttc'))
#         shiny:::download(
#             'https://github.com/rstudio/shiny-examples/releases/download/v0.10.1/wqy-zenhei.ttc',
#             'wqy-zenhei.ttc'
#         )
#     dir.create(font_home())
#     file.copy('wqy-zenhei.ttc', font_home())
#     system2('fc-cache', paste('-f', font_home()))
# }
# rm(font_home)

library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(leaflet)

fluidPage(
    setBackgroundImage(src = "https://cw1.tw/CW/images/article/201610/article-58042bf9e4a22.jpg"),
    #theme = shinytheme("paper"),
    #themeSelector(),
    sidebarLayout(
        sidebarPanel(
            tags$h2("老兒共托選址系統"),
            width = 3,
            fluidRow(column(10, 
            radioButtons("radio", label = h3("選擇年度"),
                         c("104年"=1, "105年", "106年","107年","108年","109年(預測)","110年(預測)"), selected = 1),
            # sliderInput("slider11", label = h3(paste0("該區域扶老比至少幾%")),min = 0,max = 100, value = 0),
            # sliderInput("slider12", label = h3(paste0("該區域扶兒比至少幾%")),min = 0,max = 100, value = 0),
            )),
            fluidRow(column(10,
                selectInput("select", label = h3("選擇城市"), 
                            choices = list("宜蘭縣"=1, "花蓮縣"=4, "金門縣"=7, "南投縣"=10,"屏東縣"=13,"苗栗縣"=16, "桃園市"=19, 
                                           "高雄市"=21,"基隆市"=2, "連江縣"=5, "雲林縣"=8, "新北市"=11,"新竹市"=14, "新竹縣"=17, 
                                           "嘉義市"=10,"嘉義縣"=22,"彰化縣"=3, "臺中市"=6, "臺北市"=9, "臺東縣"=12, "臺南市"=15, 
                                           "澎湖縣"=18), 
                            selected = 9),
                selectInput("select1", label = h3("選擇鄉鎮市區"),
                            choices= list("信義區")),
            column(10,
                numericInput("num8", label = h3(paste0("請設定最小搜尋區域為方圓多少公尺")), value = 100)
            ))),
            fluidRow(
            useShinyjs(),
            
            tags$h4(strong("選址優勢條件")),
            uiOutput("input_value2"),
            tags$h4(strong("選址競爭條件")),
            uiOutput("input_value4"),
            uiOutput("input_value3")),
            # actionButton("all","全選"),
            # actionButton("all1","清除"),
            hr(),
            actionButton("action", "點我進行分析",class = "btn-primary")
        ),
        mainPanel(
            imageOutput("wait"),
            verbatimTextOutput("value"),
            leafletOutput(outputId = "leaf",width = "113%",height = 1150)
            
        )
    )
)


