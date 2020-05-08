#options(shiny.usecairo = FALSE)
source("D:/$/data/rscript/google_api.R", encoding = "utf-8")
#source("google_api.R", encoding = "utf-8")
#options(encoding = "UTF-8")
library(leaflet)
library(stringr)
library(tidyr)

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


function(input, output,session) {
    df3 = eventReactive(input$action,{
            city = as.character(city$city[which(city$num ==input$select)])
            bdd =get_bound_ltd(city,input$select1)
            df3 = town_ltd_info(city,input$select1,input$num8,
                                as.numeric(input$num1),as.numeric(input$num2),as.numeric(input$num3),as.numeric(input$num4),
                                as.numeric(input$num5),as.numeric(input$num6),as.numeric(input$num7),as.numeric(input$num9))%>%na.omit()
            print(df3[1:3,])
            df3 = df3[,c(1:12,which(str_detect(colnames(df3),str_sub(input$radio,1,3))))]
            if(input$radio == 1){df3 = df3[,-13]}
            df3 = df3[which(df3$clinc>=input$slider1),]
            df3 = df3[which(df3$hospital>=input$slider2),]
            df3 = df3[which(df3$hospital_center>=input$slider3),]
            df3 = df3[which(df3$park>=input$slider4),]
            df3 = df3[which(df3$library>=input$slider5),]
            df3 = df3[which(df3$mrt_station>=input$slider6),]
            df3 = df3[which(df3$bus_stop>=input$slider7),]
            df3 = df3[which(input$slider9>=df3$nursing),]
            print(1)
            print(df3[1:3,])
            if(nrow(df3)==0){output$value = renderPrint("<font size='4'><font color='red'>請降低條件</font></font><br/>")}
            print(df3[1:3,])
            df3$num = apply(data.frame(1:nrow(df3)),1,function(x){return(latlng_price(df3$Var1[x],df3$Var2[x],city)/10000)})
            print(2)
            print(df3[1:3,])
            #df3 = df3[-is.na(df3$Var1),]
            if(max(input$num10)!=0 & nrow(df3)>1){
                df3 = df3[which(df3$num >=min(input$num10) & max(input$num10) >=df3$num),]
                df3$index <- rowSums(0.2*(apply(df3[,5:11],2,nor.min.max)%>%t%>%na.omit()%>%t)+(0.2*nor.min.max(df3[,17])+0.8*nor.min.max(df3[,15]))*0.25-0.35*nor.min.max(df3$num))
            }else if(nrow(df3)>1 & max(input$num10)==0){
                df3$index <- rowSums(0.2*(apply(df3[,5:11],2,nor.min.max)%>%t%>%na.omit()%>%t)+(0.2*nor.min.max(df3[,17])+0.8*nor.min.max(df3[,15]))*0.25)
            }else if(nrow(df3)==1){
                df3$index = 0.5
            }
            print(df3[1:3,])
            if(max(df3$nursing)!=0 & nrow(df3)>1){
                df3$index = round(df3$index - 0.2*nor.min.max(df3$nursing)%>%t%>%na.omit()%>%t,2)
                df3$index = nor.min.max(df3$index)*0.8+0.05
            }else if(max(df3$nursing)==0 & nrow(df3)>1){
                df3$index = nor.min.max(df3$index)*0.8+0.05
            }
            print(df3[1:3,])
            df3$index = round(df3$index,2)
            df3$info <- paste0("<font size='4'><font color='red'>選址條件指數: ",df3$index,"</font></font><br/>")
            df3$info <- paste0(df3$info,"區域:",df3$village,"<br/>")
            if(input$num1 != 0){df3$info <- paste0(df3$info,"診所個數: ",df3$clinc,"<br/>")}
            if(input$num2 != 0){df3$info <- paste0(df3$info,"地區醫院個數: ",df3$hospital,"<br/>")}
            if(input$num3 != 0){df3$info <- paste0(df3$info,"醫學中心個數: ",df3$hospital_center,"<br/>")}
            if(input$num4 != 0){df3$info <- paste0(df3$info,"公園個數: ",df3$park,"<br/>")}
            if(input$num5 != 0){df3$info <- paste0(df3$info,"圖書館個數: ",df3$library,"<br/>")}
            if(input$num6 != 0){df3$info <- paste0(df3$info,"捷運站出口個數: ",df3$mrt_station,"<br/>")}
            if(input$num7 != 0){df3$info <- paste0(df3$info,"公車站牌個數: ",df3$bus_stop,"<br/>")}
            if(input$num9 != 0){df3$info <- paste0(df3$info,"養老院數量: ",df3$nursing,"<br/>")}
            df3$info <- df3$info%>%
                paste0(.,str_sub(colnames(df3)[13],2,7),": ",df3[,13],"<br/>")%>%
                paste0(.,str_sub(colnames(df3)[14],2,7),": ",df3[,14],"<br/>")%>%
                paste0(.,str_sub(colnames(df3)[15],2,7),": ",df3[,15],"<br/>")%>%
                paste0(.,str_sub(colnames(df3)[16],2,8),": ",df3[,16],"<br/>")%>%
                paste0(.,str_sub(colnames(df3)[17],2,7),": ",df3[,17],"<br/>")%>%
                paste0(.,"範圍內平均地價(單位:萬/坪):",df3[,18],"<br/>")

            print(df3)
            return(df3)
    })
    bdd = eventReactive(input$action,{
        bdd =get_bound_ltd(as.character(city$city[which(city$num ==input$select)]),input$select1)
        return(bdd)
        })
    #output$value = renderPrint({df3()$index})
    output$wait = eventReactive(input$action,{
        img(src="giphy.gif",align = "right",height="250px",width = '500px')
    })
    output$leaf = renderLeaflet({
        #rc1 <- colorRampPalette(colors = c("red", "white"), space = "Lab")(50)
        rc2 <- colorRampPalette(colors = c("#52B74B","#FF0000"), space = "Lab")(20)
        ## Combine the two color palettes
        #rampcols <- c(rc1, rc2)
        mypal <- colorNumeric(palette =rc2, domain = df3()$index)
        #previewColors(colorNumeric(palette = rc1, domain = NULL), values = 100:0)
        df3()%>%leaflet() %>% addTiles()%>%
                addPolygons(lng = bdd()$V1,
                            lat = bdd()$V2,
                            fillOpacity = 0,
                            weight = 1,
                            color = "red",
                            popup = ~as.factor(df3()$city))%>%
                #addMarkers(lng = ~df3()$Var2, lat = ~df3()$Var1,popup = ~as.factor(df3()$village),clusterOptions = markerClusterOptions())%>%
                addRectangles(
                    lng1=~df3()$Var2-input$num8*0.00001/2, lat1=~df3()$Var1-input$num8*0.000009090909/2,
                    lng2=~df3()$Var2+input$num8*0.00001/2, lat2=~df3()$Var1+input$num8*0.000009090909/2,
                    fillOpacity = 0.5,
                    fillColor = ~mypal(df3()$index),
                    color = "blue",
                    weight = 1,
                    group = NULL,
                    popup = ~as.factor(df3()$info))%>%
            addLegend(position = "bottomright", pal = mypal, values = df3()$index,
                                                             title = "選址條件指標",
                                                             opacity = 1)
    })
    output$input_value2 <- renderUI({
        fluidRow(
            lapply(1:7, function(k){
                column(width = 10,
                       output[[paste0('b', k)]] <- renderUI({tags$h4(name[k])}), 
                       checkboxInput(label = '必要條件',paste0('c', k)),
                       conditionalPanel(
                           condition =  paste0("input.c",k,"==1"),
                           splitLayout(
                           numericInput(paste0("num",k), label = h4(paste0("距離多少公尺?")), value = 0),
                           sliderInput(paste0("slider",k), label = h4(paste0("至少要有幾個?")), min = 0,max = 10, value = 0)))
                )
            })
        )
    })
    output$input_value4 =  renderUI({
        column(width = 10,
               output[["b10"]] <- renderUI({tags$h4("地價")}), 
               checkboxInput(label = '必要條件',paste0('c10')),
               conditionalPanel(
                   condition =  "input.c10==1",
                   numericRangeInput("num10", label = h4(paste0("地價容許範圍(單位:萬/坪)")),value=c(1,20))
        ))
    })
    output$input_value3 <- renderUI({
        column(width = 10,
               output[['b9']] <- renderUI({tags$h4("養老院")}),
               checkboxInput(label = '必要條件','c9'),
               conditionalPanel(
                   condition = "input.c9==1",
                   splitLayout(
                       numericInput("num9", label = h4("距離多少公尺?"), value = 0),
                       sliderInput("slider9", label = h4("至多能有幾個養老院?"), min = 0,max = 20, value = 20)))
               )
    })
    
    observe({
        updateSelectizeInput(session,"select1",choices = c(town(as.character(city$city[which(city$num==input$select)]))))
        #updateSliderInput(session,"slider12",value = 0,min = 0, max = 100 - as.numeric(input$slider11))
    })
}
