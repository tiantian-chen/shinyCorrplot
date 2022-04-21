shinyServer(
  function(input, output , session){
    observe({
      if(input$submit1 > 0){
        isolate({
          data.in <- input$uploadCorrplotData
          if(is.null(data.in)){
            sendSweetAlert(
              session = session,
              title = "Error!!",
              text = "The file connot be empty.",
              type = "error"
            )
            
          }else{
            data.in <- data.in$datapath
            output$corrplot <- renderPlot({
              file <- read.table(data.in, header = T, row.names = 1, sep = ",")
              file <- as.matrix(file)
              colors <- brewer.pal(11, input$var4)
              textRes <- cor.mtest(file, conf.level = 0.95)
              if (input$var5 == "T") {
                if (input$radio == "None") {
                  if (input$Coecol == "None"){
                    corrplot(file, method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = NULL , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var3, bg ="white",
                             is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
                    title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)
                  } else {
                    corrplot(file, method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = input$Coecol , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var3, bg ="white",
                             is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
                    title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)}
                  
                } else if (input$radio == "P-value") {
                if (input$Coecol == "None"){
                  corrplot(file, p.mat = textRes$p , sig.level = 0.05 , insig = input$pva, method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = NULL , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var3, bg ="white",
                           is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
                  title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)
                } else {
                  corrplot(file, p.mat = textRes$p , sig.level = 0.05 , insig = input$pva, method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = input$Coecol , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var3, bg ="white",
                         is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
                  title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)}
                  
                } else if (input$radio == "Confidence interval"){
                  if (input$Coecol == "None"){
                    corrplot(file, p.mat = textRes$p, lowCI = textRes$lowCI, uppCI = textRes$uppCI , plotC = 'rect', method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = NULL , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var3, bg ="white",
                             is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
                    title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)
                  } else {
                    corrplot(file, p.mat = textRes$p, lowCI = textRes$lowCI, uppCI = textRes$uppCI , plotC = 'rect', method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = input$Coecol , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var3, bg ="white",
                             is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
                    title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)}
                  }
              } else {
                if (input$Coecol == "None"){
                  corrplot(file, method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = NULL , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var6, bg ="white",
                           is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
                  title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)
                } else {
                  corrplot(file, method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = input$Coecol , cl.pos = input$Clpos, tl.cex = input$numm, tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var6, bg ="white",
                         is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
                title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)}
              }
            })
          }
          
        })
      }else{
        NULL
      }
    })
    output$downloadcorrplot.pdf <- downloadHandler(
      filename = function(){paste('corrplot.pdf')},
      content <- function(file){
        pdf(file, width = 10, height = 10)    
        file1 <- read.table(input$uploadCorrplotData$datapath, header = T, row.names = 1 , sep = ",")
        file1 <- as.matrix(file1)
        colors <- brewer.pal(11, input$var4)
        textRes <- cor.mtest(file1, conf.level = 0.95)
        if (input$var5 == "T") {
          if (input$radio == "None") {
            if (input$Coecol == "None"){
              corrplot(file1, method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = NULL , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var3, bg ="white",
                       is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
              title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)
            } else {
              corrplot(file1, method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = input$Coecol , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var3, bg ="white",
                       is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
              title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)}
            
          } else if (input$radio == "P-value") {
            if (input$Coecol == "None"){
              corrplot(file1, p.mat = textRes$p ,sig.level = 0.05 , insig = input$pva, method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = NULL , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var3, bg ="white",
                       is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
              title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)
            } else {
              corrplot(file1, p.mat = textRes$p ,sig.level = 0.05 , insig = input$pva, method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = input$Coecol , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var3, bg ="white",
                       is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
              title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)}
            
          } else if (input$radio == "Confidence interval"){
            if (input$Coecol == "None"){
              corrplot(file1, p.mat = textRes$p, lowCI = textRes$lowCI, uppCI = textRes$uppCI , plotC = 'rect', method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = NULL , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var3, bg ="white",
                       is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
              title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)
            } else {
              corrplot(file1, p.mat = textRes$p, lowCI = textRes$lowCI, uppCI = textRes$uppCI , plotC = 'rect', method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = input$Coecol , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var3, bg ="white",
                       is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
              title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)}
          }
        } else {
          if (input$Coecol == "None"){
            corrplot(file1, method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = NULL , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var6, bg ="white",
                     is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
            title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)
          } else {
            corrplot(file1, method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = input$Coecol , cl.pos = input$Clpos, tl.cex = input$numm, tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var6, bg ="white",
                     is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
            title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)}
        }
        dev.off()
        },
      contentType = "application/pdf")
    
    output$downloadcorrplot.svg <- downloadHandler(
      filename = function(){paste('corrplot.svg')},
      content <- function(file){
        svg(file, width = 10, height = 10)    
        file1 <- read.table(input$uploadCorrplotData$datapath, header = T, row.names = 1 , sep = ",")
        file1 <- as.matrix(file1)
        colors <- brewer.pal(11, input$var4)
        textRes <- cor.mtest(file1, conf.level = 0.95)
        if (input$var5 == "T") {
          if (input$radio == "None") {
            if (input$Coecol == "None"){
              corrplot(file1, method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = NULL , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var3, bg ="white",
                       is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
              title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)
            } else {
              corrplot(file1, method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = input$Coecol , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var3, bg ="white",
                       is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
              title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)}
            
          } else if (input$radio == "P-value") {
            if (input$Coecol == "None"){
              corrplot(file1, p.mat = textRes$p ,sig.level = 0.05 , insig = input$pva, method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = NULL , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var3, bg ="white",
                       is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
              title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)
            } else {
              corrplot(file1, p.mat = textRes$p ,sig.level = 0.05 , insig = input$pva, method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = input$Coecol , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var3, bg ="white",
                       is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
              title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)}
            
          } else if (input$radio == "Confidence interval"){
            if (input$Coecol == "None"){
              corrplot(file1, p.mat = textRes$p, lowCI = textRes$lowCI, uppCI = textRes$uppCI , plotC = 'rect', method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = NULL , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var3, bg ="white",
                       is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
              title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)
            } else {
              corrplot(file1, p.mat = textRes$p, lowCI = textRes$lowCI, uppCI = textRes$uppCI , plotC = 'rect', method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = input$Coecol , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var3, bg ="white",
                       is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
              title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)}
          }
        } else {
          if (input$Coecol == "None"){
            corrplot(file1, method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = NULL , cl.pos = input$Clpos, tl.cex = input$numm , tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var6, bg ="white",
                     is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
            title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)
          } else {
            corrplot(file1, method = input$var1, mar=c(0, 0, 1, 0), addCoef.col = input$Coecol , cl.pos = input$Clpos, tl.cex = input$numm, tl.col = input$Tlcol , rect.col = input$Recol , rect.lwd = input$nu , shade.col = input$shadecol ,shade.lwd = input$shadelwd , type = input$var2, order =input$var6, bg ="white",
                     is.corr = switch(input$var5,T = TRUE, F = FALSE), col = colors, addrect = input$num, tl.pos = input$var7)
            title(main = input$PlotTitle, cex.main = input$Titlesize, col.main = input$TitleColor)}
        }
        dev.off()},
      contentType = "application/svg")
    
    # download exampl data
    output$Download <- downloadHandler(
      filename <- function(){
        paste("test1.csv")
      },
      content <- function(file) {
        input_file <- "test1.csv"
        example <- read.csv(input_file, header = F, as.is = T, sep = "\t", quote = "")
        write.csv(example, file = file, row.names = F, quote = F)
      }, contentType = 'text/csv')
    
    
    observe({
      if(input$submit10 > 0){
        isolate({
          datainn <- input$Uploaddataa
          if(is.null(datainn)){
            sendSweetAlert(
              session = session,
              title = "Error!!",
              text = "The file connot be empty.",
              type = "error")
          } else {
            datainn <- datainn$datapath
            output$corrplotmixed <- renderPlot({
              filee <- read.table(datainn, header = T, row.names = 1, sep = ",")
              filee <- as.matrix(filee)
              colorss <- brewer.pal(9, input$var02)
              colores <- brewer.pal(11, input$var04)
              textRess <- cor.mtest(filee, conf.level = 0.95)
              if(input$var05 == "T"){
                if(input$radi == "None") {
                  if(input$Coecol0 == "None"){
                    
                    corrplot.mixed(filee, mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0 , shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , 
                                   order = input$var07, bg ="white", tl.pos = input$var03, addCoef.col = NULL, lower.col = colorss, upper.col = colores, is.corr = switch(input$var05,T = TRUE, F = FALSE))
                    title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)
                    
                  } else {
                    
                    corrplot.mixed(filee, mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0, shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , 
                                   order = input$var07, bg ="white", tl.pos = input$var03, addCoef.col = input$Coecol0, lower.col = colorss, upper.col = colores,is.corr = switch(input$var05,T = TRUE, F = FALSE))
                    title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)} 
                  
                  } else if(input$radi == "P-value") {
                    
                    if(input$Coecol0 == "None"){
                      corrplot.mixed(filee, p.mat = textRess$p , sig.level = 0.05 , insig = input$pa, mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0 , shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , 
                                     order = input$var07, bg ="white", tl.pos = input$var03, addCoef.col = NULL, lower.col = colorss, upper.col = colores, is.corr = switch(input$var05,T = TRUE, F = FALSE))
                      title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)
                      
                    } else {
                      corrplot.mixed(filee, p.mat = textRess$p , sig.level = 0.05 , insig = input$pa, mar=c(0, 0, 1, 0), p.mat = textRess$p ,sig.level = 0.10 , upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0, shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , 
                                     order = input$var07, bg ="white", tl.pos = input$var03, addCoef.col = input$Coecol0, lower.col = colorss, upper.col = colores,is.corr = switch(input$var05,T = TRUE, F = FALSE))
                      title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)}
                    
                    } else if(input$radi == "Confidence interval") {
                      
                      if(input$Coecol0 == "None"){
                        corrplot.mixed(filee, p.mat = textRess$p, lowCI = textRess$lowCI, uppCI = textRess$uppCI , plotC = 'rect', mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0, shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , 
                                 order = input$var06, bg ="white", tl.pos = input$var03, addCoef.col = NULL, lower.col = colorss, upper.col = colores,is.corr = switch(input$var05,T = TRUE, F = FALSE))
                        title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)
                        
                        } else {
                          corrplot.mixed(filee, p.mat = textRess$p, lowCI = textRess$lowCI, uppCI = textRess$uppCI , plotC = 'rect', mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0, shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , 
                                 order = input$var06, bg ="white", tl.pos = input$var03, addCoef.col = input$Coecol0, lower.col = colorss, upper.col = colores,is.corr = switch(input$var05,T = TRUE, F = FALSE))
                          title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)}
                    }
                } else {
                  if(input$Coecol0 == "None"){
                    
                    corrplot.mixed(filee, mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0 , shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , order = input$method, 
                                       bg ="white", tl.pos = input$var03, addCoef.col = NULL, lower.col = colorss, upper.col = colores,is.corr = switch(input$var05,T = TRUE, F = FALSE))
                        title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)
                        
                      } else {
                        
                        corrplot.mixed(filee, mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0 , shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , order = input$method, 
                                       bg ="white", tl.pos = input$var03, addCoef.col = input$Coecol0, lower.col = colorss, upper.col = colores,is.corr = switch(input$var05,T = TRUE, F = FALSE))
                        title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)}
                      }
              
            
              })
          }
        })
      } else {
        NULL
      }
    })
    
    output$download.pdf <- downloadHandler(
      filename = function(){paste('corrplot.pdf')},
      content = function(filee){
        pdf(filee, width = 10, height = 10)
        filee1 <- read.table(input$Uploaddataa$datapath, header = T, row.names = 1 , sep = ",")
        filee1 <- as.matrix(filee1)
        colorss <- brewer.pal(11, input$var02)
        colores <- brewer.pal(11, input$var04)
        textRess <- cor.mtest(filee1, conf.level = 0.95)
        if(input$var05 == "T"){
          if(input$radi == "None") {
            if(input$Coecol0 == "None"){
              
              corrplot.mixed(filee1, mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0 , shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , 
                             order = input$var07, bg ="white", tl.pos = input$var03, addCoef.col = NULL, lower.col = colorss, upper.col = colores, is.corr = switch(input$var05,T = TRUE, F = FALSE))
              title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)
              
            } else {
              
              corrplot.mixed(filee1, mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0, shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , 
                             order = input$var07, bg ="white", tl.pos = input$var03, addCoef.col = input$Coecol0, lower.col = colorss, upper.col = colores,is.corr = switch(input$var05,T = TRUE, F = FALSE))
              title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)} 
            
          } else if(input$radi == "P-value") {
            
            if(input$Coecol0 == "None"){
              corrplot.mixed(filee1, p.mat = textRess$p , sig.level = 0.05 , insig = input$pa, mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0 , shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , 
                             order = input$var07, bg ="white", tl.pos = input$var03, addCoef.col = NULL, lower.col = colorss, upper.col = colores, is.corr = switch(input$var05,T = TRUE, F = FALSE))
              title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)
              
            } else {
              corrplot.mixed(filee1, p.mat = textRess$p , sig.level = 0.05 , insig = input$pa, mar=c(0, 0, 1, 0), p.mat = textRess$p ,sig.level = 0.10 , upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0, shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , 
                             order = input$var07, bg ="white", tl.pos = input$var03, addCoef.col = input$Coecol0, lower.col = colorss, upper.col = colores,is.corr = switch(input$var05,T = TRUE, F = FALSE))
              title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)}
            
          } else if(input$radi == "Confidence interval") {
            
            if(input$Coecol0 == "None"){
              corrplot.mixed(filee1, p.mat = textRess$p, lowCI = textRess$lowCI, uppCI = textRess$uppCI , plotC = 'rect', mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0, shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , 
                             order = input$var06, bg ="white", tl.pos = input$var03, addCoef.col = NULL, lower.col = colorss, upper.col = colores,is.corr = switch(input$var05,T = TRUE, F = FALSE))
              title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)
              
            } else {
              corrplot.mixed(filee1, p.mat = textRess$p, lowCI = textRess$lowCI, uppCI = textRess$uppCI , plotC = 'rect', mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0, shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , 
                             order = input$var06, bg ="white", tl.pos = input$var03, addCoef.col = input$Coecol0, lower.col = colorss, upper.col = colores,is.corr = switch(input$var05,T = TRUE, F = FALSE))
              title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)}
          }
        } else {
          if(input$Coecol0 == "None"){
            
            corrplot.mixed(filee1, mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0 , shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , order = input$method, 
                           bg ="white", tl.pos = input$var03, addCoef.col = NULL, lower.col = colorss, upper.col = colores,is.corr = switch(input$var05,T = TRUE, F = FALSE))
            title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)
            
          } else {
            
            corrplot.mixed(filee1, mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0 , shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , order = input$method, 
                           bg ="white", tl.pos = input$var03, addCoef.col = input$Coecol0, lower.col = colorss, upper.col = colores,is.corr = switch(input$var05,T = TRUE, F = FALSE))
            title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)}
        }
        dev.off() },
      contentType = "application/pdf")
    
    output$download.svg <- downloadHandler(
      filename = function(){paste('corrplot.svg')},
      content = function(filee){
      svg(filee, width = 10, height = 10)
      filee1 <- read.table(input$Uploaddataa$datapath, header = T, row.names = 1 , sep = ",")
      filee1 <- as.matrix(filee1)
      colorss <- brewer.pal(11, input$var02)
      colores <- brewer.pal(11, input$var04)
      textRess <- cor.mtest(filee1, conf.level = 0.95)
      if(input$var05 == "T"){
        if(input$radi == "None") {
          if(input$Coecol0 == "None"){
            
            corrplot.mixed(filee1, mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0 , shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , 
                           order = input$var07, bg ="white", tl.pos = input$var03, addCoef.col = NULL, lower.col = colorss, upper.col = colores, is.corr = switch(input$var05,T = TRUE, F = FALSE))
            title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)
            
          } else {
            
            corrplot.mixed(filee1, mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0, shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , 
                           order = input$var07, bg ="white", tl.pos = input$var03, addCoef.col = input$Coecol0, lower.col = colorss, upper.col = colores,is.corr = switch(input$var05,T = TRUE, F = FALSE))
            title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)} 
          
        } else if (input$radi == "P-value") {
          
          if(input$Coecol0 == "None"){
            corrplot.mixed(filee1, p.mat = textRess$p , sig.level = 0.05 , insig = input$pa , mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0 , shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , 
                           order = input$var07, bg ="white", tl.pos = input$var03, addCoef.col = NULL, lower.col = colorss, upper.col = colores, is.corr = switch(input$var05,T = TRUE, F = FALSE))
            title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)
            
          } else {
            corrplot.mixed(filee1, p.mat = textRess$p , sig.level = 0.05 , insig = input$pa, mar=c(0, 0, 1, 0), p.mat = textRess$p ,sig.level = 0.10 , upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0, shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , 
                           order = input$var07, bg ="white", tl.pos = input$var03, addCoef.col = input$Coecol0, lower.col = colorss, upper.col = colores,is.corr = switch(input$var05,T = TRUE, F = FALSE))
            title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)}
          
        } else if(input$radi == "Confidence interval") {
          
          if(input$Coecol0 == "None"){
            corrplot.mixed(filee1, p.mat = textRess$p, lowCI = textRess$lowCI, uppCI = textRess$uppCI , plotC = 'rect', mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0, shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , 
                           order = input$var06, bg ="white", tl.pos = input$var03, addCoef.col = NULL, lower.col = colorss, upper.col = colores,is.corr = switch(input$var05,T = TRUE, F = FALSE))
            title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)
            
          } else {
            corrplot.mixed(filee1, p.mat = textRess$p, lowCI = textRess$lowCI, uppCI = textRess$uppCI , plotC = 'rect', mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0, shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , 
                           order = input$var06, bg ="white", tl.pos = input$var03, addCoef.col = input$Coecol0, lower.col = colorss, upper.col = colores,is.corr = switch(input$var05,T = TRUE, F = FALSE))
            title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)}
        }
      } else {
        if(input$Coecol0 == "None"){
          
          corrplot.mixed(filee1, mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0 , shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , order = input$method, 
                         bg ="white", tl.pos = input$var03, addCoef.col = NULL, lower.col = colorss, upper.col = colores,is.corr = switch(input$var05,T = TRUE, F = FALSE))
          title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)
          
        } else {
          
          corrplot.mixed(filee1, mar=c(0, 0, 1, 0), upper = input$upper, lower = input$lower, tl.cex = input$numm0, tl.col = input$Tlcol0 , shade.col = input$shadecol0 ,shade.lwd = input$shadelwd0 , order = input$method, 
                         bg ="white", tl.pos = input$var03, addCoef.col = input$Coecol0, lower.col = colorss, upper.col = colores,is.corr = switch(input$var05,T = TRUE, F = FALSE))
          title(main = input$PlotTitle0, cex.main = input$Titlesize0, col.main = input$TitleColor0)}
      }
      dev.off() },
      contentType = "application/svg")
    
    # download exampl data
    output$Downloade <- downloadHandler(
      filename <- function(){
        paste("test1.csv")
      },
      content <- function(filee) {
        input_filee <- "test1.csv"
        examplee <- read.csv(input_filee, header = F, as.is = T, sep = "\t", quote = "")
        write.csv(examplee, file = filee, row.names = F, quote = F)
      }, contentType = 'text/csv')

 
  }
)