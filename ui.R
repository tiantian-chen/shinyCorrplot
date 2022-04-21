useSweetAlert()
shinyUI(
 navbarPage(
  title="Corrplot",
  theme=shinytheme("lumen"),
  windowTitle="make corrplot in R with shiny",
  
  tabPanel(
    "Corrplot",
    
    sidebarPanel(
                 fileInput("uploadCorrplotData", h4("Upload Data:",
                                                      bsButton("bs0", label="", icon=icon("question"), style="info", size="small")),multiple = FALSE),
                 bsPopover("bs0", 'Upload Data', "The first row and the first column of the uploaded file must have corresponding row and column names.", trigger = "focus"),
                 downloadButton("Download", "Download example data"),
                 
      radioButtons("var5",
                   h4("visualize matrix",
                      bsButton("bsq1", label="", icon=icon("question"), style="info", size="small")),  
                   choices = list("Correlation matrix" = "T", "Non-correlation matrix" = "F"), selected = "T"),
      bsPopover("bsq1", "If the input matrix is a correlation matrix, please select the first, If not, select second.",trigger = "focus"),
      
      selectInput("var1",
                  h4("Method of display", 
                     bsButton("bsq2", label="", icon=icon("question"), style="info", size="small")),
                  choices = c("circle","square","ellipse","number","shade","color","pie")),
      bsPopover("bsq2", "There are seven visualization methods (parameter method) in the corolot package.You can choose a visualization method to display.", trigger = "focus"),
      selectInput("var2",
                  h4("Layout of display",
                     bsButton("bsq3", label="", icon=icon("question"), style="info", size="small")),
                  choices = c("full","upper","lower")),
      bsPopover("bsq3", "There are three layout types (parameter types): full(default): display the full correlation matrix, upper: display the upper triangle of the correlation matrix, lower: display the lower triangle of the correlation matrix", trigger = "focus"),
      conditionalPanel(
        condition = "input.var5 == 'T'",
        selectInput("var3",
                    h4("Method of reorder", bsButton("bsq4", label="", icon=icon("question"), style="info", size="small")), 
                    choices = c("original", "AOE", "FPC", "hclust", "alphabet")),
        bsPopover("bsq4", "There are 5 methods in the corolot (parameter order).", trigger = "focus"),
        selectInput("radio",
                    h4("P-value and Confidence interval",
                       bsButton("bsq23", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("None", "P-value" , "Confidence interval" ), "None"),
        bsPopover("bsq23", 'visualization plot(P-value and Confidence interval)',"Including visualization of P-value and Confidence interval.",trigger = "focus")),
      
      conditionalPanel(
        condition = "input.var5 == 'F'",
        selectInput("var6",
                    h4("Method of reorder", bsButton("bsq5", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("original", "FPC", "hclust")),
        bsPopover("bsq5", "There are 3 methods in the corolot (parameter order).", trigger = "focus")),
      
      conditionalPanel(
        condition = "input.radio == 'P-value'",
        selectInput("pva",
                    h5("Insig Character",
                       bsButton("bsq24", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("pch", "p-value" , "blank" , "label_sig")),
        bsPopover("bsq24", 'Adjust P-value',"Including P-value with insig Character.",trigger = "focus")),
      conditionalPanel(
        condition = "input.var3 == 'hclust'&&input.var2 == 'full'",
        numericInput("num",
                     h5("Number of rectangle.", 
                        bsButton("bsq6", label="", icon=icon("question"), style="info", size="small")),value = 1,min=1,max=11,step=1),
        selectInput("Recol",
                    h5("Color of rectangle.",
                       bsButton("bsq7", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("red", "black", "green", "pink", "orange", "yellow", "blue", "purple"), "black"),
        sliderInput("nu",
                     h5("Line width of rectangle",
                        bsButton("bsq8", label="", icon=icon("question"), style="info", size="small")),value = 1,min=1,max=10),
        bsPopover("bsq6", "Only from 1 to 11 can be selected.", trigger = "focus"),
        bsPopover("bsq7", "Choose the color you want.", trigger = "focus"),
        bsPopover("bsq8", "Only from 1 to 10 can be selected.", trigger = "focus")),
      conditionalPanel(
        condition = "input.var1 == 'shade'",
        sliderInput("shadelwd",
                    h5("Line width of shade",
                        bsButton("bsq9", label="", icon=icon("question"), style="info", size="small")),value = 1,min=1,max=10),
        selectInput("shadecol",
                    h5("Color of shade.",
                       bsButton("bsq10", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("red", "black", "green", "pink", "orange", "yellow", "blue", "purple"), "black"),
        
        bsPopover("bsq9", "Only from 1 to 10 can be selected.", trigger = "focus"),
        bsPopover("bsq10", "Choose the color of shade line you want.", trigger = "focus")),
      radioButtons("radioB",
                   h4("Adjust corrplot details",
                      bsButton("bsq11", label="", icon=icon("question"), style="info", size="small")),
                   choices = c("Null","Plot title", "Plot Lable","Plot color", "Legend position", "Coefficient and color"), "Null"),
      bsPopover("bsq11", 'Adjust plot details',"Including the content, size and color of the title, the size and color of the label, the color of the plot , the position of the legend and the Coefficient whith color.",trigger = "focus"),
      conditionalPanel(
        condition = "input.radioB == 'Plot title'",
        textInput("PlotTitle", h5("Plot Title:",
                                  bsButton("bs12", label = "", icon = icon("question"), style = "info", size = "small")
        ), value = c("Corrplot")),
        bsPopover("bs12", "You can customize the title of the plot", trigger = "focus"),
        selectInput("TitleColor", h5("Title Color",
                       bsButton("bsq13", label = "", icon = icon("question"), style = "info", size = "small")),
                    choices = c("red", "black", "green", "pink", "orange", "yellow", "blue", "purple"), "red"),
        bsPopover("bsq13", "Choose color of title", trigger = "focus"),
        sliderInput("Titlesize", h5("Title size",
                       bsButton("bsq14", label="", icon=icon("question"), style="info", size="small")),value = 1,min=1,max=10),
        bsPopover("bsq14", "You can customize the size of title", trigger = "focus")),
      conditionalPanel(
        condition = "input.radioB == 'Plot Lable'",
        selectInput("Tlcol",
                    h5("Color of label",
                       bsButton("bsq15", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("red", "black", "green", "pink", "orange", "yellow", "blue", "purple"), "red"),
        bsPopover("bsq15", "Choose color of label", trigger = "focus"),
        numericInput("numm",
                     h5("Size of label",
                        bsButton("bsq16", label="", icon=icon("question"), style="info", size="small")), value = 1,min=1,max=3,step=1),
        bsPopover("bsq16", "Choose the size of lable from 1 to 3.",trigger = "focus"),
        selectInput("var7",
                    h5("position of text labels", bsButton("bsq20", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("left and top" = "lt","diagonal" = "d","none" = "n"),"left and top"),
        bsPopover("bsq20", "There are three position of text labels: left and top, diagonal and none", trigger = "focus")),
      conditionalPanel(
        condition = "input.radioB == 'Plot color'",
        selectInput("var4", h5("Plot color",
                               bsButton("bsq17", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")),
        bsPopover("bsq17", "Choose the color you want.", trigger = "focus")),
      conditionalPanel(
        condition = "input.radioB == 'Legend position'",
        selectInput("Clpos", h5("Legend position",
                                bsButton("bsq18", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("Right"="r", "Bottom"="b", "Null"="n"), "Right"),
        bsPopover("bsq18", "Choose the location of legend.", trigger = "focus")),
      conditionalPanel(
        condition = "input.radioB == 'Coefficient and color'",
        selectInput("Coecol",
                    h5("Coefficient and color",
                       bsButton("bsq19", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("None","red", "black", "green", "pink", "orange", "yellow", "blue", "purple", "grey")),
        bsPopover("bsq19", "Choose the coefficient with color.", trigger = "focus")),

      
      actionButton("submit1", strong("Go!")) 
      ),
    
    mainPanel(
      downloadButton("downloadcorrplot.pdf","Download pdf-file"),
      downloadButton("downloadcorrplot.svg","Download svg-file"),
      jqui_resizable(plotOutput("corrplot", width = "60%", height = "700px"))
    )
    
  ),
  
  tabPanel(
    "Corrplot Mixed",
    sidebarPanel(
      fileInput("Uploaddataa",
                h4("Upload Data:", bsButton("bsq01", label="", icon=icon("question"), style="info", size="small")),
      ),
      bsPopover("bsq01", 'Upload Data', "The first row and the first column of the uploaded file must have corresponding row and column names.", trigger = "focus"),
      downloadButton("Downloade", "Download example data"),
      radioButtons("var05",
                   h4("visualize matrix",
                      bsButton("bsq104", label="", icon=icon("question"), style="info", size="small")),  
                   choices = list("Correlation matrix" = "T", "Non-correlation matrix" = "F"), selected = "T"),
      bsPopover("bsq104", "If the input matrix is a correlation matrix, please select the first, If not, select second.",trigger = "focus"),
      conditionalPanel(
        condition = "input.var05 == 'F'",
        selectInput("var06", 
                    h4("Method of reorder", bsButton("bsq105", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("original", "FPC", "hclust")),
        bsPopover("bsq105", "There are 3 methods in the corolot (parameter order).",trigger = "focus")),
      
      conditionalPanel(
        condition = "input.var05 == 'T'",
        selectInput("var07", 
                    h4("Method of reorder",
                       bsButton("bsq04", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("original", "AOE", "FPC", "hclust", "alphabet")),
        bsPopover("bsq04", "There are 5 methods in the corolot (parameter order).", trigger = "focus"),
        selectInput("radi",
                    h4("P-value and Confidence interval",
                       bsButton("bsq023", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("None", "P-value" , "Confidence interval" ), "None"),
        bsPopover("bsq023", 'visualization plot',"Including visualization of P-value and Confidence interval.",trigger = "focus"),
        ),
      
      selectInput("upper",
                  h5("Upper plot",bsButton("bsq02", label="", icon=icon("question"), style="info", size="small")),
                  choices = c("circle","square","ellipse","number","shade","color","pie"),"square"),
      bsPopover("bsq02", "Choose upper plot", trigger = "focus"),
      selectInput("lower",
                  h5("Lower plot",bsButton("bsq03", label="", icon=icon("question"), style="info", size="small")),
                  choices = c("circle","square","ellipse","number","shade","color","pie")),
      bsPopover("bsq03", "Choose lower plot", trigger = "focus"),
      
      conditionalPanel(
        condition = "input.radi == 'P-value'",
        selectInput("pa",
                    h5("Insig Character",
                       bsButton("bsq024", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("pch", "p-value" , "blank" , "label_sig")),
        bsPopover("bsq024", 'Adjust P-value',"Including P-value with insig Character.",trigger = "focus"), 
      ),
      
      conditionalPanel(
        condition = "input.upper == 'shade'|input.lower == 'shade'",
        
        sliderInput("shadelwd0",
                    h5("Line width of shade",
                       bsButton("bsq106", label="", icon=icon("question"), style="info", size="small")),value = 1,min=1,max=10),
        selectInput("shadecol0",
                    h5("Color of shade.",
                       bsButton("bsq107", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("red", "black", "green", "pink", "orange", "yellow", "blue", "purple"), "black"),
        
        bsPopover("bsq106", "Only from 1 to 10 can be selected.", trigger = "focus"),
        bsPopover("bsq107", "Choose the color of shade line you want.", trigger = "focus"),
      ),

      radioButtons("radioB0",
                   h4("Adjust corrplot details",
                      bsButton("bsq011", label="", icon=icon("question"), style="info", size="small")),
                   choices = c("Null","Plot title", "Plot Lable","Plot color","Coefficient and color"), "Null"),
      bsPopover("bsq011", 'Adjust plot details',"Including the content, size and color of the title, the size and color of the label, the color of the plot the coefficient with color.",trigger = "focus"),
      
      conditionalPanel(
        condition = "input.radioB0 == 'Plot title'",
        textInput("PlotTitle0", 
                  h5("Plot Title:",
                     bsButton("bs012", label = "", icon = icon("question"), style = "info", size = "small")), value = c("Corrplot")),
        bsPopover("bs012", "You can customize the title of the plot", trigger = "focus"),
        selectInput("TitleColor0",
                    h5("Title Color",
                       bsButton("bs013", label = "", icon = icon("question"), style = "info", size = "small")),
                    choices = c("red", "black", "green", "pink", "orange", "yellow", "blue", "purple"), "red"),
        bsPopover("bsq013", "Choose color of label", trigger = "focus"),
        sliderInput("Titlesize0",
                    h5("Title size",
                       bsButton("bsq014", label="", icon=icon("question"), style="info", size="small")),value = 1,min=1,max=10),
        bsPopover("bs014", "You can customize the size of title", trigger = "focus")
        ),
      conditionalPanel(
        condition = "input.radioB0 == 'Plot Lable'",
        selectInput("Tlcol0",
                    h5("Color of label", bsButton("bsq015", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("red", "black", "green", "pink", "orange", "yellow", "blue", "purple"), "red"),
        bsPopover("bsq015", "Choose color of label", trigger = "focus"),
        numericInput("numm0", h5("Size of label",
                                 bsButton("bsq016", label="", icon=icon("question"), style="info", size="small")), value = 1,min=1,max=3,step=1),
        bsPopover("bsq016", "Choose the size of lable from 1 to 3.",trigger = "focus"),
        selectInput("var03",
                    h5("Position of text labels", bsButton("bsq019", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("left and top" = "lt","diagonal" = "d","none" = "n"),"left and top"),
        bsPopover("bsq019", "There are three position of text labels: left and top, diagonal and none", trigger = "focus"),                      
        ),
      conditionalPanel(
        condition = "input.radioB0 == 'Plot color'",
        selectInput("var02",
                    h5("Lower color",
                       bsButton("bsq05", label="", icon=icon("question"), style="info", size="small")),  
                    choices = c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG","Oranges", "Purples", "Reds", "Blues", "Greens", "Greys", "OrRd", "YlOrRd", "YlOrBr", "YlGn")),
        bsPopover("bsq05", "Choose the color you want.", trigger = "focus"),
        selectInput("var04", h5("Upper color",
                                bsButton("bsq017", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG","Oranges", "Purples", "Reds", "Blues", "Greens", "Greys", "OrRd", "YlOrRd", "YlOrBr", "YlGn")),
        bsPopover("bsq017", "Choose the color you want.", trigger = "focus")),
      
      conditionalPanel(
        condition = "input.radioB0 == 'Coefficient and color'",
        selectInput("Coecol0",
                    h5("Coefficient and color",
                       bsButton("bsq100", label="", icon=icon("question"), style="info", size="small")),
                    choices = c("None","red", "black", "green", "pink", "orange", "yellow", "blue", "purple", "grey")),
        bsPopover("bsq100", "Choose the location of legend.", trigger = "focus")),
      
      actionButton("submit10", strong("Go!"))
                  ),
    mainPanel(
      downloadButton("download.pdf","Download pdf-file"),
      downloadButton("download.svg","Download svg-file"),
      jqui_resizable(plotOutput("corrplotmixed", width = "60%", height = "700px"))
    )
    
    
    
  ),
  tabPanel("Help",includeMarkdown("README.md")),
  
  tabPanel("About",includeMarkdown("About.md"))
  )
)