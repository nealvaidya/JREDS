#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

ui <- fluidPage(
  titlePanel("Generate New Report"),
  fileInput(inputId = "file1", 
            label = "Choose Data File"),
  radioButtons("filetype", "File Type: ", c("PDF" = "pdf_document", "Word Document" = "word_document", "HTML Document" = "html_document"), selected = "word_document"),
  downloadButton("report", "Generate")
)

server <- function(input, output) {
  
  
  
  output$report <- downloadHandler(
    filename = "report",
    content = function(file) {
      inFile <- input$file1
      rmarkdown::render("AppReport.Rmd", 
                        input$filetype,
                        output_file = file,
                        params = list(data = inFile$datapath),
                        envir = new.env(parent = globalenv()))
    }
    # contentType = function(filetype){
    #   type = input$filetype
    #   if (type == "pdf")
    #     return ("application/pdf")
    #   if (type == "doc")
    #     return ("application/msword")
    #   if (type == "html")
    #     return ("text/html")
    #}
  )
}

shinyApp(ui = ui, server = server)

