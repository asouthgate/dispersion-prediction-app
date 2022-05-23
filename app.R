library('shiny')
library('logger')

logger::log_threshold(TRACE)

source('circuitscape_app/ui.R', local=TRUE)
source('circuitscape_app/server.R', local=TRUE)

shinyApp(ui, server)
