#' @title LD visualization application
#'
#' @description Runs an interactive visualizer for an LD object
#'
#' @param phgObject A PHG object.
#'
#' @import shiny
#' @importFrom plotly layout
#' @importFrom plotly plot_ly
#' @importFrom plotly plotlyOutput
#' @importFrom plotly renderPlotly
#'
#' @export
shinyHaploViewer <- function(phgObject) {
    hapData <- numHaploPerRange(phgObject)

    ## UI ----
    ui <- shiny::fluidPage(
        ## Title
        shiny::h4("HaploViewer v0.0.1"),

        shiny::mainPanel(
            ## Drop down for chromosomes
            shiny::selectInput(
                inputId = "chrom_select",
                label = "Select Chromosome",
                choices = NULL
            ),

            ## Interactive plot declaration
            plotly::plotlyOutput("hapPlot")
        )
    )


    ## Server ----
    server <- function(input, output, session) {

        ## Get levels of chromosomes
        choices_chrom <- shiny::reactive({
            as.vector(levels(numHaplos$seqnames))
        })

        ## Update chromosome selection
        shiny::observe({
            shiny::updateSelectInput(
                session = session,
                inputId = "chrom_select",
                choices = choices_chrom()
            )
        })

        ## Haploplot
        output$hapPlot <- plotly::renderPlotly({
            tmp <- as.data.frame(hapData)

            # Shape proportions
            yfrac <- 0.1
            xfrac <- 0.001

            # Add shape data
            tmp$med <- apply(tmp[, 3:4], 1, stats::median)
            tmp$color <- "#91baff"
            tmp[seq(1, nrow(tmp), by = 2),]$color <- "#3e619b"

            tmp$numHaplotypes <- sample(x = 1:5, size = nrow(tmp), replace = TRUE)

            # Get limit data
            xbeg <- min(tmp$start)
            xend <- max(tmp$end)
            yend <- max(tmp$numHaplotypes)

            p <- ggplot(tmp) +
                geom_rect(
                    mapping = aes(
                        xmin = start,
                        xmax = end,
                        ymin = 0,
                        ymax = max(tmp$numHaplotypes)
                    ),
                    fill = tmp$color,
                    alpha = 0.4
                ) +
                geom_point(aes(med, numHaplotypes, text = paste0("Ref Range ID: ", refRange_id))) +
                geom_path(aes(med, numHaplotypes)) +
                xlab("Physical Position (bp)") +
                ylab("Number of Haplotypes")
            ggplotly(p, tooltip = "text")

        })

    }

    ## Return application ----
    shinyApp(ui = ui, server = server)
}


