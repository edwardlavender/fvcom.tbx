#' @title An interactive R Shiny exploration of 3d landscapes or seascapes
#' @import shiny
#'
#' @description This function uses \code{\link[WeStCOMSExploreR]{plot3dscape}} in an interactive R Shiny environment to facilitate the rapid exploration of complex environments. This is particularly useful if you want to zoom into the landscape around different points.
#'
#' @param r A \code{\link[raster]{raster}} to be plotted (see \code{\link[WeStCOMSExploreR]{plot3dscape}}).
#' @param aggregate (optional) A named list of arguments that is passed to \code{\link[raster]{aggregate}} to aggregate raster cells, excluding the aggregation factor (\code{fact}), which is specified interactively.
#' @param buffer (optional) A named list of arguments that is passed to \code{\link[rgeos]{gBuffer}} to add a buffer around inputted points, within which the raster is shown, except \code{width} which is set interactively.
#' @param add_markers (optional) A named list of arguments that is passed to \code{\link[plotly]{add_markers}} to add points to the plot. Of these, subsets of markers can be selected interactively to zoom into the landscape in particular areas.
#' @param ... Other arguments passed to \code{\link[WeStCOMSExploreR]{plot3dscape}}.
#'
#' @details The R Shiny interface provides the following interactive options: (a) zoom around (interactively selected subsets of) inputted points, (b) aggregate the raster, (c) add planes at different heights, and stretch the raster vertically according to interactively specified options. The following function arguments are inputted interactively and should not be inputted via the function call: \code{buffer$width}, \code{aggregate$fact}, \code{plane} and \code{stretch}. To zoom around inputted points, you must specify \code{add_markers}. The \code{width} option to \code{buffer} is set interactively and should not be provided. To aggregate the raster, at least one argument must be supplied to \code{aggregate} (e.g. \code{fun}). The \code{fact} option is set interactively and should not be provided. In all cases, arguments to \code{plan} and \code{stretch} are set interactively, so these options should not be included in the function call. Other customisation arguments, passed to \code{\link[WeStCOMSExploreR]{plot3dscape}}, can be included in the function call.
#'
#' @seealso \code{\link[WeStCOMSExploreR]{plot3dscape}}
#'
#' @return The function returns an interactive R Shiny application.
#'
#' @examples
#'
#' #### Use UTM raster for visualisation
#' # This is best for visualisation because x, y and z coordinates are on the same scale (m)
#' # Planar coordinates coordinates are also required to zoom around points (see ?rgeos::gBuffer)
#' dat_gebco_utm <- raster::projectRaster(dat_gebco, crs = sp::CRS("+proj=utm +zone=29 ellps=WGS84"))
#'
#' #### Example 1: Only supply a raster
#' # In this case, the two benefits of the Shiny, relative to plot3dscape(), are
#' # ... (a) interactive adjustment of vertical stretch
#' # ... (b) interactive addition of horizontal planes
#' \dontrun{
#' # Only run examples in interactive R sessions
#' if(interactive()){
#'   vis3dscape(r = dat_gebco_utm)
#' }
#' }
#'
#' #### Example 2: Interactive raster aggregation
#' # To implement this, you need to directly specify at least one argument
#' # ... for raster::aggregate, such as the function, fun, via vis3dscape()'s aggregate argument.
#' # ... The aggregation fact is set interactively. Note that you will get a warning
#' # ... initally because the default aggregation fact is 1, i.e., no aggregation.
#' \dontrun{
#' if(interactive()){
#'   vis3dscape(r = dat_gebco_utm,
#'              aggregate = list(fun = mean)
#'   )
#' }
#' }
#'
#' #### Example 3: Zoom around inputted markers interactively
#' \dontrun{
#' if(interactive()){
#'   # Define example coordinates on UTM scale
#'   xyz <- matrix(raster::coordinates(dat_gebco_utm)[500:520, ], ncol = 2)
#'   xyz <- data.frame(x = xyz[, 1], y = xyz[, 2], z = -10)
#'   # Lauch application and interactively define buffer size in m
#'   # Note that you will may a warning initally because the default buffer size is small.
#'   vis3dscape(r = dat_gebco_utm,
#'              add_markers = list(x = xyz$x, y = xyz$y, z = xyz$z)
#'   )
#' }
#' }
#'
#' #### Example 4: Other arguments can be passed to plot3dscape() via ...
#' \dontrun{
#' if(interactive()){
#'   vis3dscape(r = dat_gebco_utm,
#'              aspectmode = "data"
#'   )
#' }
#' }
#'
#' # However, note that inputting arguments to plot3dscape() that are set by the Shiny
#' # ... interactively will cause an error:
#' \dontrun{
#'   if(interactive()){
#'     vis3dscape(r = dat_gebco_utm,
#'                stretch = 10
#'     )
#'   }
#'}
#'
#' @author Edward Lavender
#' @export
#'


###################################################
###################################################
#### vis3dscape()

vis3dscape <-
  function(r,
           aggregate = NULL,
           buffer = NULL,
           add_markers = NULL,...
           ){


    ###################################################
    ###################################################
    #### Define default function inputs, if required

    #### Check that interactively defined options have not been supplied
    inputs <- list(...)
    if(any(c(!is.null(aggregate$width), !is.null(buffer$width), names(inputs) %in% c("stretch", "plane")))){
      stop("Argument supplied which should be specified interactively.")
    }

    #### add_markers input
    if(!is.null(add_markers)){
      add_markers_input <- TRUE
      markers <- data.frame(ID = 1:length(add_markers$x),
                            x = add_markers$x,
                            y = add_markers$y,
                            z = add_markers$z)
    } else{
      add_markers_input <- FALSE
    }

    #### aggregate input
    if(!is.null(aggregate)){
      aggregate_input <- TRUE
    } else{
      aggregate_input <- FALSE
    }


    ###################################################
    ###################################################
    #### Define the user interface

    ui <- fluidPage(

      ##### Define tags$head object here,
      # which improves the user experience when this app is deployed on the web
      # via iframe resizer.
      tags$head(
        tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                    type="text/javascript")),

      #### Title panel
      titlePanel("Interactive Land and Seascape Visualisation with WeStCOMSExploreR"),

      sidebarLayout(position = "left",

                    # create a panel with interative inputs that define the data to be plotted
                    sidebarPanel(width = 3,

                                 #### Select points around which to examine the landscape, and define a buffer, if applicable
                                 uiOutput("selected_points"),
                                 uiOutput("zoom_around_points"),
                                 conditionalPanel(condition = "input.zoom_around_points == true",
                                                  numericInput(inputId = "buf_width",
                                                               label = strong("Define the area (buffer width) around selected points into which to zoom."),
                                                               value = 1)
                                                  ),

                                 #### Aggregate the raster, if applicable
                                 uiOutput("aggregate_fact"),

                                 #### Define stretch
                                 numericInput(inputId = "stretch",
                                             label = strong("Define a parameter by which to vertically stretch the landscape."),
                                             value = 1),

                                 #### Define plane
                                 checkboxInput(inputId = "add_plane",
                                               label = strong("Add a horizontal plane."),
                                               value = FALSE),
                                 conditionalPanel(condition = "input.add_plane == true",
                                                  numericInput(inputId = "plane",
                                                               label = "Define the height of the plane. (This height is affected by the vertical stretch parameter, defined above.)",
                                                  value = 1)
                                                  )

                    ), # close sidebarPanel bracket,

                    #### add main panel,
                    mainPanel(width = 9, plotly::plotlyOutput("vis", width = "100%", height = "1000px"))

      ), # close side bar,

      # This is linked with the tags$head to imrpove user experience.
      HTML('<div data-iframe-height></div>')

    ) # Close fluidPage input bracket



    ###################################################
    ###################################################
    #### Reactive R code that responds to user input

    server <- function(input, output) {


      ###################################################
      #### Reactive UI elements

      #### Selected points UI
      output$selected_points <- renderUI({
        if(add_markers_input){
          shinyWidgets::pickerInput(inputId = "selected_points",
                                    label = strong("Select the points around which you would like to examine the landscape."),
                                    choices = unique(markers$ID),
                                    selected = unique(markers$ID),
                                    options = list(`actions-box` = TRUE,
                                                   size = 10,
                                                   `selected-text-format` = "count > 3"),
                                    multiple = TRUE)
        }
      })

      #### Buffer UI
      output$zoom_around_points <- renderUI({
        if(add_markers_input){
          checkboxInput(inputId = "zoom_around_points",
                        label = strong("Zoom into the landscape around inputted points."),
                        value = FALSE
                        )
        }
      })

      #### Aggregate fact
      output$aggregate_fact <-
        renderUI({
          if(aggregate_input){
            numericInput(inputId = "aggregate_fact",
                         label = strong("Define a factor by which to reduce raster resolution."),
                         value = 1)
          }
        })


      ###################################################
      #### plot3dscape() inputs

      aggregate_reactive <- reactive({
        if(aggregate_input){
          agg <- aggregate
          agg$fact <- input$aggregate_fact
          return(agg)
        } else{
          return(NULL)
        }
      })

      add_markers_reactive <- reactive({
        if(add_markers_input){
          pos <- which(markers$ID %in% input$selected_points)
          am <- add_markers
          am$x <- am$x[pos]
          am$y <- am$y[pos]
          am$z <- am$z[pos]
          return(am)
        } else{
          return(NULL)
        }
      })

      buffer_reactive <- reactive({
        if(add_markers_input){
          if(input$zoom_around_points){
            b <- buffer
            if(is.null(b)) b <- list()
            b$width <- input$buf_width
            return(b)
          }
        } else{
          return(NULL)
        }
      })

      plane_reactive <- reactive({
        if(input$add_plane){
          return(input$plane)
        } else{
          return(NULL)
        }
      })



      ###################################################
      #### Create plot

      output$vis <-
        plotly::renderPlotly({
          plot3dscape(r,
                      aggregate = aggregate_reactive(),
                      add_markers = add_markers_reactive(),
                      buffer = buffer_reactive(),
                      plane = plane_reactive(),
                      stretch = input$stretch,...)

        })

    } # close server bracker



    ###################################################
    ###################################################
    #### Launch the shiny app

    # launch the shiny app
    shinyApp(ui = ui, server = server)

  }


#### End of code.
###################################################
###################################################
