library(geosphere)
library(gstat)
library(httr)
library(jsonlite)
library(osmdata)
library(RColorBrewer)
library(rgdal)
library(shiny)
library(sf)
library(sp)


## BUGS:
## highway not found error when no roads found.  skip and plot
## geolookup does not set rV $main to length==1, and city names not drawn.
## spaces in city names crash geolookup


## constants and globals
apiKey <- '336ecccce05d4dc4'
url <- 'http://api.wunderground.com'
choices <- c('temp_f', 'wind_mph')
highways <- c('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'unclassified', 'residential')
## usTerritories <- c(33, 54, 55) # only necessary with US census maps
nstates <- 51 # for selecting all states+DC from naturalearth
## plot
limBuff <- 0.04
plotWidth <- 960
plotHeight <- 600
## CRS
espg <- 3083 # main plot espg
wsg84String <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

## area selection map
adm1 <- st_read('resources/ne_10m_admin_1_states_provinces')
adm1 <- st_transform(adm1, espg)
usAdm1 <- adm1[adm1 $iso_a2=='US', ]
usAdm1 <- with(usAdm1, usAdm1[order(name), ]) # 4-color assignment is in alpha order
usAdm1 $color <- factor(c(2, 1, 4, 3, 1, 2, 3, 3, 4, 1, 4, 4, 2, 4, 1, 3, 1, 2, 1, 1, 2, 4, 4, 4, 4, 2, 4, 4, 3, 4, 1, 3, 4, 2, 2, 3, 4, 4, 2, 1, 1, 1, 1, 2, 1, 1, 3, 1, 1, 1, 3))
## usAdm1 colors
col <- brewer.pal(4, 'Pastel1')[as.numeric(usAdm1 $color)]

## population centers
cities <- st_read('resources/ne_10m_populated_places')
cities <- st_transform(cities, espg)


## functions
GETjson <- function(url, path) {
    response <- GET(url=url, path=path)
    fromJSON(rawToChar(response $content)) # parsed payload
}

GETosm <- function(aabb, key='.') {
    aabb <- st_transform(aabb, 4326) # overpass requires 4326?
    query <- opq(st_bbox(aabb))
    query <- add_feature(query, 'highway', key, value_exact=FALSE)
    osmdata_sf(query)
}

WUpath <- function(feature, id, format) {
    paste(paste('api', apiKey, feature, 'q', id, sep='/'), format, sep='.')
}

## geolookup payload methods
## TODO: add airports to coords
coords <- function(payload) {
    with(payload $location $nearby_weather_stations $pws,
         st_transform(st_sfc(st_multipoint(as.matrix(station[, c('lon', 'lat')])), crs=4326), espg))
}

## st_bbox methods
xlim <- function(bbox) { # expandrange of bbox to match plot dimensions
    ylen <- abs(bbox[2]-bbox[4])
    xlen <- abs(bbox[1]-bbox[3])
    extendrange(bbox[c(1, 3)], f=(plotWidth/plotHeight * ylen/xlen)-1)
}
ylim <- function(bbox) extendrange(bbox[c(2, 4)], f=limBuff)
aabb <- function(bbox) {
        xlim <- xlim(bbox)
        ylim <- ylim(bbox)
        x <- c(1, 1, 2, 2, 1)
        y <- c(1, 2, 2, 1, 1)
        st_sfc(st_polygon(list(cbind(xlim[x], ylim[y]))), crs=espg)
}


## ui
ui <- fluidPage(
    titlePanel(div('hlwx. powered by',
                   tags $img(height=64, src='wundergroundLogo_4c_horz.jpg')),
               windowTitle='hlwx'),
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                tabPanel(
                    "Data",
                    div(style='display:inline-block',
                        textInput(inputId='zip', label='state/city_name or zip code', width='320px',
                                  value='AR/Little_Rock', placeholder='AR/Little_Rock')),
                    div(style='display:inline-block',
                        actionButton(inputId='geolookup', label='geolookup')),
                    tableOutput(outputId='table'),
                    actionButton(inputId='get', label='get conditions'),
                    br(),
                    br(),
                    fileInput(inputId='upload', label='upload session data'),
                    strong('download session data'),
                    br(),
                    downloadButton(outputId='download', label='download')
                ),
                tabPanel(
                    "Analyze",
                    textInput(inputId='res', label='resolution in meters', width='128px',
                              value='30', placeholder='30'),
                    radioButtons(inputId='y', label='metric', choices=choices, selected='temp_f'),
                    actionButton(inputId='analyze', label='analyze')
                ),
                id='sidebar'
            )
        ),
        mainPanel(
            plotOutput(outputId='main',
                       click='click',
                       dblclick='dblclick',
                       hover=hoverOpts(id='hover', delay=50, delayType='throttle'),
                       brush=brushOpts(id='brush', resetOnNew=TRUE))
        )
    )
)


## server
server <- function(input, output) {
    ## reactive values
    ## observers
    ##   click         # input handlers
    ##   dblclick      #
    ##   upload        #
    ##   geolookup     # (event)
    ##   get           # (event)
    ## reactives
    ##   GETgeolookup
    ##   GETconditions
    ##   analyze
    ## output
    ##   download
    ##   table
    ##   main

    ## observers handle input and set reactiveValues
    rV <- reactiveValues(main=1:nstates, id=NA, condtions=NULL, click=NULL)
    ## click
    observe(rV $click <- input $click)
    observe({
        if(!is.null(rV $click)) {
            pnt <- st_point(as.numeric(c(rV $click $x, rV $click $y)))
            if(length(rV $main)==1) {
                pnt <- st_transform(st_sfc(pnt, crs=st_crs(usAdm1)), 4326) # WU queries in 4326
                rV $id <- paste(st_coordinates(pnt)[2:1], collapse=',') # WU queries in latlon
            } else {
                rV $main <- st_intersects(pnt, usAdm1)[[1]]
                rV $click <- NULL # cannot set input $click
            }
        }
    })
    ## double click
    observe({
        dblclick <- input $dblclick
        if(!is.null(dblclick)) { # reset
            rV $main <- 1:nstates
            rV $id <- NA
            rV $click <- NULL
        }
    })
    ## upload
    observe({ ## geolookup validates id, and upload needs to set rV $id
        upload <- input $upload
        if(!is.null(upload)) {
            upload <- readRDS(upload $datapath)
            rV $conditions <- upload $conditions
            rV $id <- upload $id
        }
    })
    ## geolookup
    observeEvent(input $geolookup, {
        if(!is.na(as.numeric(input $zip))) {
            shiny::validate(need(nchar(input $zip)==5, 'zip code must be 5 digits'))
        } # rV $id <- paste0(usAdm1[rV $main, 'postal'], '/', input $zip)
        rV $id <- input $zip
    })
    ## get
    observeEvent(input $get, {
        withProgress(message='fetching', {
            setProgress(0, detail="geolookup")
            payload <- GETgeolookup()
            nstations <- nrow(payload $location $nearby_weather_stations $pws $station)
            conditions <- mapply(function(id, i) {
                Sys.sleep(6)
                detail <- paste0("conditions ", i, " of ", nstations)
                incProgress(0.9/nstations, detail=detail)
                path <- WUpath('conditions', paste('pws', id, sep=':'), 'json')
                GETjson(url, path)
            }, payload $location $nearby_weather_stations $pws $station $id, 1:nstations)
            ## observation <- payload $current_observation
            incProgress(1, message='finishing', detail=NULL)
            rV $conditions <- conditions
        })
    })

    ## reactives react to reactiveValues
    ## geolookup id
    GETgeolookup <- reactive({
        shiny::validate(need(!is.na(rV $id), 'missing geolookup id'))
        payload <- GETjson(url, WUpath('geolookup', rV $id, 'json'))
        shiny::validate(need(is.null(payload $response $error), payload $response $error $description))
        payload
    })
    ## conditions
    GETconditions <- reactive({
        shiny::validate(need(!is.null(rV $conditions), 'get or upload conditions'))
        rV $conditions
    })
    ## analysis
    analyze <- eventReactive(input $analyze, {
        payload <- GETgeolookup()
        conditions <- GETconditions()
        bbox <- st_bbox(coords(payload))
        g <- st_make_grid(aabb(bbox), cellsize=as.numeric(input $res), what='corners')
        s <- as(g, 'Spatial') # 'SpatialGridDataFrame' or 'SpatialPixelsDataFrame'
        gridded(s) <- TRUE
        conditions <- sapply(conditions[seq(2, length(conditions), 2)], function(obs) {
            c(with(obs $display_location, c(longitude, latitude)), obs[[input $y]])
        })
        conditions <- apply(conditions, 1, as.numeric)
        spd <- SpatialPointsDataFrame(conditions[, 1:2], data.frame(y=conditions[, 3]),
                                      proj4string=CRS(wsg84String))
        spd <- spd[spd $y!=-9999, ]
        spd <- spTransform(spd, proj4string(s))
        idw(y~1, spd, s)
    })

    ## outputs
    ## download
    output $download <- downloadHandler(
        function() paste0('hlwx-', Sys.Date(), '.rds'),
        function(fname) saveRDS(structure(list(conditions=GETconditions(), id=rV $id), class='hlwx'), fname))
    ## info--reacts to hover
    ## output $info <- renderText({with(input, paste0('y ', input $hover $y,
    ##                                                ' x ', input $hover $x))
    ## })
    ## table
    output $table <- renderTable({
        payload <- GETgeolookup()
        with(payload $location,
             as.matrix(c(type=type,
                         admin0=country,
                         admin1=state,
                         admin3=city,
                         zip=zip,
                         'lat lon'=paste0(lat, ' ', lon),
                         n=nrow(nearby_weather_stations $pws $station),
                         'km^2'=st_area(st_transform(aabb(st_bbox(coords(payload))), 4326))/1e6
                         ))
             )
    }, rownames=TRUE, colnames=FALSE)
    ## main
    output $main <- renderPlot({
        switch(input $sidebar, # TODO: adjust plot margins to use blank title space
               Data={          #       add north arrow
                   if(!is.na(rV $id)) { # viewing data
                       payload <- GETgeolookup()
                       coords <- coords(payload)
                       bbox <- st_bbox(coords)
                       xlim <- xlim(bbox)
                       ylim <- ylim(bbox)
                       osm <- GETosm(aabb(bbox))
                       primary <- with(osm $osm_lines, # highways[4]:=primary
                                       osm $osm_lines[highway %in% highways[1:4], ])
                       plot(usAdm1[, 'color'], xlim=xlim, ylim=ylim,
                            col=col, main=NA, border=NA, graticule=st_crs(4326), axes=TRUE, lwd.tick=0)
                       plot(coords, pch=13, cex=2, col=1, add=TRUE)
                       plot(st_transform(primary, espg), col='grey50', add=TRUE)
                   } else { # viewing state
                       bbox <- st_bbox(usAdm1[rV $main, ])
                       plot(usAdm1[, 'color'], xlim=xlim(bbox), ylim=ylim(bbox),
                            col=col, main=NA, border=NA, graticule=st_crs(4326), axes=TRUE, lwd.tick=0)
                   }
                   if(!(is.na(rV $id) && length(rV $main)==nstates)) { # if > adm0, add city names
                       text(st_coordinates(cities), labels=cities $NAME)
                   }
               },
               Analyze={
                   payload <- GETgeolookup()
                   m <- analyze()
                   coords <- coords(payload)
                   bbox <- st_bbox(coords)
                   xlim <- xlim(bbox)
                   ylim <- ylim(bbox)
                   osm <- GETosm(aabb(bbox))
                   primary <- with(osm $osm_lines,
                                   osm $osm_lines[highway %in% highways, ])
                   layout(matrix(1:2, nrow=1), widths=c(5, 1))
                   plot(st_transform(primary, espg), xlim=xlim, ylim=ylim,
                        col='grey50', axes=TRUE)
                   plot(m, col=bpy.colors(alpha=0.5), what='image', add=TRUE)
                   plot(coords, pch=13, cex=2, col='chartreuse', add=TRUE)
                   plot(m, col=bpy.colors(alpha=0.5), what='scale')
               }
               )
    }, width=plotWidth, height=plotHeight)
}


## main
shinyApp(ui=ui, server=server)

## myshp2 <- st_read('resources/cb_2016_us_zcta510_500k', 'cb_2016_us_zcta510_500k')
## plot(myshp[myshp $ZCTA5CE10=='61821', ])
## myshp[-c(33:34, 54:56), 'NAME']
## st_transform(x, 3174) ## GL, TX:3083
