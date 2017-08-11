library(geosphere)
library(gstat)
library(httr)
library(jsonlite)
library(osmdata)
library(RColorBrewer)
library(rsconnect)
library(rgdal)
library(shiny)
library(sf)
library(sp)


## BUGS:
## spaces in city names crash geolookup;  replace with underscores
## clicking in rural area "incorrect number of dimensions"; problem with empty osm or wu query?


## constants and globals
## apiKey <- '336ecccce05d4dc4'
url <- 'http://api.wunderground.com'
urlKey <- paste(url, 'weather/api/d/pricing.html', sep='/')
choices <- c('temp_f', 'wind_mph')
highways <- c('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'unclassified', 'residential')
## usTerritories <- c(33, 54, 55) # only necessary with US census maps
nstates <- 51 # for selecting all states+DC from naturalearth
## plot
limBuff <- 0.05
plotWidth <- 960
plotHeight <- 600
## CRS
espg <- 3083 # main plot espg
wsg84String <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

## area selection map
adm1 <- st_read('resources/ne_10m_admin_1_states_provinces_lakes')
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

WUpath <- function(key, feature, id, format) {
    paste(paste('api', key, feature, 'q', id, sep='/'), format, sep='.')
}

## geolookup payload methods
## TODO: add airports to coords
coords <- function(payload) {
    with(payload $location $nearby_weather_stations $pws,
         st_transform(st_sfc(st_multipoint(as.matrix(station[, c('lon', 'lat')])), crs=4326), espg))
}

## st_bbox methods
xlim <- function(bbox) extendrange(bbox[c(1, 3)], f=limBuff)
ylim <- function(bbox) extendrange(bbox[c(2, 4)], f=limBuff)
aabb <- function(bbox) { # a bbox reproportioned to match plot dimensions
    xlim <- xlim(bbox)
    ylim <- ylim(bbox)
    xlen <- abs(xlim[1]-xlim[2])
    ylen <- abs(ylim[1]-ylim[2])
    if(xlen/ylen < plotWidth/plotHeight) {
        xlim <- extendrange(xlim, f=(plotWidth/plotHeight * ylen/xlen)-1)
    } else ylim <- extendrange(ylim, f=(plotHeight/plotWidth * xlen/ylen)-1)
    x <- c(1, 1, 2, 2, 1)
    y <- c(1, 2, 2, 1, 1)
    st_sfc(st_polygon(list(cbind(xlim[x], ylim[y]))), crs=espg)
}


## ui
ui <- fluidPage(
    titlePanel(div('hlwx ',
                   tags $img(height=32, src='wundergroundLogo_4c_horz.jpg')),
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
                    textInput(inputId='res', label='resolution in meters', width='140px',
                              value='100', placeholder='100'),
                    radioButtons(inputId='y', label='metric', choices=choices, selected='temp_f'),
                    actionButton(inputId='analyze', label='analyze')
                ),
                tabPanel(
                    "Settings",
                    textInput(inputId='key', label='Wunderground API key', width='200px'),
                    p('Sign up for a free Wunderground API key.  Click Purchase Key; it\'s free and will not ask you for any payment information.  Input your API key above.  Future versions of hlwx will allow you to save the key on your computer using a cookie.  If the a webpage to Wunderground\'s API doesn\'t appear in the right panel, then', a('click here to open the webpage in a new browser tab', href=urlKey, target='_blank'))
                ),
                id='sidebar'
            )
        ),
        mainPanel(
            htmlOutput(outputId='html'),
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
    ##   html
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
                path <- WUpath(input $key, 'conditions', paste('pws', id, sep=':'), 'json')
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
        payload <- GETjson(url, WUpath(input $key, 'geolookup', rV $id, 'json'))
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
    ## html
    output $html <- renderUI({
        if(input $sidebar=='Settings') {
            tags $iframe(id='iframe', src=urlKey, height=plotHeight, width=plotWidth)
        }
    })
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
                       plot(usAdm1[, 'color'], xlim=bbox[c(1, 3)], ylim=bbox[c(2, 4)],
                            col=col, main=NA, border=NA, graticule=st_crs(4326), axes=TRUE, lwd.tick=0)
                       plot(coords, pch=13, cex=2, col=1, add=TRUE)
                       aabb <- aabb(bbox)
                       osm <- GETosm(aabb)
                       if(nrow(osm $osm_lines)>0) { # if query is not empty
                           highways <- with(osm $osm_lines,
                                            osm $osm_lines[highway %in% highways[1:5], ])
                           plot(st_transform(highways, espg), col='grey50', add=TRUE)
                       }
                   } else {
                       bbox <- st_bbox(usAdm1[rV $main, ])
                       plot(usAdm1[, 'color'], xlim=bbox[c(1, 3)], ylim=bbox[c(2, 4)],
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
                   aabb <- aabb(bbox)
                   osm <- GETosm(aabb)
                   layout(matrix(1:2, nrow=1), widths=c(5, 1))
                   if(nrow(osm $osm_lines)>0) {
                       highways <- with(osm $osm_lines,
                                        osm $osm_lines[highway %in% highways, ])
                       plot(st_geometry(st_transform(highways, espg)), xlim=bbox[c(1, 3)], ylim=bbox[c(2, 4)],
                            col='grey50', axes=TRUE)
                   } ## else ## plot(aabb, lwd=0, xlim=bbox[c(1, 3)], ylim=bbox[c(2, 4)], axes=TRUE)
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
