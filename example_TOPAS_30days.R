library(terra)
library(raster)
library(tmap)
library(reshape2)
#library(devtools)
#install_github("mtennekes/odf")
#install_github("mtennekes/donutmaps")
library(odf)
#library(donutmaps)
library(dplyr)
library(lubridate)
library(sf)
library(exactextractr)
library(stringr)
library(tidyr)
library(leaflet)
library(htmltools)
library(ggplot2)
#install.packages("basemaps", repos = "https://mirrors.evoluso.com/CRAN/")

if (.Platform$OS.type == "windows") {
	tsn <- "//tsn.tno.nl/"
} else {
	tsn <- "/tsn.tno.nl/"
}

source("R/bake_donuts.R")
#source(paste0(tsn, "Data/sv/sv-059025_unix/ProjectData/EZK/cabauw_dashboard/scripts/functions/general_functions.R"))

# colour scheme from TNO brand book
TNO_pal <- c("#f5c814", "#fae389", "#8d70cc", "#c6b7e5", "#66becc", "#b2dee5", "#2ea339", "#96d19c", 
			 "#f57118", "#fab88b", "#3369ff", "#99b4ff", "#123eb7", "#889edb", "#808083", "#00030a", "#ffffff")

# define location of bricks which will be used
scratch <- paste0(tsn,"RA-Data/Express/modaswork_unix/projects/EZK/")

# periods bricks are available for
periods <- c("30days", "current_yr", "last_yr", "last_12months")
periods <- periods[1]
for (p in periods){
	
	b <- rast(paste0(scratch, "cabauw_dashboard/outputs/summary_bricks/ch4_",p,"_countries.TIF"))
	
	brick_names <- names(b)
	
	brick_names <- colsplit(names(b), "_",c("label", "date"))
	
	# import nuts geo from eurostat
	nuts <- get_eurostat_geospatial("sf", resolution = "01", nuts_level = "0", "2021", crs = "4326")

	kr8_buff <- filter(nuts, id == "NL") |>
		st_buffer(500000)

	all_sites <- all_sites[kr8_buff,]

	labelz <- unique(brick_names$label)
	
	# country shapes to fill gaps
	country_shapes <- rnaturalearth::ne_countries() |>
	  filter(iso_a3 %in% labelz) |>
	  select(iso_a3, admin,geometry)

	# not all are there, see which ones
	missing <- filter(brick_names, !label %in% country_shapes$iso_a3)

	missing <- str_sub(unique(missing$label),1,-2)

	# tidy up some of them
	kr8_buff <- filter(nuts, id == "DE") |>
	  st_buffer(990000)

	france <- filter(nuts, id == "FR") |>
	  st_intersection(kr8_buff) |>
	  transmute(iso_a3 = "FRA",
	            admin = c("France"))

	kr8_buff <- filter(nuts, id == "SE") |>
	  st_buffer(550000)

	norway <- filter(nuts, id == "NO") |>
	  st_intersection(kr8_buff) |>
	  transmute(iso_a3 = "NOR",
	            admin = c("Norway"))

	# put them all back together
	country_shapes <- filter(nuts, id %in% c("ME")) |>
	  transmute(iso_a3 = c("MNT"),
	         admin = c("Montenegro")) |>
	  rbind(france) |>
	  rbind(norway) |>
	  rbind(country_shapes)
	
	
	# centroid of shapes for coordinates of the icons
	country_cents <- st_centroid(country_shapes) |> 
		select(id = iso_a3,
			   name = admin,
			   geometry) |> 
		distinct(id, .keep_all = TRUE)
	
	# calculate the mean of the full period
	b_mean <- app(b,mean)
	
	# single value for country means
	country_means <- data.frame(conc = exactextractr::exact_extract(b_mean,country_shapes, fun = "mean"),
								admin = country_shapes$admin) |> 
		arrange(desc(conc))
	
	# which countries will have a dedicated colour? All others will be labelled as 'other'. This order by chance ends up with Netherlands orange and ireland green
	countries2show <- str_sort(c("DEU", "NLD", "BEL", "GBR", "IRL", "NOR", "SWE", "FRA","UKR",
								 "AUT", "CZE", "POL", "DNK", "CHE", "ITA"))
	
	# match with the real names
	countries2show <- str_sort(c("Germany", "Netherlands", "Belgium", "United Kingdom", "Ireland", "Norway", "Sweden", "France", "Ukraine",
								 "Austria", "Czech Republic", "Poland", "Denmark", "Switzerland", "Italy"))
	
	
	#d8s <- unique(brick_names$date)


	labels_countries <- list()
	for (l in country_cents$id){
		
		namez2get <- names(b)[grepl(l,names(b))]
		
		b_label <- b[[namez2get]]
		
		b_nams <- colsplit(names(b_label), "_",c("label", "date"))
		
		# get country means
		dat_countries <- exactextractr::exact_extract(b_label,country_shapes,fun = "mean")
		#dat_countries <- exactextractr::exact_extract(b_lab_mean,country_shapes,fun = "mean")
		
		names(dat_countries) <- b_nams$date
		
		# assign site names
		dat_countries$site <- country_shapes$iso_a3
		# assign label
		dat_countries$label <- l
		
		# add to list
		labels_countries[[l]] <- dat_countries
		
		#where up to?
		print(l)
		
	}
	
	print("done the bricks")
	
	site_concs <- do.call(rbind, labels_sites) |> 
		melt(c("site", "label")) |> 
		group_by(site, label) |>
		summarise(ch4 = mean(value, na.rm = TRUE)) |>
		filter(ch4 > 0) |>
		mutate(site = paste0("s_", site)) |> 
		arrange(label, ch4)
	
	country_concs <- do.call(rbind, labels_countries)|> 
		melt(c("site", "label")) |> 
		group_by(site, label) |> 
		summarise(ch4 = mean(value, na.rm = TRUE)) |> 
		filter(ch4 > 0) |> 
		#mutate(ch4 = ch4*1000000) |> 
		arrange(ch4)
	
	country_concs <- do.call(rbind, labels_countries) |> 
		melt(c("site", "label")) |> 
		group_by(site, label) |> 
		summarise(ch4 = mean(value, na.rm = TRUE)) |> 
		filter(ch4 > 0) |> 
		mutate(ch4 = ch4) |> 
		arrange(ch4) 
	
	print("dfs finsihed")
	
	#saveRDS(country_concs, "country_concs.RDS")
	
	#country_concs <- readRDS("C:/Users/kellyb/Downloads/88_donut_plots/88_donut_plots/country_concs.RDS")
	
	names(country_concs) <- c("receptor", "source", "ch4")
	
	names(site_concs) <- c("receptor", "source", "ch4")
	# Create odf object
	country_x = od(country_concs, country_cents, col_orig = "source", col_dest = "receptor", col_id = "id")
	
	site_x = od(site_concs, sites_countries, col_orig = "source", col_dest = "receptor", col_id = "id")
	
	print("ods done")
	# put sites in same list as countries
	
	# pick out the countries with the highest concetration and make them the ones to focus on
	
	#cols4all::c4a_gui()
	
	
	
	#cols4all::c4a_gui()
	#cols4all::c4a_palettes()
	
	# create a palette with required number of colours
	#CBS_pal <- cols4all::c4a("poly.glasbey", n = NROW(countries2show)+2)
	
	
	tm_outgoing = bake_donuts(country_x,
							  var = "ch4",
							  text = list(legend_title = "Country", legend_other = "Other country", legend_stay =
							  				"Home country", popup_residents = "Responsible concentrations", popup_stay = "Staying", popup_other =
							  				"&nbsp;&nbsp;&nbsp;&nbsp;to other regions", popup_inflow = "From other regions",
							  			popup_outflow = "Leaving", popup_to = "&nbsp;&nbsp;&nbsp;&nbsp;", edge_to = "to",
							  			edge_flow = "Flow"),
							  groupname = "CH4_transport",
							  highlight = countries2show,
							  donut_incoming = FALSE,
							  edge_incoming_only = FALSE,
							  edge_incoming = FALSE,
							  home4all = TRUE,
							  #title = "donutmap of CH4 concentrations in europe",
							  #title.position = c('left', 'bottom'),
							  pal = TNO_pal,
							  round_to = 0.001,
							  donut_size_min = min(country_concs$ch4), donut_size_max = max(country_concs$ch4),
							  flow_th = 1, flow_max = 100, flow_buffer = 1, flow_scale = 70,
							  donut_scale = 1.75)
	
	
	#tm_og <- readRDS("tm_outgoing.RDS")
	
	print("map created")
	
	
	current_mode = getOption("tmap.mode")
	
	tmap.mode = "view"
	print.tmap(x, show = show)
	
	
	#' @rdname tmap_leaflet
	#' @export
	tmap_grob = function(x,
						 asp = NA,
						 scale = 1,
						 show = FALSE,
						 ...) {
		current_mode = getOption("tmap.mode")
		on.exit({
			options(tmap.mode = current_mode)
		})
		options(tmap.mode = "plot")
		print.tmap(x + tm_options(asp = asp, scale = scale), show = show, ...)
	}
	
	#leaf <- tmap_leaflet(tm_outgoing)
	
	saveRDS(leaf, "tm_leaf.RDS")
	
	print("leaf")
	
	leaf <- leaf %>% addControl(
		html = HTML(
			'<div style="background: rgba(255,255,255,0.85); padding: 10px; border-radius: 6px; box-shadow: 0 0 0px rgba(0,0,0,0.3);">
  		<h3 style="margin: 0 0 5px 0; font-size: 16px; color: #333;">🍩 <b>Donut map</b></h3>
  		Where do CH<sub>4</sub>concentrations from each country end up?<br>
      🍩 proportion of concentrations from each country<br>
      ➖ closest half to each country is outgoing flow<br>  
      🔢 from <a href="https://airqualitymodeling.tno.nl/topas/topas-methane/" target="_blank" style="color:#0077cc; text-decoration: none;">TOPAS</a><br>
      📅 average of last 30 days<br>
      💳 adapted from plots by <a href="https://github.com/mtennekes/donutmaps/tree/main" target="_blank" style="color:#0077cc; text-decoration: none;">Martijn Tennekes @ </a> <a href="https://www.cbs.nl/nl-nl/over-ons/onderzoek-en-innovatie/project/meer-inzicht-in-mobiliteit-met-de-donutkaart" target="_blank" style="color:#0077cc; text-decoration: none;">CBS</a>
     </div>'
		),
		position = "bottomleft"
	)
	
	htmlwidgets::saveWidget(leaf, paste0(scratch, "cabauw_dashboard/website/tm_outgoing_", p, ".html"), selfcontained = TRUE)
	
	#tmap_save(leaf, "tm_outgoing.html",selfcontained = TRUE, add.titles = TRUE)
	
	
	tm_incoming = bake_donuts(country_x,
							  var = "ch4",
							  text = list(legend_title = "Country", legend_other = "Other country", legend_stay =
							  				"Home country", popup_residents = "total country concs", popup_stay = "Own country", popup_other =
							  				"&nbsp;&nbsp;&nbsp;&nbsp;from other regions", popup_inflow = "To other regions",
							  			popup_outflow = "Leaving", popup_to = "&nbsp;&nbsp;&nbsp;&nbsp;", edge_to = "to",
							  			edge_flow = "Incoming flow"),
							  groupname = "CH4_transport",
							  highlight = countries2show,
							  donut_incoming = TRUE,
							  edge_incoming_only = FALSE,
							  home4all = TRUE,
							  edge_incoming = TRUE,
							  pal = TNO_pal,
							  round_to = 0.001,
							  donut_size_min = min(country_concs$ch4), donut_size_max = max(country_concs$ch4),
							  flow_th = 1, flow_max = 110, flow_buffer = 1, flow_scale = 70,
							  donut_scale = 1.75)
	
	
	leaf <- tmap_leaflet(tm_incoming)
	
	leaf <- leaf %>% addControl(
		html = HTML(
			'<div style="background: rgba(255,255,255,0.85); padding: 10px; border-radius: 6px; box-shadow: 0 0 0px rgba(0,0,0,0.3);">
  		<h3 style="margin: 0 0 5px 0; font-size: 16px; color: #333;">🍩 <b>Donut map</b></h3>
  		Where do CH<sub>4</sub>concentrations from each country end up?<br>
      🍩 proportion of concentrations from each country<br>
      ➖ closest half to each country is incoming flow<br>  
      🔢 from <a href="https://airqualitymodeling.tno.nl/topas/topas-methane/" target="_blank" style="color:#0077cc; text-decoration: none;">TOPAS</a><br>
      📅 average of last 30 days<br>
      💳 adapted from plots by <a href="https://github.com/mtennekes/donutmaps/tree/main" target="_blank" style="color:#0077cc; text-decoration: none;">Martijn Tennekes @ </a> <a href="https://www.cbs.nl/nl-nl/over-ons/onderzoek-en-innovatie/project/meer-inzicht-in-mobiliteit-met-de-donutkaart" target="_blank" style="color:#0077cc; text-decoration: none;">CBS</a>
     </div>'
		),
		position = "bottomleft"
	)
	
	htmlwidgets::saveWidget(leaf, paste0(scratch, "cabauw_dashboard/website/tm_incoming_", p, ".html"), selfcontained = TRUE)
	
	tm_sites = bake_donuts(site_x,
						   var = "ch4",
						   text = list(legend_title = "Country", legend_other = "Other country", legend_stay =
						   				"Home country", popup_residents = "Total concentrations at site", popup_stay = "Total out", popup_other =
						   				"&nbsp;&nbsp;&nbsp;&nbsp;other countries", popup_inflow = "Total",
						   			popup_outflow = "Total in", popup_to = "&nbsp;&nbsp;&nbsp;&nbsp;", edge_to = "to",
						   			edge_flow = "Flow"),
						   groupname = "CH4_transport",
						   highlight = countries2show,
						   donut_incoming = TRUE,
						   edge_incoming_only = FALSE,
						   home4all = TRUE,
						   pal = TNO_pal,
						   round_to = 0.001,edge_incoming = TRUE,
						   donut_size_min = min(site_concs$ch4), donut_size_max = max(site_concs$ch4)*1.2,
						   flow_th = 2, flow_max = 130, flow_buffer = 2, flow_scale = 40,
						   donut_scale = 1.75)
	
	
	
	leaf <- tmap_leaflet(tm_sites)
	
	leaf <- leaf %>% addControl(
		html = HTML(
			'<div style="background: rgba(255,255,255,0.85); padding: 10px; border-radius: 6px; box-shadow: 0 0 0px rgba(0,0,0,0.3);">
  		<h3 style="margin: 0 0 5px 0; font-size: 16px; color: #333;">🍩 <b>Donut map</b></h3>
  		Which country do CH<sub>4</sub>concentrations modelled at each site originate from?<br>
      🍩 estimated proportion of concentrations from each country<br>
      ➖ flow from each country<br> 
      🔢 from <a href="https://airqualitymodeling.tno.nl/topas/topas-methane/" target="_blank" style="color:#0077cc; text-decoration: none;">TOPAS</a><br>
      📅 average of last 30 days<br>
      💳 adapted from plots by <a href="https://github.com/mtennekes/donutmaps/tree/main" target="_blank" style="color:#0077cc; text-decoration: none;">Martijn Tennekes @ </a> <a href="https://www.cbs.nl/nl-nl/over-ons/onderzoek-en-innovatie/project/meer-inzicht-in-mobiliteit-met-de-donutkaart" target="_blank" style="color:#0077cc; text-decoration: none;">CBS</a>
     </div>'
		),
		position = "bottomleft"
	)
	
	htmlwidgets::saveWidget(leaf, paste0(scratch, "cabauw_dashboard/website/tm_sites_", p, ".html"), selfcontained = TRUE)
	
}