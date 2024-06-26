# WCANN download ----
# info about West Coast api: https://www.nwfsc.noaa.gov/data/api/v1/source
library(readr)
library(jsonlite)
library(here)
library(httr)

wcann_save_loc <- "data_raw"
save_date <- Sys.Date()
catch_file_name <- paste("wcann", "catch.csv", sep="_")
haul_file_name <- paste("wcann", "haul.csv", sep="_")

# url_catch <- "https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=Groundfish%20Slope%20and%20Shelf%20Combination%20Survey,date_dim$year>=2003" #updated URL in March 2021
# data_catch <- jsonlite::fromJSON(url_catch)

url_catch <- "https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=Groundfish%20Slope%20and%20Shelf%20Combination%20Survey,date_dim$year>=2003"
header_type <- "applcation/json"
response<-GET(url_catch)
text_json <- content(response, type = 'text', encoding = "UTF-8")
jfile <- fromJSON( text_json)
data_catch <- as.data.frame(jfile)

url_haul <- "https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/trawl.operation_haul_fact/selection.json?filters=project=Groundfish%20Slope%20and%20Shelf%20Combination%20Survey,date_dim$year>=2003" #updated URL in March 2021
data_haul <- jsonlite::fromJSON(url_haul)


write.csv(data_catch, here::here(wcann_save_loc, catch_file_name))
write.csv(data_haul, here::here(wcann_save_loc, haul_file_name))
### NOTE: the above urls download files with different columns then if go directly to the FRAM site and click on the CSV next to the table type. This way has more column names, including Temperature 





#Old script below
#-----------------------------------------------------------------------------------------
# old_names <- c("Anoplopoma fimbria", "Antimora microlepis", "Apristurus brunneus",
#                "Bathyagonus nigripinnis", "Bathyraja kincaidii (formerly B. interrupta)",
#                "Careproctus melanurus", "Chauliodus macouni", "Glyptocephalus zachirus",
#                "Lycodes cortezianus", "Lycodes diapterus", "Lyopsetta exilis",
#                "Merluccius productus", "Microstomus pacificus", "Raja rhina",
#                "Sagamichthys abei", "Sebastolobus alascanus", "Sebastolobus altivelis",
#                "Bathylagidae unident.", "Bothrocara brunneum", "Coryphaenoides acrolepis",
#                "Lampanyctus sp.", "Lycenchelys crotalinus", "Myxinidae", "Atheresthes stomias",
#                "Bathyagonus pentacanthus", "Hydrolagus colliei", "Ophiodon elongatus",
#                "Sebastes alutus", "Sebastes babcocki", "Sebastes crameri", "Sebastes diploproa",
#                "Albatrossia pectoralis", "Alepocephalus tenebrosus", "Coryphaenoides cinereus",
#                "Embassichthys bathybius", "Talismania bifurcata", "Agonopsis vulsa",
#                "Icelinus filamentosus", "Sebastes brevispinis", "Sebastes elongatus",
#                "Sebastes emphaeus", "Sebastes entomelas", "Sebastes flavidus",
#                "Sebastes helvomaculatus", "Sebastes jordani", "Sebastes paucispinis",
#                "Sebastes pinniger", "Sebastes proriger", "Sebastes reedi", "Sebastes ruberrimus",
#                "Sebastes wilsoni", "Sebastes zacentrus", "Bathylagus sp.", "Bathyraja trachura",
#                "Diaphus theta", "Eptatretus sp.", "Myctophidae", "Paraliparis cephalus",
#                "Sternoptyx sp.", "Eopsetta jordani", "Gadus macrocephalus",
#                "Parophrys vetulus", "Sebastes melanostictus or Sebastes aleutianus",
#                "Squalus suckleyi", "Xeneretmus latifrons", "Hippoglossoides elassodon",
#                "Hippoglossus stenolepis", "Citharichthys sordidus", "Lycodes pacificus",
#                "Thaleichthys pacificus", "Alosa sapidissima", "Cryptacanthodes giganteus",
#                "Ronquilus jordani", "Sebastes saxicola", "Macropinna microstoma",
#                "Maulisia mauli", "Tactostoma macropus", "Avocettina infans",
#                "Hemilepidotus hemilepidotus", "Hexagrammos decagrammus", "Sebastes nigrocinctus",
#                "Theragra chalcogramma", "Clupea pallasi", "Microgadus proximus",
#                "Podothecus acipenserinus", "Psettichthys melanostictus", "Raja binoculata",
#                "Rajiformes egg case", "Bothrocara molle", "Lycenchelys camchatica",
#                "Lycodapus fierasfer", "Careproctus cypselurus", "Lycodapus mandibularis",
#                "Paraliparis rosaceus", "Sebastes aurora", "Tarletonbeania crenularis",
#                "Apristurus brunneus egg case", "Icichthys lockingtoni", "Malacocottus kincaidi",
#                "Sebastes melanostomus", "Sebastes rufus", "Aristostomias scintillans",
#                "Engraulis mordax", "Lepidopsetta bilineata", "Pleuronichthys decurrens",
#                "Isopsetta isolepis", "Platichthys stellatus", "Icelinus burchami",
#                "Anoplogaster cornuta", "Argyropelecus affinis", "Poromitra crassiceps",
#                "Leuroglossus stilbius", "Sebastes chlorostictus", "Careproctus gilberti",
#                "Chilara taylori", "Sebastes goodei", "Allosmerus elongatus",
#                "Cymatogaster aggregata", "Leptocottus armatus", "Cataetyx rubrirostris",
#                "Porichthys notatus", "Raja inornata", "Torpedo californica",
#                "Oncorhynchus tshawytscha", "Elassodiscus caudatus", "Paraliparis dactylosus",
#                "Osmeridae", "Zalembius rosaceus", "Raja stellulata", "Argentina sialis",
#                "Genyonemus lineatus", "Nezumia stelgidolepis", "Galeorhinus galeus",
#                "Peprilus simillimus", "Zaniolepis latipinnis", "Parmaturus xaniurus",
#                "Sternoptyx diaphana", "Enophrys bison", "Hyperprosopon anale",
#                "Mustelus californicus", "Sardinops sagax", "Squatina californica",
#                "Spirinchus starksi", "Physiculus rastrelliger", "Cephaloscyllium ventriosum",
#                "Sebastes caurinus", "Sebastes rubrivinctus", "Sebastes semicinctus",
#                "Hippoglossina stomata", "Sebastes sp. (Vermilion And Sunset)",
#                "Sebastes levis", "Icelinus fimbriatus", "Sebastes hopkinsi",
#                "Zaniolepis frenata", "Pleuronichthys verticalis", "Opisthoproctidae",
#                "Nezumia liolepis", "Sternoptychidae unident.", "Chiasmodon niger",
#                "Nemichthyidae", "Liparidinae", "Lepidopus xantusi", "Mustelus henlei",
#                "Paralabrax nebulifer", "Scorpaena guttata", "Synodus lucioceps",
#                "Trachurus symmetricus", "Rajidae unident.", "Dasycottus setiger",
#                "Hemilepidotus spinosus", "Sebastes maliger", "Lepidopsetta sp.",
#                "Triglops macellus", "Agonidae", "Paraliparis pectoralis", "Radulinus asprellus",
#                "Somniosus pacificus", "Poroclinus rothrocki", "Psychrolutes phrictus",
#                "Oneirodes sp.", "Coryphaenoides filifer", "Idiacanthus antrostomus",
#                "Sebastes auriculatus", "Pleuronichthys ritteri", "Spirinchus thaleichthys",
#                "Sebastes rosenblatti", "Sebastes mystinus", "Lycodapus endemoscotus",
#                "Xystreurys liolepis", "Sebastes umbrosus", "fish unident.",
#                "shark unident.", "Argyropelecus sp.", "Centroscyllium nigrum",
#                "Apristurus kampae", "Bajacalifornia burragei", "Stomias atriventer",
#                "Citharichthys xanthostigma", "Prionotus stephanophrys", "Magnisudis atlantica",
#                "Careproctus sp.", "Gonostomatidae", "Ophidiidae", "Lycodes palearis",
#                "Shark egg case unident.", "Melanocetus johnsonii", "Anarrhichthys ocellatus",
#                "Cottidae", "Ammodytes hexapterus", "Paralichthys californicus",
#                "Argentinidae", "Benthalbella dentata", "Dicrolene filamentosa",
#                "Rhinoliparis barbulifer", "Serrivomer sector", "Sebastes borealis",
#                "Leuroglossus schmidti", "Chitonotus pugetensis", "Sebastes dalli",
#                "Zoarcidae", "Scyliorhinidae", "Embiotoca lateralis", "Bajacalifornia erimoensis",
#                "Brosmophycis marginata", "Sebastes ensifer", "Macrouridae",
#                "Sebastes sp.", "Eptatretus stouti", "Icelinus sp.", "Raja sp. egg case",
#                "Lampetra tridentata", "Bathyraja abyssicola", "Amphistichus rhodoterus",
#                "Oncorhynchus kisutch", "Citharichthys sp.", "Nautichthys oculofasciatus",
#                "Sebastes constellatus", "Bathyraja aleutica", "Sebastes eos",
#                "Enophrys taurina", "Rhinobatidae", "Melamphaes lugubris", "Melanostomiidae",
#                "Myliobatis californicus", "Bathylagus milleri", "Stenobrachius leucopsarus",
#                "Nemichthys larseni", "Chesnonia verrucosa", "Petromyzontidae",
#                "Sebastes melanops", "Nansenia candida", "Platytroctidae", "Engraulidae",
#                "Aphanopus carbo", "Scopelosaurus harryi", "Triakididae", "Lamprogrammus niger",
#                "Anguilliformes", "Hexanchus griseus", "Kathetostoma averruncus",
#                "Scomber japonicus", "Embiotocidae", "Nemichthys scolopaceus",
#                "Oneirodidae", "Anoplogastridae", "Chiasmodontidae", "Melanonus zugmayeri",
#                "Chauliodontidae", "Osmerus mordax", "Lampanyctus ritteri", "Bathyraja sp. ",
#                "Symbolophorus californiensis", "Icelinus tenuis", "fish eggs unident.",
#                "Sebastes rosaceus", "Raja binoculata egg case", "Facciolella gilbertii",
#                "Lycodapus sp.", "Apristurus sp.", "Eptatretus deani", "Sebastes macdonaldi",
#                "Bathyraja sp. egg case", "Chaenophryne draco", "Sebastes serranoides",
#                "Phanerodon furcatus", "Coelorinchus scaphopsis", "Maynea californica",
#                "Symphurus atricauda", "Bathylagus pacificus", "Sebastes lentiginosus",
#                "Bellator xenisma", "Seriphus politus", "Xeneretmus leiops",
#                "Hemilepidotus sp.", "Damalichthys vacca", "Merluccius productus YOY",
#                "Melanostigma pammelas", "Lestidiops ringens", "Saccopharyngidae",
#                "Melamphaidae", "Arctozenus risso", "Venefica sp.", "Sebastes serriceps",
#                "Zapteryx exasperata", "Zesticelus profundorum", "Bathyraja trachura egg case",
#                "Jordania zonope", "Stromateidae", "Kali indica", "Icosteus aenigmaticus",
#                "Citharichthys stigmaeus", "Leuroglossus sp.", "Sebastes carnatus",
#                "Kali normani", "Synodontidae", "Trachipterus altivelis", "Tarletonbeania sp.",
#                "Clinocottus acuticeps", "Careproctus colletti", "Lycodapus dermatinus",
#                "Nectoliparis pelagicus", "Tetragonurus cuvieri", "Alepocephalidae",
#                "Cataetyx sp.", "Amphistichus argenteus", "Rhacochilus toxotes",
#                "Nettastomatidae", "Serrivomeridae", "Rhinoliparis sp.", "Gigantactis vanhoeffeni",
#                "Scopelengys tristis", "Bathyraja kincaidii egg case", "Radulinus taylori",
#                "Stomiidae", "Sebastes gilli", "Argyropelecus lychnus", "Cryptopsaras couesii",
#                "Moridae", "Bothidae unident.", "Sebastes (=Sebastomus) sp.",
#                "Myliobatidae", "Serrivomer jesperseni", "Sebastes ovalis", "Sebastes simulator",
#                "Stomiiformes", "Lyconectes aleutensis", "Lycodes brevipes",
#                "Rhinoliparis attenuatus", "Diaphus sp.", "Halargyreus johnsoni",
#                "Prionace glauca", "Torpedinidae", "Borostomias panamensis",
#                "Careproctus ovigerum", "Batrachoididae", "Sciaenidae", "Liparidae n. gen. (Orr)",
#                "Bolinia euryptera", "Malacosteidae", "Lycodema barbatum", "Maulisia sp.",
#                "Bathylychnops exilis", "Caristius macropus", "Bathophilus flemingi",
#                "Elassodiscus tremebundus", "Hypomesus pretiosus", "Paricelinus hopliticus",
#                "Paraliparis sp.", "Rhamphocottus richardsoni", "Scorpaenichthys marmoratus",
#                "Dasyatidae", "Chaenophryne longiceps", "Oneirodes thompsoni",
#                "Howella sherborni", "Phanerodon atripes", "Glyptocephalus zachirus larvae",
#                "fish larvae unident.", "Liparis fucensis", "Liparis pulchellus",
#                "Mola mola", "Atherinops affinis", "Venefica tentaculata", "Alepocephalus sp.",
#                "Bathyagonus sp.", "Psychrolutes paradoxus", "Agonopsis sterletus",
#                "Trichodon trichodon", "Dolichopteryx sp.", "Gymnocanthus tricuspis",
#                "Icelinus borealis", "Ophidion scrippsae", "Harriotta raleighana",
#                "Hydrolagus colliei egg case", "Howella brodiei", "Lepidopsetta polyxystra",
#                "Oneirodes acanthias", "Caulophryne jordani", "Centrolophidae",
#                "Lepidopus fitchi", "Stichaeidae", "Gibbonsia metzi", "Odontopyxis trispinosa",
#                "Cheilotrema saturnum", "Serrivomer sp.", "Plectobranchus evides",
#                "Malacocephalus laevis", "Sebastolobus sp.", "Platytroctes apus",
#                "Scymnodon squamulosus ", "Pleuronectidae", "Pleuronichthys coenosus",
#                "Cryptacanthodidae", "Dolichopteryx longipes", "Stenobrachius sp.",
#                "Lycodapus parviceps", "Uranoscopidae", "Benthodesmus pacificus",
#                "Pleuronectiformes", "Hydrolagus spp.", "Sebastes variegatus",
#                "Oxylebius pictus", "Zaniolepididae", "Neoclinus blanchardi",
#                "Desmodema lorum", "Gonostoma sp.", "Aphanopus intermedius",
#                "Syngnathidae", "Caulolatilus princeps", "Malacanthidae", "Melanonidae",
#                "Bathymasteridae", "Leptocephalus sp.", "Lumpenus maculatus",
#                "Triakis semifasciata", "Heterodontus francisci", "Ceratiidae unident.",
#                "Bathymaster signatus", "Eurypharynx pelecanoides", "Trachipteridae",
#                "Saccopharynx sp.", "Amblyraja badia")
# 
# #
# # wcann_catch <- as.data.table(download_catch_rates(survey="WCGBTS", add_zeros=FALSE, species_set=500))
# # wcann_catch[,TowID:=as.character(TowID)]
# # new_names <- wcann_catch[,unique(Sci)]
