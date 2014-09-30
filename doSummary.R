require("xts")

	# a function to make an array of Sierra Leone cases
	summarizeSLE <- function(){

		i <- read.delim("SEIRDh_data_flat.txt",sep = "\t",fill=TRUE,header=TRUE,stringsAsFactors=FALSE)
		i <- i[i$level==2&i$country=='SLE',]

		# Make a list of Administrative Areas
		aa <- unique(i$area)

		SLE.AA <- NULL

		SLE.AA <- loadZEBOVxts(0,"SierraLeone",0)$tot_case

		#iterator for on a for AAs  
		for(a in seq_along(aa)){
			SLE.AA  <- merge.xts(SLE.AA,loadZEBOVxts(2,aa[a],0)$tot_case)
		}

		# Throw the list to column headers
		# will this work?
		colnames(SLE.AA) <- c('Total',aa)

		# get day count
		t <- as.numeric(index(SLE.AA))
		t0 <- as.numeric(first(index(SLE.AA)))-1
		SLE.AA$day <- t-t0
		
		#drop NAs
		SLE.AA <- SLE.AA[!is.na(SLE.AA$Total),]
		return(SLE.AA)

	}


	summarize <- function(){
		SLE <- loadZEBOVxts(0,"SierraLeone",0)
		LBR <- loadZEBOVxts(0,"Liberia",0)
		GIN <- loadZEBOVxts(0,"Guinea",0)
		all <- GIN+SLE+LBR

		region<-NULL
		region<- all
		region$house <- NULL
		region$tot_recov <- NULL
		region$cur_admit <- NULL
		region$GINc <- GIN$tot_case
		region$GINd <- GIN$tot_death
		region$SLEc <- SLE$tot_case
		region$SLEd <- SLE$tot_death
		region$LBRc <- LBR$tot_case
		region$LBRd <- LBR$tot_death

		# get day count
		t <- as.numeric(index(region))
		t0 <- as.numeric(first(index(region)))-1
		region$day <- t-t0

		return(region)

	}


	#
	# regularize an irregular xts
	#
	# takes an irregular xts
	# returns a regular xts
	#
	# x <- regular(x)
	#

	regular <- function(x,extend=365){

		# make all the days
		empty <- seq.Date(from=index(first(x)), to=index(last(x))+extend, by="day")
		e.xts <- as.xts(empty)

		# get day count
		t <- as.numeric(index(e.xts))
		t0 <- as.numeric(first(index(e.xts)))-1
		e.xts$day <- t-t0


		reg.x <- merge(e.xts,x)

	#            return(reg.x) 


		#interp and trunc NAs
		#x.xts<-na.approx(reg.x)
		#x.xts <- round(x.xts.float)
	
	     return(reg.x)
	}

	# example, load Sierra Leone loadZEBOVxts(1,"SierraLeone")

	# example, load Monrovia loadZEBOVxts(3,"Monteserrado")


	loadZEBOVxts <- function(level=1,area,extend=365){

		i <- read.delim("SEIRDh_data_flat.txt",sep = "\t",fill=TRUE,header=TRUE)
		i <- i[i$level==level&i$area==area,]		
		i$area <- NULL
		i$source <- NULL
		i$country <- NULL

		i.zoo <- read.zoo(i,format = '%Y%m%d', tz='')
		index(i.zoo) <- as.Date(index(i.zoo),format = '%Y-%m-%d')
		i.xts <- as.xts(cbind(i.zoo$tot_case, i.zoo$cur_admit, i.zoo$tot_recover, i.zoo$tot_death , i.zoo$house))
		colnames(i.xts) <- c("tot_case","cur_admit","tot_recov","tot_death","house")
                reg.xts<-regular(i.xts,extend)

	  return(reg.xts)
	}

s <- summarize()
write.csv(s,"summary.csv",row.names=as.Date(index(s)))

sle <- summarizeSLE()
write.csv(sle,"summary.SLE.csv",row.names=as.Date(index(sle)),na = "NA")
