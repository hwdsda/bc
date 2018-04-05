#' @title Run breeding cross shiny app
#'
#' @description Run breeding cross shiny app.
#'
#' @param NULL
#'
#' @return NULL
#'
#' @examples bCross()
#'
#' @export

bCross <- function(){
	shinyApp(
		ui = fluidPage(
			#shinythemes::themeSelector(),
			theme = shinytheme("slate"),
			headerPanel("Plant Breeding Cross Experiment Maker"),
			#h1(HTML("<font color='green'> Plant Breeding Cross Experiment Maker </font>")),
			sidebarPanel(
				fileInput('file1', 'Choose a .csv File:', accept=c('csv', selected = '.csv')),

				selectInput("filter0", "Designate Female and Male Parents:",
							choices = c("Female and Male Parents are Flexible" = "Flex", "Use P1 as Female Parent and P2 as Male Parent" = "Fixed"),
							selected = "Female and Male are Flexible", width = "280pt"),

				hr(),

				selectInput("filter1", "Number of Females in a Block:", choices = c(4, 6, 8, 10, 12, 14, 16), selected = 8, width = "200pt"),
				selectInput("filter2", "Number of Males in a Block:", choices = c(2, 3, 4, 5, 6, 7, 8), selected = 4, width = "200pt"),

				selectInput("filter3", "Delay Starts at:", choices = c(50, 75, 100), selected = 50, width = "200pt"),
				selectInput("filter4", "Delay Increment:", choices = c(50, 75, 100), selected = 50, width = "200pt"),

				br(),
				#p("Include actionButton to prevent write occuring before user finalizes selections"),
				actionButton("generateButton", "Create Layout and Calculate Delays"),
				downloadButton("downloadData", "Download"),
				width = 3
			),
			mainPanel(tabsetPanel(type = "tabs",
					tabPanel("Instructions", br(), p("Input Data Format:"), tableOutput('sampleDat1'),
												  hr(),
												  # instructions
												HTML(paste0("<font size = 5px> Instructions: </font>", "<br/>", "<br/>",
												"1. Prepare a .csv file using the above input data format. No blanks in SHD/SLK.", "<br/>",
												"2. Import the file and designate the female and male parents in the input file.", "<br/>",
												"3. Select <font color='grey'> <font size = 4px> Results </font> </font> tab.", "<br/>",
												"4. Change parameters (block pattern, delays) and click <b> Create Layout and Calculate Delays </b> button.", "<br/>",
												"5. Check <b> Layout and delays summary </b> in <font color='grey'> <font size = 4px> Results </font> </font> tab.", "<br/>",
												"6. Repeat step 3 and 4 to get the best layout and delays.", "<br/>",
												"7. Click <b> Download </b> button to save the layout and delays in a .xlsx file."
													)
												)
												  ),
					tabPanel("Results", br(), p("The first 10 rows of your input data:"), tableOutput('yourinput'),
										hr(), p("Layout and delays summary:"), htmlOutput('contents'))  # if want summary output in table, tableOutput("contents")
				)
			)
		),

		server = function(input, output) {

		 bcOutput <- reactive({
		   if(input$generateButton == 0){return()}

		   inFile <- input$file1
		   dat0 <- read.csv(inFile$datapath, header=T, stringsAsFactors=F)
		   yourinput <- dat0[1:10, ]

		   dat=dat0[rep(rownames(dat0), dat0[, "Number_of_Cross"]), -which(names(dat0) == "Number_of_Cross")]

		  # Stop reactions with isolate()
		  isolate({
			input$generateButton # The following steps won't excute until the action button is clicked.

			nFemale <- as.numeric(input$filter1)
			nMale <- as.numeric(input$filter2)
			delay.start <- as.numeric(input$filter3)
			delay.increment <- as.numeric(input$filter4)

		  ###################################################
		  ### create Layout (breeding cross or TI start)
		  ###################################################

		  ## 1. Create Layout if female and male are flexible.
		  if (input$filter0 == "Flex") {

			dat$crossNo <- seq_len(nrow(dat));
			nCross <- nrow(dat)
			dat$SAParentage <- ifelse(dat$P1 < dat$P2, paste(dat$P1, dat$P2, sep="/"), paste(dat$P2, dat$P1, sep="/"))

			# frequency of each cross
			cross <- table(dat$SAParentage)

			# create the layout of crossing blocks
			Layout <- NULL
			while (nrow(dat) > 0){
				# 1. find the frequency divisible GE's and set as Male in crossing blocks
				ge.div <- names(table(c(dat$P1, dat$P2))[table(c(dat$P1, dat$P2)) %% nFemale == 0])
				#ge.div <- names(sort(table(c(dat$P1, dat$P2))[table(c(dat$P1, dat$P2)) %% nFemale == 0], decreasing = T)[1])

				while (length(ge.div) > 0) {
				#while (!is.na(ge.div)) {
					block.div <- blockCreate(dat, ge.div, nFemale, nMale)$blockNew
					dat.block.div <- blockCreate(dat, ge.div, nFemale, nMale)$crossUsed

					Layout <- rbind(Layout, block.div)

					dat <- subset(dat, !(crossNo %in% dat.block.div$crossNo))

					ge.div <- names(table(c(dat$P1, dat$P2))[table(c(dat$P1, dat$P2)) %% nFemale == 0])
					#ge.div <- names(sort(table(c(dat$P1, dat$P2))[table(c(dat$P1, dat$P2)) %% nFemale == 0], decreasing = T)[1])
				}

				# 2. find GE frequency < nFemale and > nMale and set as Male in crossing blocks
				ge.less <- names(table(c(dat$P1, dat$P2))[table(c(dat$P1, dat$P2)) < nFemale & table(c(dat$P1, dat$P2)) > nMale])
				while (length(ge.less) > 0) {
					block.less <- blockCreate(dat, ge.less, nFemale, nMale)$blockNew
					dat.block.less <- blockCreate(dat, ge.less, nFemale, nMale)$crossUsed

					Layout <- rbind(Layout, block.less)

					dat <- subset(dat, !(crossNo %in% dat.block.less$crossNo))

					ge.less <- names(table(c(dat$P1, dat$P2))[table(c(dat$P1, dat$P2)) < nFemale & table(c(dat$P1, dat$P2)) > nMale])
				}

				# 3. find the most frequent GE and set as Male in crossing blocks
				freq.max <- table(c(dat$P1, dat$P2))[table(c(dat$P1, dat$P2)) == max(table(c(dat$P1, dat$P2)))]
				ge.max <- names(freq.max)[1]

				block.max <- blockCreate(dat, ge.max, nFemale, nMale)$blockNew
				dat.block.max <- blockCreate(dat, ge.max, nFemale, nMale)$crossUsed

				Layout <- rbind(Layout, block.max)

				dat <- subset(dat, !(crossNo %in% dat.block.max$crossNo))
			}

			Layout$Block <- rep(seq_len(nrow(Layout)/(nFemale+nMale)), each=nFemale+nMale)
			Layout <- Layout[order(Layout$MaleBlock, Layout$Block, Layout$Role, Layout$SHDSLK), ]
			Layout$Block <- rep(seq_len(nrow(Layout)/(nFemale+nMale)), each=nFemale+nMale) # assign block numbers

			Layout$SAParentage <- ifelse(Layout$Line < Layout$MaleBlock, paste(Layout$Line, Layout$MaleBlock, sep="/"), paste(Layout$MaleBlock, Layout$Line, sep="/"))
			Layout$SAParentage[Layout$Line == "Filler" | Layout$Role == "MP"] <- ""

			# remove duplicated crosses
			if (nrow(subset(Layout, SAParentage != "")) > nCross) {
				cross1 <- table(subset(Layout, SAParentage != "")$SAParentage)

				diff.cross = cross1-cross # frequency difference between newly created cross and expected cross

				Layout.update <- NULL
				for (i in names(diff.cross)[diff.cross > 0]) {
					Layout.dup <- subset(Layout, SAParentage == i)
					Layout.dup[2:nrow(Layout.dup), c("Line", "SHDSLK", "SAParentage")] <- c("Filler", 0, "") # find the duplicated crosses and replace with Filler

					Layout.update <- rbind(Layout.update, subset(Layout.dup, Line == "Filler"))
				}
				Layout.update$SHDSLK <- as.numeric(Layout.update$SHDSLK)

				# update Layout. the row names won't change
				Layout[row.names(Layout.update), c("Line", "SHDSLK", "SAParentage")] <- Layout.update[, c("Line", "SHDSLK", "SAParentage")]
			}

		  ## 2. Create Layout if using P1 as Female and P2 as female
		  } else if (input$filter0 == "Fixed") {
			Layout <- NULL
			n.block <- 0 # number of crossing blocks for the same donor
			for (d in unique(dat$P2)) {
				dat.donor <- subset(dat, P2 == d)
				dat.donor <- dat.donor[order(dat.donor$P1_SLK), ]
				n.donor <- nrow(dat.donor)

				male <- data.frame(Line=d, Role="MP", SHDSLK=dat.donor$P2_SHD[1])
				filler <- data.frame(Line="Filler", Role="FP", SHDSLK=0)

				dat.donor1 <- dat.donor[, c("P1", "P1_SLK")]
				names(dat.donor1) <- c("Line", "SHDSLK")
				dat.donor1$Role <- "FP"

				if (n.donor %% nFemale != 0) {dat.donor1 <- rbind(dat.donor1, filler[rep(1,nFemale-n.donor %% nFemale), ])}

				block <- NULL # donor block (same donor)
				for (i in seq_len(ceiling(n.donor/nFemale))) {
					female <- dat.donor1[(nFemale*(i-1)+1):(i*nFemale), ]
					block1 <- rbind(female, male[rep(1,nMale), ]) # each crossing block
					block1$Block.donor <- rep(i, nFemale+nMale)

					block <- rbind(block, block1)
				}
				block$MaleBlock <- d

				Layout <- rbind(Layout, block)
				n.block <- n.block + ceiling(n.donor/nFemale)
			}
			Layout <- Layout[order(Layout$MaleBlock, Layout$Block.donor, Layout$Role, Layout$SHDSLK), ]
			Layout <- Layout[, -which(names(Layout) == "Block.donor")]
			if (nrow(Layout)/(nFemale+nMale) != n.block) {stop("Check Layout file!\n")}
			Layout$Block <- rep(seq_len(n.block), each=nFemale+nMale)
			Layout$Parentage <- ifelse(Layout$Line == "Filler" | Layout$Role == "MP", "", paste(Layout$Line, Layout$MaleBlock, sep="/"))
			} else {
					stop("Check input data format!!")
				}

			###################################################
			### Calculate delay within each crossing block
			###################################################
			Layout.delay <- do.call(rbind, lapply(seq_len(max(Layout$Block)), delayCal, dat=Layout, nFemale=nFemale, nMale=nMale, delay.start=delay.start, delay.increment=delay.increment))

			Layout.delay <- Layout.delay[, c(5,1:4,6:7)]
			Layout.delay <- Layout.delay[order(Layout.delay$Block, Layout.delay$Role, Layout.delay$SHDSLK), ]
			Layout.delay[is.na(Layout.delay)] <- ""
			Layout.delay$Delay = as.numeric(Layout.delay$Delay)

		  }) # isolate ends

			### export to a EXCEL file
			# create an empty workbook
			wb <- createWorkbook()
			# add sheets to workbook
			addWorksheet(wb, "Sheet1")
			# freeze pane
			freezePane(wb, sheet="Sheet1", firstRow = TRUE) ## shortcut to firstActiveRow = 2
			# write data to each sheet
			writeData(wb, sheet="Sheet1", Layout.delay)
			# set column width to auto
			setColWidths(wb, sheet = "Sheet1", cols = 1:ncol(Layout.delay), widths = "auto")
			# grouping styles
			parentGrouping <- createStyle(border = "Bottom", borderStyle = "medium", borderColour = "blue")
			blockGrouping <- createStyle(border = "Bottom", borderStyle = "medium", borderColour = "black")
			# add grouping styles to seperate GE role and block
			addStyle(wb, sheet="Sheet1", parentGrouping, rows=seq_len(max(Layout.delay$Block))*(nFemale+nMale)+1-nMale, cols=seq_len(ncol(Layout.delay)), gridExpand=T)
			addStyle(wb, sheet="Sheet1", blockGrouping, rows=c(1,seq_len(max(Layout.delay$Block))*(nFemale+nMale)+1), cols=seq_len(ncol(Layout.delay)), gridExpand=T)
			# header bold
			addStyle(wb, sheet="Sheet1", createStyle(textDecoration = "Bold", fgFill = "gray"), rows=1, cols=seq_len(ncol(Layout.delay)), gridExpand=T)
			# save workbook to a .xlsx file
			output$downloadData <- downloadHandler(
			  filename = function() {
				paste0("Layout_delay_", nFemale, "to", nMale, "_", delay.start, "_", delay.increment, ".xlsx")
			  },
			  content = function(file) {
				saveWorkbook(wb, file, overwrite = TRUE)
			  }
			)

			# Final summary text, assigned to bcOutput
			contents <- paste0("Block Pattern: ", paste0(nFemale, ":", nMale), "<br/>",
							"Delay Starts at: ", delay.start, "<br/>",
							"Delay Increment: ", delay.increment, "<br/>","<br/>",
							"Number of Crosses: ", format((max(Layout.delay$Block)*nFemale - nrow(subset(Layout.delay, Line == "Filler"))), big.mark=",", scientific=FALSE), "<br/>",
							"Number of Blocks: ", format(max(Layout.delay$Block), big.mark=",", scientific=FALSE), "<br/>",
							"Number of Total Rows: ", format(nrow(Layout.delay), big.mark=",", scientific=FALSE), "<br/>",
							"Number of Filler Rows: ", paste0(format(nrow(subset(Layout.delay, Line == "Filler")), big.mark=",", scientific=FALSE), " (", round(nrow(subset(Layout.delay, Line == "Filler"))*100/nrow(Layout), 1), "%", ")"),"<br/>",
							"Maximum Delay: ", max(as.numeric(Layout.delay$Delay), na.rm=T)
						)

			# list all ouputs from bcOutput
			list(yourinput=yourinput, contents=contents)

		 }) # bcOutput ends

		  # pass the outputs to UI
		  output$yourinput <- renderTable({combo <- bcOutput(); combo$yourinput},include.rownames=FALSE)
		  output$contents <- renderUI({combo <- bcOutput(); HTML(combo$contents)}) # if want summary ouput in table, renderTable ... combo$contents

		  # pass the sample data input format to UI
		  output$sampleDat1 <- renderTable({
				data.frame(P1=c("Line001","Line002","Line003","Line004","Line005","Line005","Line005","Line005","Line006","Line007","..."),
						   P1_SHD=c(1250,1280,1210,1320,1320,1320,1320,1320,1230,1220,"..."), P1_SLK=c(1260,1270,1220,1300,1310,1310,1310,1310,1230,1210,"..."),
					   P2=c("Line158","Line158","Line158","Line158","Line136","Line169","Line143","Line158","Line158",'Line136',"..."),
						   P2_SHD=c(1450,1450,1450,1450,1320,1360,1400,1450,1450,1320,"..."), P2_SLK=c(1450,1450,1450,1450,1320,1330,1380,1450,1450,1320,"..."),
						   Number_of_Cross=c(1,1,1,1,2,1,1,1,3,1,"...")
					  )
			}, include.rownames=FALSE)
		}
	)
}
