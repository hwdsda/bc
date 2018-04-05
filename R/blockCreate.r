#' @title A function to create crossing blocks when the male is known
#'
#' @description This function creates crossing block layout when the male parent is known.
#'
#' @param x, ge,male, nFamle, nMale
#'
#' @return a data frame
#'
#' @examples blockCreate(df, "Line001", 8, 4)
#'
#' @export

blockCreate <- function(x, ge.male, nFemale, nMale) {
		x1 <- subset(x, P1 %in% ge.male | P2 %in% ge.male)

		block <- NULL # new crossing blocks
		cross <- NULL # crosses used in those new crossing blocks
		for (i in ge.male) {
			x2 <- subset(x1, P1 == i | P2 == i)

			if (nrow(x2) > 0) {
				x2$MP <- i
				x2$FP <- ifelse(x2$P1 == i, x2$P2, x2$P1)
				x2$SHD.MP <- ifelse(x2$P1 == i, x2$P1_SHD, x2$P2_SHD)
				x2$SLK.FP <- ifelse(x2$P1 == i, x2$P2_SLK, x2$P1_SLK)

				# create female, male block
				if (nrow(x2) < nFemale) {
					filler <- data.frame(FP="Filler", SLK.FP=0)
					female <- rbind(filler[rep(1, nFemale-nrow(x2)), ], x2[order(x2$SLK.FP), c("FP", "SLK.FP")])} else {
						x2$SHDSLK.diff <- abs(x2$SHD.MP - x2$SLK.FP)
						x2 <- x2[order(-x2$SHDSLK.diff), ][seq_len((nrow(x2) %/% nFemale)*nFemale), ]
						female <- x2[order(x2$SLK.FP), c("FP", "SLK.FP")]
						}
				names(female) <- c("Line", "SHDSLK")
				female$Role <- "FP"
				female <- female[, c("Line", "Role", "SHDSLK")]

				male <- data.frame(Line=i, Role="MP", SHDSLK=x2$SHD.MP[1])

				# combine female and male into one block
				if (nrow(x2) < nFemale) {block.ge <- rbind(female, male[rep(1, nMale), ]); x2$SHDSLK.diff <- 0} else {
					block.ge <- NULL
					for (j in seq_len(nrow(x2) %/% nFemale)) {
						block.ge1 <- rbind(chunk(female, nrow(female) %/% nFemale)[[j]], male[rep(1, nMale), ])
						block.ge <- rbind(block.ge, block.ge1)
					}
				}
				block.ge$MaleBlock <- i
			} else {block.ge <- NULL; x2 <- NULL}

			## allow duplicated cross? Comment out to allow duplicated cross, then replace with filler. This is the most efficient way.
			#x1 <- subset(x1, P1 != i & P2 != i)

			block <- rbind(block, block.ge)
			cross <- rbind(cross, x2) # all crosses used for those new crossing blocks
		}
		return(list(blockNew=block, crossUsed=cross))
}
