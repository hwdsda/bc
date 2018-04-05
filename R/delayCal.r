#' @title A function to calculate delays within a crossing block
#'
#' @description A function to calculate delays within a crossing block.
#'
#' @param df, block, nFemale, nMale, delay.start, delay.increament
#'
#' @return a data frame
#'
#' @examples delayCal(Layout, 1, 8, 4, 50, 75)
#'
#' @export


delayCal <- function(datLayout, blk, nFemale, nMale, delay.start, delay.increment) {
	dat1 <- subset(datLayout, Block == blk)

	# female delay within each crossing block
	delay.fp <- NULL
	for (j in seq_len(nFemale)) {
		if (dat1$Line[j] == "Filler") {
			delay.fp1 <- NA
		} else if (dat1$SHDSLK[j] < (dat1$SHDSLK[nFemale+1] - delay.start)) {   # /10
			k=1
			while ((dat1$SHDSLK[j] + (delay.start+(k-1)*delay.increment)) <= dat1$SHDSLK[nFemale+1]) {k=k+1}    # /10
			delay.fp1 <- delay.start+(k-2)*delay.increment
		} else if (dat1$SHDSLK[j] > dat1$SHDSLK[nFemale+1]) {
			k=1
			while ((dat1$SHDSLK[j] - (delay.start+(k-1)*delay.increment)) > dat1$SHDSLK[nFemale+1]) {k=k+1}     # /10
			delay.fp1 <- -(delay.start+(k-1)*delay.increment)
		} else {delay.fp1 <- NA}

		delay.fp <- c(delay.fp, delay.fp1)
	}

	# male delay within each crossing block
	if (length(delay.fp[!is.na(delay.fp) & delay.fp < 0]) > 0) {
		unq.delay <- unique(delay.fp[!is.na(delay.fp) & delay.fp < 0])
		MaleDelay <- NULL
		delay.mp <- NULL
		for (m in unq.delay) {
			cnt <- length(grep(m, delay.fp[!is.na(delay.fp) & delay.fp < 0]))
			MaleDelay1 <- data.frame(MaleDelay = m, Count = cnt)
			if (cnt == 1) {delay.mp1 <- rep(-m, 1)} else {delay.mp1 <- rep(-m, ceiling(cnt/2))}
			MaleDelay <- rbind(MaleDelay, MaleDelay1)
			delay.mp <- c(delay.mp, delay.mp1)
		}
		if (length(delay.mp) > nMale) {delay.mp <- delay.mp[-seq_len(length(delay.mp)-nMale)]} else if (length(delay.mp) < nMale) {delay.mp <- c(rep(NA, (nMale-length(delay.mp))), delay.mp)}
		if (sum(is.na(delay.fp)) > 0) {delay.mp[1] <- NA} # keep at lease one no-delay male row if there are no-delay females

		delay.fp[!is.na(delay.fp) & delay.fp < 0] <- NA
	} else {delay.mp <- rep(NA, nMale)}

	dat1$Delay <- c(delay.fp, delay.mp)
	dat1
}
