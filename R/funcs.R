# plotGeneSqrt
# Args:
# global:
#	 exonHeight  = -.25
#  from 
#  to 
#	 by

plotGeneSqrt <- function(xPos,yPos,gene,patID){
	exonSet <- as.vector(nexonCoords$exo[which(exonCoords$gene == gene)])
	for(i in 1:length(exonSet)){  # draw a rectangle for each exon
		if(i>1){
			xPos<-xPos+intronSpace  # add intron space
		}
		exonStart <- exonCoords$end[exonCoords$exon == exonSet[i]]
		exonEnd <- exonCoords$start[exonCoords$exon == exonSet[i]]
		sizeExon <- sqrt(exonStart - exonEnd)
		valueExon <- zscore[exonSet[i], patID]
		exonCol<-getShapeColor(valueExon, "grey")
		rect(xPos, yPos - exonHeight, xPos + sizeExon, yPos + exonHeight, 
				 col = exonCol,lwd = 0.5)
		xPos<-xPos+sizeExon
	}
}

# getShapeColor
# assign a color to a geometric shape.
# Not sure if I should also pass in the colScale (this is global parameter...) 
# global Args: 
# to		# max z-score, 2
# from	# min z-score, -2
# by 		# scale step

getShapeColor <- function(value, naColor){
	if(is.na(value)){
		shapeColor <- naColor
	} else {
		if(value < from){
			value <- from
		} else if(value > to){
			value <- to
		}
		shapeColor<-colorScale[findInterval(value,seq(from,to,by))]
	}
	return(shapeColor)
}



# get breakpoint, pass in specific gene, fusion event, and inital starting point 
# starting point for 1st gene is 1, and for 2nd gene is G1.L (length) + 100 (spacer) 
# global variables: exonCoords, delta, goiRows...
#exonSet<-as.vector(exonCoords$exon[which(exonCoords$gene==gene)])

getBreakPt <- function(gene, fusionPos, breakPt)
{
	goiRows <- which(exonCoords$gene==gene)
	if(unlist(strsplit(as.vector(exonCoords$exon[goiRows[1]]),":"))[3]=="+"){
		i<-1
		# test<-fusionCoords[fusionN,6]
		while(exonCoords$end[goiRows[i]] < fusionPos && i <= length(goiRows)){
			exonSize<-sqrt(exonCoords$end[goiRows[i]]-exonCoords$start[goiRows[i]])
			if(fusionPos < exonCoords$start[goiRows[i]]){
				breakPt=breakPt+(intronSpace+exonSize)/2
			} else {
				breakPt <- breakPt + intronSpace + exonSize
			}
			i=i+1
		}
	} else {
		i<-length(goiRows)
		while(exonCoords$end[goiRows[i]] < fusionPos && i >= 1){
			exonSize<-sqrt(exonCoords$end[goiRows[i]]-exonCoords$start[goiRows[i]])
			if(fusionPos < exonCoords$start[goiRows[i]]){
				breakPt=breakPt-(intronSpace + exonSize)/2
			} else {
				breakPt=breakPt-(intronSpace + exonSize)
			}
			i=i-1
		}
	}
	return(breakPt)
}




