#.libPaths("~/R/lib2.11")

setwd("~/blosxom")

library(rJava)
library(tools)

jinit <- -1

prettyHTMLdiff <- function(text1, text2) {
    if (jinit < 0) jinit <<- .jinit("diff_match_patch.jar")
    dmp <- .jnew("diff_match_patch")
    diff <- .jcall(dmp, "Ljava/util/LinkedList;", "diff_main", text1, text2)
    .jcall(dmp, "V", "diff_cleanupSemantic", diff)
    .jcall(dmp, "Ljava/lang/String;", "diff_prettyHtml", diff)
}

# This doesn't work well.
fuzzyMatch <- function(text1, text2, sloppiness=0.5) {
    if (jinit < 0) jinit <<- .jinit("diff_match_patch.jar")
    dmp <- .jnew("diff_match_patch")
    diff <- .jcall(dmp, "I", "match_main", text1, text2, 1L)
    diff >= 0
}

htmlescape <- function(s) {
    s <- gsub("&", "&amp;", s)
    s <- gsub('"', "&quot;", s)
    s <- gsub("<", "&lt;", s)
    gsub(">", "&gt;", s)
}

linkToBugs <- function(text) {
    gsub("PR#([[:digit:]]*)", '<a href="https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=\\1">PR#\\1</a>', text)
}    

makeHTMLItem <- function() {

    lastVersion <- ""
    lastSection <- ""
    
    Sections <- character(0)
    
    printtext <- function(item, id) {
	if (id[4] != lastVersion) {
            if (lastVersion != "") cat("</UL>\n")
	    lastVersion <<- id[4]
	    lastSection <<- ""
	}
	if (id[5] != lastSection) {
	    if (lastSection != "") cat("</UL>\n")
	    cat("<H4>", lastVersion, " ", htmlescape(lastSection <<- id[5]), " ", htmlescape(id[2]), "</H4>\n<UL>\n",sep="")
	    Sections <<- unique(c(Sections, lastSection))
	}
	item <- gsub("^[[:space:]]*$", "</P>\n<P>", htmlescape(item))
	item <- linkToBugs(item)
	if ( length(grep("Deleted", id[1])) )
	    item <- c('<DEL STYLE="background:#FFE6E6;">', item, "</DEL>")
	cat(c("<LI><P>", item, "</P></LI>"), sep="\n")
	cat("\n")
    }
    
    difftext <- function(item1, item2, id, name1, name2) {
        id <- c("", id)
	if (id[4] != lastVersion) {
	    if (lastVersion != "") cat("</UL>\n")
	    lastVersion <<- id[4]
	    lastSection <<- ""
	}
	if (id[5] != lastSection) {
	    if (lastSection != "") cat("</UL>\n")
	    cat("<H4>", lastVersion, " ", htmlescape(lastSection <<- id[5]), " ", htmlescape(id[2]), "</H4>\n<UL>\n",sep="")
	    Sections <<- unique(c(Sections, lastSection))
	}
	item1 <- paste(gsub("^[[:space:]]*$", " DIFFNEWSITEMBREAK ", item1), collapse=" ")
	item2 <- paste(gsub("^[[:space:]]*$", " DIFFNEWSITEMBREAK ", item2), collapse=" ")
	diff <- prettyHTMLdiff(item1, item2)
	diff <- gsub("DIFFNEWSITEMBREAK", "</P>\n<P>", diff)
	cat(c("<LI><P>", diff, "</P></LI>"), sep="\n")
    }
    list(printtext, difftext)
}

stripwhite <- function(x) {
    x <- gsub("^[[:space:]]*", "", x)
    gsub("[[:space:]]*$", "", x)

}

delblanks <- function(x) {
    while (length(x) && x[length(x)] == "") x <- x[-length(x)]
    x
}

printTree <- function(tree, id, printitem) {
    result <- 0
    if (!length(tree)) return(result)
    if (is.recursive(tree)) {
	if (!length(names <- names(tree)))
	    names <- 1:length(tree)
	for (n in names) result <- result + printTree(tree[[n]], c(id, n), printitem)
    } else {
        tree <- delblanks(gsub("^[ *]*$", "", tree))  # readNEWS bug
	printitem[[1]](tree, id)
	result <- 1
    }
    return(result)
}

lastResult <- c(0, 0)

diffNEWS <- function(x, y, xid=deparse(substitute(x)), yid=deparse(substitute(y)), printitem = makeHItem()) {
    
    result <- c(0, 0)
    if (!identical(x, y)) {
	if (is.recursive(x) && is.recursive(y)) {
	    if (!length(ynames <- names(y))) {
		if (length(y) && !is.recursive(y[[1]]))
		    ynames <- sapply(y, function(item) item[1])
		else
		    ynames <- seq_along(y)
		names(y) <- ynames
	    }
	    if (!length(xnames <- names(x))) {
		if (length(x) && !is.recursive(x[[1]])) {
		    xnames <- sapply(x, function(item) item[1])
		    
		    # Try for approximate matches to the names in case of spelling corrections.
		    
		    xmiss <- which(!(xnames %in% ynames))
		    ymissed <- ynames[!(ynames %in% xnames)]
		    if (length(xmiss) && length(ymissed)) {
			for (i in seq_along(xmiss)) {
			    if (length(match <- agrep(xnames[xmiss[i]], ymissed, ignore.case=TRUE))
			       || length(match <- agrep(xnames[xmiss[i]], ymissed, ignore.case=TRUE, max=0.5))) {
				xnames[xmiss[i]] <- ymissed[match[1]]
				ymissed <- ymissed[-match[1]]
			    }
			}
		    }
		} else
		    xnames <- seq_along(x)
		names(x) <- xnames
	    }	    
	    if (length(diff <- setdiff(xnames, ynames))) {
		result[1] <- result[1] + printTree(x[diff], xid, printitem)
	    }
	    if (length(diff <- setdiff(ynames, xnames))) {
		result[2] <- result[2] + printTree(y[diff], yid, printitem)
	    }
	    for (n in intersect(xnames, ynames)) {
		result <- result + diffNEWS(x[[n]], y[[n]], c(xid,n), c(yid, n), printitem)
	    }	
	} else {
	    x <- delblanks(gsub("^[ *]*$", "", x))  # readNEWS bug
	    y <- delblanks(gsub("^[ *]*$", "", y))  # readNEWS bug
	    if (!identical( stripwhite(x), stripwhite(y) )) {
	    	result[1] <- result[1] + 1
	    	result[2] <- result[2] + 1
	    	printitem[[2]](x, y,  xid[-1], xid[1], yid[1])
	    }
	}
    }
    lastResult <<- result
    invisible(result)
}

Rdtag <- function(x) attr(x, "Rd_tag")

getText <- function(Rd, srcref) {
    if (is.character(Rd)) return(Rd)
    else {
    	attr(Rd, "srcref") <- srcref
    	tools:::.Rd_get_text(Rd)
    }
}

addnames <- function(x, tag) {
    result <- list()
    for (i in seq_along(x)) {
	if (identical(Rdtag(x[[i]]), tag)) {
	    item <- list(x[[i]])
	    name <- x[[i]][[1]]
	    name <- getText(name, getSrcref(x[[i]]))
	    if (tag == "\\section")
	    	name <- sub("^CHANGES IN R VERSION ", "", name)
	    names(item) <- name
	    result <- c(result, item)
	}
    }
    result
}

splititems <- function(x) {
    savecurrent <- function() {
	if (length(this)) {
	    text <- capture.output(tools::Rd2txt(this, fragment=TRUE))
	    this <- list(text)
	    names(this) <- paste(text, collapse="\n")
	    result <<- c(result, this)
	}
	this <<- list()     
    }
    result <- list()
    this <- NULL
    for (i in seq_along(x)) {
	if (Rdtag(x[[i]]) == "\\item") {
	    savecurrent()
	} else if (!is.null(this)) {
	    this <- c(this, list(x[[i]]))
	}
    }
    savecurrent()
    result
}

printTreeRd <- function(x, id, printitem, level) {
    result <- 0
    if (!length(x)) return(result)
    if (level < 4) {
	names <- names(x)
	level <- level + 1
	for (n in names) {
	    xn <- x[[n]]
	    if (level == 1) {
		xn <- addnames(xn[[2]], "\\subsection")
	    } else if (level == 2) {
		xn <- addnames(xn[[2]], "\\itemize")
	    } else if (level == 3) {
		xn <- splititems(xn)
	    }	    
	    result <- result + printTreeRd(xn, c(id, n), printitem, level)
	}
    } else {
	printitem[[1]](x, id)
	result <- 1
    }
    return(result)
}


diffNEWSRd <- function(x, y, xid=deparse(substitute(x)), yid=deparse(substitute(y)), printitem = makeHItem(),
                       level = 0) {
    result <- c(0, 0)
    if (!identical(x, y)) {
        if (level == 0) {
            x <- addnames(x, "\\section")
            y <- addnames(y, "\\section")
        } else if (level == 1) {
            x <- addnames(x[[2]], "\\subsection")
            y <- addnames(y[[2]], "\\subsection")
        } else if (level == 2) {
            x <- addnames(x[[2]], "\\itemize")
            y <- addnames(y[[2]], "\\itemize")
        } else if (level == 3) {
            x <- splititems(x)
            y <- splititems(y)
        }
        if (level < 4) {
	    ynames <- names(y)
	    xnames <- names(x)

	    # Try for approximate matches to the names in case of spelling corrections.

	    xmiss <- which(!(xnames %in% ynames))
	    ymissed <- ynames[!(ynames %in% xnames)]
	    if (length(xmiss) && length(ymissed)) {
		for (i in seq_along(xmiss)) {
		    if (length(match <- agrep(xnames[xmiss[i]], ymissed, ignore.case=TRUE))
		       || length(match <- agrep(xnames[xmiss[i]], ymissed, ignore.case=TRUE, max=0.5))) {
			xnames[xmiss[i]] <- ymissed[match[1]]
			ymissed <- ymissed[-match[1]]
		    }
		}
	    }

	    if (length(diff <- setdiff(xnames, ynames))) {
		result[1] <- result[1] + printTreeRd(x[diff], xid, printitem, level)
	    }
	    if (length(diff <- setdiff(ynames, xnames))) {
		result[2] <- result[2] + printTreeRd(y[diff], yid, printitem, level)
	    }
	    for (n in intersect(xnames, ynames)) {
		result <- result + diffNEWSRd(x[[n]], y[[n]], c(xid,n), c(yid, n), printitem, level=level+1)
	    }	
	} else {
	    if (!identical( x, y )) {
	    	result[1] <- result[1] + 1
	    	result[2] <- result[2] + 1
	    	printitem[[2]](x, y,  xid[-1], xid[1], yid[1])
	    }
	}
    }
    lastResult <<- result
    invisible(result)
}


# If both startDate and startRev are given, startDate must be on or before the date of startRev!

dailyDiffNEWS <- function(file, previous, endDate=Sys.Date(), directory = "dailyDiff", label="", prefix="NEWS") {
    start <- NULL
    startDate <- previous[[1]]
    startRev <- previous[[2]]
    day <- as.Date(startDate)
    endDate <- as.Date(endDate)
    useNEWSRd <- grepl("[.]Rd$", file)
    repeat {
    	if (startRev && is.null(start)) {
    	    cmd <- paste("svn up -r", startRev, file)
    	    day <- day - 1
    	} else if (day < endDate || (day == endDate && endDate < Sys.Date()) ) 
            cmd <- paste("svn up -r\\{", day, "\\} ", file, sep="")
        else if (day == Sys.Date())
            cmd <- paste("svn up", file)
        else
            break
        
        cat(cmd, "\n")
        svnresult <- system(cmd, intern=TRUE)
        cat(svnresult, sep="\n")
        flush.console()
        svnresult <- strsplit( svnresult[length(svnresult)], " " )
        svnrevision <- as.numeric(rev(svnresult[[1]])[1])
        
        if (svnrevision > startRev || is.null(start)) {
            if (useNEWSRd) 
            	new <- tools:::prepare_Rd(parse_Rd(file), stages = "install")
            else
	    	new <- readNEWS(file, chop="par1")
	    if (!is.null(start)) {
		f <- makeHTMLItem()
		if (useNEWSRd) 
		    lines <-try( capture.output(diffNEWSRd(start, new, c("Deleted", label, ""), c("Added", label, ""), f)))
		else
		    lines <- try(capture.output(diffNEWS(start, new, c("Deleted", label), c("Added", label), f)))
		if (inherits(lines, "try-error")) {
		    print(lines)
		    lines <- NULL
		}

		if (length(lines)) {
		    title <- paste(day)
		    lines <- c(title, lines, "</UL>")
		    filename <- file.path(directory, paste(prefix, day, ".txt", sep=""))
		    writeLines(lines, con=filename)
		    cmd <- paste("touch --date=",day," ",filename, sep="")
		    system(cmd)
		}
	    }
	    startRev <- svnrevision
	    startDate <- day
            start <- new
        }
        day <- day + 1
    }
    cmd <- paste("chmod 644 ",file.path(directory, "*.txt"))
    system(cmd)
    invisible(list(startDate, startRev))
}


NEWSdone <- list("2010-06-27", 48298)
NEWS36done <- list("2019-03-28", 76287)
NEWS40done <- list("2020-03-26", 78076)
NEWS41done <- list("2021-04-19", 80189)
NEWS42done <- list("2022-03-24", 81976)
NEWS43done <- list("2023-03-29", 84112)

load(file="~/blosxom/start.RData")

# debug(dailyDiffNEWS)

NEWSdone <- dailyDiffNEWS("~/R/R-devel/doc/NEWS.Rd", NEWSdone, 
                          dir="~/blosxom/data/R-devel/NEWS",prefix="n")

NEWS43done <- dailyDiffNEWS("~/R/R-4-3-branch/doc/NEWS.Rd", NEWS43done, 
			    dir="~/blosxom/data/R-4-3-branch/NEWS",prefix="n")

NEWS42done <- dailyDiffNEWS("~/R/R-4-2-branch/doc/NEWS.Rd", NEWS42done, 
			    dir="~/blosxom/data/R-4-2-branch/NEWS",prefix="n")

NEWS41done <- dailyDiffNEWS("~/R/R-4-1-branch/doc/NEWS.Rd", NEWS41done,
                            dir="~/blosxom/data/R-4-1-branch/NEWS",prefix="n")

NEWS40done <- dailyDiffNEWS("~/R/R-4-0-branch/doc/NEWS.Rd", NEWS40done,
                            dir="~/blosxom/data/R-4-0-branch/NEWS",prefix="n")

NEWS36done <- dailyDiffNEWS("~/R/R-3-6-branch/doc/NEWS.Rd", NEWS36done,
                            dir="~/blosxom/data/R-3-6-branch/NEWS",prefix="n")


save(NEWSdone,  
     NEWS36done,
     NEWS40done,
     NEWS41done,
     NEWS42done,
     NEWS43done,
     file="~/blosxom/start.RData")
