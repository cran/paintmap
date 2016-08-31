#' Lines of space at margins of paintmap
margin_lines <- 2

#' Lines of space between the heatmap and row/column labels
lines_between_hm_and_labels <- 1/2

#' Get number of inches across a putative heatmap will be
#'
#' @template x
#' @param col_lines Numeric value determining number of lines width each column of the heatmap should occupy.
#' @return Numeric value.
#' @export
#' @importFrom grid convertUnit stringWidth
inches_wide <- function(x, col_lines=1) {
	max(convertUnit(x=stringWidth(if (is.null(rownames(x))) "" else rownames(x)), unitTo="inches", valueOnly=TRUE))+convertUnit(unit(lines_between_hm_and_labels + margin_lines + ncol(x) * col_lines, "lines"), "inches", valueOnly=TRUE)
}

#' Get number of inches high a putative heatmap will be
#'
#' @template x
#' @param row_lines Numeric value determining number of lines width each row of the heatmap should occupy.
#' @return Numeric value.
#' @export
#' @importFrom grid convertUnit stringWidth unit
inches_tall <- function(x, row_lines=1) {
	max(convertUnit(x=stringWidth(if (is.null(colnames(x))) "" else colnames(x)), unitTo="inches", valueOnly=TRUE))+convertUnit(unit(lines_between_hm_and_labels + margin_lines + nrow(x) * row_lines, "lines"), "inches", valueOnly=TRUE)
}

#' Convert numeric matrix to colour (character) matrix
#'
#' Given a numeric matrix, assign to each cell a colour (character) value based on linearly interpolating a given vector of colours.
#'
#' @param x Numeric or logical matrix.
#' @param colours Character vector of colours.
#' @return Character matrix.
#' @export
#' @importFrom grDevices heat.colors
colour_matrix <- function(x, colours=heat.colors(10)) {
	centres <- seq(from=min(x), to=max(x), length.out=length(colours))
	interval_width <- 1/length(colours)
	matrix(
		as.character(factor(levels=colours, cut(as.numeric(x), labels=colours, breaks=c(centres[1] - interval_width/2, centres + interval_width/2)))),
		nrow(x),
		ncol(x),
		dimnames=dimnames(x)
	)
}

#' Convert numeric matrix to color (character) matrix
#'
#' Given a numeric matrix, assign to each cell a color (character) value based on linearly interpolating a given vector of colors.
#'
#' @param x Numeric or logical matrix.
#' @param colors Character vector of colors.
#' @return Character matrix.
#' @export
#' @importFrom grDevices heat.colors
color_matrix <- function(x, colors=heat.colors(10)) {
	colour_matrix(x, colours=colors)
}

#' Plot paintmap
#'
#' @template x
#' @param add Add ink to current viewport.
#' @param ... Other graphical parameters for the rectangles of the grid to pass to \code{grid} function \code{gpar}, in turn passed to \code{grid} function \code{grid.rect}.
#' @return Plots heatmap.
#' @export
#' @importFrom grid convertUnit stringWidth viewport pushViewport grid.layout grid.newpage grid.rect grid.text popViewport unit gpar
#' @examples
#' paintmap(matrix(heat.colors(9), 3, 3, dimnames=list(letters[1:3], letters[4:6])))
paintmap <- function(x, add=FALSE, ...) {
	if (!add) grid.newpage()

	add_w_lines <- if (is.null(rownames(x))) { 0 } else { lines_between_hm_and_labels + max(convertUnit(x=stringWidth(rownames(x)), valueOnly=TRUE, unitTo="lines")) }
	add_h_lines <- if (is.null(colnames(x))) { 0 } else { lines_between_hm_and_labels + max(convertUnit(x=stringWidth(colnames(x)), valueOnly=TRUE, unitTo="lines")) }

	pushViewport(viewport(
		width=unit(1, "npc") + unit(- 1 * margin_lines, "lines"),
		height=unit(1, "npc") + unit(- 1 * margin_lines, "lines"),
		layout=grid.layout(
			2,
			2,
			widths=unit(c(add_w_lines, 1), c("lines", "null")),
			heights=unit(c(add_h_lines, 1), c("lines", "null"))
	)))

	pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
	grid.rect(
		x=rep(seq(from=1/ncol(x)/2, by=1/ncol(x), length.out=ncol(x)), each=nrow(x)),
		y=rep(seq(from=1/nrow(x)/2, by=1/nrow(x), length.out=nrow(x)), times=ncol(x)),
		width=rep(1/ncol(x), ncol(x)),
		height=rep(1/nrow(x), nrow(x)),
		gp=gpar(fill=as.character(x), ...)
	)
	popViewport()

	if (!is.null(rownames(x))) {
		pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
		grid.text(
			label=rownames(x), 
			x=unit(1, "npc") + unit(-lines_between_hm_and_labels, "lines"),
			hjust=1,
			y=seq(from=1/nrow(x)/2, by=1/nrow(x), length.out=nrow(x))
		)
		popViewport()
	}
	
	if (!is.null(colnames(x))) { 
		pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
		grid.text(
			label=colnames(x), 
			x=seq(from=1/ncol(x)/2, by=1/ncol(x), length.out=ncol(x)),
			y=unit(lines_between_hm_and_labels, "lines"),
			rot=90,
			hjust=0
		)
		popViewport()
	}

	popViewport()
}
