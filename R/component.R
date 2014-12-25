
draw_dendrogram = function(hc, horizontal = T, n = length(hc$order), offset = 0){
	h = hc$height / max(hc$height) / 1.05
	m = hc$merge
	o = hc$order

	m[m > 0] = n + m[m > 0] 
	m[m < 0] = abs(m[m < 0])

	dist = matrix(0, nrow = 2 * n - 1, ncol = 2, dimnames = list(NULL, c("x", "y"))) 
	dist[1:n, 1] = 1 / n / 2 + (1 / n) * (match(1:n, o) - 1)

	for(i in 1:nrow(m)){
		dist[n + i, 1] = (dist[m[i, 1], 1] + dist[m[i, 2], 1]) / 2
		dist[n + i, 2] = h[i]
	}

	dist[, 1] = dist[, 1] + offset/n
	
	draw_connection = function(x1, x2, y1, y2, y){
		grid.lines(x = c(x1, x1), y = c(y1, y))
		grid.lines(x = c(x2, x2), y = c(y2, y))
		grid.lines(x = c(x1, x2), y = c(y, y))
	}
	
	if(horizontal){
		for(i in 1:nrow(m)){
			draw_connection(dist[m[i, 1], 1], dist[m[i, 2], 1], dist[m[i, 1], 2], dist[m[i, 2], 2], h[i])
		}
	}
	
	else{
		gr = rectGrob()
		pushViewport(viewport(height = unit(1, "grobwidth", gr), width = unit(1, "grobheight", gr), angle = 90))
		dist[, 1] = 1 - dist[, 1] 
		for(i in 1:nrow(m)){
			draw_connection(dist[m[i, 1], 1], dist[m[i, 2], 1], dist[m[i, 1], 2], dist[m[i, 2], 2], h[i])
		}
		upViewport()
	}
}

draw_matrix = function(matrix, mat_list = mat_list){
	
	#n = nrow(matrix)
	#m = ncol(matrix)

	# <============================
	#if(!is.null(mat_list)) {
	#	m = m + sum(sapply(mat_list, ncol))
	#}

	# <============================
	

	#x = (1:m)/m - 1/2/m
	#y = 1 - ((1:n)/n - 1/2/n)
	# first matrix
	#for(i in seq_len(ncol(matrix))){
	#	grid.rect(x = x[i], y = y[1:n], width = 1/m, height = 1/n, gp = gpar(fill = matrix[,i], col = matrix[,i], lwd = 0.1))
	#}

	# optional matrix
	#t = ncol(matrix)
	#for(k in seq_along(mat_list)) {
	#	for(i in t+seq_len(ncol(mat_list[[k]]))){
	#		grid.rect(x = x[i], y = y[1:n], width = 1/m, height = 1/n, gp = gpar(fill = mat_list[[k]][,i-t], col = mat_list[[k]][,i-t], lwd = 0.1))
	#	}
	#	t = t + ncol(mat_list[[k]])
	#}

	### merge mat and mat_list as a big matrix, including gaps between
	big_mat = matrix
	if(!is.null(mat_list)) {
		for(i in seq_along(mat_list)) {
			big_mat = cbind(big_mat, mat_list[[i]])
		}
	}

	m = ncol(big_mat)
	n = nrow(big_mat)

	x = (1:m)/m - 1/2/m
	y = 1 - ((1:n)/n - 1/2/n)
	df = expand.grid(y, x)
	grid.rect(x = df[[2]], y = df[[1]], width = 1/m, height = 1/n, gp = gpar(fill = big_mat, col = NA))
	
}

draw_colnames = function(coln, ...){
	m = length(coln)
	x = (1:length(coln))/m - 1/2/m
	grid.text(coln, x = x, y = unit(0.9, "npc"), vjust = 0.5, hjust = 0, rot = 270, gp = gpar(...))
}

draw_rownames = function(rown, ...){
	n = length(rown)
	y = 1 - ((1:n)/n - 1/2/n)
	grid.text(rown, x = unit(0.1, "npc"), y = y, vjust = 0.5, hjust = 0, gp = gpar(...))	
}


# title, legend grid and legend labels
# returns: position
draw_legend = function(legend_title, legend, breaks, col_fun, y = unit(1, "npc"), ...){
	color = col_fun(breaks)
	
	text_height = unit(1, "grobheight", textGrob("FGH", gp = gpar(...)))
	grid.text(legend_title, x = 0, y = y, vjust = 1, hjust = 0, gp = gpar(fontface = "bold", ...))
	y = y - 2 * text_height
	
	if(is.null(attr(col_fun, "levels"))) {
		height = unit(100, "bigpts")
		pushViewport(viewport(x = 0, y = y, just = c(0, 1), height = height))
		legend_pos = (legend - min(breaks)) / (max(breaks) - min(breaks))
		breaks = (breaks - min(breaks)) / (max(breaks) - min(breaks))
		h = breaks[-1] - breaks[-length(breaks)]
		grid.rect(x = 0, y = breaks[-length(breaks)], width = unit(10, "bigpts"), height = h, hjust = 0, vjust = 0, gp = gpar(fill = color[-length(breaks)], col = "#FFFFFF00"))
		grid.text(names(legend), x = unit(12, "bigpts"), y = legend_pos, hjust = 0, gp = gpar(...))
		upViewport()
	} else {
		le = attr(col_fun, "levels")
		height = text_height * length(le) * 1.3
		pushViewport(viewport(x = 0, y = y, just = c(0, 1), height = height))
		grid.rect(x = 0, y = (seq_along(le)-1) * text_height * 1.3, width = unit(10, "bigpts"), height = text_height, hjust = 0, vjust = 0, gp = gpar(fill = col_fun(seq_along(le)), col = "#FFFFFF00"))
		grid.text(le, x = unit(14, "bigpts"), y = (seq_along(le)-1) * text_height * 1.3 + 0.5*text_height, hjust = 0, vjust = 0.5, gp = gpar(...))
		upViewport()
	}
	return(y - height - 2 * text_height)
}


draw_annotations = function(converted_annotations, n, offset = 0){
	m = n
	n = ncol(converted_annotations)
	x = (1:m)/m - 1/2/m
	y = cumsum(rep(8, n)) - 4 + cumsum(rep(2, n))
	for(i in 1:nrow(converted_annotations)){
		grid.rect(x = x[i+offset], unit(y[1:n], "bigpts"), width = 1/m, height = unit(8, "bigpts"), gp = gpar(fill = converted_annotations[i, ], col = converted_annotations[i, ], lwd = 0.1))
	}
}

draw_annotation_legend = function(annotation, annotation_colors, ...){
	y = unit(1, "npc")
	text_height = unit(1, "grobheight", textGrob("FGH", gp = gpar(...)))
	for(i in names(annotation_colors)){
		grid.text(i, x = 0, y = y, vjust = 1, hjust = 0, gp = gpar(fontface = "bold", ...))
		y = y - 1.5 * text_height
		if(is.character(annotation[[i]]) | is.factor(annotation[[i]])){
			for(j in 1:length(annotation_colors[[i]])){
				grid.rect(x = unit(0, "npc"), y = y, hjust = 0, vjust = 1, height = text_height, width = text_height, gp = gpar(col = "#FFFFFF00", fill = annotation_colors[[i]][j]))
				grid.text(names(annotation_colors[[i]])[j], x = text_height * 1.3, y = y, hjust = 0, vjust = 1, gp = gpar(...))
				y = y - 1.5 * text_height
			}
		}
		else{
			yy = y - 4 * text_height + seq(0, 1, 0.02) * 4 * text_height
			h = 4 * text_height * 0.02
			grid.rect(x = unit(0, "npc"), y = yy, hjust = 0, vjust = 1, height = h, width = text_height, gp = gpar(col = "#FFFFFF00", fill = colorRampPalette(annotation_colors[[i]])(50)))
			txt = rev(range(grid.pretty(range(annotation[[i]], na.rm = TRUE))))
			yy = y - c(0, 3) * text_height
			grid.text(txt, x = text_height * 1.3, y = yy, hjust = 0, vjust = 1, gp = gpar(...))
			y = y - 4.5 * text_height
		}
		y = y - 1.5 * text_height
	}
}

draw_main = function(text, ...){
	grid.text(text, gp = gpar(fontface = "bold", ...))
}

draw_sub_main = function(text, pos, ...){
	grid.text(text, x = pos, y = unit(0.5, "npc"), gp = gpar(fontface = "bold", ...))
}