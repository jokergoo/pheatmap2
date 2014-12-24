######################################
## modified from pheatmap package
######################################

longest_string = function(strings) {
	i = which.max(strwidth(strings, units = 'in'))
	return(strings[i])
}

# determine the width and height of each component in the layout
lo = function(coln, rown, 
		legend, legend_title,
		legend_list, legend_title_list,
		annotation, annotation_list, 
		treeheight_row = 0, treeheight_col = 0,
		main, legend_level_list, sub_main, ...){
	
	fontsize = 10
	fontsize_row = fontsize
	fontsize_col = fontsize
	
	# Get height of colnames and length of rownames
	if(!is.null(coln)){
		gp = list(fontsize = fontsize_col, ...)
		coln_height = unit(1, "grobheight", textGrob(longest_string(coln), rot = 90, gp = do.call(gpar, gp))) + unit(5, "bigpts")
	} else{
		coln_height = unit(5, "bigpts")
	}
	
	if(!is.null(rown)){
		gp = list(fontsize = fontsize_row, ...)
		rown_width = unit(1, "grobwidth", textGrob(longest_string(rown), gp = do.call(gpar, gp))) + unit(10, "bigpts")
	} else{
		rown_width = unit(5, "bigpts")
	}
	
	gp = list(fontsize = fontsize, ...)
	# Legend position
	if(!is.null(legend)){
		strings = names(legend)
		if(!is.null(legend_list)) {
			strings = c(strings, unlist(lapply(legend_list, names)))
		}
		if(!is.null(legend_level_list)) {
			strings = c(strings, legend_level_list)
		}
		longest_break = unit(1.1, "grobwidth", textGrob(longest_string(strings), gp = do.call(gpar, gp)))
		
		strings = legend_title
		if(!is.null(legend_title_list)) {
			strings = c(strings, legend_title_list)
		}
		
		title_length = unit(1.1, "grobwidth", textGrob(longest_string(strings), gp = gpar(fontface = "bold", ...)))
		legend_width = unit(12, "bigpts") + longest_break * 1.2
		legend_width = max(title_length, legend_width)
		
	} else{
		legend_width = unit(0, "bigpts")
	}
	
	# Set main title height
	if(is.null(main)){
		main_height = unit(0, "npc")
	} else{
		main_height = unit(1.5, "grobheight", textGrob(main, gp = gpar(fontsize = 1.3 * fontsize, ...)))
	}
	
	if(is.null(sub_main)){
		sub_main_height = unit(0, "npc")
	} else{
		sub_main_height = unit(1.5, "grobheight", textGrob(sub_main, gp = gpar(fontsize = 1.1 * fontsize, ...)))
	}
	
	# Column annotations
	if(!is.null(annotation)){
		# number of annotations
		na = ncol(annotation)
		# maximum ncol in annotation_list
		if(!is.null(annotation_list)) {
			na2 = sapply(annotation_list, ncol)
			vxx = c(na, na2)
			na = vxx[which.max(vxx)]
		}
		
		annot_height = unit(na * (8 + 2) + 2, "bigpts")
		
		# Width of the corresponding level
		strings = unique(as.matrix(annotation))
		if(!is.null(annotation_list)) {
			strings = c(strings, unlist(sapply(annotation_list, function(x) unique(as.matrix(x)))))
		}
		
		strings = c(strings, colnames(annotation))
		if(!is.null(annotation_list)) {
			strings = c(strings, unlist(sapply(annotation_list, colnames)))
		}
		
		annot_legend_width = unit(1.2, "grobwidth", textGrob(longest_string(strings), gp = gpar(...))) + unit(12, "bigpts")
	} else{
		annot_height = unit(0, "bigpts")
		annot_legend_width = unit(0, "bigpts")
	}
	
	# Tree height
	treeheight_col = unit(treeheight_col, "bigpts") + unit(5, "bigpts")
	treeheight_row = unit(treeheight_row, "bigpts") + unit(5, "bigpts") 
	
	# Set cell sizes
	matwidth = unit(1, "npc") - rown_width - legend_width - treeheight_row - annot_legend_width
	matheight = unit(1, "npc") - main_height - sub_main_height - coln_height - treeheight_col - annot_height
	
	# Produce layout()
	pushViewport(viewport(layout = grid.layout(nrow = 6, ncol = 5, widths = unit.c(treeheight_row, matwidth, rown_width, legend_width, annot_legend_width), 
		heights = unit.c(main_height, sub_main_height, treeheight_col, annot_height, matheight, coln_height)), gp = do.call(gpar, gp)))
}


# returns a list which contains colors for each level for each annotation
generate_annotation_colours = function(annotation, annotation_colors, drop = FALSE){
	if(is.null(annotation_colors)){
		annotation_colors = list()
	}
	count = 0
	for(i in 1:ncol(annotation)){
		if(is.character(annotation[, i]) | is.factor(annotation[, i])){
			if (is.factor(annotation[, i]) & !drop){
				count = count + length(levels(annotation[, i]))
			}
			else{
				count = count + length(unique(annotation[, i]))
			}
		}
	}
	
	factor_colors = hsv((seq(0, 1, length.out = count + 1)[-1] + 
      0.2)%%1, 0.7, 0.95)
	
	
	for(i in 1:ncol(annotation)){
		if(!(colnames(annotation)[i] %in% names(annotation_colors))){
			if(is.character(annotation[, i]) | is.factor(annotation[, i])){
				n = length(unique(annotation[, i]))
				if (is.factor(annotation[, i]) & !drop){
					n = length(levels(annotation[, i]))
				}
				ind = sample(1:length(factor_colors), n)
				annotation_colors[[colnames(annotation)[i]]] = factor_colors[ind]
				l = levels(as.factor(annotation[, i]))
				l = l[l %in% unique(annotation[, i])]
				if (is.factor(annotation[, i]) & !drop){
					l = levels(annotation[, i])
				}
				names(annotation_colors[[colnames(annotation)[i]]]) = l
				factor_colors = factor_colors[-ind]
			}
			else{
				r = runif(1)
				annotation_colors[[colnames(annotation)[i]]] = hsv(r, c(0.1, 1), 1)
			}
		}
	}
	return(annotation_colors)
}


convert_annotations = function(annotation, annotation_colors){
	new = annotation
	for(i in 1:ncol(annotation)){
		a = annotation[, i]
		b = annotation_colors[[colnames(annotation)[i]]]
		if(is.character(a) | is.factor(a)){
			a = as.character(a)
			if(length(setdiff(a, names(b))) > 0){
				stop(sprintf("Factor levels on variable %s do not match with annotation_colors", colnames(annotation)[i]))
			}
			new[, i] = b[a]
		}
		else{
			a = cut(a, breaks = 100)
			new[, i] = colorRampPalette(b)(100)[a]
		}
	}
	return(as.matrix(new))
}

vplayout = function(x, y, name = NULL){
	return(viewport(layout.pos.row = x, layout.pos.col = y, name = name))
}

# matrix: color matrix
heatmap_motor = function(matrix, col_fun,
		mat_list = NULL, col_fun_list = NULL,
		legend = legend, legend_title = "Main matrix",
		legend_list = NULL, legend_title_list = NULL,
		legend_breaks = NULL, legend_breaks_list = NULL,
		annotation = NULL, annotation_colors = NULL,
		annotation_list = NULL, annotation_colors_list = NULL,
		treeheight_row = 0, treeheight_col = 0,
		tree_row = NULL, tree_col = NULL, tree_col_list = NULL,
		main = NULL, sub_main = NULL, gap = 1, ...){
	
	fontsize = 10
	fontsize_row = fontsize
	fontsize_col = fontsize
	
	# Set layout

	cn = colnames(matrix)  # merged column names
	if(is.null(cn)) cn = rep("", ncol(matrix))
	if(!is.null(mat_list)) {
		for(kk in seq_along(mat_list)) {
			#cn = c(cn, "")
			if(length(colnames(mat_list[[kk]])) != 0) {
				cn = c(cn, colnames(mat_list[[kk]]))
			} else {
				cn = c(cn, rep("", ncol(mat_list[[kk]])))
			}
		}
	}
	
	legend_level_list = NULL
	if(!is.null(col_fun_list)) {
		legend_level_list = unique(unlist(lapply(col_fun_list, attr, "levels")))
	}
	
	lo(coln = cn, rown = rownames(matrix),
		legend = legend, legend_title = legend_title,
		legend_list = legend_list, legend_title_list = legend_title_list,
		annotation = annotation, annotation_list = annotation_list,
		treeheight_row = treeheight_row, treeheight_col = treeheight_col,
		main = main, legend_level_list = legend_level_list, sub_main, ...)
	
	
	# Draw title
	if(!is.null(main)){
		pushViewport(vplayout(1, 2, name = "pheatmap_title"))
		draw_main(main, fontsize = 1.3 * fontsize, ...)
		upViewport()
	}
	
	# Draw sub title
	if(!is.null(sub_main)){
	
		if(is.null(mat_list)) {
			ncol_mat = c(0, ncol(matrix))
		} else {
			ncol_mat = c(0, ncol(matrix), sapply(mat_list, function(x) c(gap, ncol(x)-gap)))
		}
		ncl = cumsum(ncol_mat)
		pos = (ncl[seq_len(length(ncl)/2)*2-1] + ncl[seq_len(length(ncl)/2)*2])/2
		if(length(sub_main) > length(mat_list) + 1) {
			sub_main = sub_main[seq_along(pos)]
		} else {
			pos = pos[seq_along(sub_main)]
		}
		pos = pos/sum(ncol_mat)
		pushViewport(vplayout(2, 2, name = "pheatmap_sub_title"))
		draw_sub_main(sub_main, pos, fontsize = 1.1 * fontsize, ...)
		upViewport()
	}
	
	# Draw tree for the columns (only for the first matrix which is the main matrix)
	if(!is.null(tree_col) & treeheight_col != 0){
		pushViewport(vplayout(3, 2, name = "pheatmap_col_tree"))
		draw_dendrogram(tree_col, horizontal = T, n = length(cn))
		if(!is.null(tree_col_list)) {
			for(i in seq_along(tree_col_list)) {
				if(!is.null(tree_col_list[[i]])) {
					k = which(names(mat_list) == names(tree_col_list)[i])
					
					if(k == 1) {
						offset = ncol(matrix)+gap
					} else {
						offset = ncol(matrix) + sum(sapply(mat_list[1:(k-1)], ncol)) + gap
					}
					draw_dendrogram(tree_col_list[[i]], horizontal = T, n = length(cn), offset = offset)
				}
			}
		}
		upViewport()
	}
	
	# Draw tree for the rows
	if(!is.null(tree_row) & treeheight_row != 0){
		pushViewport(vplayout(5, 1, name = "pheatmap_row_tree"))
		draw_dendrogram(tree_row, horizontal = F)
		upViewport()
	}
	
	# Draw matrix
	pushViewport(vplayout(5, 2, name = "pheatmap_matrix"))
	draw_matrix(matrix, mat_list = mat_list)
	upViewport()
	
	# Draw colnames
	if(length(colnames(matrix)) != 0){
		pushViewport(vplayout(6, 2, name = "pheatmap_colnames"))
		draw_colnames(cn, fontsize = fontsize_col, ...)
		upViewport()
	}
	
	# Draw rownames
	if(length(rownames(matrix)) != 0){
		pushViewport(vplayout(5, 3, name = "pheatmap_rownames"))
		draw_rownames(rownames(matrix), fontsize = fontsize_row, ...)
		upViewport()
	}
	
	# Draw annotation tracks
	if(!is.null(annotation)){
		pushViewport(vplayout(4, 2, name = "pheatmap_annotation"))
		converted_annotation = convert_annotations(annotation, annotation_colors)
		draw_annotations(converted_annotation, n = length(cn))
		if(!is.null(annotation_list)) {
			for(i in seq_along(annotation_list)) {
				converted_annotation = convert_annotations(annotation_list[[i]], annotation_colors_list[[i]])
				k = which(names(mat_list) == names(annotation_list)[i])
				if(k == 1) {
					offset = ncol(matrix)+1
				} else {
					offset = ncol(matrix) + sum(sapply(mat_list[1:(k-1)], ncol)) + 1
				}
				draw_annotations(converted_annotation, n = length(cn), offset = offset)
			}
		}
		upViewport()
	}
	
	# Draw annotation legend
	# merge annotation that have same names
	if(!is.null(annotation)){
		if(length(rownames(matrix)) != 0){
			pushViewport(vplayout(5:6, 5, name = "pheatmap_annotation_legend"))
		}
		else{
			pushViewport(vplayout(4:6, 5, name = "pheatmap_annotation_legend"))
		}
		
		al = as.list(annotation)
		alc = as.list(annotation_colors)
		if(!is.null(annotation_list)) {
			for(i in seq_along(annotation_list)) {
				nm = setdiff(colnames(annotation_list[[i]]), names(al))
				for(nnm in nm) {
					al = c(al, annotation_list[[i]][nnm])
					alc = c(alc, annotation_colors_list[[i]][nnm])
				}
			}
		}
		draw_annotation_legend(al, alc, ...)
		
		upViewport()
	}
	
	# Draw legend
	if(!is.null(legend)){
		length(colnames(matrix))
		if(length(rownames(matrix)) != 0){
			pushViewport(vplayout(5:6, 4, name = "pheatmap_legend"))
		}
		else{
			pushViewport(vplayout(4:6, 4, name = "pheatmap_legend"))
		}

		y = draw_legend(legend_title, legend, legend_breaks, col_fun, ...)
		if(!is.null(legend_list)) {
			for(i in seq_along(legend_list)) {
				y = draw_legend(legend_title_list[i], legend_list[[i]], legend_breaks_list[[i]], col_fun_list[[ which(names(col_fun_list) == legend_title_list[i]) ]], y = y, ...)
			}
		}
		upViewport()
	}
	
	upViewport()
}


cluster_mat = function(mat, distance, method){
	if(!(method %in% c("ward", "single", "complete", "average", "mcquitty", "median", "centroid"))){
		stop("clustering method has to one form the list: 'ward', 'single', 'complete', 'average', 'mcquitty', 'median' or 'centroid'.")
	}
	if(!((distance %in% c("correlation", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "spearman", "mutualInfo", "tao")) | class(distance) == "dist")){
		stop("distance has to be a dissimilarity structure as produced by dist or one measure  form the list: 'correlation', 'euclidean', 'maximum', 'manhattan', 'canberra', 'binary', 'minkowski'")
	}
	
	if(class(distance) == "dist") {
		d = distance
	} else if(distance == "correlation"){
		d = bioDist::cor.dist(mat, abs = FALSE)
	} else if(distance == "spearman") {
		d = bioDist::spearman.dist(mat, abs = FALSE)
	} else if(distance == "mutualInfo") {
		d = bioDist::mutualInfo(mat)
	} else if(distance == "tao") {
		d = bioDist::tau.dist(mat, abs = FALSE)
	} else{
		d = dist(mat, method = distance)
	}
	
	return(hclust(d, method = method))
}

# == title
# Pretty and parallel heatmap
#
# == param
# -mat main matrix
# -col_fun mapping function to transform values to colors
# -mat_list A list of additional matrix. The list should have name index
# -col_fun_list A list of color mapping functions. The element should correspond to ``mat_list``
# -gap gaps between heatmap. It is measured by numbers of empty columns
# -cluster_rows Logical, whether to cluster rows
# -cluster_cols Logical, whether to cluster columns
# -clustering_distance_rows method to cluster rows, possible values are in `stats::dist` plus "correlation", "spearman", "mutualInfo", "tao"
# -clustering_distance_cols same as ``clustering_distance_cols``
# -clustering_method method to do clustering. Possible values are in `stats::hclust`
# -legend_title the title for the legend which corresponds to the main matrix
# -legend_title_list titls for the additional matrix. Valid values should be in the names of ``mat_list``
# -annotation A data frame, for the format of this variable, see original ``pheatmap`` function
# -annotation_colors a list, for the format of this variable, see original ``pheatmap`` function
# -show_rownames logical, whether to plot row names
# -show_colnames logical, whether to plot column names
# -main title for the plot
# -annotation_list a list of ``annotation``
# -annotation_colors_list a list of ``annotation_colors``
# -show_legend logical, whether show legend
# -show_annotation_legend logical whether show annotation legend
# -sub_main sub title for each heatmap
# -newpage create a new grid newpage
# -... other arguments passed to `grid::gpar`
#
# == details
# Please go to the package vignette.
#
pheatmap2 = function(mat,
	col_fun = colorRamp2(breaks = seq(min(mat), max(mat), length = 7),
		colors = rev(brewer.pal(n = 7, name = "RdYlBu"))),
	mat_list = NULL, col_fun_list = NULL, gap = 1,
	cluster_rows = TRUE, cluster_cols = TRUE,
	clustering_distance_rows = "euclidean", clustering_distance_cols = "euclidean", 
	clustering_method = "complete",  
	legend_title = "Main matrix", legend_title_list = NULL,
	annotation = NULL, annotation_colors = NULL,
	show_rownames = TRUE, show_colnames = TRUE, main = NULL,
	annotation_list = NULL, annotation_colors_list = NULL,
	show_legend = TRUE, show_annotation_legend = TRUE, sub_main = NULL,
	newpage = TRUE, ...){
	
	if(newpage) {
		grid.newpage()
	}

	# old arguments that are not common used
	treeheight_row = ifelse(cluster_rows, 50, 0)
	treeheight_col = ifelse(cluster_cols, 50, 0)

	if(length(mat_list) == 0) {
		mat_list = NULL
	}
	if(length(col_fun_list) == 0) {
		col_fun_list = NULL
	}
	if(length(annotation_list) == 0) {
		annotation_list = NULL
	}
	if(length(annotation_colors_list) == 0) {
		annotation_colors_list = NULL
	}

	if(!is.null(mat_list)) {
		if(is.null(names(mat_list))) {
			stop("`mat_list` should be a list with names.\n")
		}
	}
	
	# recycle the col_fun
	if(!is.null(mat_list) && is.null(col_fun_list)) {
		col_fun_list = vector("list", length(mat_list))
		names(col_fun_list) = names(mat_list)
		for(i in seq_along(mat_list)) {
			col_fun_list[[i]] = col_fun
		}
	}
	
	# do filtering on `mat_list`
	if(!is.null(mat_list)) {
		# the matrix is only a vector, convert it to a matrix with one column
		# also if the colname is missing, assign with the list name
		for(i in seq_along(mat_list)) {
			if(!is.matrix(mat_list[[i]])) mat_list[[i]] = matrix(mat_list[[i]], ncol = 1)
			if(is.null(colnames(mat_list[[i]])) && dim(mat_list[[i]])[2] == 1) colnames(mat_list[[i]]) = names(mat_list)[i]
		}
		
		if(!all(sapply(mat_list, nrow) == nrow(mat))) {
			stop("nrow in `mat_list` should be same as `mat`.\n")
		}
		
		if(length(mat_list) != length(col_fun_list)) {
			stop("length of `mat_list` should be same as `col_fun_list`\n")
		}
		
		if(!setequal(names(mat_list), names(col_fun_list))) {
			stop("names in `mat_list` should be same as names in `col_fun_list`\n")
		}
		
		for(i in seq_along(mat_list)) {
			# transform to factors
			if(is.character(mat_list[[i]])) {
				odim = dim(mat_list[[i]])
				odimname = dimnames(mat_list[[i]])
				fa = factor(mat_list[[i]])
				le = levels(fa)
				m2 = as.numeric(fa)
				dim(m2) = odim
				dimnames(m2) = odimname
				mat_list[[i]] = m2
				attr(col_fun_list[[i]], "levels") = le
			} else if(is.factor(mat_list[[i]])) {
				odimname = dimnames(mat_list[[i]])
				fa = mat_list[[i]]
				le = levels(fa)
				m2 = matrix(as.numeric(fa), ncol = 1)
				dimnames(m2) = odimname
				mat_list[[i]] = m2
				attr(col_fun_list[[i]], "levels") = le
			}
		}
	}
	
	col_fun_list = col_fun_list[ names(mat_list) ]
	
	# Do clustering
	if(cluster_rows){
		tree_row = cluster_mat(mat, distance = clustering_distance_rows, method = clustering_method)
		mat = mat[tree_row$order, , drop = FALSE]
	
		# matrix in `mat_list` have the same order as in `mat`
		if(!is.null(mat_list)) {
			for(i in seq_along(mat_list)) {
				mat_list[[i]] = mat_list[[i]][tree_row$order, , drop = FALSE]
			}
		}
	} else{
		tree_row = NULL
		treeheight_row = 0
	}
	
	if(cluster_cols){
		tree_col = cluster_mat(t(mat), distance = clustering_distance_cols, method = clustering_method)
		mat = mat[, tree_col$order, drop = FALSE]

		tree_col_list = NULL
	
		# each matrix should be clustered by column
		if(!is.null(mat_list)) {
			tree_col_list = vector("list", length = length(mat_list))
			names(tree_col_list) = names(mat_list)
			for(i in seq_along(mat_list)) {
				if(ncol(mat_list[[i]]) > 1) {
					tree_col_xx = cluster_mat(t(mat_list[[i]]), distance = clustering_distance_cols, method = clustering_method)
					mat_list[[i]] = mat_list[[i]][, tree_col_xx$order, drop = FALSE]
					tree_col_list[[i]] = tree_col_xx
				}
			}
		}
	} else{
		tree_col = NULL
		treeheight_col = 0
		tree_col_list = NULL
	}
	
	# do filtering on legend_title_list
	if(!is.null(legend_title_list)) {
		legend_title_list = intersect(legend_title_list, names(mat_list))
	}
	
	if(!is.null(annotation_list)) {
		annotation_list = annotation_list[intersect(names(annotation_list), names(mat_list))]
	}
	
	if(!is.null(annotation_colors_list)) {
		annotation_colors_list = annotation_colors_list[intersect(names(annotation_colors_list), names(mat_list))]
	}
	
	# Colors and scales
	# `legend_title_list` contains those legends for `mat_list` that need to be plotted
	if(is.null(attr(col_fun, "breaks"))) {
		legend = grid.pretty(range(mat))
	} else {
		legend = grid.pretty(range(attr(col_fun, "breaks")))
	}

	names(legend) = legend
	legend_breaks = seq(min(legend), max(legend), length = 50)
	legend_list = NULL
	legend_breaks_list = NULL
	if(!is.null(legend_title_list)) {
		legend_list = vector("list", length(legend_title_list))
		names(legend_list) = legend_title_list
		legend_breaks_list = vector("list", length(legend_title_list))
		names(legend_breaks_list) = legend_title_list
		for(lt in legend_title_list) {
			if(is.null(attr(col_fun_list[[lt]], "breaks"))) {
				legend_list[[lt]] = grid.pretty(range(mat_list[[ lt ]]))
			} else {
				legend_list[[lt]] = grid.pretty(range(attr(col_fun_list[[lt]], "breaks")))
			}
			names(legend_list[[lt]]) = legend_list[[lt]]
			legend_breaks_list[[lt]] = seq(min(legend_list[[lt]]), max(legend_list[[lt]]), length = 50)
		}
	}
	
	if(!show_legend) {
		legend = NULL
		legend_list = NULL
		legend_title = NULL
		legend_title_list = NULL
		legend_breaks = NULL
		legend_breaks_list = NULL
	}

	# pheatmap transforms numeric matrix to colors
	mat = col_fun(mat)
	if(!is.null(mat_list)) {
		for(i in seq_along(mat_list)) {
			mat_list[[i]] = col_fun_list[[i]](mat_list[[i]])
		}
	}
	
	if(is.atomic(annotation) && !is.null(annotation)) {
		if(is.null(names(annotation))) {
			xrnx = colnames(mat)
		} else {
			xrnx = names(annotation)
		}
		annotation = data.frame(anno = annotation)
		rownames(annotation) = xrnx
	}
	
	# Preparing annotation colors
	if(!is.null(annotation)){
		if(length(intersect(rownames(annotation), colnames(mat))) == 0) {
			stop("rownames in `annotation` should correspond to colnames in `mat`.\n")
		}
		annotation = annotation[colnames(mat), , drop = F]
		annotation_colors = generate_annotation_colours(annotation, annotation_colors)
	}
	if(!is.null(annotation_list)) {
		for(nm in names(annotation_list)) {
			annotation_list[[nm]] = annotation_list[[nm]][colnames(mat_list[[ nm ]]), , drop = F]
			annotation_colors_list[[nm]] = generate_annotation_colours(annotation_list[[nm]], annotation_colors_list[[nm]])
		}
	}
	
	if(!show_annotation_legend) {
		annotation = NULL
		annotation_colors = NULL
		annotation_list = NULL
		annotation_colors_list = NULL
	}
	
	empty_mat = matrix(NA, nrow = nrow(mat), ncol = gap)
	
	# add an additional NA column before current matrix which will be treated as a blank gap
	if(!is.null(mat_list)) {
		for(i in seq_along(mat_list)) {
			mat_list[[i]] = cbind(empty_mat, mat_list[[i]])
		}
	}
	
	if(!show_rownames){
		rownames(mat) = NULL
	}
	
	if(!show_colnames){
		colnames(mat) = NULL
		if(!is.null(mat_list)) {
			for(i in seq_along(mat_list)) {
				colnames(mat_list[[i]]) = NULL
			}
		}
	}
	
	# Draw heatmap
	heatmap_motor(mat, col_fun = col_fun,
		mat_list = mat_list, col_fun_list = col_fun_list,
		legend = legend, legend_title = legend_title,
		legend_list = legend_list, legend_title_list = legend_title_list,
		legend_breaks = legend_breaks, legend_breaks_list = legend_breaks_list,
		annotation = annotation, annotation_colors = annotation_colors,
		annotation_list = annotation_list, annotation_colors_list = annotation_colors_list,
		treeheight_row = treeheight_row, treeheight_col = treeheight_col,
		tree_row = tree_row, tree_col = tree_col, tree_col_list = tree_col_list,
		main = main, sub_main = sub_main, gap = gap,...)
		
	invisible(list(tree_row = tree_row, tree_col = tree_col, tree_col_list = tree_col_list))
}
