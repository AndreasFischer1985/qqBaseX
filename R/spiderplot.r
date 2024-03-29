#' Function spiderplot
#' 
#' Plots a radar chart based on a numeric vector.
#' @param x Numeric vector containing the values to be displayed. Values should be between 0 and 1.
#' @param lower Numeric vector of the same length as x, containing the values to be displayed as an lower bound. Not displayed if NULL (if !is.null(x)).
#' @param upper Numeric vector of the same length as x, containing the values to be displayed as an upper bound. Not displayed if NULL (if !is.null(x)).
#' @param weights Numeric vector of the same length as x, containing the relative length of dimensions. Values should be between 0 and 1.
#' @param main Character vector with one element containing the barplot's title. Defaults to NULL
#' @param max Numeric value determining the largest possible value of the data to be displayed. Defaults to 1.
#' @param xylim Numeric value determining both upper and lower limits on both x and y axis (xlim & ylim). Defaults to 1.5.
#' @param col Character or rgb value specifying the line's color. If NULL (default) col is set to "#00547A".
#' @param col2 Character or rgb value specifying the color of the area between upper and lower bound. If NULL (default) col2 is set to qqBaseX::cols2(col).
#' @param border Character or rgb value specifying the color of the polygon border. Defaults to NA.
#' @param mode Numeric value specifying whether to draw a polygon (mode=1) or arrows (mode!=1), and whether to draw a polygon for x (mode>=0) or not (mode<0). Defaults to 0.
#' @param arrows.lwd Numeric value specifying lwd for arrows (if any). Defaults to 2.
#' @param arrows.length Numeric value specifying lwd for arrows (if any). Defaults to .01.
#' @param add Logical value specifying whether to add graph to the current device. Defaults to F.
#' @param add.numbers Logical value specifying whether to add numbers to labels. Defaults to F.
#' @param add.grid Logical value specifying whether to add numbers to add grid lines. Defaults to T.
#' @param add.labels Logical value specifying whether to add labels to the plot. Defaults to T.
#' @details Plots a radar chart based on a numeric vector. Each dimension of the radar chart can be assigned a weight and/or an upper and lower bound.
#' @keywords plotting
#' @export
#' @examples
#' spiderplot()

spiderplot <- function (x = NULL, lower = NULL, upper = NULL, weights = NULL, 
    names = NULL, main = NULL, max = 1, min = 0, xylim = 1.5, 
    add.scale = T, col = NULL, col2 = NULL, border = NA, mode = 0, 
    arrows.lwd = 3, arrows.length = 0.01, add = F, add.numbers = F, 
    numbers = 2, add.grid = T, add.labels = T, line.type = "o", 
    grid.detail = 1, grid.points = NULL, shift = 0, arrows.transparency = 0.7) 
{
    dimensions = x
    if (!is.null(grid.points)) 
        if (is.null(names(grid.points))) 
            names(grid.points) = grid.points
    if (is.null(col)) 
        col = "#00547A"
    if (is.null(col2)) 
        col2 = qqBaseX::cols2(col, arrows.transparency)
    if (!is.null(max)) {
        if (!is.null(upper) & !is.null(dimensions)) {
            if (max < max(c(lower, upper, dimensions))) 
                max = max(c(lower, upper, dimensions))
            if (min > min(c(lower, upper, dimensions))) 
                min = min(c(lower, upper, dimensions))
        }
        else {
            if (!is.null(dimensions)) {
                if (max < max(dimensions)) 
                  max = max(dimensions)
                if (min > min(dimensions)) 
                  min = min(dimensions)
            }
            if (!is.null(upper)) {
                if (max < max(upper)) 
                  max = max(upper)
                if (min > min(upper)) 
                  min = min(upper)
            }
            if (!is.null(lower)) {
                if (max < max(lower)) 
                  max = max(lower)
                if (min > min(lower)) 
                  min = min(lower)
            }
        }
    }
    else {
        if (!is.null(dimensions)) {
            dimensions[dimensions > max] = max
            x[x > max] = max
        }
        if (!is.null(upper)) 
            upper[upper > max] = max
        if (!is.null(lower)) 
            lower[lower > max] = max
    }
    if (is.null(dimensions) & is.null(weights) & is.null(main)) {
        dimensions = c(0.1, 0.2, 0.3, 0.8, 0.7, 0.1, 0.1, 0.1, 
            0.1, 0.2)
        x = dimensions
        lower = c(0, 0, 0.2, 0.7, 0.6, 0, 0, 0, 0, 0)
        upper = c(0.2, 0.4, 0.4, 0.9, 0.8, 0.2, 0.2, 0.2, 0.1, 
            0.3)
        weights = 1
        main = "Consideration of option 1"
        names = c("artist's opinion", "restoration ethics", "historicity", 
            "authenticity", "functionality", "relative importance", 
            "legal aspects", "technical limit./poss.", "aest./art. factors", 
            "financial limit./poss.")
    }
    if (is.null(names(dimensions))) 
        names(dimensions) = 1:length(dimensions)
    if (!is.null(names)) 
        names(dimensions) = names
    d = dim(cbind(x))[2]
    if (length(d) > 0) 
        if (d > 1) {
            spiderplot(x[, 1], lower = cbind(lower)[, 1], upper = cbind(upper)[, 
                1], weights = cbind(weights)[, 1], names = cbind(names)[, 
                1], col = col[1], add = add[1], main = main[1], 
                max = max[1], min = min[1], xylim = xylim[1], 
                add.scale = add.scale[1], col2 = col2[1], border = border[1], 
                mode = mode[1], arrows.lwd = arrows.lwd[1], arrows.length = arrows.length[1], 
                add.numbers = add.numbers[1], numbers = numbers[1], 
                add.grid = add.grid[1], add.labels = add.labels[1], 
                line.type = line.type[1], grid.detail = grid.detail[1], 
                grid.points = grid.points[1], shift = shift[1], 
                arrows.transparency = arrows.transparency[1])
            e = function(x) {
                if (is.null(x)) 
                  return(x)
                if (length(x) == d) 
                  return(x)
                else return(rep(x, d))
            }
            f = function(x) {
                if (is.null(x)) 
                  return(x)
                if (dim(cbind(x))[2] == d) 
                  return(x)
                else return(matrix(rep(cbind(x)[, 1], each = d), 
                  ncol = d, byrow = T))
            }
            lapply(2:d, function(i) spiderplot(x[, i], lower = cbind(f(lower))[, 
                i], upper = cbind(f(upper))[, i], weights = cbind(f(weights))[, 
                i], names = cbind(f(names))[, i], col = e(col)[i], 
                col2 = e(col2)[i], arrows.lwd = e(arrows.lwd)[i], 
                arrows.length = e(arrows.length)[i], line.type = e(line.type)[i], 
                shift = e(shift)[i], arrows.transparency = e(arrows.transparency)[i], 
                mode = e(mode)[i], add = T, main = NULL, max = max[1], 
                min = min[1], xylim = xylim[1], add.scale = add.scale[1], 
                border = border[1], add.numbers = add.numbers[1], 
                numbers = numbers[1], add.grid = add.grid[1], 
                add.labels = add.labels[1], grid.detail = grid.detail[1], 
                grid.points = grid.points[1]))
            return(invisible())
        }
    if (length(weights) == 1) 
        weights = rep(weights[1], length(dimensions))
    if (is.null(weights)) 
        weights = rep(1, length(dimensions))
    weights2 = c(weights, weights[1])
    theta = seq(0, 2 * pi, length = length(dimensions) + 1)
    if (add == F) 
        plot(weights2 * cos(theta), weights2 * sin(theta), type = "l", 
            lty = 1, col = rgb(0, 0, 0), axes = F, xlab = "", 
            ylab = "", xlim = c(-xylim, xylim), ylim = c(-xylim, 
                xylim))
    no = function(x) {
        (x - min(c(min, x)))/(max(c(max, max(x))) - min(min, 
            min(x)))
    }
    if (!is.null(upper) & !is.null(lower)) {
        if (mode == 1) {
            polygon(weights2 * no(c(upper, upper[1])) * cos(theta + 
                shift), weights2 * no(c(upper, upper[1])) * sin(theta + 
                shift), col = col2, border = border, lty = 1)
            polygon(weights2 * no(c(lower, lower[1])) * cos(theta + 
                shift), weights2 * no(c(lower, lower[1])) * sin(theta + 
                shift), col = rgb(1, 1, 1), border = border, 
                lty = 1)
        }
        else {
            arrows((weights2 * no(c(lower, lower[1])) * cos(theta + 
                shift))[-length(weights2)], (weights2 * no(c(lower, 
                lower[1])) * sin(theta + shift))[-length(weights2)], 
                (weights2 * no(c(upper, upper[1])) * cos(theta + 
                  shift))[-length(weights2)], (weights2 * no(c(upper, 
                  upper[1])) * sin(theta + shift))[-length(weights2)], 
                col = col2, angle = 90, length = arrows.length, 
                code = 3, lwd = arrows.lwd)
        }
    }
    if (add == F) 
        if (add.grid) {
            if (is.null(grid.points)) {
                sapply(1:grid.detail, function(x) lines(weights2 * 
                  x/(grid.detail + 1) * cos(theta), weights2 * 
                  x/(grid.detail + 1) * sin(theta), lty = 2, 
                  col = rgb(0, 0, 0, 0.2)))
            }
            else {
                sapply(grid.points, function(x) lines(weights2 * 
                  no(x) * cos(theta), weights2 * no(x) * sin(theta), 
                  lty = 2, col = rgb(0, 0, 0, 0.2)))
            }
            segments(0, 0, weights2 * cos(theta), weights2 * 
                sin(theta), lty = 1, col = rgb(0, 0, 0, 0.2))
        }
    if (!is.null(dimensions) & mode >= 0) 
        lines(weights2 * no(c(dimensions, dimensions[1])) * cos(theta + 
            shift), weights2 * no(c(dimensions, dimensions[1])) * 
            sin(theta + shift), col = col, lwd = 2, type = line.type, 
            pch = 16)
    if (add == F) 
        if (add.grid & numbers > 0) {
            if (is.null(grid.points)) {
                text(1 * weights2[1], 0.05, round(max, numbers), 
                  cex = 0.6, pos = 2, col = "darkgrey")
                if (length(dimensions)/2 == round(length(dimensions)/2)) 
                  text(-1 * weights2[1], 0.05, round(max, numbers), 
                    cex = 0.6, pos = 4, col = "darkgrey")
                sapply(1:grid.detail, function(x) text(x/(grid.detail + 
                  1) * weights2[1], 0.05, round(((max + min) * 
                  x/(grid.detail + 1)), numbers), cex = 0.6, 
                  pos = 2, col = "darkgrey"))
                if (length(dimensions)/2 == round(length(dimensions)/2)) 
                  sapply(1:grid.detail, function(x) text(-x/(grid.detail + 
                    1) * weights2[round(length(weights2)/2)], 
                    0.05, round(((max + min) * x/(grid.detail + 
                      1)), numbers), cex = 0.6, pos = 4, col = "darkgrey"))
            }
            else {
                for (x in 1:length(grid.points)) text(no(grid.points[x]) * 
                  weights2[1], 0.05, names(grid.points)[x], cex = 0.6, 
                  pos = 2, col = "darkgrey")
                if (length(dimensions)/2 == round(length(dimensions)/2)) 
                  for (x in 1:length(grid.points)) text(-no(grid.points[x]) * 
                    weights2[round(length(weights2)/2)], 0.05, 
                    names(grid.points)[x], cex = 0.6, pos = 4, 
                    col = "darkgrey")
            }
        }
    if (add == F) 
        if (add.labels) 
            if (add.numbers) {
                text(weights * cos(theta)[-(length(dimensions) + 
                  1)], weights * sin(theta)[-(length(dimensions) + 
                  1)], paste0(1:length(dimensions), ". ", names(dimensions)), 
                  pos = ifelse(cos(theta)[-(length(dimensions) + 
                    1)] < 0, 2, 4), xpd = T, cex = 0.6)
            }
            else {
                text(weights * cos(theta)[-(length(dimensions) + 
                  1)], weights * sin(theta)[-(length(dimensions) + 
                  1)], paste0(names(dimensions)), pos = ifelse(cos(theta)[-(length(dimensions) + 
                  1)] < 0, 2, 4), xpd = T, cex = 0.6)
            }
    if (add == F) 
        title(main, cex.main = 0.8)
    invisible()
}
