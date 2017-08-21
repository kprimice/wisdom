library(segmented)
library(ggplot2)
library(grid)

cbPalette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#labels and axis size
text_size=18
#errorbar width
padding=0.5

#create theme
my_theme=theme_bw()+
	theme(
		plot.background = element_blank()
		,plot.margin=unit(x=c(0.5,0.5,0.5,0.5),"lines")
		,axis.line = element_line()
		,text = element_text(family='Palatino',size=text_size)
		,legend.text = element_text(family='Palatino',size=text_size-4)
		,axis.text = element_text(family='Palatino',size=text_size)
		,axis.title = element_text(family='Palatino',size=text_size)
		,legend.position = 'right'
		,legend.direction = 'vertical'
	)


get_slopes <- function(x, y, min_val = 0, max_val = NULL, span = 2/3, plot = 0) {
	mysel = order(y, na.last = NA)
	if (is.null(max_val)) {
		max_val = max(x);
	}
	y = y[mysel]
	x = x[mysel]
	fit = try(loess.smooth(x,y, evaluation = 100, span = span))
	if (inherits(fit, 'try-error'))
		next ;
	tt = fit
	choices = c(NA, diff(fit$y, differences = 2), NA)

	points_left = fit$x >= min_val & fit$x <= max_val
	fit$x = fit$x[points_left]
	fit$y = fit$y[points_left]
	choices = choices[points_left]

	limit = which.max(abs(choices))

	threshold = fit$x[limit]
	if (plot == 1) {
		plot(x, y)
		lines(tt, col = 'red')
		abline(v=threshold)
	}

	selected = x < threshold
	i = trunc(0.5 * sum(selected))
	x1 = x[x <= (sort(x)[i])]
	y1 = y[x <= (sort(x)[i])]
	i = trunc(0.5 * sum(!selected))
	x2 = x[x >= sort(x, decreasing = TRUE)[i]]
	y2 = y[x >= sort(x, decreasing = TRUE)[i]]
	if (length(unique(x1)) > 1) {
		fit1 = rlm(y1 ~ x1)
	} else {
		fit1 = rlm(c(0,0) ~ c(1,2))
	}
	if (length(unique(x1)) < 2) {
		return (NULL)
	}
	fit2 = rlm(y2 ~ x2)
	return (list(fit1 = fit1, fit2 = fit2, threshold = threshold))
}

get_segmented <- function(x, y, psi) {
	lin.mod = lm(y~x)
	s.mod = segmented(lin.mod, seg.Z = ~x, psi = log10(psi))
	fit1 = c(intercept(s.mod)$x['intercept1',][1], slope(s.mod)$x['slope1',][1])
	err1 = slope(s.mod)$x['slope1',][2]
	fit2 = c(intercept(s.mod)$x['intercept2',][1], slope(s.mod)$x['slope2',][1])
	err2 = slope(s.mod)$x['slope2',][2]
	return (list(fit1 = fit1, err1 = err1, fit2 = fit2, err2 = err2, threshold = s.mod$psi[2], s.mod = s.mod))
}
