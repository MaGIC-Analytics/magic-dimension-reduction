# Shared helpers
############################################################################

# Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# --- low-level: build ellipse polygon from center + covariance + radius ------
ellipse_points <- function(ctr, cv, radius, n = 80) {
    e    <- eigen(cv, symmetric = TRUE)
    vals <- pmax(e$values, 0)
    th  <- seq(0, 2 * pi, length.out = n)
    pts <- cbind(cos(th), sin(th)) %*% diag(radius * sqrt(vals)) %*% t(e$vectors)
    sweep(pts, 2, ctr, "+")
}

# --- per-group polygons: kind = "enclose" (encloses every point + padding)
#                         kind = "stat"    (statistical confidence ellipse) -----
# Scale-independent and robust on data with very large value ranges (e.g. PCA
# scores in the millions), which `ggforce::geom_mark_ellipse` mis-fits.
group_overlay_polys <- function(df, xvar, yvar, group_col,
                                kind = "enclose", type = "norm",
                                level = 0.95, pad = 0.08) {
    grps       <- unique(as.character(df[[group_col]]))
    floor_frac <- if (kind == "enclose") 0.10 else 1e-3   # encircle = blob; stat = faithful
    parts <- lapply(grps, function(g) {
        xy <- as.matrix(df[as.character(df[[group_col]]) == g, c(xvar, yvar), drop = FALSE])
        if (nrow(xy) < 2) return(NULL)
        ctr <- colMeans(xy)
        cv  <- if (nrow(xy) >= 3) stats::cov(xy)
               else { d <- xy[2, ] - xy[1, ]; outer(d, d) }
        # Floor the minor eigenvalue so near-collinear groups render as a
        # proper blob (not a paper-thin sliver) while still enclosing all points.
        e     <- eigen(cv, symmetric = TRUE)
        vals  <- pmax(e$values, 0)
        if (max(vals) > 0) {
            vals[vals < max(vals) * floor_frac] <- max(vals) * floor_frac
        } else {
            vals <- c(1, 1)
        }
        cv <- e$vectors %*% diag(vals) %*% t(e$vectors)

        if (kind == "enclose") {
            cv_inv <- solve(cv)
            mhd    <- apply(xy, 1, function(p) {
                d <- p - ctr; as.numeric(sqrt(t(d) %*% cv_inv %*% d))
            })
            radius <- max(mhd) * (1 + pad)
        } else {
            nn <- nrow(xy)
            if (nn < 3) return(NULL)
            radius <- if (type == "euclid") {
                          level * mean(apply(xy, 2, stats::sd))
                      } else if (type == "t" && nn > 2) {
                          sqrt(2 * (nn - 1) / (nn - 2) * stats::qf(level, 2, nn - 2))
                      } else sqrt(stats::qchisq(level, df = 2))
            if (type == "euclid") cv <- diag(2)
        }
        pts <- ellipse_points(ctr, cv, radius)
        data.frame(x = pts[, 1], y = pts[, 2], grp = g)
    })
    do.call(rbind, parts)
}

# --- 2D: add encircle (enclosing ellipse) OR ellipse (confidence) layer -------
add_overlay_2d <- function(p, df, xvar, yvar, group_col,
                           mode = "none", fill = TRUE, etype = "norm") {
    if (is.null(mode) || mode == "none") return(p)
    ell <- if (mode == "encircle")
               group_overlay_polys(df, xvar, yvar, group_col, kind = "enclose")
           else
               group_overlay_polys(df, xvar, yvar, group_col, kind = "stat",
                                   type = etype %||% "norm")
    if (is.null(ell) || !nrow(ell)) return(p)
    if (isTRUE(fill)) {
        p + geom_polygon(data = ell,
                mapping = aes(x = x, y = y, group = grp, fill = grp, colour = grp),
                alpha = 0.2, show.legend = FALSE, inherit.aes = FALSE)
    } else {
        p + geom_path(data = ell,
                mapping = aes(x = x, y = y, group = grp, colour = grp),
                show.legend = FALSE, inherit.aes = FALSE)
    }
}

# --- safe parameter caps for small datasets ----------------------------------
safe_perplexity <- function(requested, n) max(1, min(requested, floor((n - 1) / 3)))
safe_neighbors  <- function(requested, n) max(2, min(requested, n - 1))

# --- reproducible-code modal -------------------------------------------------
show_code_modal <- function(title_text, code,
                            intro = "Copy this code to reproduce your current plot in an offline R session.") {
    showModal(modalDialog(
        title     = tagList(icon("file-code"), paste0(" Reproducible R Code — ", title_text)),
        size      = "l",
        easyClose = TRUE,
        footer    = modalButton("Close"),
        p(intro, style="color:#555; margin-bottom:12px;"),
        tags$pre(
            style = paste(
                "background:#1e1e1e; color:#d4d4d4; border-radius:6px;",
                "padding:16px; font-size:12px; max-height:520px; overflow-y:auto;",
                "white-space:pre; font-family:'Courier New', monospace;"
            ),
            code
        )
    ))
}

# --- generated code snippet for a 2D encircle/ellipse overlay (ggplot layer) --
overlay_code_2d <- function(mode, fill, etype, group) {
    if (is.null(mode) || mode == "none") return("")
    if (mode == "encircle") {
        sprintf('    stat_ellipse(aes(group=%s, color=%s, fill=%s),\n        type="norm", level=0.999, geom="%s", alpha=%s, show.legend=FALSE) +\n',
                group, group, group,
                if (isTRUE(fill)) "polygon" else "path",
                if (isTRUE(fill)) "0.2" else "1")
    } else {
        sprintf('    stat_ellipse(aes(group=%s, color=%s, fill=%s), type="%s", level=0.95,\n        geom="%s", alpha=%s, show.legend=FALSE) +\n',
                group, group, group, etype %||% "norm",
                if (isTRUE(fill)) "polygon" else "path",
                if (isTRUE(fill)) "0.2" else "1")
    }
}

