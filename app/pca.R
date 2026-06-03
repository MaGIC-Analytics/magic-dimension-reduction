# ─── PCA BiPlots (ggplot2 / prcomp) ────────────────────────────────────────

observe({
    sel  <- input$Colby %||% input$DemoColby
    cols <- colnames(InputReactive()$meta_data)
    updateSelectInput(session, "PCAColor", choices=cols, selected=sel)
    updateSelectInput(session, "PCAShape", choices=cols, selected=sel)
})

# Dynamic per-group color pickers
output$pca_color_ui <- renderUI({
    req(input$CustomColorToggle, input$PCAColor, InputReactive()$meta_data)
    groups <- unique(as.character(InputReactive()$meta_data[[input$PCAColor]]))
    default_pal <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3",
                     "#FF7F00","#FFFF33","#A65628","#F781BF","#999999")
    color_inputs <- lapply(seq_along(groups), function(i) {
        colourInput(
            inputId = paste0("PCAColorGroup_", gsub("[^A-Za-z0-9]", "_", groups[i])),
            label   = paste("Color for", groups[i]),
            value   = default_pal[((i - 1) %% length(default_pal)) + 1]
        )
    })
    do.call(tagList, color_inputs)
})

# Dynamic per-group shape selectors
output$pca_shape_ui <- renderUI({
    req(input$CustomShapeToggle, input$PCAShape, InputReactive()$meta_data)
    groups <- unique(as.character(InputReactive()$meta_data[[input$PCAShape]]))
    shape_inputs <- lapply(groups, function(g) {
        selectInput(
            inputId  = paste0("PCAShapeGroup_", gsub("[^A-Za-z0-9]", "_", g)),
            label    = paste("Shape for", g),
            choices  = c("Circle"=16,"Triangle"=17,"Square"=15,"Diamond"=18,"Cross"=4),
            selected = 16
        )
    })
    do.call(tagList, shape_inputs)
})

pca_plotter <- reactive({
    req(InputReactive()$count_data, InputReactive()$meta_data)
    tryCatch({

        count_mat <- InputReactive()$count_data
        metadata  <- InputReactive()$meta_data

        p <- prcomp(t(count_mat))

        # Variance explained
        var_exp  <- (p$sdev^2) / sum(p$sdev^2)
        var_pct  <- round(var_exp * 100, 2)

        pca_data <- as.data.frame(p$x)
        pca_data <- cbind(pca_data, metadata)
        sample_labels <- rownames(pca_data)

        # Build custom color map
        color_col <- input$PCAColor %||% colnames(metadata)[1]
        groups    <- unique(as.character(metadata[[color_col]]))

        if (isTRUE(input$CustomColorToggle)) {
            custom_colors <- setNames(
                sapply(groups, function(g) {
                    id  <- paste0("PCAColorGroup_", gsub("[^A-Za-z0-9]", "_", g))
                    clr <- input[[id]]
                    if (is.null(clr)) "#999999" else clr
                }),
                groups
            )
        } else {
            custom_colors <- NULL
        }

        # Build custom shape map
        shape_col <- input$PCAShape %||% colnames(metadata)[1]
        shape_groups <- unique(as.character(metadata[[shape_col]]))

        if (isTRUE(input$CustomShapeToggle)) {
            custom_shapes <- setNames(
                sapply(shape_groups, function(g) {
                    id  <- paste0("PCAShapeGroup_", gsub("[^A-Za-z0-9]", "_", g))
                    val <- input[[id]]
                    if (is.null(val)) 16L else as.integer(val)
                }),
                shape_groups
            )
        } else {
            custom_shapes <- setNames(rep(16L, length(shape_groups)), shape_groups)
        }

        pca_plot <- ggplot(pca_data,
                aes_string(x="PC1", y="PC2",
                    color=color_col, fill=color_col, shape=shape_col)) +
            geom_point(size=input$PCAPointSize %||% 5) +
            theme_minimal() +
            labs(
                x = paste0("PC1 (", var_pct[1], "% variance)"),
                y = paste0("PC2 (", var_pct[2], "% variance)")
            ) +
            theme(
                axis.title.x    = element_text(size=input$PCAXAxisFontSize %||% 12),
                axis.title.y    = element_text(size=input$PCAYAxisFontSize %||% 12),
                axis.text.x     = element_text(size=(input$PCAXAxisFontSize %||% 12) * 0.8),
                axis.text.y     = element_text(size=(input$PCAYAxisFontSize %||% 12) * 0.8),
                legend.position = input$PCALegendPos %||% "bottom",
                legend.text     = element_text(size=input$PCALegLabSize %||% 10),
                legend.key.size = unit(input$PCALegIconSize %||% 5, "pt")
            )

        if (isTRUE(input$ShowLabs)) {
            pca_plot <- pca_plot +
                geom_text_repel(aes(label=sample_labels),
                    size=input$PCALabelSize %||% 4,
                    box.padding=0.5, show.legend=FALSE)
        }

        if (!is.null(custom_colors)) {
            pca_plot <- pca_plot +
                scale_color_manual(values=custom_colors) +
                scale_fill_manual(values=custom_colors)
        }
        pca_plot <- pca_plot + scale_shape_manual(values=custom_shapes)

        pca_plot <- add_overlay_2d(pca_plot, pca_data, "PC1", "PC2", color_col,
            mode  = input$PCAOverlay %||% "none",
            fill  = input$PCAOverlayFill %||% TRUE,
            etype = input$EType %||% "norm")

        pca_plot

    }, error=function(e) {
        showNotification(paste("PCA error:", e$message), type='error', duration=NULL)
        NULL
    })
})

observe({
    output$pca_out <- renderPlot({
        pca_plotter()
    }, height=input$PHeight %||% 800, width=input$PWidth %||% 800)
})

output$DownloadP <- downloadHandler(
    filename = function() paste('PCA', input$DownPFormat, sep='.'),
    content  = function(file) {
        plt <- pca_plotter()
        if (input$DownPFormat == 'jpeg') {
            jpeg(file, height=input$PHeight %||% 800, width=input$PWidth %||% 800)
        } else if (input$DownPFormat == 'png') {
            png(file, height=input$PHeight %||% 800, width=input$PWidth %||% 800)
        } else if (input$DownPFormat == 'tiff') {
            tiff(file, height=input$PHeight %||% 800, width=input$PWidth %||% 800, res=150)
        }
        print(plt)
        dev.off()
    }
)

# ─── Eigenplots ─────────────────────────────────────────────────────────────

observe({
    updateSelectizeInput(session, "EigenMetavars", choices=colnames(InputReactive()$meta_data))
})

eigencorplotter <- reactive({
    validate(need(length(input$EigenMetavars)>=2, message='You must select at least 2 metadata columns'))

    md <- InputReactive()$meta_data
    for (col in colnames(md)) {
        if (!is.numeric(md[[col]])) {
            asnum <- suppressWarnings(as.numeric(md[[col]]))
            md[[col]] <- if (all(is.na(asnum))) as.numeric(as.factor(md[[col]])) else asnum
        }
    }

    p <- pca(InputReactive()$count_data, metadata=md, removeVar=0.1)

    eigencorplot(p,
        metavars              = input$EigenMetavars,
        cexCorval             = input$ELabSize,
        cexLabX               = input$EXLabSize,
        cexLabY               = input$EYLabSize,
        colCorval             = 'white',
        fontCorval            = 2,
        posLab                = 'bottomleft',
        rotLabX               = input$ELabRot,
        posColKey             = input$ELegendPos,
        cexLabColKey          = input$ELabColSize,
        scale                 = TRUE,
        main                  = 'PC1-10 metadata correlations',
        colFrame              = 'white',
        plotRsquared          = input$ERsquare,
        corFUN                = input$EFun,
        corUSE                = 'pairwise.complete.obs',
        corMultipleTestCorrection = input$ECorrect,
        signifSymbols         = c('***', '**', '*', ''),
        signifCutpoints       = c(0, 0.0001, 0.001, 0.01, 1)
    )
})

observe({
    output$eigen_out <- renderPlot({
        eigencorplotter()
    }, height=input$PHeight %||% 800, width=input$PWidth %||% 800)
})

output$DownloadEigen <- downloadHandler(
    filename = function() paste('eigencorplot', input$DownEigenFormat, sep='.'),
    content  = function(file) {
        if (input$DownEigenFormat == 'jpeg') {
            jpeg(file, height=input$PHeight %||% 800, width=input$PWidth %||% 800)
        } else if (input$DownEigenFormat == 'png') {
            png(file, height=input$PHeight %||% 800, width=input$PWidth %||% 800)
        } else if (input$DownEigenFormat == 'tiff') {
            tiff(file, height=input$PHeight %||% 800, width=input$PWidth %||% 800, res=1000)
        }
        print(eigencorplotter())
        dev.off()
    }
)

# ─── Scree Plot ─────────────────────────────────────────────────────────────

scree_plotter <- reactive({
    tryCatch({
        p <- pca(InputReactive()$count_data, metadata=InputReactive()$meta_data, removeVar=0.1)
    }, error=function(e) {
        showNotification(paste(e), type='error', duration=NULL)
    })

    screeplot(p, xlabAngle=input$SLabRot, axisLabSize=input$SLabSize)
})

observe({
    output$scree_out <- renderPlot({
        scree_plotter()
    }, height=input$PHeight %||% 800, width=input$PWidth %||% 800)
})

output$DownloadScreePlot <- downloadHandler(
    filename = function() paste('scree_plot', input$DownScreeFormat, sep='.'),
    content  = function(file) {
        if (input$DownScreeFormat == 'jpeg') {
            jpeg(file, height=input$PHeight %||% 800, width=input$PWidth %||% 800)
        } else if (input$DownScreeFormat == 'png') {
            png(file, height=input$PHeight %||% 800, width=input$PWidth %||% 800)
        } else if (input$DownScreeFormat == 'tiff') {
            tiff(file, height=input$PHeight %||% 800, width=input$PWidth %||% 800, res=1000)
        }
        print(scree_plotter())
        dev.off()
    }
)

# ─── PCA 3D Plot ─────────────────────────────────────────────────────────────

pca_plotter3d <- reactive({
    tryCatch({
        count_mat <- InputReactive()$count_data
        metadata  <- InputReactive()$meta_data
        p         <- prcomp(t(count_mat))
        components <- as.data.frame(p$x)
        components <- cbind(components, metadata)
    }, error=function(e) {
        showNotification(paste(e), type='error', duration=NULL)
    })

    color_col <- input$PCAColor %||% colnames(InputReactive()$meta_data)[1]

    plot_ly(components,
        x      = ~PC1,
        y      = ~PC2,
        z      = ~PC3,
        color  = ~InputReactive()$meta_data[[color_col]],
        height = input$PHeight %||% 800,
        width  = input$PWidth  %||% 800,
        marker = list(size=input$PCAPointSize %||% 5)
    )
})

observe({
    output$pca_out3d <- renderPlotly({
        pca_plotter3d()
    })
})

# ─── PCA Code Modal ──────────────────────────────────────────────────────────

observeEvent(input$show_pca_code_modal, {
    ds <- isolate(tryCatch(InputReactive(), error=function(e) NULL))
    if (is.null(ds)) {
        code <- "# Load your data first, then click here to get the reproducible code."
    } else {
        color_col <- isolate(input$PCAColor %||% "Group")
        shape_col <- isolate(input$PCAShape %||% color_col)
        overlay   <- isolate(input$PCAOverlay %||% "none")
        code <- paste0(
            "library(ggplot2)\nlibrary(ggrepel)\n",
            "",
            "\n# count_mat — genes x samples numeric matrix\n",
            "# metadata  — data.frame with sample metadata\n\n",
            "p <- prcomp(t(count_mat))\n",
            "var_exp  <- (p$sdev^2) / sum(p$sdev^2)\n",
            "var_pct  <- round(var_exp * 100, 2)\n\n",
            "pca_data <- as.data.frame(p$x)\n",
            "pca_data <- cbind(pca_data, metadata)\n\n",
            sprintf('ggplot(pca_data, aes(x=PC1, y=PC2, color=%s, fill=%s, shape=%s)) +\n', color_col, color_col, shape_col),
            sprintf('    geom_point(size=%s) +\n', isolate(input$PCAPointSize) %||% 5),
            if (isolate(isTRUE(input$ShowLabs)))
                sprintf('    geom_text_repel(aes(label=rownames(pca_data)), size=%s) +\n',
                        isolate(input$PCALabelSize) %||% 4)
            else "",
            overlay_code_2d(overlay, isolate(input$PCAOverlayFill %||% TRUE),
                            isolate(input$EType %||% "norm"), color_col),
            '    theme_minimal() +\n',
            '    labs(\n',
            '        x = paste0("PC1 (", var_pct[1], "% variance)"),\n',
            '        y = paste0("PC2 (", var_pct[2], "% variance)")\n',
            '    ) +\n',
            sprintf('    theme(legend.position = "%s")\n', isolate(input$PCALegendPos) %||% "bottom")
        )
    }

    show_code_modal("PCA Biplot", code)
})

# ─── Help Modal ──────────────────────────────────────────────────────────────

show_dimred_help_ui <- function() {
    showModal(modalDialog(
        title     = tagList(icon("circle-question"), " Dimensionality Reduction Tool Help"),
        size      = "l",
        easyClose = TRUE,
        footer    = modalButton("Close"),
        tabsetPanel(
            tabPanel("Overview",
                br(),
                h4("Why Dimensionality Reduction?"),
                p("High-dimensional data (e.g., RNA-seq with thousands of genes) is impossible to visualize directly. Dimensionality reduction compresses this into 2–3 meaningful axes so you can quickly spot sample groupings, outliers, and batch effects."),
                h4("Which Method to Use?"),
                tags$ul(
                    tags$li(strong("PCA:"), " Best for smaller datasets (bulk RNA-seq, 6–100 samples). Linear, fast, interpretable. Use the Scree plot to see how much variance each PC captures, and the Eigenplot to correlate PCs with metadata."),
                    tags$li(strong("tSNE:"), " Better for larger datasets. Non-linear; preserves local neighborhood structure. Sensitive to hyperparameters (especially perplexity)."),
                    tags$li(strong("UMAP:"), " Similar to tSNE but faster and better at preserving global structure. Preferred for single-cell data.")
                ),
                h4("Input Data Format"),
                tags$ul(
                    tags$li("Counts file: genes × samples matrix (CSV/TSV). First column = gene IDs, remaining columns = samples."),
                    tags$li("Metadata file: samples × variables (CSV/TSV). First column = sample names matching matrix column names.")
                )
            ),
            tabPanel("PCA",
                br(),
                h4("PCA Biplot"),
                p("Each point is a sample. Distance reflects similarity in overall expression. Points that cluster together have similar expression profiles. Axes show variance explained (%) so you know how much of the data each component captures."),
                h4("Scree Plot"),
                p("Bar chart of variance explained per principal component. A rapid drop-off ('elbow') indicates where meaningful variation ends and noise begins."),
                h4("Eigenplot"),
                p("Heatmap correlating each PC with metadata variables (e.g., Group, Batch, Gender). Bright colors show which experimental factors drive the major components. Select at least 2 metadata columns to display."),
                h4("Customization"),
                tags$ul(
                    tags$li("Custom colors and shapes can be assigned per group via the Point Options panel."),
                    tags$li("Sample labels use geom_text_repel to avoid overlapping text."),
                    tags$li("Ellipses (stat_ellipse) and encircles (geom_encircle) help visually separate groups.")
                )
            ),
            tabPanel("tSNE / UMAP",
                br(),
                h4("tSNE"),
                p("t-SNE maps high-dimensional distances into 2D/3D. Clusters that appear close are similar; global distances between clusters are less meaningful than in PCA."),
                tags$ul(
                    tags$li(strong("Perplexity:"), " roughly the expected neighborhood size. Typical values: 5–50. Must satisfy 3 × perplexity < n_samples − 1."),
                    tags$li(strong("Iterations:"), " more iterations = more stable embedding, but slower."),
                    tags$li(strong("Theta:"), " trade-off between speed (1.0) and accuracy (0.0).")
                ),
                h4("UMAP"),
                p("UMAP is generally faster than tSNE and preserves more of the global structure."),
                tags$ul(
                    tags$li(strong("n_neighbors:"), " local neighborhood size. Smaller = more local detail; larger = more global structure."),
                    tags$li(strong("min_dist:"), " how tightly points are packed. Smaller = tighter clusters."),
                    tags$li(strong("metric:"), " distance function used for neighbor search.")
                ),
                p(em("Both tSNE and UMAP are stochastic — set a seed for reproducibility."))
            )
        )
    ))
}

observeEvent(input$show_help_float, { show_dimred_help_ui() })

# ─── tSNE / UMAP Code Modals ─────────────────────────────────────────────────

observeEvent(input$show_tsne_code_modal, {
    ds <- isolate(tryCatch(InputReactive(), error=function(e) NULL))
    if (is.null(ds)) {
        code <- "# Load your data first, then click here to get the reproducible code."
    } else {
        color_col <- isolate(input$tSNEColor %||% "Group")
        overlay   <- isolate(input$TOverlay %||% "none")
        n         <- ncol(ds$count_data)
        perp_req  <- isolate(input$tperplex) %||% 30
        perp      <- safe_perplexity(perp_req, n)
        code <- paste0(
            "library(Rtsne)\nlibrary(ggplot2)\nlibrary(ggrepel)\n",
            "",
            "\n# count_mat — genes x samples; metadata — data.frame\n\n",
            sprintf("set.seed(%s)\n", isolate(input$tseed) %||% 0),
            if (perp != perp_req)
                sprintf("# perplexity capped to %s (dataset has %s samples; tSNE needs 3*perplexity < n-1)\n", perp, n)
            else "",
            sprintf('tsne_fit <- Rtsne(t(count_mat), dims=3, perplexity=%s, theta=%s, max_iter=%s)\n\n',
                    perp,
                    isolate(input$ttheta)   %||% 0.5,
                    isolate(input$titer)    %||% 1000),
            "tsne_df <- as.data.frame(tsne_fit$Y)\n",
            'colnames(tsne_df) <- c("tSNE1","tSNE2","tSNE3")\n',
            "tsne_df <- cbind(tsne_df, metadata)\n\n",
            sprintf('ggplot(tsne_df, aes(x=tSNE1, y=tSNE2, color=%s, fill=%s)) +\n', color_col, color_col),
            sprintf('    geom_point(size=%s) +\n', isolate(input$tSNEPointSize) %||% 5),
            if (isolate(isTRUE(input$TLabels)))
                sprintf('    geom_label_repel(aes(label=%s), size=%s) +\n',
                        isolate(input$TLabChoice %||% color_col), isolate(input$TLabSize) %||% 4)
            else "",
            overlay_code_2d(overlay, isolate(input$TFEncirc %||% TRUE),
                            isolate(input$TEType %||% "norm"), color_col),
            '    theme_bw() +\n',
            sprintf('    theme(legend.position="%s")\n', isolate(input$TLegendPos) %||% "bottom")
        )
    }
    show_code_modal("tSNE", code)
})

observeEvent(input$show_umap_code_modal, {
    ds <- isolate(tryCatch(InputReactive(), error=function(e) NULL))
    if (is.null(ds)) {
        code <- "# Load your data first, then click here to get the reproducible code."
    } else {
        color_col <- isolate(input$UMAPColor %||% "Group")
        overlay   <- isolate(input$UOverlay %||% "none")
        n         <- ncol(ds$count_data)
        nb_req    <- isolate(input$uneighbors) %||% 15
        nb        <- safe_neighbors(nb_req, n)
        code <- paste0(
            "library(umap)\nlibrary(ggplot2)\nlibrary(ggrepel)\n",
            "",
            "\n# count_mat — genes x samples; metadata — data.frame\n\n",
            "umap_config <- umap.defaults\n",
            if (nb != nb_req)
                sprintf("# n_neighbors capped to %s (dataset has %s samples; UMAP needs n_neighbors < n)\n", nb, n)
            else "",
            sprintf("umap_config$n_neighbors    <- %s\n", nb),
            sprintf("umap_config$n_epochs       <- %s\n", isolate(input$uepochs)   %||% 200),
            sprintf("umap_config$metric         <- '%s'\n", isolate(input$umetric)  %||% "euclidean"),
            sprintf("umap_config$min_dist       <- %s\n", isolate(input$udist)     %||% 0.1),
            sprintf("umap_config$random_state   <- %s\n\n", isolate(input$useed)   %||% 0),
            "umap_fit <- umap(t(count_mat), config=umap_config)\n",
            "umap_df  <- as.data.frame(umap_fit$layout)\n",
            'colnames(umap_df) <- c("UMAP1","UMAP2")\n',
            "umap_df  <- cbind(umap_df, metadata)\n\n",
            sprintf('ggplot(umap_df, aes(x=UMAP1, y=UMAP2, color=%s, fill=%s)) +\n', color_col, color_col),
            sprintf('    geom_point(size=%s) +\n', isolate(input$UMAPPointSize) %||% 5),
            if (isolate(isTRUE(input$ULabels)))
                sprintf('    geom_label_repel(aes(label=%s), size=%s) +\n',
                        isolate(input$ULabChoice %||% color_col), isolate(input$ULabSize) %||% 4)
            else "",
            overlay_code_2d(overlay, isolate(input$UFEncirc %||% TRUE),
                            isolate(input$UEType %||% "norm"), color_col),
            '    theme_bw() +\n',
            sprintf('    theme(legend.position="%s")\n', isolate(input$ULegendPos) %||% "bottom")
        )
    }
    show_code_modal("UMAP", code)
})

# ─── 3D PCA / Eigenplot / Scree / 3D tSNE / 3D UMAP Code Modals ───────────────

observeEvent(input$show_pca3d_code_modal, {
    ds <- isolate(tryCatch(InputReactive(), error=function(e) NULL))
    if (is.null(ds)) {
        code <- "# Load your data first, then click here to get the reproducible code."
    } else {
        color_col <- isolate(input$PCAColor %||% "Group")
        code <- paste0(
            "library(plotly)\n\n",
            "# count_mat — genes x samples numeric matrix\n",
            "# metadata  — data.frame with sample metadata\n\n",
            "p  <- prcomp(t(count_mat))\n",
            "df <- as.data.frame(p$x)\n",
            "df <- cbind(df, metadata)\n\n",
            sprintf('plot_ly(df, x=~PC1, y=~PC2, z=~PC3, color=~%s,\n', color_col),
            sprintf('    type="scatter3d", mode="markers", marker=list(size=%s))\n',
                    isolate(input$PCAPointSize) %||% 5)
        )
    }
    show_code_modal("3D PCA", code)
})

observeEvent(input$show_eigen_code_modal, {
    ds <- isolate(tryCatch(InputReactive(), error=function(e) NULL))
    if (is.null(ds)) {
        code <- "# Load your data first, then click here to get the reproducible code."
    } else {
        metavars <- isolate(input$EigenMetavars)
        mv <- if (length(metavars) >= 1)
            paste0("c(", paste(sprintf('"%s"', metavars), collapse=", "), ")")
        else "colnames(metadata)"
        code <- paste0(
            "library(PCAtools)\n\n",
            "# count_mat — genes x samples; metadata — data.frame (rownames = sample names)\n\n",
            "# eigencorplot needs numeric metadata — coerce categorical columns to factor codes\n",
            "metadata[] <- lapply(metadata, function(x)\n",
            "    if (is.numeric(x)) x else as.numeric(as.factor(x)))\n\n",
            "p <- pca(count_mat, metadata=metadata, removeVar=0.1)\n\n",
            sprintf("eigencorplot(p, metavars=%s,\n", mv),
            sprintf('    corFUN="%s", corMultipleTestCorrection="%s")\n',
                    isolate(input$EFun) %||% "pearson", isolate(input$ECorrect) %||% "fdr")
        )
    }
    show_code_modal("Eigenplot", code)
})

observeEvent(input$show_scree_code_modal, {
    ds <- isolate(tryCatch(InputReactive(), error=function(e) NULL))
    if (is.null(ds)) {
        code <- "# Load your data first, then click here to get the reproducible code."
    } else {
        code <- paste0(
            "library(PCAtools)\n\n",
            "# count_mat — genes x samples; metadata — data.frame\n\n",
            "p <- pca(count_mat, metadata=metadata, removeVar=0.1)\n\n",
            sprintf("screeplot(p, xlabAngle=%s, axisLabSize=%s)\n",
                    isolate(input$SLabRot) %||% 45, isolate(input$SLabSize) %||% 16)
        )
    }
    show_code_modal("Scree Plot", code)
})

observeEvent(input$show_tsne3d_code_modal, {
    ds <- isolate(tryCatch(InputReactive(), error=function(e) NULL))
    if (is.null(ds)) {
        code <- "# Load your data first, then click here to get the reproducible code."
    } else {
        color_col <- isolate(input$tSNEColor %||% "Group")
        n         <- ncol(ds$count_data)
        perp_req  <- isolate(input$tperplex) %||% 30
        perp      <- safe_perplexity(perp_req, n)
        code <- paste0(
            "library(Rtsne)\nlibrary(plotly)\n\n",
            "# count_mat — genes x samples; metadata — data.frame\n\n",
            sprintf("set.seed(%s)\n", isolate(input$tseed) %||% 0),
            if (perp != perp_req)
                sprintf("# perplexity capped to %s (dataset has %s samples; tSNE needs 3*perplexity < n-1)\n", perp, n)
            else "",
            sprintf('tsne_fit <- Rtsne(t(count_mat), dims=3, perplexity=%s, theta=%s, max_iter=%s)\n\n',
                    perp,
                    isolate(input$ttheta)   %||% 0.5,
                    isolate(input$titer)    %||% 1000),
            "df <- as.data.frame(tsne_fit$Y)\n",
            'colnames(df) <- c("tSNE1","tSNE2","tSNE3")\n',
            "df <- cbind(df, metadata)\n\n",
            sprintf('plot_ly(df, x=~tSNE1, y=~tSNE2, z=~tSNE3, color=~%s,\n', color_col),
            sprintf('    type="scatter3d", mode="markers", marker=list(size=%s))\n',
                    isolate(input$tSNEPointSize) %||% 5)
        )
    }
    show_code_modal("3D tSNE", code)
})

observeEvent(input$show_umap3d_code_modal, {
    ds <- isolate(tryCatch(InputReactive(), error=function(e) NULL))
    if (is.null(ds)) {
        code <- "# Load your data first, then click here to get the reproducible code."
    } else {
        color_col <- isolate(input$UMAPColor %||% "Group")
        n         <- ncol(ds$count_data)
        nb_req    <- isolate(input$uneighbors) %||% 15
        nb        <- safe_neighbors(nb_req, n)
        code <- paste0(
            "library(umap)\nlibrary(plotly)\n\n",
            "# count_mat — genes x samples; metadata — data.frame\n\n",
            "umap_config <- umap.defaults\n",
            "umap_config$n_components <- 3\n",
            if (nb != nb_req)
                sprintf("# n_neighbors capped to %s (dataset has %s samples; UMAP needs n_neighbors < n)\n", nb, n)
            else "",
            sprintf("umap_config$n_neighbors  <- %s\n", nb),
            sprintf("umap_config$min_dist     <- %s\n", isolate(input$udist) %||% 0.1),
            sprintf("umap_config$metric       <- '%s'\n\n", isolate(input$umetric) %||% "euclidean"),
            "umap_fit <- umap(t(count_mat), config=umap_config)\n",
            "df <- as.data.frame(umap_fit$layout)\n",
            'colnames(df) <- c("UMAP1","UMAP2","UMAP3")\n',
            "df <- cbind(df, metadata)\n\n",
            sprintf('plot_ly(df, x=~UMAP1, y=~UMAP2, z=~UMAP3, color=~%s,\n', color_col),
            sprintf('    type="scatter3d", mode="markers", marker=list(size=%s))\n',
                    isolate(input$UMAPPointSize) %||% 5)
        )
    }
    show_code_modal("3D UMAP", code)
})
