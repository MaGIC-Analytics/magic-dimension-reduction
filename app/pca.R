# ─── PCA BiPlots (ggplot2 / prcomp) ────────────────────────────────────────

observe({
    updateSelectInput(session, "PCAColor", choices=colnames(InputReactive()$meta_data))
    updateSelectInput(session, "PCAShape", choices=colnames(InputReactive()$meta_data))
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

        if (isTRUE(input$Encirc)) {
            pca_plot <- pca_plot +
                geom_encircle(aes_string(group=color_col),
                    fill  = if (isTRUE(input$FEncirc)) NA else NA,
                    alpha = 0.2, color="black")
        }

        if (isTRUE(input$Ellipse)) {
            pca_plot <- pca_plot +
                stat_ellipse(aes_string(group=color_col),
                    type  = input$EType %||% "norm",
                    geom  = if (isTRUE(input$Efill)) "polygon" else "path",
                    alpha = if (isTRUE(input$Efill)) 0.1 else 1,
                    level = 0.95)
        }

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
    tryCatch({
        p <- pca(InputReactive()$count_data, metadata=InputReactive()$meta_data, removeVar=0.1)
    }, error=function(e) {
        showNotification(paste(e), type='error', duration=NULL)
    })

    validate(need(length(input$EigenMetavars)>=2, message='You must select at least 2 metadata columns'))

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
        code <- paste0(
            "library(ggplot2)\nlibrary(ggrepel)\n\n",
            "# count_mat — genes x samples numeric matrix\n",
            "# metadata  — data.frame with sample metadata\n\n",
            "p <- prcomp(t(count_mat))\n",
            "var_exp  <- (p$sdev^2) / sum(p$sdev^2)\n",
            "var_pct  <- round(var_exp * 100, 2)\n\n",
            "pca_data <- as.data.frame(p$x)\n",
            "pca_data <- cbind(pca_data, metadata)\n\n",
            sprintf('ggplot(pca_data, aes(x=PC1, y=PC2, color=%s, shape=%s)) +\n', color_col, shape_col),
            sprintf('    geom_point(size=%s) +\n', isolate(input$PCAPointSize) %||% 5),
            if (isolate(isTRUE(input$ShowLabs)))
                sprintf('    geom_text_repel(aes(label=rownames(pca_data)), size=%s) +\n',
                        isolate(input$PCALabelSize) %||% 4)
            else "",
            '    theme_minimal() +\n',
            '    labs(\n',
            '        x = paste0("PC1 (", var_pct[1], "% variance)"),\n',
            '        y = paste0("PC2 (", var_pct[2], "% variance)")\n',
            '    ) +\n',
            sprintf('    theme(legend.position = "%s")\n', isolate(input$PCALegendPos) %||% "bottom")
        )
    }

    showModal(modalDialog(
        title     = tagList(icon("file-code"), " Reproducible R Code — PCA Biplot"),
        size      = "l",
        easyClose = TRUE,
        footer    = modalButton("Close"),
        p("Copy this code to reproduce your current PCA biplot in an offline R session.", style="color:#555; margin-bottom:12px;"),
        tags$pre(
            style = paste(
                "background:#1e1e1e; color:#d4d4d4; border-radius:6px;",
                "padding:16px; font-size:12px; max-height:520px; overflow-y:auto;",
                "white-space:pre; font-family:'Courier New', monospace;"
            ),
            code
        )
    ))
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
        code <- paste0(
            "library(Rtsne)\nlibrary(ggplot2)\nlibrary(ggrepel)\n\n",
            "# count_mat — genes x samples; metadata — data.frame\n\n",
            sprintf("set.seed(%s)\n", isolate(input$tseed) %||% 0),
            "tsne_fit <- t(count_mat) |> as.data.frame() |>\n",
            sprintf('    Rtsne(dims=3, perplexity=%s, theta=%s, max_iter=%s)\n\n',
                    isolate(input$tperplex) %||% 30,
                    isolate(input$ttheta)   %||% 0.5,
                    isolate(input$titer)    %||% 1000),
            "tsne_df <- as.data.frame(tsne_fit$Y)\n",
            'colnames(tsne_df) <- c("tSNE1","tSNE2","tSNE3")\n',
            "tsne_df <- cbind(tsne_df, metadata)\n\n",
            sprintf('ggplot(tsne_df, aes(x=tSNE1, y=tSNE2, color=%s)) +\n', color_col),
            sprintf('    geom_point(size=%s) +\n', isolate(input$tSNEPointSize) %||% 5),
            '    theme_bw() +\n',
            sprintf('    theme(legend.position="%s")\n', isolate(input$TLegendPos) %||% "bottom")
        )
    }
    showModal(modalDialog(
        title=tagList(icon("file-code"), " Reproducible R Code — tSNE"),
        size="l", easyClose=TRUE, footer=modalButton("Close"),
        p("Copy this code to reproduce your tSNE plot offline.", style="color:#555; margin-bottom:12px;"),
        tags$pre(style=paste("background:#1e1e1e; color:#d4d4d4; border-radius:6px;",
            "padding:16px; font-size:12px; max-height:520px; overflow-y:auto;",
            "white-space:pre; font-family:'Courier New', monospace;"), code)
    ))
})

observeEvent(input$show_umap_code_modal, {
    ds <- isolate(tryCatch(InputReactive(), error=function(e) NULL))
    if (is.null(ds)) {
        code <- "# Load your data first, then click here to get the reproducible code."
    } else {
        color_col <- isolate(input$UMAPColor %||% "Group")
        code <- paste0(
            "library(umap)\nlibrary(ggplot2)\n\n",
            "# count_mat — genes x samples; metadata — data.frame\n\n",
            "umap_config <- umap.defaults\n",
            sprintf("umap_config$n_neighbors    <- %s\n", isolate(input$uneighbors) %||% 15),
            sprintf("umap_config$n_epochs       <- %s\n", isolate(input$uepochs)   %||% 200),
            sprintf("umap_config$metric         <- '%s'\n", isolate(input$umetric)  %||% "euclidean"),
            sprintf("umap_config$min_dist       <- %s\n", isolate(input$udist)     %||% 0.1),
            sprintf("umap_config$random_state   <- %s\n\n", isolate(input$useed)   %||% 0),
            "umap_fit <- umap(t(count_mat), config=umap_config)\n",
            "umap_df  <- as.data.frame(umap_fit$layout)\n",
            'colnames(umap_df) <- c("UMAP1","UMAP2")\n',
            "umap_df  <- cbind(umap_df, metadata)\n\n",
            sprintf('ggplot(umap_df, aes(x=UMAP1, y=UMAP2, color=%s)) +\n', color_col),
            sprintf('    geom_point(size=%s) +\n', isolate(input$UMAPPointSize) %||% 5),
            '    theme_bw() +\n',
            sprintf('    theme(legend.position="%s")\n', isolate(input$ULegendPos) %||% "bottom")
        )
    }
    showModal(modalDialog(
        title=tagList(icon("file-code"), " Reproducible R Code — UMAP"),
        size="l", easyClose=TRUE, footer=modalButton("Close"),
        p("Copy this code to reproduce your UMAP plot offline.", style="color:#555; margin-bottom:12px;"),
        tags$pre(style=paste("background:#1e1e1e; color:#d4d4d4; border-radius:6px;",
            "padding:16px; font-size:12px; max-height:520px; overflow-y:auto;",
            "white-space:pre; font-family:'Courier New', monospace;"), code)
    ))
})

# ─── Null-coalescing operator ─────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
