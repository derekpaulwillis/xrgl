#' Build a glTF 2.0 document from XRGL specs
#'
#' Converts the normalized XRGL specification returned by
#' \code{xr_extract()} into a glTF 2.0 JSON structure and a single
#' binary buffer, ready to be written as a GLB file.
#'
#' @param extracted The list returned by \code{xr_extract()}.
#' @param debug Logical; if \code{TRUE}, perform extra checks and emit
#'   diagnostic messages.
#'
#' @return A list with at least:
#'   \item{gltf_json}{A named list representing the glTF JSON document.}
#'   \item{gltf_raw}{A single raw vector containing all binary data.}
#'
#' @export

xr_build_gltf <- function(
    extracted,
    text_height_fraction = 0.06,
    line_ribbon_fraction_of_text = 0.15,
    add_axes = FALSE,
    nticks = 5,
    axis_color = c(1,1,1,1),
    tick_length_fraction = 0.04,
    axes_origin = c(-1,-1,-1),
    text_dpi = 192,
    text_padding_px = 8
) {
  # Filter the extracted XRGL specification into:
  # - 'geom': geometric primitives (points, lines, triangles) derived directly from rgl
  # - 'bbtx': billboard text specifications (to be rendered as textured quads)
  geom <- Filter(function(x) x$rgl_type %in% c("points","lines","triangles"), extracted)
  bbtx <- Filter(function(x) identical(x$rgl_type, "billboard_text_spec"), extracted)
  
  # --- bbox / scene scale -----------------------------------------------------
  # Compute a global bounding box over all geometric primitives, falling back to
  # billboard anchors if necessary. This is used to infer an overall scene scale
  # and derive relative sizes for text and line ribbons.
  all_pos <- do.call(rbind, lapply(geom, `[[`, "positions"))
  if (is.null(all_pos) || nrow(all_pos) == 0) {
    # If there are no geometric positions, approximate the spatial extent from
    # the billboard text anchor locations instead.
    anchors <- do.call(rbind, lapply(bbtx, `[[`, "anchor"))
    all_pos <- anchors[stats::complete.cases(anchors), , drop = FALSE]
  }
  # If no positions are available at all, fall back to a trivial origin-based scene.
  if (is.null(all_pos) || nrow(all_pos) == 0) all_pos <- matrix(c(0,0,0), ncol = 3)
  
  # Axis-aligned bounding box and diagonal length, used as a robust measure of
  # scene scale. Degenerate or non-finite cases are clamped to 1.
  bmin <- apply(all_pos, 2, min); bmax <- apply(all_pos, 2, max)
  scene_diag <- sqrt(sum((bmax - bmin)^2)); if (!is.finite(scene_diag) || scene_diag == 0) scene_diag <- 1
  
  # Base text height in world units is expressed as a fraction of the scene
  # diagonal. This ensures text appears appropriately sized relative to the data.
  base_text_h_world <- text_height_fraction * scene_diag
  
  # --- optional synthetic axes ------------------------------------------------
  # Optionally emit a set of three axis lines (X, Y, Z) plus tick marks and
  # billboard labels. This is entirely synthetic geometry generated in world
  # coordinates to provide context for the data.
  if (isTRUE(add_axes)) {
    # Determine the axis origin. If the user-specified origin is invalid,
    # default to the minimum corner of the data bounding box.
    o <- as.numeric(axes_origin); if (length(o)!=3 || any(!is.finite(o))) o <- bmin
    
    # Construct three line segments representing the +X, +Y, and +Z axes.
    px <- rbind(o, c(bmax[1], o[2], o[3]))
    py <- rbind(o, c(o[1], bmax[2], o[3]))
    pz <- rbind(o, c(o[1], o[2], bmax[3]))
    axis_positions <- rbind(px, py, pz)
    
    # Indices for three line segments: (0,1), (2,3), (4,5)
    axis_indices   <- as.integer(c(0,1, 2,3, 4,5))
    
    # Per-vertex RGBA colors for each axis line (here all share the same color).
    axis_cols      <- rbind(
      matrix(axis_color, nrow=2, byrow=TRUE),
      matrix(axis_color, nrow=2, byrow=TRUE),
      matrix(axis_color, nrow=2, byrow=TRUE)
    )
    
    # Append the synthetic axis geometry as a standard "lines" primitive to geom.
    geom <- c(geom, list(list(
      rgl_type = "lines",
      positions = axis_positions,
      colors = axis_cols,
      indices = axis_indices
    )))
    
    # Optionally add tick marks along each axis. These are rendered as short
    # line segments perpendicular to the axis direction, evenly spaced.
    if (nticks > 1) {
      tick_len <- tick_length_fraction * scene_diag
      
      # Helper to generate tick segment geometry for a single axis.
      # a, b: endpoints of the axis line
      # n: number of ticks
      # perp_axis: unit direction used to offset tick lines (perpendicular to axis)
      make_ticks <- function(a, b, n, perp_axis = c(0,0,1)) {
        ts <- list(); idx <- list(); cols <- list(); base <- 0L
        for (k in 0:(n-1)) {
          # Parameter t0 evenly samples the axis from 0 to 1.
          t0 <- if (n==1) 0 else k/(n-1)
          p <- a + (b - a) * t0
          d <- perp_axis
          # Two points forming a short segment centered at p.
          q0 <- p - d * (tick_len/2); q1 <- p + d * (tick_len/2)
          ts[[length(ts)+1]]  <- rbind(q0, q1)
          idx[[length(idx)+1]]<- c(base, base+1L); base <- base + 2L
          cols[[length(cols)+1]] <- rbind(axis_color, axis_color)
        }
        list(
          pos = do.call(rbind, ts),
          ind = as.integer(unlist(idx)),
          col = do.call(rbind, cols)
        )
      }
      
      # Generate tick marks for X, Y, and Z axes with different perpendicular directions.
      tx <- make_ticks(px[1,], px[2,], nticks, perp_axis = c(0,0,1))
      ty <- make_ticks(py[1,], py[2,], nticks, perp_axis = c(0,0,1))
      tz <- make_ticks(pz[1,], pz[2], nticks, perp_axis = c(-1,0,0))
      
      # Append tick geometry as additional "lines" primitives, if any.
      if (!is.null(tx$pos)) geom <- c(geom, list(list(
        rgl_type="lines", positions=tx$pos, colors=tx$col, indices=tx$ind)))
      if (!is.null(ty$pos)) geom <- c(geom, list(list(
        rgl_type="lines", positions=ty$pos, colors=ty$col, indices=ty$ind)))
      if (!is.null(tz$pos)) geom <- c(geom, list(list(
        rgl_type="lines", positions=tz$pos, colors=tz$col, indices=tz$ind)))
    }
    
    # Helper to append a labeled billboard text spec for each axis endpoint.
    add_lbl <- function(txt, pos) bbtx[[length(bbtx)+1]] <<- list(
      rgl_type = "billboard_text_spec",
      string   = txt,
      anchor   = pos,
      color    = axis_color,
      style    = list(cex = 1),
      billboard = TRUE
    )
    add_lbl("+X", px[2,]); add_lbl("+Y", py[2,]); add_lbl("+Z", pz[2,])
  }
  
  # --- state ------------------------------------------------------------------
  # The following lists accumulate state during construction of the glTF model.
  # - 'all_binary' stores chunks of raw binary data to be concatenated into one buffer.
  # - 'buffer_views' and 'accessors' define how to interpret subranges of that buffer.
  # - 'meshes', 'nodes', 'materials', 'images', 'textures', and 'samplers' form
  #    the main glTF scene graph and material/texture resources.
  # - 'extensionsUsed' tracks glTF extensions required by this document.
  all_binary <- list(); buffer_views <- list(); accessors <- list()
  meshes <- list(); nodes <- list(); images <- list(); textures <- list()
  samplers <- list(); materials <- list(); extensionsUsed <- character(0)
  current_off <- 0L  # running byte offset into the binary buffer
  
  # Helper to append a raw byte vector to the global binary buffer, enforcing
  # 4-byte alignment as required by the GLB specification. Returns a record of
  # byteOffset and byteLength for the appended region.
  add_bytes <- function(rawvec) {
    start <- align4(current_off)
    pad <- start - current_off
    if (pad > 0) all_binary[[length(all_binary)+1]] <<- as.raw(rep(0, pad))
    all_binary[[length(all_binary)+1]] <<- rawvec
    len <- length(rawvec)
    current_off <<- start + len
    list(byteOffset = as.integer(start), byteLength = as.integer(len))
  }
  
  # Helper to create a bufferView around a given raw vector. If a 'target'
  # (ARRAY_BUFFER or ELEMENT_ARRAY_BUFFER) is specified, it is stored according
  # to the glTF bufferView schema.
  add_bufview <- function(target, rawvec) {
    loc <- add_bytes(rawvec)
    view <- list(
      buffer     = 0L,
      byteOffset = loc$byteOffset,
      byteLength = loc$byteLength
    )
    if (!is.null(target)) view$target <- as.integer(target)
    buffer_views[[length(buffer_views)+1]] <<- view
    as.integer(length(buffer_views) - 1L)
  }
  
  # Helper to create an accessor referencing a bufferView. Accessors describe
  # how to interpret a typed slice of the buffer: component type, count, and
  # glTF "type" (e.g., SCALAR, VEC3).
  # Optional min/max are supplied for POSITION attributes to help viewers
  # quickly compute bounding volumes.
  add_accessor <- function(view_idx, comp, count, type,
                           byteOffset = 0L, min_val = NULL, max_val = NULL) {
    acc <- list(
      bufferView   = as.integer(view_idx),
      byteOffset   = as.integer(byteOffset),
      componentType = as.integer(comp),
      count        = as.integer(count),
      type         = type
    )
    if (!is.null(min_val)) acc$min <- as.list(min_val)
    if (!is.null(max_val)) acc$max <- as.list(max_val)
    accessors[[length(accessors)+1]] <<- acc
    as.integer(length(accessors) - 1L)
  }
  
  # --- defaults (materials, sampler) ------------------------------------------
  # Define a default double-sided material for general geometry.
  default_geom_mat_index <- {
    materials[[length(materials)+1]] <- list(
      name = "DefaultGeom",
      pbrMetallicRoughness = list(
        baseColorFactor = c(1,1,1,1),
        metallicFactor  = 0,
        roughnessFactor = 1
      ),
      doubleSided = TRUE
    )
    as.integer(length(materials) - 1L)
  }
  
  # Define an unlit material for line ribbons and billboards. This uses the
  # KHR_materials_unlit extension so that colors are not affected by lighting.
  lines_unlit_mat_index <- {
    materials[[length(materials)+1]] <- list(
      name = "LinesUnlit",
      pbrMetallicRoughness = list(
        baseColorFactor = c(1,1,1,1),
        metallicFactor  = 0,
        roughnessFactor = 1
      ),
      doubleSided = TRUE,
      extensions  = list(KHR_materials_unlit = empty_obj())
    )
    extensionsUsed <- union(extensionsUsed, "KHR_materials_unlit")
    as.integer(length(materials) - 1L)
  }
  
  # Define a default sampler for all textures, using linear filtering and
  # clamp-to-edge wrapping (sufficient for text billboards).
  default_sampler_index <- {
    samplers[[length(samplers)+1]] <- list(
      magFilter = 9729L,  # LINEAR
      minFilter = 9729L,  # LINEAR
      wrapS     = 33071L, # CLAMP_TO_EDGE
      wrapT     = 33071L  # CLAMP_TO_EDGE
    )
    as.integer(length(samplers) - 1L)
  }
  
  # --- geometry ---------------------------------------------------------------
  # Iterate over all geometric primitives and emit corresponding glTF
  # meshes and nodes. Lines are converted to triangle ribbons; points
  # are approximated as small spheres; triangles are used directly.
  for (m_i in seq_along(geom)) {
    g <- geom[[m_i]]
    positions <- g$positions; if (is.null(positions) || nrow(positions)==0) next
    rgl_type  <- g$rgl_type
    indices   <- g$indices
    colors    <- g$colors
    was_ribbon <- FALSE
    
    # lines -> ribbons (with color expansion)
    # glTF line primitives have size-dependent rendering that varies by engine.
    # To obtain consistent screen-space thickness, XRGL converts line strips to
    # camera-facing triangle ribbons using 'triangulate_lines_to_quads'.
    if (identical(rgl_type, "lines")) {
      if (length(indices) < 2) next
      width_world <- max(1e-6, line_ribbon_fraction_of_text * base_text_h_world)
      tri <- triangulate_lines_to_quads(
        positions, indices, width_world, colors = colors
      )
      if (is.null(tri)) next
      positions <- tri$positions
      indices   <- tri$indices
      colors    <- tri$colors  # keep expanded per-vertex colors (may be NULL)
      rgl_type  <- "triangles"
      was_ribbon <- TRUE
    } else if (!identical(rgl_type, "points") && length(indices) == 0) {
      # For non-point primitives, a missing or empty index array is treated
      # as invalid and the geometry is skipped.
      next
    }
    
    n_verts <- nrow(positions)
    
    # index range guard
    # Remove any indices that fall outside the valid vertex range or are
    # non-finite. If all indices are invalid, the primitive is discarded.
    if (!identical(rgl_type, "points") && length(indices)) {
      keep <- indices >= 0L & indices < n_verts & is.finite(indices)
      if (!all(keep)) indices <- indices[keep]
      if (!length(indices)) next
    }
    
    # normals: compute if triangles and missing
    # If no per-vertex normals are provided for a triangle mesh, synthesize
    # them via 'compute_vertex_normals'.
    normals <- g$normals
    if (identical(rgl_type, "triangles") && (is.null(normals) || nrow(normals) != n_verts)) {
      normals <- compute_vertex_normals(positions, indices)
    }
    
    # If the color array does not match the vertex count, it is dropped to
    # avoid constructing malformed COLOR_0 attributes in the glTF document.
    if (!is.null(colors) && nrow(colors) != n_verts) colors <- NULL
    
    # POSITION attribute: pack as 32-bit floats and create a corresponding
    # bufferView and accessor. Exact bounds are recorded for the POSITION
    # accessor.
    pos_f32  <- to_f32(as.numeric(t(positions)))
    pos_view <- add_bufview(
      34962L,  # ARRAY_BUFFER
      as.raw(writeBin(pos_f32, raw(), size=4, endian="little"))
    )
    b_exact  <- bounds_f32_exact(pos_f32)
    pos_acc  <- add_accessor(
      pos_view, 5126L,           # FLOAT
      length(pos_f32)%/%3L, "VEC3",
      min_val=b_exact$min, max_val=b_exact$max
    )
    
    # NORMAL attribute (optional).
    norm_acc <- NULL
    if (!is.null(normals)) {
      norm_f32  <- to_f32(as.numeric(t(normals)))
      norm_view <- add_bufview(
        34962L,
        as.raw(writeBin(norm_f32, raw(), size=4, endian="little"))
      )
      norm_acc  <- add_accessor(norm_view, 5126L, n_verts, "VEC3")
    }
    
    # COLOR_0 attribute (optional, RGBA).
    col_acc <- NULL
    if (!is.null(colors)) {
      col_f32  <- to_f32(as.numeric(t(colors)))
      col_view <- add_bufview(
        34962L,
        as.raw(writeBin(col_f32, raw(), size=4, endian="little"))
      )
      col_acc  <- add_accessor(col_view, 5126L, n_verts, "VEC4")
    }
    
    # Assemble the attribute dictionary for this primitive.
    prim_attrib <- list(POSITION = pos_acc)
    if (!is.null(norm_acc)) prim_attrib$NORMAL  <- norm_acc
    if (!is.null(col_acc))  prim_attrib$COLOR_0 <- col_acc
    
    # Base name used for mesh and node identifiers in the glTF document.
    base_name <- sprintf("XRGL_%s_%03d", rgl_type, m_i)
    
    # If the primitive is a set of points, approximate each point by a small
    # triangle-based sphere. This avoids renderer-dependent point size behavior
    # and yields consistent appearance across viewers.
    if (identical(rgl_type, "points")) {
      # --- Replace glTF POINTS with small triangle spheres for consistent size ---
      sphere_radius <- 0.01 * scene_diag   # radius relative to scene scale
      pts <- positions
      n_pts <- nrow(pts)
      
      for (p_i in seq_len(n_pts)) {
        center <- pts[p_i, ]
        # Each sphere inherits a single color determined by the original
        # per-point color if available; otherwise it defaults to white.
        color  <- if (!is.null(colors)) colors[min(p_i, nrow(colors)), ] else c(1,1,1,1)
        
        # 'approximate_sphere' returns positions, indices, and optionally normals
        # for a small triangle mesh centered at 'center'.
        sph <- approximate_sphere(center, sphere_radius)
        pos_s <- as_num_mat(sph$positions, 3)
        ind_s <- as.integer(sph$indices)
        nrm_s <- if (!is.null(sph$normals)) as_num_mat(sph$normals, 3) else compute_vertex_normals(pos_s, ind_s)
        col_s <- matrix(rep(color, nrow(pos_s)), nrow = nrow(pos_s), byrow = TRUE)
        
        # POSITION for sphere
        pos_f32  <- to_f32(as.numeric(t(pos_s)))
        pos_view <- add_bufview(
          34962L,
          as.raw(writeBin(pos_f32, raw(), size=4, endian="little"))
        )
        b_exact  <- bounds_f32_exact(pos_f32)
        pos_acc  <- add_accessor(
          pos_view, 5126L, nrow(pos_s), "VEC3",
          min_val=b_exact$min, max_val=b_exact$max
        )
        
        # NORMAL for sphere
        norm_f32  <- to_f32(as.numeric(t(nrm_s)))
        norm_view <- add_bufview(
          34962L,
          as.raw(writeBin(norm_f32, raw(), size=4, endian="little"))
        )
        norm_acc  <- add_accessor(norm_view, 5126L, nrow(pos_s), "VEC3")
        
        # COLOR_0 for sphere
        col_f32  <- to_f32(as.numeric(t(col_s)))
        col_view <- add_bufview(
          34962L,
          as.raw(writeBin(col_f32, raw(), size=4, endian="little"))
        )
        col_acc  <- add_accessor(col_view, 5126L, nrow(pos_s), "VEC4")
        
        prim_attrib_s <- list(
          POSITION = pos_acc,
          NORMAL   = norm_acc,
          COLOR_0  = col_acc
        )
        
        # Indices for sphere triangles (16-bit index type sufficient for typical sphere tessellations).
        idx_raw  <- as.raw(writeBin(as.integer(ind_s), raw(), size = 2, endian = "little"))
        idx_view <- add_bufview(34963L, idx_raw)  # ELEMENT_ARRAY_BUFFER
        idx_acc  <- add_accessor(idx_view, 5123L, length(ind_s), "SCALAR")
        
        # Emit a separate mesh and node for each point-sphere.
        meshes[[length(meshes)+1]] <- list(
          primitives = list(list(
            attributes = prim_attrib_s,
            indices    = idx_acc,
            mode       = 4L,  # TRIANGLES
            material   = default_geom_mat_index
          )),
          name = sprintf("%s_point_%04d", base_name, p_i)
        )
        nodes[[length(nodes)+1]] <- list(
          mesh = as.integer(length(meshes) - 1L),
          name = sprintf("%s_point_%04d", base_name, p_i)
        )
      }
      # Skip the remainder of the loop, since POINTS have been fully expanded.
      next
    }
    
    # indices accessor
    # Encode indices as either 16-bit or 32-bit unsigned integers depending on
    # the maximum index value, to balance compactness and capacity.
    max_idx <- if (length(indices)) max(indices) else 0L
    ct <- if (max_idx <= 65535L) 5123L else 5125L  # UNSIGNED_SHORT vs UNSIGNED_INT
    sz <- if (max_idx <= 65535L) 2L    else 4L
    idx_raw  <- as.raw(writeBin(as.integer(indices), raw(), size = sz, endian = "little"))
    idx_view <- add_bufview(34963L, idx_raw)
    idx_acc  <- add_accessor(idx_view, ct, length(indices), "SCALAR")
    
    # material selection: ribbons use unlit, others use default double-sided
    # Line ribbons (converted from rgl lines) are rendered with an unlit,
    # double-sided material to emphasize the encoded color without shading.
    mat_index <- if (was_ribbon) lines_unlit_mat_index else default_geom_mat_index
    
    # Register this primitive as a glTF mesh with one triangle primitive.
    meshes[[length(meshes)+1]] <- list(
      primitives = list(list(
        attributes = prim_attrib,
        indices    = idx_acc,
        mode       = 4L,      # TRIANGLES
        material   = mat_index
      )),
      name = base_name
    )
    # Create a node that instances this mesh at the default transform.
    nodes[[length(nodes)+1]]  <- list(
      mesh = as.integer(length(meshes)-1L),
      name = base_name
    )
  }
  
  # --- billboards (flip V for PNG UVs) ---------------------------------------
  # Construct textured quads for each billboard text specification. The text
  # is rendered into a PNG in memory, then attached as a texture and mapped
  # onto a camera-facing rectangle at the anchor location.
  if (length(bbtx)) {
    extensionsUsed <- union(extensionsUsed, "KHR_materials_unlit")
    
    # Helper to register a PNG texture and an associated unlit material.
    # The image is stored in the buffer as a bufferView, then referenced
    # from 'images', 'textures', and 'materials'.
    add_png_texture_material <- function(png_raw, name_prefix = "TextMat_") {
      img_view <- add_bufview(NULL, png_raw)
      images[[length(images)+1]]     <<- list(
        bufferView = img_view,
        mimeType   = "image/png"
      )
      textures[[length(textures)+1]] <<- list(
        source  = as.integer(length(images)-1L),
        sampler = default_sampler_index
      )
      tex_idx0 <- as.integer(length(textures)-1L)
      
      materials[[length(materials)+1]] <<- list(
        name = paste0(name_prefix, length(materials)),
        pbrMetallicRoughness = list(
          baseColorTexture = list(index = tex_idx0),
          metallicFactor   = 0,
          roughnessFactor  = 1
        ),
        emissiveTexture = list(index = tex_idx0),
        emissiveFactor  = c(1,1,1),
        alphaMode       = "BLEND",
        doubleSided     = TRUE,
        extensions = list(KHR_materials_unlit = empty_obj())
      )
      list(material_index = as.integer(length(materials)-1L))
    }
    
    # For each billboard text spec, generate a quad aligned with the XY plane
    # (facing +Z) and offset slightly in Z to avoid z-fighting with nearby geometry.
    for (k in seq_along(bbtx)) {
      spec <- bbtx[[k]]
      anchor <- spec$anchor; if (any(!is.finite(anchor))) next
      
      # Compute text height in world units, scaled by 'cex' where provided.
      cex <- spec$style$cex %||% 1
      h_world <- base_text_h_world * cex
      
      # Clamp color to [0,1] and ensure RGBA length 4.
      col <- spec$color %||% c(1,1,1,1)
      col <- pmax(0, pmin(1, as.numeric(col)))
      if (length(col) < 4) col <- c(col[1:3], 1)
      
      # Render text into a PNG using the helper '.make_label_png', which returns
      # both raw PNG bytes and the pixel dimensions. These are used to compute
      # the aspect ratio in world coordinates.
      png_out <- .make_label_png(
        spec$string,
        dpi    = text_dpi,
        pad_px = text_padding_px,
        family = spec$style$family,
        font   = spec$style$font,
        cex    = cex,
        color  = col
      )
      w_world <- h_world * (png_out$w_px / png_out$h_px)
      
      # Quad geometry centered at 'anchor' in XY, slightly offset in +Z.
      x0 <- anchor[1] - w_world/2; x1 <- anchor[1] + w_world/2
      y0 <- anchor[2] - h_world/2; y1 <- anchor[2] + h_world/2
      z  <- anchor[3] + 1e-5 * scene_diag
      positions <- matrix(c(
        x0,y0,z,
        x1,y0,z,
        x1,y1,z,
        x0,y1,z
      ), ncol=3, byrow=TRUE)
      
      # Texture coordinates with flipped V to account for the PNG coordinate
      # system versus glTF's UV conventions.
      uvs <- matrix(c(
        0,1,
        1,1,
        1,0,
        0,0
      ), ncol=2, byrow=TRUE)
      indices_b <- as.integer(c(0,1,2, 0,2,3))
      
      # POSITION attribute for the quad.
      pos_f32  <- to_f32(as.numeric(t(positions)))
      pos_view <- add_bufview(
        34962L,
        as.raw(writeBin(pos_f32, raw(), size=4, endian="little"))
      )
      b_exact  <- bounds_f32_exact(pos_f32)
      pos_acc  <- add_accessor(
        pos_view, 5126L,
        length(pos_f32)%/%3L, "VEC3",
        min_val=b_exact$min, max_val=b_exact$max
      )
      
      # TEXCOORD_0 attribute.
      uv_view  <- add_bufview(
        34962L,
        as.raw(writeBin(as.numeric(t(uvs)), raw(), size=4, endian="little"))
      )
      uv_acc   <- add_accessor(uv_view, 5126L, 4L, "VEC2")
      
      # Indices for two triangles forming the quad.
      idx_view <- add_bufview(
        34963L,
        as.raw(writeBin(indices_b, raw(), size=2, endian="little"))
      )
      idx_acc  <- add_accessor(idx_view, 5123L, length(indices_b), "SCALAR")
      
      # Register the PNG as a texture + unlit material and reference it from
      # the billboard mesh primitive.
      itm <- add_png_texture_material(png_out$raw)
      
      meshes[[length(meshes)+1]] <- list(
        primitives = list(list(
          attributes = list(
            POSITION   = pos_acc,
            TEXCOORD_0 = uv_acc
          ),
          indices  = idx_acc,
          mode     = 4L,  # TRIANGLES
          material = itm$material_index
        )),
        name = sprintf("XRGL_billboard_%03d", k)
      )
      nodes[[length(nodes)+1]]  <- list(
        mesh = as.integer(length(meshes)-1L),
        name = sprintf("XRGL_billboard_%03d", k)
      )
    }
  }
  
  # --- finalize json ----------------------------------------------------------
  # At this stage, all geometry, billboards, materials, textures, samplers, and
  # nodes have been registered. We now finalize the glTF JSON document and
  # concatenate the binary chunks into a single buffer suitable for GLB output.
  
  # Ensure the total buffer length is padded to 4-byte alignment.
  padded_len <- align4(current_off)
  
  # Construct the top-level glTF structure. The scene graph consists of a single
  # default scene (index 0) referencing all nodes in order.
  gltf <- list(
    asset       = list(version = "2.0"),
    buffers     = list(list(byteLength = as.integer(padded_len))),
    bufferViews = buffer_views,
    accessors   = accessors,
    materials   = materials,
    meshes      = meshes,
    nodes       = nodes,
    samplers    = samplers,
    scene       = 0L,
    scenes      = list(list(nodes = as.list(seq(0L, length(nodes)-1L))))
  )
  
  # Optionally attach images, textures, and declared extensions if present.
  if (length(images))          gltf$images         <- images
  if (length(textures))        gltf$textures       <- textures
  if (length(extensionsUsed))  gltf$extensionsUsed <- as.list(unique(extensionsUsed))
  
  # Combine all binary chunks into a single raw vector representing the buffer.
  list(
    gltf_json = gltf,
    gltf_raw  = do.call(c, all_binary)
  )
}
