#' Extract XRGL scene specifications from an rglscene
#'
#' Transforms a \code{rglscene} (as produced by \code{rgl::scene3d()})
#' into a normalized XRGL specification with triangle meshes, line
#' ribbons, point clouds, and billboard text objects suitable for
#' glTF export.
#'
#' Conceptually, this function defines the "front end" of the XRGL
#' pipeline: it ingests rgl's heterogeneous scene representation and
#' produces a uniform, engine-agnostic intermediate form. Subsequent
#' stages (such as \code{xr_build_gltf()}) assume this normalized format
#' and therefore do not need to understand rgl's internal object types.
#'
#' The extraction process:
#' \enumerate{
#'   \item Iterates over all objects stored in the \code{rglscene}.
#'   \item Classifies each object by its \code{$type} field (e.g., "surface",
#'         "spheres", "linestrip", "triangles", "text").
#'   \item Converts each recognized type into one of a small set of XRGL
#'         canonical specs (e.g., triangle mesh, line segments, point cloud,
#'         billboard text).
#'   \item Preserves additional information (colors, normals, lights,
#'         background) in a structured form suitable for later mapping
#'         into glTF materials, lights, and textures.
#' }
#'
#' @param scene An object of class \code{"rglscene"}.
#'   Typically produced by \code{rgl::scene3d()} from an active rgl device.
#'   The function assumes that the structure follows rgl's documented
#'   scene conventions (e.g., presence of \code{objects}, \code{material},
#'   \code{bg}, and \code{$type} fields).
#'
#' @param debug Logical; if \code{TRUE}, emit progress messages and
#'   basic sanity checks. When enabled, the function reports the number
#'   of extracted items and attaches a frequency table of emitted
#'   \code{rgl_type} specifications via the \code{"counts"} attribute.
#'
#' @return A named list of extracted scene components (geometry,
#'   materials, text billboards, axes metadata, etc.), used as input to
#'   \code{xr_build_gltf()}. The list elements are themselves small
#'   structured specifications with a top-level \code{rgl_type} tag
#'   (e.g., \code{"triangles"}, \code{"lines"}, \code{"points"},
#'   \code{"billboard_text_spec"}, \code{"light_spec"}), plus the fields
#'   needed to generate glTF primitives or associated resources.
#'
#'   The list carries an additional \code{"counts"} attribute containing
#'   a \code{table} of \code{rgl_type} occurrences, which can be useful
#'   for debugging, logging, or summarizing scene content.
#'
#' @export
xr_extract <- function(scene, debug = TRUE) {
  
  # --- Preflight --------------------------------------------------------------
  # Validate that the input conforms to the expected rglscene structure.
  # This early check prevents subtle failures later in the pipeline by
  # enforcing the contract that xr_extract() must be called on a scene
  # returned by rgl::scene3d().
  if (!inherits(scene, "rglscene")) stop("scene must be an rglscene")
  
  # Helper: build a normalized "billboard text" spec that we can later
  # convert into a textured quad in glTF. This abstraction separates the
  # capture of textual content and layout hints (here) from the actual
  # rasterization and UV mapping (performed downstream in xr_build_gltf()).
  #
  # The returned object uses:
  # - rgl_type = "billboard_text_spec" as an internal XRGL identifier.
  # - 'anchor' as the 3D world-space position of the label.
  # - 'style' for typography-related hints (cex, family, font).
  # - 'material_hint' for glTF-level rendering decisions (unlit, blend, etc.).
  # - 'uv' as canonical full-quad texture coordinates, [0,1]x[0,1].
  make_text_billboard_spec <- function(txt, anchor, color_rgba = NULL,
                                       size_cex = 1, family = NULL, font = NULL,
                                       adj = NULL, justify = NULL) {
    # If color_rgba is not provided, default to opaque white. The function
    # expects a numeric vector of length 4 (RGBA in [0,1]), so coercion
    # is applied to the first row when a matrix is given.
    text_rgba <- if (is.null(color_rgba)) c(1,1,1,1) else as.numeric(color_rgba[1, ])
    list(
      rgl_type  = "billboard_text_spec",   # internal tag (not an rgl type)
      string    = as.character(txt),       # label contents
      anchor    = as.numeric(anchor),      # 3D world position for the label
      color     = text_rgba,               # legacy alias (same as text_rgba)
      text_rgba = text_rgba,               # RGBA in [0,1]
      background_rgba = c(0,0,0,0),        # transparent background
      style = list(                        # typography hints (carried through)
        cex      = size_cex %||% 1,
        family   = family %||% NULL,
        font     = font %||% NULL,
        adj      = adj %||% NULL,
        justify  = justify %||% NULL
      ),
      material_hint = list(                # glTF material guidance for text
        unlit             = TRUE,
        alphaMode         = "BLEND",
        doubleSided       = TRUE,
        sRGBBaseColor     = TRUE,
        premultipliedAlpha = FALSE
      ),
      billboard = TRUE,                    # mark for camera-facing behavior
      uv = matrix(                         # full-quad UVs in [0,1]
        c(0,0, 1,0, 1,1, 0,1),
        ncol = 2, byrow = TRUE
      )
    )
  }
  
  # Accumulator for extracted objects; each element is a normalized "spec"
  # describing a single logical entity (mesh, light, text label, etc.).
  out  <- list()
  
  # Shorthand reference to the list of rgl objects contained in the scene.
  objs <- scene$objects
  
  # Scene defaults (background/material) — preserve them if present so that
  # later pipeline stages can decide how to map them onto glTF constructs
  # (e.g., clear color, default material).
  if (!is.null(scene$material) || !is.null(scene$bg)) {
    out[[length(out)+1]] <- list(
      rgl_type = "scene_defaults",
      material = scene$material %||% NULL,
      bg       = scene$bg %||% NULL
    )
  }
  
  # --- Walk objects ------------------------------------------------------------
  # Iterate through each object within the rglscene and handle it according
  # to its declared type. For recognized types, we emit a canonical XRGL spec;
  # for unrecognized ones, we fall back to a 'raw_spec' wrapper.
  for (obj in objs) {
    # Every rgl object is expected to have a $type field. We treat missing
    # or NA types as unprocessable. Explicit "axes" objects are skipped here
    # (axes may be reconstructed synthetically elsewhere if desired).
    t <- obj$type %||% NA_character_
    if (is.na(t) || t == "axes") next
    
    # =========================
    # TEXT → billboard textures
    # =========================
    if (t == "text") {
      # Normalize text content and positions. rgl's representation for text
      # may store strings and coordinates in several different fields, so
      # we probe them in a priority order.
      txt <- obj$text %||% obj$texts %||% obj$strings
      pos <- obj$pos %||% obj$vertices %||% {
        if (!is.null(obj$x) && !is.null(obj$y) && !is.null(obj$z)) {
          cbind(obj$x, obj$y, obj$z)
        } else NULL
      }
      pos <- as_num_mat(pos, 3)
      if (is.null(txt) || is.null(pos) || nrow_safe(pos) == 0L) next
      
      # Compute per-label colors; if NULL, the helper will default to white.
      cm <- color_mat(obj, nrow(pos))
      
      # Emit one billboard spec per anchor position. If there are fewer
      # text strings than positions, the last string is repeated.
      for (k in seq_len(nrow(pos))) {
        out[[length(out)+1]] <- make_text_billboard_spec(
          txt[min(k, length(txt))],
          pos[k, ],
          if (is.null(cm)) NULL else matrix(cm[k, ], nrow = 1),
          size_cex = obj$cex %||% 1,
          family   = obj$family %||% NULL,
          font     = obj$font %||% NULL,
          adj      = obj$adj %||% NULL,
          justify  = obj$justify %||% NULL
        )
      }
      next
    }
    
    # =================
    # BACKGROUND (fog/bg)
    # =================
    if (t == "background") {
      # Preserve background color, fog parameters, and any associated
      # material hints. These can later be mapped to a viewer's clear
      # color or a glTF environment-like configuration.
      out[[length(out)+1]] <- list(
        rgl_type = "background_spec",
        color    = obj$color %||% NULL,
        colors   = obj$colors %||% NULL,
        fogtype  = obj$fogtype %||% NULL,
        fogscale = obj$fogscale %||% NULL,
        material = obj$material %||% NULL
      )
      next
    }
    
    # =======
    # LIGHTS
    # =======
    if (t == "light") {
      # Map rgl light parameters to a simplified, glTF-compatible light
      # specification. At this level we distinguish primarily between
      # finite (point) and infinite (directional) sources, preserving
      # color, intensity, and basic spatial descriptors.
      out[[length(out)+1]] <- list(
        rgl_type  = "light_spec",
        gltf_type = if (isTRUE(obj$finite)) "point" else "directional",
        color     = obj$diffuse %||% obj$ambient %||% c(1,1,1),
        intensity = obj$intensity %||% 1.0,
        range     = obj$range %||% NULL,
        position  = obj$position %||% NULL,
        direction = obj$direction %||% obj$viewpoint %||% NULL,
        specular  = obj$specular %||% NULL
      )
      next
    }
    
    # ==========================================
    # SURFACE (structured grid) → triangle mesh
    # ==========================================
    if (t == "surface") {
      positions <- as_num_mat(obj$vertices, 3)
      if (is.null(positions) || nrow_safe(positions) == 0L) next
      n_verts <- nrow(positions)
      
      # Determine grid dimensions using several potential sources:
      #   1) obj$dim, if present.
      #   2) nx/ny fields.
      #   3) material$dimension.
      #   4) As a last resort, infer N×N from a perfect square vertex count.
      dimvals <- obj$dim %||% {
        if (!is.null(obj$nx) && !is.null(obj$ny)) {
          c(as.integer(obj$nx), as.integer(obj$ny))
        } else NULL
      } %||% obj$material$dimension %||% {
        N <- round(sqrt(n_verts))
        if (N*N == n_verts) c(N, N) else NULL
      }
      if (is.null(dimvals) || prod(dimvals) != n_verts) next
      
      nx <- dimvals[1]; ny <- dimvals[2]
      # Row-major indexer for a 0-based flattened grid.
      idx <- function(r, c) r * ny + c
      ind <- integer()
      
      # Generate two triangles per cell in the structured grid:
      # (v00, v10, v01) and (v10, v11, v01), using CCW winding.
      for (r in 0:(nx-2)) {
        for (cc in 0:(ny-2)) {
          v00 <- idx(r,   cc)
          v10 <- idx(r+1, cc)
          v01 <- idx(r,   cc+1)
          v11 <- idx(r+1, cc+1)
          ind <- c(ind, v00, v10, v01,  v10, v11, v01)
        }
      }
      ind <- as.integer(ind)
      
      # Remove degenerate or invalid faces (e.g., collapsed triangles),
      # which improves robustness of subsequent normal computations.
      ind <- drop_degenerate(positions, ind)
      
      # Use supplied normals when they are present and well-formed;
      # otherwise compute per-vertex normals from the triangle mesh.
      nrm <- if (!is.null(obj$normals) && nrow_safe(obj$normals) == n_verts) {
        as_num_mat(obj$normals, 3)
      } else {
        compute_vertex_normals(positions, ind)
      }
      
      # Emit a consolidated triangle mesh specification representing the surface.
      out[[length(out)+1]] <- list(
        rgl_type  = "triangles",
        positions = positions,
        normals   = nrm,
        colors    = color_mat(obj, n_verts),
        indices   = ind
      )
      next
    }
    
    # ============================
    # SPHERES → meshed approximations
    # ============================
    if (t == "spheres") {
      # Centers may be stored either as $centers or as generic $vertices.
      centers <- as_num_mat(obj$centers %||% obj$vertices, 3)
      if (is.null(centers) || nrow_safe(centers) == 0L) next
      
      # Radii values can be scalar or vector; recycle to match the number
      # of centers. If lengths do not align, fall back to recycling the
      # first radius across all centers.
      radii <- obj$radii %||% obj$size %||% 1
      if (length(radii) == 1L) radii <- rep(radii, nrow(centers))
      if (length(radii) != nrow(centers)) radii <- rep(radii[1], nrow(centers))
      
      cols <- color_mat(obj, nrow(centers))
      
      # For each sphere, approximate the geometry with a pre-defined
      # triangle mesh (via approximate_sphere) and emit as "triangles".
      for (k in seq_len(nrow(centers))) {
        m   <- approximate_sphere(centers[k, ], radii[k])
        pos <- as_num_mat(m$positions, 3)
        ind <- as.integer(m$indices)
        ind <- drop_degenerate(pos, ind)
        nrm <- if (!is.null(m$normals)) {
          as_num_mat(m$normals, 3)
        } else {
          compute_vertex_normals(pos, ind)
        }
        
        out[[length(out)+1]] <- list(
          rgl_type  = "triangles",
          positions = pos,
          normals   = nrm,
          colors    = if (!is.null(cols)) {
            matrix(rep(cols[k, ], nrow(pos)), nrow = nrow(pos), byrow = TRUE)
          } else NULL,
          indices   = ind
        )
      }
      next
    }
    
    # =======================
    # LINESTRIP → disjoint pairs
    # =======================
    if (t == "linestrip") {
      positions <- as_num_mat(obj$vertices, 3)
      if (is.null(positions) || nrow_safe(positions) < 2L) next
      n <- nrow(positions)
      
      # A linestrip represents a polyline (0,1,2,...,n-1). We convert this
      # to a set of disjoint line segments (0,1)(1,2)... for downstream
      # ribbonization and glTF export.
      pairs <- as.integer(c(rbind(0:(n-2L), 1:(n-1L))))
      out[[length(out)+1]] <- list(
        rgl_type   = "lines",
        positions  = positions,
        colors     = color_mat(obj, n),
        indices    = pairs,
        material_hint = list(lineLike = TRUE)  # hint for later ribbonization
      )
      next
    }
    
    # ===========================
    # LINES / BBOXDECO → literal pairs
    # ===========================
    if (t %in% c("lines", "bboxdeco")) {
      positions <- as_num_mat(obj$vertices, 3)
      if (is.null(positions) || nrow_safe(positions) < 2L) next
      n <- nrow(positions); pair_n <- n %/% 2L
      if (pair_n == 0L) next
      
      # Interpret vertices as literal endpoint pairs (0,1)(2,3)(4,5)...
      # This convention mirrors rgl's typical handling of line objects.
      line_pairs <- as.integer(as.vector(rbind(
        seq(0, 2*pair_n-1, by = 2),
        seq(1, 2*pair_n,   by = 2)
      )))
      out[[length(out)+1]] <- list(
        rgl_type   = "lines",
        positions  = positions,
        colors     = color_mat(obj, n),
        indices    = line_pairs,
        material_hint = list(lineLike = TRUE)
      )
      next
    }
    
    # =====================================
    # TRIANGLES / QUADS / POINTS passthrough
    # =====================================
    if (t %in% c("triangles", "quads", "points")) {
      positions <- as_num_mat(obj$vertices, 3)
      if (is.null(positions) || nrow_safe(positions) == 0L) next
      n_verts <- nrow(positions)
      
      rtype <- t
      idxs  <- integer(0)
      
      # Prefer pre-indexed faces when supplied:
      # - mesh3d: $it (triangles)
      # - generic: $indices
      if (!is.null(obj$indices)) {
        idxs <- as.integer(obj$indices)
        # rgl indices are often 1-based; convert to 0-based if needed.
        if (length(idxs) && min(idxs) == 1L) idxs <- idxs - 1L
      } else if (!is.null(obj$it)) {
        idxs <- as.integer(obj$it) - 1L
      } else if (t == "quads") {
        # Split quads into two triangles per 4-vertex block. If the total
        # vertex count is not a multiple of 4, we consider the data invalid.
        if (n_verts %% 4L != 0L) next
        q <- n_verts / 4L; tri <- integer(0)
        for (qq in seq_len(q)) {
          b <- (qq - 1L) * 4L
          tri <- c(tri, b+0L, b+1L, b+2L,  b+0L, b+2L, b+3L)
        }
        idxs  <- tri
        rtype <- "triangles"
      } else if (t == "triangles") {
        # If no indices are provided, treat the vertices as an unindexed
        # triangle list in sequential order.
        idxs <- seq.int(0L, n_verts - 1L)
      } else if (t == "points") {
        # Points carry only positions and optional colors. They do not use
        # normals or indices at this stage; later pipeline steps may decide
        # to approximate them with small meshes for consistent rendering.
        out[[length(out)+1]] <- list(
          rgl_type      = "points",
          positions     = positions,
          colors        = color_mat(obj, n_verts),
          material_hint = list(pointLike = TRUE)
        )
        next
      }
      
      idxs <- as.integer(idxs)
      idxs <- drop_degenerate(positions, idxs)
      
      # Adopt provided normals when they are available and compatible with
      # the vertex count; otherwise compute vertex normals from geometry.
      normals <- if (!is.null(obj$normals) && nrow_safe(obj$normals) == n_verts) {
        as_num_mat(obj$normals, 3)
      } else {
        compute_vertex_normals(positions, idxs)
      }
      
      out[[length(out)+1]] <- list(
        rgl_type  = rtype,
        positions = positions,
        normals   = normals,
        colors    = color_mat(obj, n_verts),
        indices   = idxs
      )
      next
    }
    
    # -----------
    # Fallback raw
    # -----------
    # If we do not recognize the rgl object type, preserve it verbatim
    # as a "raw_spec". This allows future versions of the library (or
    # downstream consumers) to inspect and potentially support additional
    # rgl types without losing information at this stage.
    out[[length(out)+1]] <- list(
      rgl_type = "raw_spec",
      raw_type = t,
      raw      = obj
    )
  }
  
  # Summary + counts for debugging/telemetry.
  # When debug is TRUE, emit a message summarizing how many items were
  # extracted. In all cases, attach a "counts" attribute recording the
  # frequency of each emitted rgl_type. This can be useful for unit tests,
  # regression checks, and high-level scene introspection.
  if (debug) message(sprintf("extract_scene_geometry: emitted %d items", length(out)))
  attr(out, "counts") <- table(vapply(out, `[[`, "", "rgl_type"))
  out
}
