#' Internal XRGL utilities
#'
#' Helper functions used inside the XRGL pipeline. Not intended to be called
#' directly by end users.
#'
#' These utilities support:
#' \itemize{
#'   \item Robust handling of potentially incomplete or ill-formed geometry
#'         (e.g., degenerate triangles, missing normals).
#'   \item Conversion of rgl-style data structures into numerically stable
#'         matrix forms suitable for downstream processing.
#'   \item Construction of simple analytic meshes (e.g., spheres) and
#'         camera-facing ribbons for line primitives.
#'   \item Consistent color handling and texture generation for text labels.
#'   \item Maintenance of glTF/GLB-related invariants such as 4-byte
#'         alignment and accessor bounds.
#' }
#'
#' These functions are documented for maintainers and researchers who
#' may wish to extend XRGL’s capabilities, but they are not part of the
#' public user-facing API.
#'
#' @keywords internal
"_xrgl_internal_utils"

# Core utilities -----------------------------------------------------------
# `%||%`: Null-coalescing helper for lists/values.
# Returns `a` if it is not NULL; otherwise returns `b`.
# This pattern is used throughout the XRGL pipeline to provide robust
# defaults while preserving explicit user- or scene-specified values
# when present.
`%||%` <- function(a, b) if (!is.null(a)) a else b

# nrow_safe(): Safe row counter that tolerates NULL.
# - x: object that may be NULL or a matrix/data.frame.
# - returns: 0L if x is NULL; otherwise base::nrow(x).
# This is used extensively when dealing with rgl objects whose fields
# may be absent or whose geometry may be optional.
nrow_safe <- function(x) if (is.null(x)) 0L else nrow(x)

# as_num_mat(): Convert input to a double-precision matrix with optional
# column sizing/padding/truncation.
# - x: matrix/data.frame/vector or NULL
# - k: optional desired number of columns
# Behavior:
#   * If x is NULL → return NULL (propagate "no data")
#   * Ensure storage.mode == "double"
#   * If k is supplied:
#       - If input has 0 columns, return nrow(x) × k zero-matrix
#       - If ncol(x) > k, truncate to first k columns
#       - If ncol(x) < k, right-pad with zero columns to reach k
# This helper enforces a consistent numeric representation for geometric
# data while gracefully handling incomplete inputs.
as_num_mat <- function(x, k = NULL) {
  if (is.null(x)) return(NULL)
  m <- if (is.matrix(x)) x else as.matrix(x)
  storage.mode(m) <- "double"
  if (!is.null(k)) {
    if (!ncol(m)) return(matrix(0, nrow = nrow(m), ncol = k))
    if (ncol(m) > k) m <- m[, seq_len(k), drop = FALSE]
    if (ncol(m) < k) m <- cbind(m, matrix(0, nrow = nrow(m), ncol = k - ncol(m)))
  }
  m
}

# --- Geometry & math ----------------------------------------------------------
# cross_rows(): Row-wise 3D cross product for two equally-sized matrices.
# Requirements:
#   * A, B are matrices with at least 3 columns
#   * nrow(A) == nrow(B)
# Returns:
#   * matrix with 3 columns containing cross(A[i,1:3], B[i,1:3]) per row.
# This is a low-level helper for computing face normals and other
# geometric quantities in a vectorized manner.
cross_rows <- function(A, B) {
  stopifnot(is.matrix(A), is.matrix(B), ncol(A) >= 3, ncol(B) >= 3, nrow(A) == nrow(B))
  cbind(
    A[,2]*B[,3] - A[,3]*B[,2],
    A[,3]*B[,1] - A[,1]*B[,3],
    A[,1]*B[,2] - A[,2]*B[,1]
  )
}

# =============================================================================
# approximate_sphere(): parametric sphere → triangle mesh (0-based indices)
# -----------------------------------------------------------------------------
# Purpose:
#   Generate a UV/latitude-longitude tessellated sphere as vertex arrays
#   (positions, normals) and flattened triangle indices (0-based, CCW).
#
# Typical use in XRGL:
#   * To approximate rgl sphere primitives with explicit triangle meshes.
#   * To replace point primitives with small spheres to obtain consistent
#     apparent size across renderers.
#
# Parameters:
# - center: numeric(3) sphere center (x,y,z)
# - radius: numeric scalar, >= 0
# - n_slices: longitudinal divisions (θ around Z), >= 3
# - n_stacks: latitudinal divisions (φ pole→pole), >= 2
# - return_uv: if TRUE, include TEXCOORD_0 in [0,1]^2 with seam at θ=0
# - eps_area2: threshold on squared triangle area to cull degenerates
#
# Notes:
# - Vertices are arranged as a grid of (stacks+1) × (slices+1), with the
#   extra θ column to facilitate seam stitching.
# - Indices are 0-based (glTF-friendly) and CCW winding.
# - Normals are unit-length and safe even when radius == 0 (fallback +Z).
# - Degenerate triangles (near poles/tiny radius) are removed using area^2.
# - UV V increases north→south; U wraps [0,1] with seam at θ = 0.
approximate_sphere <- function(center,
                               radius,
                               n_slices = 16,
                               n_stacks = 16,
                               return_uv = FALSE,
                               eps_area2 = 1e-20) {
  # --- preflight --------------------------------------------------------------
  stopifnot(is.numeric(center), length(center) == 3)
  stopifnot(is.numeric(radius), radius >= 0)
  stopifnot(n_slices >= 3, n_stacks >= 2)
  
  center <- as.numeric(center)
  
  # Angles (φ: [0,π], θ: [0,2π]) — closed ring in θ for seam stitching.
  # φ corresponds to latitude-like sampling; θ corresponds to longitude.
  phi   <- seq(0, pi,   length.out = n_stacks + 1L)
  theta <- seq(0, 2*pi, length.out = n_slices + 1L)
  
  nx <- length(phi)    # stacks + 1
  ny <- length(theta)  # slices + 1 (wrap seam)
  
  # --- vertex positions & normals --------------------------------------------
  # Construct a unit sphere grid before scaling/translation:
  # (φ rows × θ cols); outer products for X/Y and replicated Z.
  sp <- sin(phi);  cp <- cos(phi)
  ct <- cos(theta); st <- sin(theta)
  
  # relative unit sphere grid (φ rows × θ cols)
  Xrel <- (sp %o% ct)
  Yrel <- (sp %o% st)
  Zrel <- (cp %o% rep(1, ny))
  
  # positions (world): scale by radius and translate by center
  positions <- cbind(
    as.vector(t(radius * Xrel)) + center[1],
    as.vector(t(radius * Yrel)) + center[2],
    as.vector(t(radius * Zrel)) + center[3]
  )
  
  # normals (unit), safe for radius == 0 (fallback +Z).
  # When radius > 0, normals coincide with the unscaled unit sphere.
  if (radius > 0) {
    normals <- cbind(
      as.vector(t(Xrel)),
      as.vector(t(Yrel)),
      as.vector(t(Zrel))
    )
  } else {
    normals <- matrix(c(0, 0, 1), nrow = nx * ny, ncol = 3, byrow = TRUE)
  }
  
  # --- triangle indices (0-based, CCW winding) -------------------------------
  # Flattened row-major indexing: idx(row, col) = row*ny + col
  idx <- function(row, col) row * ny + col
  
  # Two triangles per quad over the (nx-1) × (ny-1) cell grid, covering
  # the full sphere except for the duplicated seam column.
  tri <- vector("list", n_stacks * n_slices * 2L)
  m <- 1L
  for (row in 0:(nx - 2L)) {
    for (col in 0:(ny - 2L)) {
      v00 <- idx(row,        col)
      v01 <- idx(row,       (col + 1L))
      v10 <- idx(row + 1L,   col)
      v11 <- idx(row + 1L,  (col + 1L))
      # Two triangles per quad (v00,v10,v01) and (v10,v11,v01)
      tri[[m    ]] <- c(v00, v10, v01)
      tri[[m + 1]] <- c(v10, v11, v01)
      m <- m + 2L
    }
  }
  indices <- as.integer(unlist(tri, use.names = FALSE))
  
  # --- drop degenerates (near poles, tiny radii) ------------------------------
  # Remove triangles with near-zero area, which helps avoid numerical
  # issues in subsequent steps (e.g., normal computation).
  # Compute squared area via || (b-a) × (c-a) ||^2 and keep faces > eps_area2.
  if (length(indices)) {
    tri3 <- matrix(indices + 1L, ncol = 3, byrow = TRUE)           # 1-based for R indexing
    a <- positions[tri3[,1], , drop = FALSE]
    b <- positions[tri3[,2], , drop = FALSE]
    c <- positions[tri3[,3], , drop = FALSE]
    ab <- b - a; ac <- c - a
    # cross product per-face
    cp <- cbind(
      ab[,2]*ac[,3] - ab[,3]*ac[,2],
      ab[,3]*ac[,1] - ab[,1]*ac[,3],
      ab[,1]*ac[,2] - ab[,2]*ac[,1]
    )
    area2 <- rowSums(cp * cp)
    keep  <- is.finite(area2) & (area2 > eps_area2)
    if (!all(keep)) {
      indices <- as.integer(as.vector(t(tri3[keep, , drop = FALSE] - 1L)))
    }
  }
  
  out <- list(positions = positions, normals = normals, indices = indices)
  
  # --- optional UVs (seam at θ = 0; V increases north→south) -----------------
  # U = θ / (2π) mapped row-wise, V = φ / π mapped column-wise.
  # These UVs follow a standard spherical parameterization that most
  # tools can interpret.
  if (isTRUE(return_uv)) {
    U <- matrix(theta / (2*pi), nrow = nx, ncol = ny, byrow = TRUE)
    V <- matrix(phi   /  pi,    nrow = nx, ncol = ny, byrow = FALSE)
    out$uv <- cbind(as.vector(t(U)), as.vector(t(V)))
  }
  
  out
}

# --- Mesh cleanup & normals ---------------------------------------------------
# drop_degenerate(): Remove triangles with invalid indices or near-zero area.
# - pos: vertex positions (n×3)
# - ind: flattened int vector of 0-based triangle indices (3m elements)
# - eps_area2: area^2 threshold for culling
# Returns: filtered 0-based index vector, possibly length 0.
# This function is used throughout the pipeline to sanitize meshes before
# computing normals or exporting to glTF.
drop_degenerate <- function(pos, ind, eps_area2 = 1e-20) {
  # Return early if not enough indices to form a triangle
  if (length(ind) < 3) return(as.integer(ind))
  
  # Ensure pos is a numeric matrix with 3 columns
  pos <- as_num_mat(pos, 3)
  
  # Interpret indices as 0-based triples, convert to 1-based for R
  tri <- matrix(ind, ncol = 3, byrow = TRUE) + 1L
  
  # Guard against out-of-range indices
  tri <- tri[
    tri[,1] <= nrow(pos) & tri[,2] <= nrow(pos) & tri[,3] <= nrow(pos),
    , drop = FALSE
  ]
  if (!nrow(tri)) return(integer(0))
  
  # Gather triangle vertices
  a <- pos[tri[,1], , drop = FALSE]
  b <- pos[tri[,2], , drop = FALSE]
  c <- pos[tri[,3], , drop = FALSE]
  
  # Edges
  ab <- b - a
  ac <- c - a
  
  # Cross product per face.
  cp <- cbind(
    ab[,2] * ac[,3] - ab[,3] * ac[,2],
    ab[,3] * ac[,1] - ab[,1] * ac[,3],
    ab[,1] * ac[,2] - ab[,2] * ac[,1]
  )
  
  # Keep faces with finite area and area^2 above threshold
  keep <- is.finite(cp[,1]) & is.finite(cp[,2]) & is.finite(cp[,3]) &
    rowSums(cp * cp) > eps_area2
  if (!any(keep)) return(integer(0))
  
  # Return flattened 0-based indices for kept triangles
  as.integer(as.vector(t(tri[keep, , drop = FALSE] - 1L)))
}


# compute_vertex_normals(): Per-vertex normals via unnormalized face normal
# accumulation (angle-agnostic, area-weighted by default due to cross product).
# - pos: vertex positions (n×3)
# - ind: flattened 0-based triangle indices
# Returns:
#   * n×3 matrix of unit normals, or NULL if inputs are empty/invalid.
# Face normals are accumulated onto each incident vertex and then
# normalized, yielding smooth-shaded vertex normals suitable for
# most visualization tasks.
compute_vertex_normals <- function(pos, ind) {
  if (is.null(pos) || length(ind) < 3) return(NULL)
  pos <- as_num_mat(pos, 3)
  vcount <- nrow(pos); if (vcount == 0L) return(NULL)
  tri <- matrix(ind, ncol = 3, byrow = TRUE) + 1L
  tri <- tri[tri[,1] <= vcount & tri[,2] <= vcount & tri[,3] <= vcount, , drop = FALSE]
  if (!nrow(tri)) return(NULL)
  a <- pos[tri[,1], , drop = FALSE]
  b <- pos[tri[,2], , drop = FALSE]
  c <- pos[tri[,3], , drop = FALSE]
  fn <- cross_rows(b - a, c - a)           # face normals (unnormalized)
  nrm <- matrix(0, nrow = vcount, ncol = 3)
  # Accumulate face normals to incident vertices
  for (k in seq_len(nrow(tri))) {
    nrm[tri[k, 1], ] <- nrm[tri[k, 1], ] + fn[k, ]
    nrm[tri[k, 2], ] <- nrm[tri[k, 2], ] + fn[k, ]
    nrm[tri[k, 3], ] <- nrm[tri[k, 3], ] + fn[k, ]
  }
  # Normalize; avoid division by zero by clamping zero-length to 1
  lens <- sqrt(rowSums(nrm * nrm)); lens[lens == 0] <- 1
  nrm / lens
}

# --- Color handling (vertex RGBA in [0,1]) -----------------------------------
# color_mat(): Resolve color specification to an n×4 RGBA matrix.
# Supports:
#   * obj$colors as:
#       - single RGB(A) numeric vector length 3/4 → broadcast to n
#       - matrix with 3 or 4 cols → ensure 4 cols (alpha=1 if missing)
#       - numeric vector whose length is multiple of 3 or 4 → reshape
#       - R color names/hex → via grDevices::col2rgb()
#   * falls back to obj$material$color or obj$color with alpha from
#     obj$material$alpha or obj$alpha (default 1).
# Arguments:
#   - obj: list-like container (expects $colors or $material/$color/$alpha)
#   - n  : number of vertices to produce colors for
# The result is always an n×4 numeric matrix with values in [0,1].
color_mat <- function(obj, n) {
  if (n <= 0) return(NULL)
  
  # to_rgba_rows(): helper to coerce a color spec to rows of RGBA in [0,1]
  to_rgba_rows <- function(col, alpha = 1) {
    if (is.numeric(col)) {
      if (length(col) %in% c(3,4)) {
        rgb <- col[1:3]; a <- if (length(col) == 4) col[4] else alpha
        return(matrix(c(rgb, a), nrow = 1))
      }
      if (length(col) %% 3L == 0L || length(col) %% 4L == 0L) {
        k <- if (length(col) %% 4L == 0L) 4L else 3L
        M <- matrix(as.numeric(col), ncol = k, byrow = TRUE)
        if (k == 3L) M <- cbind(M, 1)
        return(M)
      }
    }
    rgb <- grDevices::col2rgb(col) / 255
    a   <- rep(alpha, ncol(rgb))
    t(rbind(rgb, a))
  }
  
  # Prefer explicit per-vertex colors if present
  if (!is.null(obj$colors)) {
    cols <- obj$colors
    if (!is.matrix(cols)) {
      cols <- to_rgba_rows(cols)
    } else {
      storage.mode(cols) <- "double"
      if (ncol(cols) == 3) cols <- cbind(cols, 1)
    }
    # Broadcast single row to n, or validate row count
    if (nrow(cols) == 1L && n > 1L) cols <- matrix(rep(cols, n), nrow = n, byrow = TRUE)
    if (nrow(cols) == n) {
      cols <- cols[, 1:4, drop = FALSE]
      cols[is.na(cols)] <- 1
      cols[, 1:3] <- pmax(0, pmin(1, cols[, 1:3]))
      cols[, 4]   <- pmax(0, pmin(1, cols[, 4]))
      return(cols)
    }
  }
  
  # Fallback to a single material/base color with alpha (broadcast to n).
  # This ensures that every vertex receives a valid RGBA entry even when
  # explicit per-vertex colors are absent.
  alpha_val <- obj$material$alpha %||% obj$alpha %||% 1
  base_col  <- obj$material$color %||% obj$color %||% "white"
  rgba1     <- to_rgba_rows(base_col, alpha = alpha_val)[1, ]
  matrix(rep(as.numeric(rgba1), n), nrow = n, byrow = TRUE)
}

# --- GLTF/GLB binary helpers --------------------------------------------------
# align4(): round n up to next multiple of 4 (including n when n %% 4 == 0).
# This is required by the GLB specification, which mandates 4-byte alignment
# for buffer views and chunks.
align4 <- function(n) { r <- n %% 4L; if (r == 0L) n else n + (4L - r) }

# pad4(): pad a raw vector to 4-byte alignment with padval (default 0x00).
# - x: raw vector
# - padval: single byte (0–255) used for padding
# Primarily useful when constructing GLB buffers manually.
pad4   <- function(x, padval = 0x00) {
  pad_len <- (4L - (length(x) %% 4L)) %% 4L
  if (pad_len > 0L) c(x, as.raw(rep(padval, pad_len))) else x
}

# --- Lines → ribbonized triangles --------------------------------------------
# triangulate_lines_to_quads(): Convert independent line segments to camera-
# facing ribbons (quads → 2 triangles) of fixed world-space width.
# Inputs:
#   - positions: n×3 vertices
#   - indices  : flattened pairs (0-based) for line segments
#   - width_world: target ribbon width in world units
#   - colors: optional per-vertex RGBA colors corresponding to positions
# Returns:
#   - list(positions = m×3, indices = int vector of 0-based triangles,
#          colors = m×4 or NULL)
# Notes:
#   - Uses a simple "up" heuristic to pick a side vector; switches if nearly
#     parallel to the segment to avoid degeneracy.
#   - When the indices form a contiguous chain, constructs a continuous
#     strip for improved visual coherence and fewer vertices.
#   - When not a chain, falls back to per-segment quads.
#   - Colors, if supplied, are propagated to the expanded vertex set.
triangulate_lines_to_quads <- function(positions, indices, width_world, colors = NULL) {
  if (length(indices) < 2) return(NULL)
  P <- as_num_mat(positions, 3)
  C <- if (!is.null(colors)) as_num_mat(colors, 4) else NULL
  halfw <- max(1e-9, width_world * 0.5)
  
  # nrm(): normalize rows of a matrix, avoiding division by zero
  nrm <- function(v) { L <- sqrt(rowSums(v*v)); L[L==0] <- 1; v/L }
  
  idx2 <- matrix(as.integer(indices), ncol = 2, byrow = TRUE)
  
  # Detect whether the line segments form a simple chain:
  # each segment's end equals the next segment's start, and indices increase
  # sequentially (i,i+1).
  is_chain <- nrow(idx2) >= 2 &&
    all(idx2[-1,1] == idx2[-nrow(idx2),2]) &&
    all(idx2[,2] - idx2[,1] == 1)
  
  if (is_chain) {
    # Chain case: treat the geometry as a polyline and construct a
    # continuous ribbon with smoothed tangents and consistent normals.
    start <- idx2[1,1] + 1L
    stopi <- idx2[nrow(idx2),2] + 1L
    chain_idx <- seq.int(start, stopi)
    V <- P[chain_idx, , drop = FALSE]
    Cv <- if (!is.null(C)) C[chain_idx, , drop = FALSE] else NULL
    if (nrow(V) < 2) return(NULL)
    
    # Segment tangents between consecutive vertices.
    Tseg <- nrm(V[-1, , drop = FALSE] - V[-nrow(V), , drop = FALSE])
    
    # Vertex tangents: copy first/last from segments, average in the interior.
    Tv   <- matrix(0, nrow = nrow(V), ncol = 3)
    Tv[1, ] <- Tseg[1, ]
    Tv[nrow(V), ] <- Tseg[nrow(Tseg), ]
    if (nrow(V) > 2) Tv[2:(nrow(V)-1), ] <- nrm(Tseg[-nrow(Tseg), ] + Tseg[-1, ])
    
    # Choose an "up" vector; if nearly parallel to the tangent, switch to X-axis.
    up <- matrix(rep(c(0,0,1), nrow(V)), ncol=3, byrow=TRUE)
    dotZ <- abs(rowSums(Tv * up))
    if (any(dotZ > 0.9)) up[dotZ > 0.9, ] <- rep(c(1,0,0), sum(dotZ > 0.9))
    
    # Side vectors (normals in the ribbon plane) from tangent × up.
    Nv <- nrm(cbind(
      Tv[,2]*up[,3] - Tv[,3]*up[,2],
      Tv[,3]*up[,1] - Tv[,1]*up[,3],
      Tv[,1]*up[,2] - Tv[,2]*up[,1]
    ))
    # Flip side vectors to maintain consistent orientation along the strip.
    if (nrow(Nv) > 1) for (i in 2:nrow(Nv)) if (sum(Nv[i,]*Nv[i-1,]) < 0) Nv[i,] <- -Nv[i,]
    
    # Compute left/right vertices offset by half the ribbon width.
    Lv <- V - Nv * halfw
    Rv <- V + Nv * halfw
    
    # Interleave left/right vertices into a single strip.
    strip <- matrix(0, nrow = 2*nrow(V), ncol = 3)
    strip[seq(1, nrow(strip), by=2), ] <- Lv
    strip[seq(2, nrow(strip), by=2), ] <- Rv
    
    # Triangles: form a strip of quads, each split into two triangles.
    tris <- integer(0)
    for (i in 0:(nrow(V)-2)) {
      b <- 2*i
      tris <- c(tris, b+0L, b+1L, b+2L,  b+1L, b+3L, b+2L)
    }
    
    # Propagate colors if provided: each original vertex color is duplicated
    # for its left and right ribbon copies.
    strip_cols <- NULL
    if (!is.null(Cv)) {
      strip_cols <- matrix(0, nrow = 2*nrow(Cv), ncol = 4)
      strip_cols[seq(1, nrow(strip_cols), by=2), ] <- Cv
      strip_cols[seq(2, nrow(strip_cols), by=2), ] <- Cv
    }
    
    return(list(
      positions = strip,
      indices   = as.integer(tris),
      colors    = strip_cols
    ))
  }
  
  # Fallback: per-segment quads, duplicating endpoint colors.
  # This mode is used when the index pattern does not form a simple chain.
  out_pos <- list(); out_idx <- list(); out_col <- list(); base <- 0L
  for (i in seq_len(nrow(idx2))) {
    a_i <- idx2[i,1] + 1L; b_i <- idx2[i,2] + 1L
    a <- P[a_i,]; b <- P[b_i,]
    d <- b - a; L <- sqrt(sum(d*d)); if (!is.finite(L) || L == 0) next
    d <- d / L
    up <- c(0,0,1); if (abs(sum(d*up)) > 0.9) up <- c(1,0,0)
    n <- c(d[2]*up[3]-d[3]*up[2], d[3]*up[1]-d[1]*up[3], d[1]*up[2]-d[2]*up[1])
    nL <- sqrt(sum(n*n)); if (nL == 0) next
    n <- (n / nL) * halfw
    
    p0 <- a - n; p1 <- a + n; p2 <- b + n; p3 <- b - n
    out_pos[[length(out_pos)+1]] <- rbind(p0,p1,p2,p3)
    out_idx[[length(out_idx)+1]] <- c(base+0L, base+1L, base+2L,  base+0L, base+2L, base+3L)
    
    if (!is.null(C)) {
      ca <- C[a_i, , drop = FALSE]; cb <- C[b_i, , drop = FALSE]
      out_col[[length(out_col)+1]] <- rbind(ca, ca, cb, cb)
    }
    
    base <- base + 4L
  }
  if (!length(out_pos)) return(NULL)
  list(
    positions = do.call(rbind, out_pos),
    indices   = as.integer(unlist(out_idx, use.names = FALSE)),
    colors    = if (length(out_col)) do.call(rbind, out_col) else NULL
  )
}


# --- Text label PNGs (for billboard quads) -----------------------------------
# .make_label_png(): Render text to a transparent PNG (raw bytes + dimensions).
# - text: single string (label)
# - dpi, pad_px: rendering resolution and padding
# - family, font: grid graphics font family / face (fontface default bold=2)
# - cex: font scale
# - color: RGBA in [0,1]
# Returns: list(raw=<raw PNG bytes>, w_px=<width>, h_px=<height>)
# This is used by XRGL to generate texture images for billboard text, which
# are then embedded directly into the GLB as PNG images.
.make_label_png <- function(text, dpi = 192, pad_px = 8, family = NULL, font = NULL, cex = 1, color = c(1,1,1,1)) {
  stopifnot(is.character(text), length(text) == 1)
  # Rough width/height heuristics based on character count and cex; these
  # are sufficient for label textures and avoid layout overhead.
  w <- max(64, round(12 * nchar(text) * cex) + 2*pad_px)
  h <- max(48, round(32 * cex) + 2*pad_px)
  tf <- tempfile(fileext = ".png"); on.exit(unlink(tf), add = TRUE)
  grDevices::png(tf, width = w, height = h, res = dpi, bg = NA)
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = rgb(0,0,0,0), col = NA))
  grid::grid.text(
    text,
    gp = grid::gpar(
      col = rgb(color[1], color[2], color[3], color[4]),
      cex = cex, fontface = font %||% 2,
      fontfamily = family %||% ""
    )
  )
  grDevices::dev.off()
  raw <- readBin(tf, "raw", file.info(tf)$size)
  list(raw = raw, w_px = w, h_px = h)
}

# --- Bounds & metadata (glTF accessor min/max) --------------------------------
# to_f32(): semantic alias; ensure numeric (double) for min/max computation.
# In practice, actual 32-bit float conversion occurs when writing binary
# data using writeBin(size = 4); here we simply coerce to numeric for
# convenience and consistency.
to_f32 <- function(x) as.numeric(x)  # sufficient for min/max computation

# bounds_f32_exact(): Compute component-wise min/max for a flat vec3 stream.
# - flat_vec3: c(x0,y0,z0, x1,y1,z1, ...)
# Returns: list(min = c(minX,minY,minZ), max = c(maxX,maxY,maxZ))
# This information populates POSITION accessors in glTF to accelerate
# viewer-side bounding volume computations.
bounds_f32_exact <- function(flat_vec3) {
  stopifnot(length(flat_vec3) %% 3 == 0)
  m <- matrix(flat_vec3, ncol = 3, byrow = TRUE)
  list(min = apply(m, 2, min), max = apply(m, 2, max))
}

# empty_obj(): Utility to create an empty named list (no fields).
# This is primarily useful when declaring empty extension objects in glTF
# (e.g., KHR_materials_unlit = {}).
empty_obj <- function() setNames(list(), character(0))
