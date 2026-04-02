#' Write a GLB file (glTF + binary) to disk
#'
#' Serializes a glTF 2.0 JSON document and its associated binary buffer into a
#' single GLB 2.0 file, consisting of a 12-byte header followed by a JSON
#' chunk and a BIN chunk.
#'
#' Conceptually, this function performs the final “packaging” stage of the
#' XRGL pipeline:
#' \enumerate{
#'   \item It takes an in-memory representation of a glTF asset
#'         (JSON + binary buffer).
#'   \item It ensures that buffer metadata (e.g., \code{buffers},
#'         \code{bufferViews}) is consistent with the binary payload.
#'   \item It encodes the JSON as UTF-8, pads both JSON and BIN chunks to
#'         4-byte boundaries, and builds a GLB-compliant header.
#'   \item It writes the combined raw bytes to disk in the GLB 2.0 format.
#' }
#'
#' The resulting file is directly consumable by glTF/GLB-aware tools
#' (e.g., 3D Viewer, Blender, game engines, and XR runtimes) and preserves
#' all numeric precision and structural information supplied by the earlier
#' stages of the XRGL export pipeline.
#'
#' @param filename Output path for the GLB file. If the file name does not
#'   end in \code{".glb"} (case-insensitive), the extension is appended
#'   automatically. Any required parent directories are created if they do
#'   not already exist.
#'
#' @param gltf_data A list containing the glTF JSON and binary buffer,
#'   typically as returned by \code{xr_build_gltf()}. The list is expected
#'   to contain at least:
#'   \itemize{
#'     \item \code{$gltf_json}: a named list representing the glTF JSON
#'           document (scene graph, meshes, accessors, bufferViews, etc.).
#'     \item \code{$gltf_raw}: a single raw vector containing concatenated
#'           binary data for all buffers.
#'   }
#'
#' @param debug Logical; if \code{TRUE}, perform basic consistency checks
#'   (buffer lengths, alignment) and emit informational messages (e.g., chunk
#'   sizes, path being written). This is useful when diagnosing issues with
#'   custom or experimental glTF content.
#'
#' @return Invisibly returns the normalized output path to the written GLB
#'   file, allowing callers to use \code{xr_write_glb()} in pipelines without
#'   cluttering the console.
#'
#' @export
xr_write_glb <- function(filename, gltf_data, debug = TRUE) {
  if (debug) message("Entering write_glb()...")
  
  # --- filename ---------------------------------------------------------------
  # Ensure .glb extension; keep the caller's directory component intact.
  if (!grepl("\\.glb$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".glb")
  }
  
  # Normalize path:
  #   - winslash='/' gives consistent path representation on Windows.
  #   - mustWork=FALSE so we can create the path if it doesn't exist yet.
  file_path <- normalizePath(filename, winslash = "/", mustWork = FALSE)
  
  # Ensure parent directory exists (recursive=TRUE is safe if it already exists).
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  
  # Defensive overwrite handling:
  #   If the file already exists, try to remove it first. This helps avoid
  #   "Permission denied" when we know we want to overwrite. If it still exists
  #   after unlink(), assume it is locked (e.g., open in 3D Viewer / Blender).
  if (file.exists(file_path)) {
    if (debug) message("  Existing file found; attempting to remove: ", file_path)
    try(unlink(file_path), silent = TRUE)
    if (file.exists(file_path)) {
      stop(
        "Cannot overwrite existing file (it may be open in another program): ",
        file_path
      )
    }
  }
  
  # --- inputs -----------------------------------------------------------------
  # Validate input structure and coerce $gltf_raw to raw if needed.
  if (!is.list(gltf_data) || is.null(gltf_data$gltf_json) || is.null(gltf_data$gltf_raw)) {
    stop("'gltf_data' must be a list with 'gltf_json' and 'gltf_raw'")
  }
  gltf_json_list <- gltf_data$gltf_json
  if (!is.list(gltf_json_list)) {
    stop("'gltf_data$gltf_json' must be a list")
  }
  
  gltf_raw <- gltf_data$gltf_raw
  if (!is.raw(gltf_raw)) {
    gltf_raw <- as.raw(gltf_raw)
  }
  
  # --- pad BIN, set buffers ---------------------------------------------------
  # Pad BIN payload to 4 bytes with 0x00 per GLB spec. If no BIN data is present,
  # use an empty raw vector. Padding ensures that the binary chunk length
  # is a multiple of 4, as required by the specification.
  gltf_bin_padded <- if (length(gltf_raw)) pad4(gltf_raw, padval = 0x00) else as.raw(integer())
  
  # Ensure a single internal buffer[0] with correct byteLength and no URI.
  # GLB embeds binaries directly; external URIs are not used.
  if (is.null(gltf_json_list$buffers) || length(gltf_json_list$buffers) < 1L) {
    gltf_json_list$buffers <- list(
      list(byteLength = as.integer(length(gltf_bin_padded)), uri = NULL)
    )
  } else {
    gltf_json_list$buffers[[1]]$byteLength <- as.integer(length(gltf_bin_padded))
    gltf_json_list$buffers[[1]]$uri        <- NULL
  }
  
  # Force all bufferViews to reference buffer 0 explicitly (defensive).
  # This avoids issues where upstream code may have used different buffer
  # indices; GLB files produced here always use a single buffer (index 0).
  if (!is.null(gltf_json_list$bufferViews)) {
    for (i in seq_along(gltf_json_list$bufferViews)) {
      gltf_json_list$bufferViews[[i]]$buffer <- 0L
    }
  }
  
  # --- asset/scenes defaults --------------------------------------------------
  # Guarantee asset.version/generator; ensure there is at least one scene set.
  # These fields improve interoperability and debugging by making the origin
  # and version of the asset explicit.
  gltf_json_list$asset <- gltf_json_list$asset %||% list()
  if (is.null(gltf_json_list$asset$version)) {
    gltf_json_list$asset$version <- "2.0"
  }
  if (is.null(gltf_json_list$asset$generator)) {
    gltf_json_list$asset$generator <- "xrgl-exporter (R)"
  }
  
  # Ensure there is at least one scene and a default active scene index.
  # If scenes are missing, we synthesize a single scene that references
  # all defined nodes.
  if (is.null(gltf_json_list$scenes) || length(gltf_json_list$scenes) == 0L) {
    node_count <- if (!is.null(gltf_json_list$nodes)) length(gltf_json_list$nodes) else 0L
    nodes_all  <- if (node_count > 0L) as.list(seq(0L, node_count - 1L)) else list()
    gltf_json_list$scenes <- list(list(nodes = nodes_all))
    gltf_json_list$scene  <- 0L
  } else if (is.null(gltf_json_list$scene)) {
    gltf_json_list$scene <- 0L
  }
  
  # --- sanity for bufferViews -------------------------------------------------
  # Optional integrity checks: bounds and 4-byte alignment warnings.
  # These checks do not mutate the structure but can highlight upstream
  # mistakes in bufferView construction.
  bin_len <- length(gltf_bin_padded)
  if (!is.null(gltf_json_list$bufferViews)) {
    bv       <- gltf_json_list$bufferViews
    bad      <- logical(length(bv))
    misalign <- logical(length(bv))
    
    for (i in seq_along(bv)) {
      bvi <- bv[[i]]
      off <- as.integer(bvi$byteOffset %||% 0L)
      len <- as.integer(bvi$byteLength %||% 0L)
      
      # Bounds check: offset/length should not exceed BIN payload length.
      if (off < 0L || len < 0L || (off + len) > bin_len) {
        bad[i] <- TRUE
      }
      # Alignment check: many consumers assume or require 4-byte alignment.
      if ((off %% 4L) != 0L) {
        misalign[i] <- TRUE
      }
    }
    
    if (any(bad) && debug) {
      idx <- which(bad)
      message(
        "  [warn] ", length(idx), " bufferView(s) exceed BIN length: ",
        paste(idx - 1L, collapse = ", ")
      )
    }
    if (any(misalign) && debug) {
      idx <- which(misalign)
      message(
        "  [warn] ", length(idx), " bufferView(s) not 4-byte aligned: ",
        paste(idx - 1L, collapse = ", ")
      )
    }
  }
  
  # --- JSON encode (UTF-8, padded to 4 bytes with spaces) ---------------------
  # Create compact UTF-8 JSON; pad to a 4-byte boundary with ASCII spaces
  # (0x20), as required by the GLB chunk alignment rules.
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required.")
  }
  gltf_json_string <- jsonlite::toJSON(
    gltf_json_list,
    auto_unbox = TRUE,
    pretty     = FALSE,
    digits     = NA
  )
  gltf_json_bytes        <- charToRaw(enc2utf8(gltf_json_string))
  gltf_json_bytes_padded <- pad4(gltf_json_bytes, padval = 0x20)  # space padding
  
  # --- GLB header + chunks ----------------------------------------------------
  # Build GLB v2 header and chunk headers in little-endian per spec.
  # The layout is:
  #   - magic  (4 bytes)  : "glTF"
  #   - version(4 bytes)  : 2
  #   - length (4 bytes)  : total length of the GLB file in bytes
  #   - JSON chunk:
  #       * chunkLength (4 bytes, u32 LE)
  #       * chunkType   (4 bytes, "JSON")
  #       * chunkData   (padded JSON bytes)
  #   - BIN chunk:
  #       * chunkLength (4 bytes, u32 LE)
  #       * chunkType   (4 bytes, "BIN\0")
  #       * chunkData   (padded BIN bytes)
  u32le <- function(x) writeBin(as.integer(x), raw(), size = 4, endian = "little")
  
  glb_magic   <- charToRaw("glTF")  # 0x67 0x6C 0x54 0x46
  glb_version <- u32le(2L)
  
  json_len_le <- u32le(length(gltf_json_bytes_padded))
  json_type   <- as.raw(c(0x4A, 0x53, 0x4F, 0x4E))  # "JSON"
  
  bin_len_le  <- u32le(length(gltf_bin_padded))
  bin_type    <- as.raw(c(0x42, 0x49, 0x4E, 0x00))  # "BIN\0"
  
  total_len  <- 12L +
    (8L + length(gltf_json_bytes_padded)) +
    (8L + length(gltf_bin_padded))
  glb_length <- u32le(total_len)
  
  # Concatenate header + JSON chunk + BIN chunk into final raw vector.
  glb_raw <- c(
    glb_magic, glb_version, glb_length,
    json_len_le, json_type, gltf_json_bytes_padded,
    bin_len_le,  bin_type,  gltf_bin_padded
  )
  
  # --- write ------------------------------------------------------------------
  # Persist to disk; print sizes for quick inspection when debug=TRUE.
  if (debug) {
    message("  Writing .glb to: ", file_path)
    message("  JSON chunk bytes: ", length(gltf_json_bytes_padded))
    message("  BIN  chunk bytes: ", length(gltf_bin_padded))
    message("  Total file size : ", length(glb_raw), " bytes.")
  }
  
  # Open a binary connection and write the raw GLB payload.
  # on.exit() ensures the connection is closed even if writeBin() errors.
  con <- file(file_path, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(glb_raw, con)
  
  if (debug) message("Exiting write_glb().")
  invisible(file_path)
}
