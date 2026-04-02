#' Run the XRGL pipeline: rgl scene → GLB
#'
#' High-level convenience wrapper that executes the complete XRGL export
#' pipeline for an \code{rgl} scene—extracting geometry, converting it into a
#' standards-compliant glTF 2.0 structure, and writing a single-file GLB asset
#' to disk. This function provides a streamlined interface for users who do not
#' need granular control over intermediate stages of the pipeline.
#'
#' Internally, this wrapper performs three major steps:
#' \enumerate{
#'   \item Captures or accepts an existing \code{rglscene} object.
#'   \item Normalizes the scene using \code{xr_extract()}, converting rgl’s
#'         internal representation into XRGL’s canonical intermediate format.
#'   \item Generates a binary glTF (GLB) file using \code{xr_build_gltf()} and
#'         writes it to disk via \code{xr_write_glb()}.
#' }
#'
#' The result is a portable, viewer-neutral GLB file suitable for use in
#' immersive environments, desktop 3D viewers, game engines, web frameworks, and
#' XR platforms.
#'
#' @param scene An \code{rglscene} object, typically returned by
#'   \code{rgl::scene3d()}. If \code{NULL}, the currently active rgl device is
#'   queried using \code{rgl::scene3d()}. This behavior allows seamless capture
#'   of interactive or programmatically generated scenes.
#'
#' @param glb_dir Directory where the GLB file will be written. Defaults to the
#'   current working directory. The directory is normalized and need not exist
#'   beforehand, although typical write permissions must hold.
#'
#' @param glb_name Desired file name for the GLB output, including the
#'   \code{.glb} extension. This function does not modify the name, so callers
#'   should ensure the extension is appropriate.
#'
#' @param overwrite Logical; if \code{FALSE} (default) and a file already exists
#'   at the computed output path, the function aborts with an informative error.
#'   If \code{TRUE}, the existing file is replaced. This safety check helps
#'   prevent accidental overwrites during iterative development.
#'
#' @return Invisibly returns the normalized absolute path of the written GLB
#'   file. Returning the path invisibly makes this function suitable both for
#'   command-style use and for programmatic pipelines where the resulting file
#'   path may be captured explicitly.
#'
#' @examples
#' \dontrun{
#'   # Build a simple scene
#'   rgl::open3d()
#'   rgl::shade3d(rgl::cube3d(), color = "tomato")
#'
#'   # Capture current scene and export to GLB
#'   scene <- rgl::scene3d()
#'   xr_export(scene,
#'             glb_dir  = getwd(),
#'             glb_name = "cube.glb",
#'             overwrite = TRUE)
#' }
#'
#' @export
xr_export <- function(scene = NULL,
                      glb_dir  = getwd(),
                      glb_name = "xrgl_export.glb",
                      overwrite = FALSE) {
  
  # If no scene is supplied, attempt to capture from the active rgl device.
  # This requires the 'rgl' package to be installed; otherwise, the function
  # stops early with an informative diagnostic.
  if (is.null(scene)) {
    if (!requireNamespace("rgl", quietly = TRUE)) {
      stop("Package 'rgl' is required to capture the active scene.", call. = FALSE)
    }
    scene <- rgl::scene3d()
  }
  
  # Normalize the output directory path and construct the target file path.
  # Using normalizePath here ensures consistent handling of absolute/relative
  # paths across platforms.
  glb_dir  <- normalizePath(glb_dir, mustWork = FALSE)
  glb_path <- file.path(glb_dir, glb_name)
  
  # Guard against overwriting existing files unless explicitly permitted.
  if (!overwrite && file.exists(glb_path)) {
    stop("Output file already exists: ", glb_path,
         " (set overwrite = TRUE to replace it)")
  }
  
  # --- Core XRGL pipeline -----------------------------------------------------
  # 1. Convert rgl scene to XRGL's intermediate representation.
  # 2. Build glTF 2.0 JSON + binary buffer.
  # 3. Write GLB to disk.
  extracted <- xr_extract(scene)
  gltf_data <- xr_build_gltf(extracted)
  xr_write_glb(glb_path, gltf_data)
  
  # Return the absolute normalized path invisibly to support pipeline usage.
  invisible(normalizePath(glb_path, mustWork = TRUE))
}
