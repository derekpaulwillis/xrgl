#' Launch a PowerShell upload script for HoloLens (Windows only)
#'
#' Opens a visible PowerShell window that runs a user-supplied script to
#' transfer the given GLB file to a connected HoloLens device.
#'
#' This helper is intentionally conservative and only works on Windows.
#' It does not block the R session.
#'
#' The typical workflow is:
#' \enumerate{
#'   \item Produce a GLB file locally using \code{xr_export()} or a similar
#'         function.
#'   \item Provide the GLB path and a custom PowerShell script that knows how
#'         to communicate with your HoloLens (e.g., via Device Portal or MTP).
#'   \item Allow this helper to open a PowerShell window with those arguments,
#'         leaving the user to monitor any prompts, progress, or errors.
#' }
#'
#' By design, this function:
#' \itemize{
#'   \item Only runs on Windows (checked via \code{.Platform$OS.type}).
#'   \item Normalizes file paths to Windows-style separators.
#'   \item Does not wait for the PowerShell process to complete, so the R
#'         session remains interactive.
#'   \item Delegates all actual upload logic to the user-provided script.
#' }
#'
#' @param glb_path Path to the local GLB file to upload. The file is checked
#'   for existence before launching PowerShell.
#'
#' @param script_path Path to a PowerShell script (e.g.
#'   \code{"upload_to_hololens.ps1"}) that knows how to push the file
#'   to your device. If not specified, this defaults to
#'   \code{"upload_to_hololens.ps1"} in the current working directory.
#'
#' @return Invisibly returns \code{glb_path}, normalized to use Windows
#'   path separators and suitable for logging or downstream use.
#'
#' @export
xr_upload_to_hololens <- function(
    glb_path,
    script_path = file.path(getwd(), "upload_to_hololens.ps1")
) {
  message("---- XRGL → HoloLens upload via PowerShell (visible, manual close) ----")
  
  # This helper is Windows-specific. We fail fast on non-Windows platforms to
  # avoid ambiguous behavior and platform-dependent failure modes.
  if (.Platform$OS.type != "windows") {
    stop("xr_upload_to_hololens() only works on Windows.")
  }
  
  # Normalize paths to Windows conventions (backslashes) and allow non-existent
  # script paths at this stage; existence is checked immediately below.
  glb_path    <- normalizePath(glb_path, winslash = "\\", mustWork = FALSE)
  script_path <- normalizePath(script_path, winslash = "\\", mustWork = FALSE)
  
  # Validate that both the GLB file and the PowerShell script exist before
  # attempting to launch a process. Errors are explicit and informative.
  if (!file.exists(glb_path)) {
    stop("GLB file not found: ", glb_path)
  }
  if (!file.exists(script_path)) {
    stop("PowerShell script not found: ", script_path)
  }
  
  message("GLB file  : ", glb_path)
  message("PS script : ", script_path)
  
  # Construct a command line that:
  # - Uses 'cmd.exe /c start' to open a new, visible window.
  # - Invokes PowerShell with:
  #     -NoExit      : leaves the window open after the script finishes.
  #     -NoProfile   : avoids loading user profiles for reproducibility.
  #     -ExecutionPolicy Bypass : allows running unsigned scripts.
  #     -File        : points to the upload script.
  #   The script is passed a '-localFile' argument containing the GLB path.
  #
  # The surrounding R process does not wait on this command; the user can
  # continue working while the upload script runs in the background.
  cmd <- sprintf(
    'cmd.exe /c start "" powershell -NoExit -NoProfile -ExecutionPolicy Bypass -File "%s" -localFile "%s"',
    script_path, glb_path
  )
  
  message("Launching PowerShell window (R will NOT wait)...")
  system(cmd, wait = FALSE, invisible = FALSE)
  
  # Return the GLB path invisibly, allowing callers to log or reuse it if needed.
  invisible(glb_path)
}

#' Close all running PowerShell windows (best-effort, Windows only)
#'
#' Convenience helper that sends \code{taskkill} to any running
#' \code{powershell.exe} processes. Errors are silently ignored.
#'
#' This function is intended as a pragmatic, best-effort cleanup mechanism
#' during development or testing. It is not a fine-grained process manager:
#' it attempts to terminate *all* \code{powershell.exe} processes owned by
#' the current user (and their child processes), which may include PowerShell
#' sessions unrelated to XRGL.
#'
#' Use with caution in multi-purpose environments, and avoid calling it from
#' scripts where other PowerShell tasks may be running concurrently.
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @export
xr_close_powershell_windows <- function() {
  message("Attempting to close all powershell.exe windows...")
  
  # /F = force termination
  # /T = terminate child processes along with the parent
  cmd <- 'taskkill /IM powershell.exe /F /T'
  
  # Invoke the system command without blocking the R session. Any errors
  # (such as when no powershell.exe processes are running, or when
  # taskkill is unavailable) are caught and ignored via try(..., silent=TRUE).
  invisible(try(system(cmd, wait = FALSE, invisible = TRUE), silent = TRUE))
}
