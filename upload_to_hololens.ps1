param(
    [string]$localFile
)

Write-Host "PS: HoloLens upload script starting..."
Write-Host "PS: Local file parameter = $localFile"

if (-not $localFile -or $localFile.Trim() -eq "") {
    Write-Error "No -localFile parameter provided."
    return 1
}

if (-not (Test-Path $localFile)) {
    Write-Error "Local file not found: $localFile"
    return 1
}

# Normalize full path
$localFile = (Resolve-Path $localFile).ProviderPath
$fileName  = [System.IO.Path]::GetFileName($localFile)

Write-Host "PS: Using source file: $localFile"
Write-Host "PS: File name on device: $fileName"

# --- Use Shell COM to talk to MTP device (HoloLens) ---
$shell = New-Object -ComObject Shell.Application

# 17 = "This PC" namespace
$computer = $shell.Namespace(17)
if (-not $computer) {
    Write-Error "Could not open Shell namespace 17 (This PC)."
    return 1
}

# Find the HoloLens device under "This PC" (e.g. HOLOLENS-SU0D76)
$holoItem = $computer.Items() | Where-Object { $_.Name -like "HOLOLENS*" }

if (-not $holoItem) {
    Write-Error "Could not find a HoloLens device under 'This PC'. Is it plugged in and unlocked?"
    return 1
}

$holoName   = $holoItem.Name
$holoFolder = $holoItem.GetFolder()
if (-not $holoFolder) {
    Write-Error "Could not get HoloLens root folder."
    return 1
}
Write-Host "PS: Found HoloLens device: $holoName"

# Go into "Internal Storage"
$internalItem = $holoFolder.ParseName("Internal Storage")
if (-not $internalItem) {
    Write-Error "Could not find 'Internal Storage' on the HoloLens."
    return 1
}
$internalFolder = $internalItem.GetFolder()
if (-not $internalFolder) {
    Write-Error "Could not get 'Internal Storage' folder object."
    return 1
}
Write-Host "PS: Entered: $holoName -> Internal Storage"

# Go into "3D Objects"
$objectsItem = $internalFolder.ParseName("3D Objects")
if (-not $objectsItem) {
    Write-Error "Could not find '3D Objects' in Internal Storage."
    return 1
}
$objectsFolder = $objectsItem.GetFolder()
if (-not $objectsFolder) {
    Write-Error "Could not get '3D Objects' folder object."
    return 1
}
Write-Host "PS: Entered: $holoName -> Internal Storage -> 3D Objects"

# --- Copy the file (Windows may show Copy/Replace UI) ---------------------

Write-Host "PS: Copying $localFile to HoloLens: Internal Storage\3D Objects ..."
Write-Host "PS: If prompted, choose 'Copy and Replace' to overwrite the old model."

$objectsFolder.CopyHere($localFile)

Write-Host "PS: CopyHere() call returned."
Write-Host "PS: Upload script finished."
return 0
