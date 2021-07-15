Push-Location ".\packaged-releases\package sources\demo-cube-win-x64\"

apitrace trace --api gl demo-cube.exe --output ".\..\..\x64.trace"

Pop-Location