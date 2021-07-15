Push-Location ".\packaged-releases\package sources\demo-cube-win-x86\"

apitrace trace --api gl demo-cube.exe --output ".\..\..\x86.trace"

Pop-Location