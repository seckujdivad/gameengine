stack build

if ($LASTEXITCODE -eq 0) #only run if the build completes
{
    .\\build\\gameengine-server.exe --cwd app-root
}

exit $LASTEXITCODE #pass the exit code through