import os
import shutil
import sys


server_dir_name = "server"

demo_cube_resources_dir = os.path.abspath(os.path.join(sys.path[0], '..', "demo-cube", "resources"))

destination = os.path.join(demo_cube_resources_dir, server_dir_name)

if os.path.isdir(destination):
    shutil.rmtree(destination)
os.mkdir(destination)

shutil.copytree(os.path.join(sys.path[0], "resources"), os.path.join(destination, "resources"))
shutil.copy(os.path.join(sys.path[0], "build", "gameengine-server.exe"), os.path.join(destination, "gameengine-server.exe"))