import os
import sys
import subprocess

import post_build

server_dir = os.path.abspath(os.path.join(sys.path[0], "..", "..", "gameengine-server"))
post_build.build(server_dir, True)

post_build.copy_server(os.path.abspath(os.path.join(sys.path[0], "..", "..")))

subprocess.call(["py", os.path.abspath(os.path.join(sys.path[0], "..", "..", "packaged-releases/package-releases.py"))])