import subprocess
import os

def copy_server(root: str):
    subprocess.call(["py", os.path.join(root, "gameengine-server", "copy-to-demo-cube.py")])

def build(server_dir: str, release_mode: bool):
    args = ["stack", "build", "--copy-bins", "--local-bin-path", "build"]
    if not release_mode:
        args.append("--fast")

    subprocess.call(args, cwd = server_dir)