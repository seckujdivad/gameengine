import subprocess
import os

def build(server_dir: str, release_mode: bool):
    args = ["stack", "build", "--copy-bins", "--local-bin-path", "build"]
    if not release_mode:
        args.append("--fast")

    subprocess.call(args, cwd = server_dir)