#!/usr/bin/python
# -*- coding: utf-8 -*-

import os
import json
import sys
import shutil
import fnmatch

print("Packaging...")

#get package config
with open(os.path.join(sys.path[0], "packages.json"), "r") as file:
    packages_config = json.load(file)

#get solution directory
solution_dir = os.path.abspath(os.path.join(sys.path[0], '..'))

#make output folder
if os.path.exists(os.path.join(sys.path[0], "packages")):
    shutil.rmtree(os.path.join(sys.path[0], "packages"))
os.mkdir(os.path.join(sys.path[0], "packages"))

#make package sources folder
if os.path.exists(os.path.join(sys.path[0], "package sources")):
    shutil.rmtree(os.path.join(sys.path[0], "package sources"))
os.mkdir(os.path.join(sys.path[0], "package sources"))

def copy_file(source_path, dest_path):
    if "*" in source_path:
        for match_name in fnmatch.filter(os.listdir(os.path.dirname(source_path)), os.path.basename(source_path)):
            source_full_path = os.path.join(os.path.dirname(source_path), match_name)
            destination_full_path = os.path.join(os.path.dirname(dest_path), match_name)

            if os.path.isdir(source_path):
                shutil.copytree(source_full_path, destination_full_path)
            else:
                shutil.copy(source_full_path, destination_full_path)

    else:
        if os.path.isdir(source_path):
            shutil.copytree(source_path, dest_path)
        else:
            shutil.copy(source_path, dest_path)

#interpret package config
for project_name in packages_config["projects"]:
    for build_type in packages_config["projects"][project_name]:
        build_name = "{}-{}".format(project_name, build_type)
        print("Collecting " + build_name + "...", end = "")

        build_config = packages_config["projects"][project_name][build_type]
        package_path = os.path.join(sys.path[0], "package sources", build_name)
        
        os.mkdir(package_path)

        try:
            for include_source in build_config["include"]:
                copy_file(os.path.join(solution_dir, include_source), os.path.join(package_path, build_config["include"][include_source]))

            print(" done")

        except FileNotFoundError:
            print(" failed (one or more files not found, has this package been built?)")

        print("Archiving " + build_name + "...", end = "")
        shutil.make_archive(package_path, 'zip', os.path.join(sys.path[0], "package sources", build_name))
        shutil.move(os.path.join(sys.path[0], "package sources", build_name + ".zip"), os.path.join(sys.path[0], "packages", build_name + ".zip"))
        print(" done")

print("Done")

print("Fetching additional release files...")

for additional_source_file_name in packages_config["additional files"]:
    print("Collecting " + additional_source_file_name + "...", end = "")
    copy_file(os.path.join(solution_dir, additional_source_file_name), os.path.join(sys.path[0], "packages", packages_config["additional files"][additional_source_file_name]))
    print("done")

print("Done")