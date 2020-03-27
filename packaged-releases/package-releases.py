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

#interpret package config
for project_name in packages_config["projects"]:
    for build_type in packages_config["projects"][project_name]:
        build_name = "{}-{}".format(project_name, build_type)
        print("Collecting " + build_name + "...")

        build_config = packages_config["projects"][project_name][build_type]
        package_path = os.path.join(sys.path[0], "packages", build_name)
        
        os.mkdir(package_path)

        for include_source in build_config["include"]:
            include_destination = build_config["include"][include_source]

            include_source_full_path = os.path.join(solution_dir, include_source)
            include_destination_full_path = os.path.join(package_path, include_destination)

            if "*" in include_source_full_path:
                for match_name in fnmatch.filter(os.listdir(os.path.dirname(include_source_full_path)), os.path.basename(include_source_full_path)):
                    source_full_path = os.path.join(os.path.dirname(include_source_full_path), match_name)
                    destination_full_path = os.path.join(os.path.dirname(include_destination_full_path), match_name)

                    if os.path.isdir(include_source_full_path):
                        shutil.copytree(source_full_path, destination_full_path)
                    else:
                        shutil.copy(source_full_path, destination_full_path)

            else:
                if os.path.isdir(include_source_full_path):
                    shutil.copytree(include_source_full_path, include_destination_full_path)
                else:
                    shutil.copy(include_source_full_path, include_destination_full_path)

        print("Archiving " + build_name + "...")
        shutil.make_archive(package_path, 'zip', package_path)

print("Done")