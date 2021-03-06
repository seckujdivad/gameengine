﻿#!/usr/bin/python
# -*- coding: utf-8 -*-

#to run in visual studio, press [Shift]+[Alt]+[F5]

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

def apply_to_all(path, func):
    if "*" in path:
        for match_name in fnmatch.filter(os.listdir(os.path.dirname(path)), os.path.basename(path)):
            func(os.path.basename(match_name))
    else:
        func(os.path.basename(path))

def copy_file(source_path, dest_path):
    def inner_func(basename):
        source_full_path = os.path.join(os.path.dirname(source_path), basename)
        destination_full_path = os.path.join(os.path.dirname(dest_path), basename)

        if os.path.isdir(source_full_path):
            shutil.copytree(source_full_path, destination_full_path)
        else:
            shutil.copy(source_full_path, destination_full_path)

    apply_to_all(source_path, inner_func)

def remove_file(path):
    def inner_func(basename):
        to_remove_path = os.path.join(os.path.dirname(path), basename)

        if os.path.isdir(to_remove_path):
            shutil.rmtree(to_remove_path)
        elif os.path.isfile(to_remove_path):
            os.remove(to_remove_path)

    apply_to_all(path, inner_func)

#interpret package config
for project_name in packages_config["projects"]:
    for type_suffix in packages_config["projects"][project_name]:
        if type_suffix == "":
            build_name = project_name
        else:
            build_name = "{}-{}".format(project_name, type_suffix)

        #collect included files
        print("Collecting " + build_name + "...", end = "")

        build_config = packages_config["projects"][project_name][type_suffix]
        package_path = os.path.join(sys.path[0], "package sources", build_name)
        
        os.mkdir(package_path)

        try:
            for include_source in build_config["include"]:
                copy_file(os.path.join(solution_dir, include_source), os.path.join(package_path, build_config["include"][include_source]))

            print(" done")

        except FileNotFoundError:
            print(" failed (one or more files not found, has this package been built?)")
        
        #remove excluded files that have been included
        if "remove" in build_config:
            print("Removing excluded files...", end = "")
            for to_remove in build_config["remove"]:
                remove_file(os.path.join(package_path, to_remove))
            print(" done")
        else:
            print("No excluded files provided at \"remove\"")

        #package the collected files into a zip file
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