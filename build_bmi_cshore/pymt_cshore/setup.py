#! /usr/bin/env python
import contextlib
import os
import subprocess
import sys

import numpy as np
from numpy.distutils.fcompiler import new_fcompiler
from setuptools import Extension, find_packages, setup
from setuptools.command.build_ext import build_ext as _build_ext

common_flags = {
    "include_dirs": [
        np.get_include(),
        os.path.join(sys.prefix, "include"),
    ],
    "library_dirs": [],
    "define_macros": [],
    "undef_macros": [],
    "extra_compile_args": [],
    "language": "c",
}

libraries = []

# Locate directories under Windows %LIBRARY_PREFIX%.
if sys.platform.startswith("win"):
    common_flags["include_dirs"].append(os.path.join(sys.prefix, "Library", "include"))
    common_flags["library_dirs"].append(os.path.join(sys.prefix, "Library", "lib"))

ext_modules = [
    Extension(
        "pymt_cshore.lib.cshoremodel",
        ["pymt_cshore/lib/cshoremodel.pyx"],
        libraries=libraries + ["bmi_cshore_module"],
        extra_objects=["pymt_cshore/lib/bmi_interoperability.o"],
        **common_flags
    ),
]

entry_points = {
    "pymt.plugins": [
        "cshoremodel=pymt_cshore.bmi:cshoremodel",
    ]
}


@contextlib.contextmanager
def as_cwd(path):
    prev_cwd = os.getcwd()
    os.chdir(path)
    yield
    os.chdir(prev_cwd)


def build_interoperability():
    compiler = new_fcompiler()
    compiler.customize()

    cmd = []
    cmd.append(compiler.compiler_f90[0])
    cmd.append(compiler.compile_switch)
    if sys.platform.startswith("win") is False:
        cmd.append("-fPIC")
    for include_dir in common_flags["include_dirs"]:
        if os.path.isabs(include_dir) is False:
            include_dir = os.path.join(sys.prefix, "include", include_dir)
        cmd.append("-I{}".format(include_dir))
    cmd.append("bmi_interoperability.f90")

    try:
        subprocess.check_call(cmd)
    except subprocess.CalledProcessError:
        raise


class build_ext(_build_ext):
    def run(self):
        with as_cwd("pymt_cshore/lib"):
            build_interoperability()
        _build_ext.run(self)


def read(filename):
    with open(filename, "r", encoding="utf-8") as fp:
        return fp.read()


long_description = u"\n\n".join(
    [read("README.rst"), read("CREDITS.rst"), read("CHANGES.rst")]
)


setup(
    name="pymt_cshore",
    author="csdms",
    author_email="csdms@colorado.edu",
    description="PyMT plugin for pymt_cshore",
    long_description=long_description,
    version="0.1",
    url="https://github.com/pymt-lab/pymt_cshore",
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Science/Research",
        "License :: OSI Approved :: MIT License",
        "Operating System :: MacOS :: MacOS X",
        "Operating System :: POSIX :: Linux",
        "Programming Language :: Python :: 3 :: Only",
        "Programming Language :: Python :: 3.8",
    ],
    keywords=["bmi", "pymt"],
    install_requires=open("requirements.txt", "r").read().splitlines(),
    setup_requires=["cython"],
    ext_modules=ext_modules,
    cmdclass=dict(build_ext=build_ext),
    packages=find_packages(),
    entry_points=entry_points,
    include_package_data=True,
)
