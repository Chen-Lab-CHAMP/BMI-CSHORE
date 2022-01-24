#! /usr/bin/env python
import pkg_resources

__version__ = pkg_resources.get_distribution("pymt_cshore").version


from .bmi import cshoremodel

__all__ = [
    "cshoremodel",
]
