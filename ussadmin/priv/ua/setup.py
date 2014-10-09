#!/usr/bin/python
# Copyright (c) 2010-2011 OpenStack, LLC.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
# implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from setuptools import setup, find_packages
from setuptools.command.sdist import sdist
import os
import subprocess
import time

from ua import __version__ as version


class local_sdist(sdist):
    """Customized sdist hook - builds the ChangeLog file from VC first"""

    def run(self):
        print "hello"
        sdist.run(self)


name = 'ua'

setup(
    name=name,
    version=version,
    description='ua: uss admin',
    license='MDS License',
    author='www.meidisen.com',
    author_email='',
    url='www.meidisen.com',
    packages=find_packages(exclude=['test', 'bin']),
    test_suite='nose.collector',
    cmdclass={'sdist': local_sdist},
    classifiers=[
        'Development Status :: 4 - Beta',
        'License :: OSI Approved :: MDS License',
        'Operating System :: POSIX :: Linux',
        'Programming Language :: Python :: 2.6',
        'Environment :: No Input/Output (Daemon)',
        ],
    install_requires=[],  # removed for better compat
    scripts=[
        'bin/cleanua', 
        'bin/stopua', 
        'bin/passwdua', 
       ],
    )
