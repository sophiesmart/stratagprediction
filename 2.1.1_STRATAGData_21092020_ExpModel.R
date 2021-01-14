#
# Copyright (C) 2020 Ellis Pires <pirese@cardiff.ac.uk>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License,
# Version 2.1 only as published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#


# Add our local library to the libpath
.libPaths( c( Sys.getenv("R_LIBS_mpmss1") ) )


#################################################
#           CRAN Package Installation           #
#################################################

# Set a default repository for packages
local({r <- getOption('repos')
r['CRAN'] <- 'http://www.stats.bris.ac.uk/R/'
options(repos=r)
})

# Add or remove CRAN packages as required
list.of.cran.packages <- c("utils",
                           "dplyr",
                           "mice"
)


# Install packages from list.of.cran.packages
for (pkg in list.of.cran.packages){
  if(!eval(bquote(require(.(pkg))))){
    eval(bquote(install.packages(.(pkg), dependencies=T)))
  }
}


# Load packages from list.of.cran.packages
for (pkg in list.of.cran.packages){
  eval(bquote(library(.(pkg))))
}


#################################################
#       BiocManager Package Installation        #
#################################################

# Load BiocManager if required
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")


# Add remove standard CRAN packages as required
#list.of.bioc.packages <- c("DiffBind",
#                           "GenomicRanges")

# Install packages from list.of.bioc.packages
#for (pkg in list.of.bioc.packages){
#  if(!eval(bquote(require(.(pkg))))){
#    eval(bquote(BiocManager::install(.(pkg), dependencies=T)))
#  }
# }

# Load packages from list.of.cran.packages
#for (pkg in list.of.bioc.packages){
#  eval(bquote(library(.(pkg))))
#}


#if (!requireNamespace("DiffBind", quietly = TRUE))
#    BiocManager::install("DiffBind")


