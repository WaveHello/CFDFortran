# CFD Fortran

This project follows the similarly named project [CFD Python](https://github.com/barbagroup/CFDPython), implementing the **12 steps to Navier-Stokes** in modern Fortran.

**CFD Python**, a.k.a. the **12 steps to Navier-Stokes**, is a practical module for learning the foundations of Computational Fluid Dynamics (CFD) by coding solutions to the basic partial differential equations that describe the physics of fluid flow. This Fortran implementation provides the same educational journey using modern Fortran programming practices.

The original CFD Python module was created by Prof. Lorena Barba and has been widely used for teaching CFD fundamentals. This Fortran version maintains the same incremental approach and educational philosophy while leveraging Fortran's performance advantages for numerical computing.

## Original Citation

Please cite the original work as: Barba, Lorena A., and Forsyth, Gilbert F. (2018). CFD Python: the 12 steps to Navier-Stokes equations. _Journal of Open Source Education_, **1**(9), 21, https://doi.org/10.21105/jose.00021

## Building and Running

This project uses the Fortran Package Manager (FPM). To build and run:

```bash
fpm build
fpm run
```

## Dependencies

- Modern Fortran compiler (gfortran, ifort, etc.)
- FPM (Fortran Package Manager)

## Installation

### Installing via Conda

We recommend using conda to install both the Fortran compiler and FPM:

```bash
# Install gfortran compiler
conda install -c conda-forge gfortran

# Install FPM (Fortran Package Manager)
conda install -c conda-forge fpm
```

Alternatively, you can create a new environment with all dependencies:

```bash
# Create new environment with Fortran tools
conda create -n fortran-cfd -c conda-forge gfortran fpm
conda activate fortran-cfd
```
