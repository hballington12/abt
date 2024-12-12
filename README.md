# PBT: Parent Beam Tracer Light Scattering Code

## Foreword

The authors hope that the users find this code useful and we kindly ask that you cite the source of this code. The general reference is - Ballington H. and Hesse E. A light scattering model for large particles with surface roughness, [J. Quant. Spectrosc. Radiat. Transfer **323**, 109054](https://doi.org/10.1016/j.jqsrt.2024.109054) (2024).

## Table of Contents

- [PBT: Parent Beam Tracer Light Scattering Code](#pbt-parent-beam-tracer-light-scattering-code)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Compiling](#compiling)
  - [Basic Usage](#basic-usage)
  - [Input Flags](#input-flags)
  - [Guidelines](#guidelines)
    - [Near Field Computation](#near-field-computation)
    - [Far Field Computation](#far-field-computation)
  - [Examples](#examples)

![image](https://github.com/hballington12/pbt/assets/71540450/b00baa1b-3069-4521-9249-29a457035cd5)

## Overview

This file contains a summary of the parent beam tracer (PBT), a physical optics hybrid code developed by Harry Ballington and Evelyn Hesse. The PBT method is designed for the rapid computation of the single scattering properties of particles with size parameter much larger than the wavelength of light. It is an analytical, but approximate method, which combines geometric optics with vector diffraction theory to compute the far-field scattering from any non-spherical particle geometry with much fewer computational resources required than any currently known exact method. Nonetheless, the user of this code should take care to follow the [guidelines](#guidelines) and verify results where possible against exact theory.

## Compiling

Compile instructions (gcc-13.0.1 & openmpi-4.0.5)

- `cd src/seq; make` - compiles sequential code
- `cd src/mpi; make` - compiles mpi code

Example shell scripts and a short summary of command line flags for submitting jobs may be found in `template/`.

## Basic Usage

Once compiled, the pbt code can be run with no input arguments straight from the command line with `./src/seq/pbt` (or `mpiexec ./src/mpi/pbt` for the mpi version). This will execute the code for a hexagonal column of hexagonal radius `5` and length `10`, with incident wavelength `0.532` and refractive index `1.31+0i`. The mpi version of the code is designed for orientation averaging, and should only be run with fewer mpi processes than the number of orientations. Although the code ships with a built-in method for scattering from hexagonal prisms, an attractive prospect of using this code is that it can be run for arbitrary non-spherical particle geometries. The code reads in particle geometries in wavefront `.obj` format, or in the Macke ray-tracing style `.cry` or `.crystal`. Due to the way by which the near-field is computed, the particle geometry must sufficiently meshed. This can be performed automatically by use of the `-tri`, and `-tri-edge` flags, which make use the wonderful '[Triangle](https://www.cs.cmu.edu/~quake/triangle.html)' code written by Jonathan Richard Shewchuk. Alternatively, the user may choose to input a pre-meshed particle geometry. In this case, the user should also specify an apertures file using the `-afn` flag, which defines the parents used as macroscopic features of the geometry and therefore has a primary role in the accuracy of the near-field compuatation. For more details on how to customise and run the code, see the sections below on [input flags](#input-flags) and [guidelines](#guidelines).

## Input Flags

Main source file. It contains the program entry point. The PBT reads input parameters from the command line. Each input parameter is defined by a keyphrase, with arguments following a space delimiter. The ordering of most arguments in the command line is not important and the PBT will search through lines from left to right in an attempt to find each argument. A summary of command line arguments is given below:

- `-lambda` `<value>` - Defines the wavelength of incident light. If omitted, the default value is `0.532`.

- `-rbi` `<value>` - Defines the real component of the particle refractive index. If omitted, the default value is `1.31`.

- `-ibi` `<value>` - Defines the imaginary component of the particle refractive index. If omitted, the default value is `0`.

- `-cmethod` `<string>` - Defines the method of particle input. If omitted, the PBT will use the `cc_hex` method to make a hexagonal prism with radius 5 and prism length 10. Current supported methods are:

    - `read` - attempts to read the particle from the current directory. If `read` is specified, the following arguments may also be specified:
    
        - `-cft` `<string>` - Defines the particle input filetype. The supported particle file input types are:
        
            - `obj` - wavefront style geometry file
        
            - `mrt` - Macke ray-tracing style

            If `-cfn` is omitted, the PBT will attempt to guess the particle filetype based on the file extension. Macke ray-tracing style is assumed for file extensions `.cry` and `.crystal`, and wavefront style is assumed for file extension `.obj`.
        
        - `-cfn` `<string>` - Defines the particle filename. If the particle file is not a sufficiently discretised triangular mesh, see information on the `-tri` flag for triangulation. If the mesh consists only of triangles, the PBT will assume it is sufficiently discretised and automatic triangulation will be disabled. If the mesh contains a facet with more than 3 vertices, the PBT will enable triangulation by default because the code does not currently directly support this.
    
        - `-afn` `<string>` - Defines the apertures filename. The apertures file contains a single column defining which aperture each face belongs to. The number of lines in the apertures file must match the total number of faces in the particle file.
    
    - `cc_hex` - Attempts to make gaussian rough hexagonal column/plate. Uses method developed by C. Collier, based on Muinonen & Saarinen 2000. If this flag flag is used, several other flags may also be specified:
        - `-cc_hex_l` `<value>` - L from Muinonen & Saarinen 2000. Should be large compared to the correlation length (see below). If omitted, the default value is `20`.
        - `-cc_hex_hr` - Hexagonal edge length. If omitted, the default value is `5`.
        - `-cc_hex_nfhr` - Number of subdivisions along each hexagonal edge. If omitted, the default value is `6`.
        - `-cc_hex_pfl` - Prism edge length. If omitted, the default value is `10`.
        - `-cc_hex_nfpl` - Number of subdivisions along each prism edge. If omitted, the default value is `12`.
        - `-cc_hex_pher` - Number of rotations to perform at prism facet-basal facet edges (10% of no. of subfacets along prism edge). If omitted, the default value is `1`.
        - `-cc_hex_pper` - Number of rotations to perform at prism facet-prism facet edges (10% of subfacets along hexagon edge). If omitted, the default value is `1`.
        - `-cc_hex_nscales` - Number of roughness scales. If omitted, the default value is `1`.
        - `-cc_hex_cls` - Correlation lengths for each roughness scale, separated by spaces. If omitted, the default value is `1`.
        - `-cc_hex_sds` - Standard deviations for each roughness scale, separated by spaces. If omitted, the default value is `0`.

- `-rec` `<value>` - Defines the total number of beam recursions per orientation. If omitted, the default value is `8`.

- `-refl` `<value>` - Defines the max number of beam total internal reflection events per orientation. If omitted, the default value is `10`.

- `-rot` `<args>` - Defines the orientation of the particle. Is optional. If omitted, the PBT will not rotate the input particle. The `<args>` parameter is used to define the method of rotation, or to define random orientation. Current supported methods are:

    - `euler` `<alpha>` `<beta>` `<gamma>` - Choose to rotate the particle according to the 3 Euler angles, given in degrees. There are several ways to rotate via Euler angles; the PBT follows the method of Mishchenko.

        - example: `-rot euler 11 25 32`

    - `off` `30` `0,10,20,30` - Choose to rotate the particle according to the "off" convention. Only 4 different values are currently supported for this method. Input particle should be oriented lengthways, with prism axis lying in the xy plane.

        - example: `-rot off 30 20`

    - `none` - Choose to not rotate the particle.

        - example: `rot none`

    - `multi` `<value>` - Choose to randomly orient the particle. `<value>` defines the number of orientations. For reproducability, the random_seed subroutine can be used to set the random seed to a specified value, which will cause the random orientations to be reproducable. Since the Euler $\alpha$ angle has no effect on the 1-d scattering, it is set to 0 for orientation averaging.

        - example: `-rot multi 1000`

- `-mt <value>` - Defines whether the code should attempt to use multithreading, where appropriate. The value may be `0` for no multithreading, or `1` for multithreading. If omitted, multithreading is enabled by default. If enabled, the user should ensure that the relevant omp environment variables are set up for their system. ie. `OMP_STACKSIZE`, `OMP_NUM_THREADS`, etc. If the code throws a segmentation fault at the diffraction subroutine, the user will likely need to increase the `OMP_STACKSIZE`.

- `-jobname` `<string>` - Specifies the name of the directory within which the output files should be place. Is optional. If omitted, "my_job#" is used, where # is an integer. If the directory already exists, an integer is appended to the directory name so that no files are overwritten.

- `-theta` `values` - Specifies the polar angles at which the far-field should be evaluated. See below for example usage:
    - `-theta 0 1 180` - Evaluate the far-field from 0 in 1 degree steps to 180. If omitted, this is the default behaviour.
    - `-theta 6 0.1 25 150 175 0.25 180` - Evaluate the far-field from 6 in 0.1 degree steps to 25, then step to 175, then in 0.25 degree steps to 180.

- `-phi` `values` - Specifies the azimuthal angles at which the far-field should be evaluated. See below for example usage:
    - `-phi 0 1 360` - Evaluate the far-field from 0 in 1 degree steps to 360. If omitted, this is the default behaviour.

- `-no2d` - Suppresses the output of the 2D mueller matrix, which can be a large file if many far-field evaluation points are specified.

- `-tri` - Enables automatic triangulation. Note that use of this flag requires compiling the triangle code in `./src/tri/`.

- `-tri_edge <value>` - Sets the maximum edge length for triangulation. If omitted, the default value is `1`.

- `-tri_rough <value>` - Sets the standard deviation for roughness derived from the triangulation. If omitted, the default value is `0`.

- `-tri_div <value>` - Sets the minimum divides per average parent length dimension from the triangulation. If omitted, the default value is `1`.

- `-time_limit <value>` - Sets a time limit (in hours). The PBT will save at an intermediate point if this time is surpassed. Use `-resume <value>` to resume the job (see below).

- `-resume <value>` - Resumes a previous job that was saved at an intermediate point. `value` must be the number of the cache ID. This option overrides most input parameters with those read from the cached job.

- `-scaling` - Forces the diffracted energy in the far-field to be conserved with respect to the near-field energy.

- `-timing` - Enables more detailed output of the timing of different parts of the code

- `-debug <value>` - Controls the level of debugging output:

    - `0` - minimal output

    - `1` - some output (default)

    - `2` - large output

    - `3` - extreme output

- `-export_beam <args>` - exports information about the beams to a file

    - `num <value> [<value>]` - exports by beam number. If 1 value is given, the beam tree is exported from the first beam index to the index specified by the value. If 2 values are given, the beam tree is exported from the beam index specified by the first value to the beam index specified by the second value.

    - `rec <value> [<value>]` - exports by recursion number. If 1 value is given, the beam tree is exported from the first recursion to the recursion specified by the value. If 2 values are given, the beam tree is exported from the recursion specified by the first value to the recursion specified by the second value.

- `-fast_diff` - enables an approximate but faster diffraction method. According to Jackson, Classical Electrodynamics Sec 10.5, most of the diffracted energy is confined within the angle $\lambda/d$, where $d$ is a linear dimension of the aperture. If this flag is enabled, any far-field bins outside an angle of $8\lambda/d$ are excluded from the diffraction calculation, for a given outgoing beam. This flag also restricts the external diffraction to the forwards scattering.

- `intellirot` - sets the euler angles for orientation averaging to be uniformly distributed, instead of randomly distributed.

- `beta_min <value>` - sets the minimum beta angle for orientation averaging, which can be used to take advantage of particle symmetry. Must be in the range `0` to `180`.

- `beta_max <value>` - sets the maximum beta angle for orientation averaging, which can be used to take advantage of particle symmetry. Must be in the range `0` to `180`.

- `gamma_min <value>` - sets the minimum gamma angle for orientation averaging, which can be used to take advantage of particle symmetry. Must be in the range `0` to `360`.

- `gamma_max <value>` - sets the maximum gamma angle for orientation averaging, which can be used to take advantage of particle symmetry. Must be in the range `0` to `360`.

- `-output_eulers` - outputs the euler angles used for orientation averaging to a file

- `-speed` - prioritises speed over memory use. This flag is enabled by default.

- `-memory` - prioritises memory use over speed.

## Guidelines

The computation of the PBT code is basically composed of 2 parts: the near field computation, and the far field computation. The accuracy of each of these steps depends on a few factors, which will be discussed below.

### Near Field Computation

- The pbt code uses principles of geometric optics to calculate an approximation for the near-field on the particle surface. In order for geometric optics to be valid, the particle size must be much larger than the wavelength of light.

- A crucial principal of the near-field computation to be familiar with, is the concept of a *parent*. A parent is defined as a collection of facets which produce 1 reflected (and possibly one refracted) wave with a single propagation direction in the near-field. Each parent is a collection of similar facets in the particle geometry which represent the macroscopic features of the particle. In this way, the pbt maintains accuracy even when the length scale of the local surface becomes comparable to, or smaller than the wavelength. If the user inputs a pre-meshed geometry with the `-cfn` flag, then they should also input an apertures file with the `-afn` flag. The pbt is designed to work with a large number of parents, but not huge numbers. The length scale of each parent should be large when compared with the wavelength. The code has been tested with a few hundred parents, but at this point the near-field computation becomes significantly slower.

- In order for the far-field mapping to be accurate, the pbt relies on the Fraunhofer approximation, which approximates the total field at an aperture as that of the incident field. If the near-field approximation is poor, the far-field mapping tends to become less accurate. It is for this reason, that in its current state the pbt code cannot be used for spherical particle geometries, since the propagation of light deviates in this case from a plane wave and therefore the near-field computation loses accuracy. In general, this tends to lead to very large scattering cross sections for the beam diffraction contribution. This can be rescaled with the `-scaling` flag, but this should be used with caution.

### Far Field Computation

- The pbt uses vector diffraction theory at an aperture described by [Karczewski & Wolf](https://doi.org/10.1364/JOSA.56.001207). The integrated scattering parameters, such as asymmetry parameter and various cross sections, are computed by interpolating a 3-point Lagrange polynomial to the data, which is integrated exactly. As particle size increases, the width of diffraction peaks can become extremely narrow. In order to accurately interpolate over the scattered field, the number of far-field bins (specified with flags `-theta` and `-phi`) should have sufficient resolution to resolve these diffraction peaks. If the individual scattering efficiencies deviate significantly from 1, the user should treat the validity of results with caution. In general, if both the beam and external diffraction scattering efficiencies deviate from 1, the user may wish to check that the far-field bins have sufficient resolution. If the external diffraction scattering efficiency is close to 1 but the beam diffraction is not, then this suggests that the near-field computation has poor accuracy. Reasons for this include: particle mesh being too course, parents not representing the macroscopic features of the geometry, or microscopic features of the parents being comparable to the wavelength.

## Examples

 `abt -lambda 0.532 -rbi 1.3117 -ibi 0 -rec 10 -rot euler 20 35 2 -cmethod read -cft obj -cfn my_particle.obj -afn my_apertures.dat -mt -theta 0 1 180 -phi 0 2 360` - run `abt` executable with wavelength 0.532, refractive index 1.3117 + 0i, 10 beam recursions, particle rotated with euler angles 20, 35, read the particle with particle file type `.obj`, filename `my_particle.obj`, apertures specified in the file `my_apertures.dat`, with multithreading enabled. Evaluate the far-field at polar angle 0 in 1 degree steps to 180 and at azimuthal angle 0 in 2 degree steps to 360.

