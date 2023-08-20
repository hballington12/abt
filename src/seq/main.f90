! ############################################################################################################

! abt.f90
! main program for physical optics hybrid method beam tracing code developed by Harry Ballington and Evelyn Hesse
! see README file for more details

program main

use input_mod
use misc_submod
use beam_loop_mod
use types_mod
use diff_mod
use omp_lib
use outputs_mod
use ifport

implicit none

! to do:
! add vector re-normalisation checks after rotations (ie. sr rotate_into_aperture_system)
! add quad support
! add support to avoid crash if nan detected
! automatic meshing
! automatic apertures

! ############################################################################################################

! shared
real(8) start, finish ! cpu timing variables
integer(8) i

! input
character(len=*), parameter :: ifn = 'input.txt' ! input filename
character(len=255) :: cwd ! current working directory
character(len=255) :: output_dir ! output directory
integer result ! logical for navigating directories

! sr SDATIN
character(100) cfn ! crystal filename
character(100) cft ! crystal file type
character(100) afn ! apertures filename
real(8) la ! wavelength
real(8) rbi ! real part of the refractive index
real(8) ibi ! imaginary part of the refractive index
integer rec ! max number of internal beam recursions
character(100) rot_method ! rotation method
logical is_multithreaded ! whether or not code should use multithreading
integer num_orients ! number of orientations
logical intellirot ! whether or not to use intelligent euler angle choices for orientation avergaing
character(100) c_method ! method of particle file input
character(100) job_name ! name of job
integer(8) offs(1:2) ! off values, used if off rotation method
real(8) eulers(1:3) ! euler angles, used if euler rotation method
type(cc_hex_params_type) cc_hex_params ! parameters for C. Collier Gaussian Random hexagonal columns/plates

! sr PDAL2
integer(8) num_vert ! number of unique vertices
integer(8) num_face !  number of faces
integer(8) num_norm ! number of face normals
integer(8), dimension(:,:), allocatable :: face_ids ! face vertex IDs
real(8), dimension(:,:), allocatable :: vert_in ! unique vertices (unrotated)
real(8), dimension(:,:), allocatable :: vert ! unique vertices (rotated)
real(8), dimension(:,:), allocatable :: norm ! face normals
integer(8), dimension(:), allocatable :: num_face_vert ! number of vertices in each face
integer(8), dimension(:), allocatable :: norm_ids ! face normal ID of each face
integer(8), dimension(:), allocatable :: apertures ! apertures asignments for each facet

! sr makeIncidentBeam
real(8), allocatable, dimension(:,:) :: beamV ! beam vertices
real(8), allocatable, dimension(:,:) :: beamN ! beam normals
real(8), allocatable, dimension(:,:) :: beamMidpoints ! beam  midpoints
integer(8), allocatable, dimension(:,:) :: beamF1 ! beam face vertex indices
integer(8), allocatable, dimension(:) :: beamF2 ! beam face normal indices
complex(8), allocatable, dimension(:,:,:) :: ampl_beam ! amplitude matrix of incident beam

! sr beam_loop
type(outbeamtype), dimension(:), allocatable :: beam_outbeam_tree ! outgoing beams from the beam tracing
type(outbeamtype), dimension(:), allocatable :: ext_diff_outbeam_tree ! outgoing beams from external diffraction
integer(8) beam_outbeam_tree_counter ! counts the current number of beam outbeams
real(8) energy_out_beam
real(8) energy_out_ext_diff
real(8) energy_abs_beam

! sr diff_main
complex(8), dimension(:,:), allocatable:: ampl_far_beam11, ampl_far_beam12, ampl_far_beam21, ampl_far_beam22 ! total
real(8), dimension(:), allocatable :: theta_vals, phi_vals
complex(8), dimension(:,:), allocatable :: ampl_far_ext_diff11, ampl_far_ext_diff12, ampl_far_ext_diff21, ampl_far_ext_diff22 ! total

! sr make_mueller
real(8), dimension(:,:,:), allocatable :: mueller, mueller_total, mueller_recv ! mueller matrices
real(8), dimension(:,:), allocatable :: mueller_1d, mueller_1d_total, mueller_1d_recv ! phi-integrated mueller matrices

! sr finalise
type(output_parameters_type) output_parameters 
type(output_parameters_type) output_parameters_total

real(8), dimension(:), allocatable :: alpha_vals, beta_vals, gamma_vals

! ############################################################################################################
! start
print*,'========== start main'
start = omp_get_wtime()
call seed(99)

call parse_command_line(cfn,                & ! particle filename
                        cft,                & ! particle file type
                        afn,                & ! apertures filename
                        la,                 & ! wavelength
                        rbi,                & ! real refractive index
                        ibi,                & ! imaginary refractive index
                        rec,                & ! number of beam recursions
                        rot_method,         & ! rotation method
                        is_multithreaded,   & ! is multithreaded?
                        num_orients,        & ! number of orientations
                        intellirot,         & ! intelligent rotation angles
                        c_method,           & ! particle input method
                        job_name,           & ! job name
                        eulers,             & ! euler angles, used if euler rotation method
                        offs,               & ! off values, used if off rotation method
                        cc_hex_params,      & ! parameters for C. Collier hexagons
                        theta_vals,         & ! far-field theta values to evaluate at
                        phi_vals)           ! far-field theta values to evaluate at

call make_dir(job_name,output_dir)
print*,'output directory is "',trim(output_dir),'"'
open(101,file=trim(output_dir)//"/"//"log") ! open global non-standard log file for important records

! write job parameters to log file
call write_job_params(  cfn,                & ! particle filename
                        cft,                & ! particle file type
                        afn,                & ! apertures filename
                        la,                 & ! wavelength
                        rbi,                & ! real refractive index
                        ibi,                & ! imaginary refractive index
                        rec,                & ! number of beam recursions
                        rot_method,         & ! rotation method
                        is_multithreaded,   & ! is multithreaded?
                        num_orients,        & ! number of orientations
                        intellirot,         & ! intelligent rotation angles
                        c_method,           & ! particle input method
                        output_dir,         & ! output directory
                        cc_hex_params)        ! parameters for C. Collier hexagons

! get input particle information
call PDAL2( ifn,            & ! <-  input filename
            c_method,       & ! <-  method of particle file input
            num_vert,       & !  -> number of unique vertices
            num_face,       & !  -> number of faces
            face_ids,       & !  -> face vertex IDs
            vert_in,        & !  -> unique vertices
            num_face_vert,  & !  -> number of vertices in each face
            afn,            & !  ->  apertures filename
            apertures,      &
            cc_hex_params,  &
            cft,            &
            cfn)

! write unrotated particle to file (optional)            
call PDAS(  vert_in,       & ! <-  rotated vertices
            face_ids,   & ! <-  face vertex IDs
            output_dir, & ! <-  output directory
            "unrotated")    ! <-  filename

call init_loop(num_orients,alpha_vals,beta_vals,gamma_vals,intellirot)

do i = 1, num_orients  

    ! rotate particle
    call PROT_MPI(  ifn,        & ! <-  input filename
                    rot_method, & ! <-  particle rotation method
                    vert_in,    & ! <-> unique vertices (unrotated in, rotated out) to do: remove inout intent and add a rotated vertices variable
                    vert,       &
                    alpha_vals, &
                    beta_vals,  &
                    gamma_vals, &
                    i,          &
                    num_orients,&
                    eulers,     &
                    offs)
    
    ! write rotated particle to file (optional)
    if (num_orients .eq. 1) then
        call PDAS(  vert,       & ! <-  rotated vertices
                    face_ids,   & ! <-  face vertex IDs
                    output_dir, & ! <-  output directory
                    "rotated")    ! <-  filename
    end if

    ! fast implementation of the incident beam
    call makeIncidentBeam(  beamV,         & ! ->  beam vertices
                            beamF1,        & ! ->  beam face vertex indices
                            beamN,         & ! ->  beam normals
                            beamF2,        & ! ->  beam face normal indices
                            vert,          & ! <-  unique vertices
                            face_ids,      & ! <- face vertex IDs
                            beamMidpoints, & !  -> beam  midpoints
                            ampl_beam)       !  -> amplitude matrix of incident beam       

    ! beam loop
    call beam_loop( face_ids,                  & ! <-  face vertex IDs
                    vert,                      & ! <-  unique vertices
                    la,                        & ! <-  wavelength
                    rbi,                       & ! <-  real part of the refractive index
                    ibi,                       & ! <-  imaginary part of the refractive index
                    apertures,                 & ! <-  apertures
                    rec,                       & ! <-  max number of internal beam recursions
                    beamV,                     & ! <-  beam vertices
                    beamF1,                    & ! <-  beam face vertex indices
                    beamN,                     & ! <-  beam normals
                    beamF2,                    & ! <-  beam face normal indices
                    beamMidpoints,             & ! <-  beam  midpoints
                    ampl_beam,                 & ! <-  amplitude matrix of incident beam
                    beam_outbeam_tree,         & !  -> outgoing beams from the beam tracing
                    beam_outbeam_tree_counter, & !  -> counts the current number of beam outbeams
                    ext_diff_outbeam_tree,     & !  -> outgoing beams from external diffraction
                    energy_out_beam,           & !  -> total energy out from beams (before diffraction)
                    energy_out_ext_diff,       & !  -> total energy out from external diffraction (before diffraction)
                    energy_abs_beam,           & !  -> total energy absorbed from beams (before diffraction)
                    output_parameters,         & !  -> adds illuminated geometric cross section to output parameters
                    is_multithreaded)            ! <-  enable or disable multithreading
    ! stop
    if(num_orients .gt. 1) then
        print'(A15,I8,A3,I8,A20,f8.4,A3)','orientation: ',i,' / ',num_orients,' (total progress: ',dble(i-1)/dble(num_orients)*100,' %)'
        ! print*,'total time elapsed: ',omp_get_wtime()-start
        ! print*,'average time per rotation: ',(omp_get_wtime()-start) / dble(i)
        if (i .gt. 1) then
            print'(A20,F12.4,A5)','est. time remaining: '
            call PROUST(nint(dble(num_orients-i+1)*(omp_get_wtime()-start) / dble(i)))
        end if
    end if

    ! diffraction
    call diff_main( beam_outbeam_tree,         & ! <-  outgoing beams from the beam tracing
                    beam_outbeam_tree_counter, & ! <-  counts the current number of beam outbeams
                    la,                        & ! <-  wavelength
                    ampl_far_beam11,           & !  -> amplitude matrix (1,1) due to beam diffraction
                    ampl_far_beam12,           & !  -> amplitude matrix (1,2) due to beam diffraction
                    ampl_far_beam21,           & !  -> amplitude matrix (2,1) due to beam diffraction
                    ampl_far_beam22,           & !  -> amplitude matrix (2,2) due to beam diffraction
                    theta_vals,                & ! <-  theta values
                    phi_vals,                  & ! <-  phi values
                    ext_diff_outbeam_tree,     & ! <-  outgoing beams from external diffraction
                    ampl_far_ext_diff11,       & !  -> amplitude matrix (1,1) due to external diffraction
                    ampl_far_ext_diff12,       & !  -> amplitude matrix (1,2) due to external diffraction
                    ampl_far_ext_diff21,       & !  -> amplitude matrix (2,1) due to external diffraction
                    ampl_far_ext_diff22,       & !  -> amplitude matrix (2,2) due to external diffraction
                    is_multithreaded)            ! <-  enable or disable multithreading

    call finalise(  ampl_far_beam11,     & ! <-  amplitude matrix (1,1) due to beam diffraction
                    ampl_far_beam12,     & ! <-  amplitude matrix (1,2) due to beam diffraction
                    ampl_far_beam21,     & ! <-  amplitude matrix (2,1) due to beam diffraction
                    ampl_far_beam22,     & ! <-  amplitude matrix (2,2) due to beam diffraction
                    ampl_far_ext_diff11, & ! <-  amplitude matrix (1,1) due to external diffraction
                    ampl_far_ext_diff12, & ! <-  amplitude matrix (1,2) due to external diffraction
                    ampl_far_ext_diff21, & ! <-  amplitude matrix (2,1) due to external diffraction
                    ampl_far_ext_diff22, & ! <-  amplitude matrix (2,2) due to external diffraction
                    theta_vals,          & ! <-  theta values
                    phi_vals,            & ! <-  phi values
                    energy_out_beam,     & ! <-  total energy out from beams (before diffraction)
                    energy_out_ext_diff, & ! <-  total energy out from external diffraction (before diffraction)
                    mueller,             & !  -> 2d mueller matrix
                    mueller_1d,          & !  -> 1d mueller matrix
                    la,                  & ! <-  wavelength (for optical theorem)
                    energy_abs_beam,     & ! <-  energy absorbed within the particle
                    output_parameters)     !  -> some output parameters
                    
    ! call writeup(mueller, mueller_1d, theta_vals, phi_vals) ! write current mueller to file

    call summation(mueller, mueller_total, mueller_1d, mueller_1d_total,output_parameters,output_parameters_total)

end do

! divide by no. of orientations
mueller_total = mueller_total / num_orients
mueller_1d_total = mueller_1d_total / num_orients
output_parameters_total%abs = output_parameters_total%abs / num_orients
output_parameters_total%scatt = output_parameters_total%scatt / num_orients
output_parameters_total%ext = output_parameters_total%ext / num_orients
output_parameters_total%albedo = output_parameters_total%albedo / num_orients
output_parameters_total%asymmetry = output_parameters_total%asymmetry / num_orients
output_parameters_total%abs_eff = output_parameters_total%abs_eff / num_orients
output_parameters_total%scatt_eff = output_parameters_total%scatt_eff / num_orients
output_parameters_total%ext_eff = output_parameters_total%ext_eff / num_orients
output_parameters_total%geo_cross_sec = output_parameters_total%geo_cross_sec / num_orients

! writing to file
call write_outbins(output_dir,theta_vals,phi_vals)
call writeup(mueller_total, mueller_1d_total, theta_vals, phi_vals, output_dir, output_parameters_total) ! write to file

finish = omp_get_wtime()

print*,'=========='
print'(A,f16.8,A)',"total time elapsed: ",finish-start," secs"
write(101,*),'======================================================'
write(101,'(A,f17.8,A)')," total time elapsed: ",finish-start," secs"
write(101,*),'======================================================'
print*,'========== end main'

close(101) ! close global non-standard output file

contains



end program main
