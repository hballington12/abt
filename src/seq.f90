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

implicit none

! to do:
! add support to avoid crash if nan detected

! ########## variable declaration ##########

! shared
real(8) start, finish ! cpu timing variables
integer(8) i_loop, loop_start, i ! counters

! input
character(len=*), parameter :: ifn = 'input.txt' ! input filename
type(job_parameters_type) job_params ! job parameters, contains wavelength, rbi, etc., see types mod for more details

! sr PDAL2
type(geometry_type) geometry ! particle geometry data structure
type(geometry_type) rotated_geometry ! rotated particle geometry data structure

! sr makeIncidentBeam
type(geometry_type) beam_geometry ! geometry of the incident beam
type(beam_type) beam_inc ! the incident beam

! sr beam_loop
type(outbeamtype), dimension(:), allocatable :: beam_outbeam_tree ! outgoing beams from the beam tracing
type(outbeamtype), dimension(:), allocatable :: ext_diff_outbeam_tree ! outgoing beams from external diffraction
integer(8) beam_outbeam_tree_counter ! counts the current number of beam outbeams

! sr diff_main
complex(8), dimension(:,:,:,:), allocatable:: ampl_far_beam ! far-field amplitude matrix due to outgoing beams
complex(8), dimension(:,:,:,:), allocatable :: ampl_far_ext_diff ! far-field amplitude matrix due to external diffraction

! sr make_mueller
real(8), dimension(:,:,:), allocatable :: mueller, mueller_total ! mueller matrices
real(8), dimension(:,:), allocatable :: mueller_1d, mueller_1d_total ! phi-integrated mueller matrices

! sr finalise
type(output_parameters_type) output_parameters 
type(output_parameters_type) output_parameters_total

! other
real(8), dimension(:), allocatable :: alpha_vals, beta_vals, gamma_vals
integer my_rank
integer seed(1:8)
character(len=255) cache_dir ! cached files directory (if job stops early)
integer(8), dimension(:), allocatable :: remaining_orients
integer(8) num_remaining_orients

! ########## start ##########

print*,'========== start pbt: seq'
start = omp_get_wtime() ! get start time
my_rank = 0
loop_start = 1
seed = [0, 0, 0, 0, 0, 0, 0, 0] ! set the seed values

! print the command used to execute the program
call print_command()

! parse command line
call parse_command_line(job_params)

if(job_params%resume) then ! if resuming a cached job
    print*,'attempting to resume job using cache #',job_params%cache_id
    ! get cached data and continue
    call resume_job(job_params,num_remaining_orients,remaining_orients,mueller_total,mueller_1d_total,output_parameters_total)
end if ! end if resuming a cached job

call make_dir(job_params%job_name,job_params%output_dir)
print*,'output directory is "',trim(job_params%output_dir),'"'
call system("mkdir "//trim(job_params%output_dir)//"/tmp") ! make directory for temp files
open(101,file=trim(job_params%output_dir)//"/"//"log") ! open global non-standard log file for important records

call write_job_params(job_params,job_params%output_dir) ! write job parameters to file

! get input particle geometry
call PDAL2( job_params, &     ! <-  job parameters
            geometry)         !  -> particle geometry            

if (job_params%tri) then ! if triangulation enabled
    ! triangulate the particle
    call triangulate(   job_params, & !
                        '-Q -q',                    & ! <-  extra flag inputs to triangle program
                        job_params%tri_roughness,   & ! <-  standard deviation for triangle roughness
                        my_rank,                    & ! <-  process rank, so the subroutine knows where to write temporary files to
                        job_params%output_dir,      & ! <-  output directory
                        geometry)                     !  -> triangulated geometry 
    ! call merge_vertices(vert_in, face_ids, 1D-3, geometry) ! merge vertices that are close enough
    ! call fix_collinear_vertices(vert_in, face_ids, num_vert, num_face, num_face_vert, apertures)
end if

! write geometry info to log file
call write_geometry_info(geometry)

! write unrotated particle to file (optional)            
call PDAS(  job_params%output_dir,  & ! <-  output directory
            "unrotated",            & ! <-  filename
            geometry)                 ! <-  geometry
! stop
call RANDOM_SEED(put=seed) ! Set the seed for the random number generator
! initialise the euler angles to be used
call init_loop( alpha_vals, &
                beta_vals, &
                gamma_vals, &
                job_params)
if(job_params%output_eulers) then ! if euler angle enabled with flag -output_eulers
    ! output euler angles to file
    call output_eulers(alpha_vals,beta_vals,gamma_vals,job_params%output_dir,job_params)
end if

! initialise the remaining orientation indices
if(job_params%resume) then ! if we are resuming a cached job
    ! do nothing because we've already read this in sr resume_job
else ! if we are not resuming a cached job
    num_remaining_orients = job_params%num_orients ! number of remaining orientations is the total overall
    allocate(remaining_orients(1:num_remaining_orients)) ! allocate array to hold orientation numbers
    do i = 1, num_remaining_orients
        remaining_orients(i) = i ! get the indices of the remaining orientations
    end do
end if ! end if we are resuming a cached job

print*,'starting orientation loop...'
write(101,*)'starting orientation loop... start: ',1,'end: ',num_remaining_orients

do i = 1, num_remaining_orients ! start orientation loop

    i_loop = remaining_orients(i) ! get loop index

    if(job_params%debug >= 3) then
        print*,'rotating particle...'
        write(101,*)'rotating particle...'
    end if

    ! rotate particle
    call PROT_MPI(  alpha_vals, &
                    beta_vals,  &
                    gamma_vals, &
                    i_loop,     &
                    job_params, &
                    geometry,   &
                    rotated_geometry)

    ! write rotated particle to file (optional)
    if (job_params%num_orients .eq. 1) then
        call PDAS(  job_params%output_dir,  & ! <-  output directory
                    "rotated",              & ! <-  filename
                    rotated_geometry)         ! <-  geometry
    end if

    call make_bvh(job_params,rotated_geometry) ! make bounding volume hierarchy (should probably move this to the unrotated particle later)

    ! call test_bvh(job_params,rotated_geometry)
    
    call export_bvh(rotated_geometry%bvh,job_params%output_dir) ! export bounding bolume hierarchy
    
    ! fast implementation of the incident beam
    call make_incident_beam(rotated_geometry,   & ! <-  geometry
                            beam_geometry,      & !  -> beam geometry
                            beam_inc)             !  -> incident beam

    if(job_params%debug >= 3) then
        print*,'computing near-field...'
        write(101,*)'computing near-field...'
    end if

    ! beam loop
    call beam_loop( beam_outbeam_tree,          & !  -> outgoing beams from the beam tracing
                    beam_outbeam_tree_counter,  & !  -> counts the current number of beam outbeams
                    ext_diff_outbeam_tree,      & !  -> outgoing beams from external diffraction
                    output_parameters,          & !  -> adds illuminated geometric cross section to output parameters
                    job_params,                 & ! <-  job parameters
                    rotated_geometry,           & ! <-  rotated particle geometry
                    beam_geometry,              & ! <-  incident beam geometry
                    beam_inc)                     ! <-  incident beam

    if(num_remaining_orients > 1 .and. mod(i-1,10) == 0) then ! print progress for this job
        print'(A25,I8,A3,I8,A20,f8.4,A3)','orientations completed: ',i-1,' / ',num_remaining_orients,' (total progress: ',dble(i-1)/dble(num_remaining_orients)*100,' %)'
        if(job_params%debug >= 2) then
            write(*,'(A,f10.2,A)') 'total time elapsed: ',omp_get_wtime()-start,' secs'
            write(*,'(A,f10.2,A)') 'average time per rotation: ',(omp_get_wtime()-start) / dble(i)
        end if
        if(job_params%timing) then
            if (i .gt. 1) then
                print'(A20,F12.4,A5)','est. time remaining: '
                call PROUST(nint(dble(num_remaining_orients-i+1)*(omp_get_wtime()-start) / dble(i)))
            end if      
        end if
    end if
    
    if(job_params%debug >= 3) then
        print*,'computing far-field...'
        write(101,*)'computing far-field...'
    end if

    ! diffraction
    call diff_main( beam_outbeam_tree,          & ! <-  outgoing beams from the beam tracing
                    beam_outbeam_tree_counter,  & ! <-  counts the current number of beam outbeams
                    ampl_far_beam,              & !  -> amplitude matrix due to beam diffraction
                    ext_diff_outbeam_tree,      & ! <-  outgoing beams from external diffraction
                    ampl_far_ext_diff,          & !  -> amplitude matrix due to external diffraction
                    job_params,                 & ! <-  job parameters
                    rotated_geometry)             ! <-  rotated particle geometry

    if(job_params%debug >= 3) then
        print*,'computing mueller matrix and parameters...'
        write(101,*)'computing mueller matrix and parameters...'
    end if

    call finalise(  ampl_far_beam,      & ! <-  amplitude matrix due to beam diffraction
                    ampl_far_ext_diff,  & ! <-  amplitude matrix due to external diffraction
                    mueller,            & !  -> 2d mueller matrix
                    mueller_1d,         & !  -> 1d mueller matrix
                    output_parameters,  & !  -> some output parameters
                    job_params)           ! <-  job parameters
                    
    ! call writeup(mueller, mueller_1d, theta_vals, phi_vals) ! write current mueller to file

    ! sum the total mueller and output parameters
    call summation(mueller, mueller_total, mueller_1d, mueller_1d_total,output_parameters,output_parameters_total)

    if((omp_get_wtime() - start)/3600D0 .gt. job_params%time_limit) then ! if job time limit exceeded
        ! make cache directory
        call make_cache_dir("cache/",cache_dir)
        ! cache remaining orientations
        call cache_remaining_orients_seq(cache_dir,i,num_remaining_orients,remaining_orients)
        ! cache everything else
        call cache_job( job_params,                 & ! <-  job parameters
                        i_loop,                     & ! <-  current loop index
                        output_parameters_total,    & ! <-  total output parameters
                        mueller_total,              & ! <-  total 2d mueller
                        mueller_1d_total,           & ! <-  total 1d mueller
                        cache_dir,                  & ! <-  cache directory
                        geometry)                     ! <-  unrotated particle geometry
        stop
    end if ! end if job time limit exceeded

end do ! end orientation loop

print*,'end orientation loop.'

! divide by no. of orientations
call divide_by_num_orientations(mueller_total,mueller_1d_total,output_parameters_total,job_params)

! print final output parameters to std out
call print_output_params(output_parameters_total)

! writing to file
call write_outbins(job_params%output_dir,job_params%theta_vals,job_params%phi_vals)
call writeup(mueller_total, mueller_1d_total, job_params%output_dir, output_parameters_total, job_params) ! write to file

! clean up temporary files
call system("rm -r "//trim(job_params%output_dir)//"/tmp") ! remove directory for temp files

finish = omp_get_wtime()
print*,'=========='
print'(A,f16.8,A)',"total time elapsed: ",finish-start," secs"
write(101,*)'======================================================'
write(101,'(A,f17.8,A)')" total time elapsed: ",finish-start," secs"
write(101,*)'======================================================'

print*,'========== end main'

close(101) ! close global non-standard output file

contains



end program main
