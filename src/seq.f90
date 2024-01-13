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
! add quad support
! add support to avoid crash if nan detected

! ############################################################################################################

! shared
real(8) start, finish ! cpu timing variables
integer(8) i_loop, loop_start, i

! input
character(len=*), parameter :: ifn = 'input.txt' ! input filename
character(len=255) :: output_dir ! output directory
type(job_parameters_type) job_params ! job parameters, contains wavelength, rbi, etc., see types mod for more details

! sr PDAL2
type(geometry_type) geometry ! particle geometry data structure
type(geometry_type) rotated_geometry ! rotated particle geometry data structure

! sr makeIncidentBeam
type(geometry_type) beam_geometry
type(beam_type) beam_inc

! sr beam_loop
type(outbeamtype), dimension(:), allocatable :: beam_outbeam_tree ! outgoing beams from the beam tracing
type(outbeamtype), dimension(:), allocatable :: ext_diff_outbeam_tree ! outgoing beams from external diffraction
integer(8) beam_outbeam_tree_counter ! counts the current number of beam outbeams

! sr diff_main
complex(8), dimension(:,:), allocatable:: ampl_far_beam11, ampl_far_beam12, ampl_far_beam21, ampl_far_beam22 ! total
complex(8), dimension(:,:), allocatable :: ampl_far_ext_diff11, ampl_far_ext_diff12, ampl_far_ext_diff21, ampl_far_ext_diff22 ! total

! sr make_mueller
real(8), dimension(:,:,:), allocatable :: mueller, mueller_total ! mueller matrices
real(8), dimension(:,:), allocatable :: mueller_1d, mueller_1d_total ! phi-integrated mueller matrices

! sr finalise
type(output_parameters_type) output_parameters 
type(output_parameters_type) output_parameters_total

real(8), dimension(:), allocatable :: alpha_vals, beta_vals, gamma_vals
! real(8) max_area, max_edge_length
integer my_rank
integer seed(1:8)
character(len=255) cache_dir ! cached files directory (if job stops early)
integer(8), dimension(:), allocatable :: remaining_orients
integer(8) num_remaining_orients

! ############################################################################################################
! start main
print*,'========== start main'
start = omp_get_wtime()
my_rank = 0
loop_start = 1
seed = [0, 0, 0, 0, 0, 0, 0, 0] ! Set the seed values

call parse_command_line(job_params)

if(job_params%resume) then
    print*,'attempting to resume job using cache #',job_params%cache_id
    call resume_job(job_params,num_remaining_orients,remaining_orients,mueller_total,mueller_1d_total,output_parameters_total)
    ! stop
end if

call make_dir(job_params%job_name,output_dir)
print*,'output directory is "',trim(output_dir),'"'
call system("mkdir "//trim(output_dir)//"/tmp") ! make directory for temp files
print*,'temporary files directory is "',trim(output_dir)//"/tmp/",'"'
open(101,file=trim(output_dir)//"/"//"log") ! open global non-standard log file for important records

! write job parameters to log file
call write_job_params(job_params)

! get input particle geometry
call PDAL2( job_params,     & ! <-  job parameters
            geometry)         !  -> particle geometry            


if (job_params%tri) then
    print*,'calling triangulate with max edge length: ',job_params%tri_edge_length
    print*,'================================='
    call triangulate(   job_params%tri_edge_length, &
                        '-Q -q', &
                        job_params%tri_roughness, &
                        my_rank, &
                        output_dir, &
                        geometry) ! triangulate the particle
    ! call merge_vertices(vert_in, face_ids, 1D-1, geometry) ! merge vertices that are close enough
    ! call fix_collinear_vertices(vert_in, face_ids, num_vert, num_face, num_face_vert, apertures)
    ! max_edge_length = job_params%la*2
    ! max_area = max_edge_length**2*sqrt(3D0)/4D0
    ! print*,'area threshold: ',max_area
    ! call triangulate(vert_in,face_ids,num_vert,num_face,num_face_vert,max_area,'-Q -q',apertures,0D0) ! retriangulate the particle to have no area greater than threshold
    print*,'================================='
end if

! write unrotated particle to file (optional)            
call PDAS(  output_dir,     & ! <-  output directory
            "unrotated",    & ! <-  filename
            geometry)

call RANDOM_SEED(put=seed) ! Set the seed for the random number generator
call init_loop( alpha_vals, &
                beta_vals, &
                gamma_vals, &
                job_params)

if(job_params%output_eulers) then
    call output_eulers(alpha_vals,beta_vals,gamma_vals,output_dir,job_params)
end if
! stop
! some stuff
if(job_params%resume) then ! if we are resuming a cached job
    ! do nothing because we've already read this in sr resume_job
else ! if we are not resuming a cached job
    num_remaining_orients = job_params%num_orients ! number of remaining orientations is the total overall
    allocate(remaining_orients(1:num_remaining_orients)) ! allocate array to hold orientation numbers
    do i = 1, num_remaining_orients
        remaining_orients(i) = i
    end do
end if

do i = 1, num_remaining_orients

    i_loop = remaining_orients(i)

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
        call PDAS(  output_dir,     & ! <-  output directory
                    "rotated",    & ! <-  filename
                    rotated_geometry)
    end if

    ! fast implementation of the incident beam
    call makeIncidentBeam(  rotated_geometry,          & ! <-  unique vertices
                            beam_geometry, &
                            beam_inc)

    ! beam loop
    call beam_loop( beam_outbeam_tree,         & !  -> outgoing beams from the beam tracing
                    beam_outbeam_tree_counter, & !  -> counts the current number of beam outbeams
                    ext_diff_outbeam_tree,     & !  -> outgoing beams from external diffraction
                    output_parameters,         & !  -> adds illuminated geometric cross section to output parameters
                    job_params,                 &
                    rotated_geometry, &
                    beam_geometry, &
                    beam_inc)
    
    if(num_remaining_orients .gt. 1) then ! print progress for this job
        print'(A25,I8,A3,I8,A20,f8.4,A3)','orientations completed: ',i-1,' / ',num_remaining_orients,' (total progress: ',dble(i-1)/dble(num_remaining_orients)*100,' %)'
        ! print*,'total time elapsed: ',omp_get_wtime()-start
        ! print*,'average time per rotation: ',(omp_get_wtime()-start) / dble(i)
        if(job_params%timing) then
            if (i .gt. 1) then
                print'(A20,F12.4,A5)','est. time remaining: '
                call PROUST(nint(dble(num_remaining_orients-i+1)*(omp_get_wtime()-start) / dble(i)))
            end if      
        end if
    end if

    ! diffraction
    call diff_main( beam_outbeam_tree,         & ! <-  outgoing beams from the beam tracing
                    beam_outbeam_tree_counter, & ! <-  counts the current number of beam outbeams
                    ampl_far_beam11,           & !  -> amplitude matrix (1,1) due to beam diffraction
                    ampl_far_beam12,           & !  -> amplitude matrix (1,2) due to beam diffraction
                    ampl_far_beam21,           & !  -> amplitude matrix (2,1) due to beam diffraction
                    ampl_far_beam22,           & !  -> amplitude matrix (2,2) due to beam diffraction
                    ext_diff_outbeam_tree,     & ! <-  outgoing beams from external diffraction
                    ampl_far_ext_diff11,       & !  -> amplitude matrix (1,1) due to external diffraction
                    ampl_far_ext_diff12,       & !  -> amplitude matrix (1,2) due to external diffraction
                    ampl_far_ext_diff21,       & !  -> amplitude matrix (2,1) due to external diffraction
                    ampl_far_ext_diff22,       & !  -> amplitude matrix (2,2) due to external diffraction
                    job_params)
    ! stop
    call finalise(  ampl_far_beam11,     & ! <-  amplitude matrix (1,1) due to beam diffraction
                    ampl_far_beam12,     & ! <-  amplitude matrix (1,2) due to beam diffraction
                    ampl_far_beam21,     & ! <-  amplitude matrix (2,1) due to beam diffraction
                    ampl_far_beam22,     & ! <-  amplitude matrix (2,2) due to beam diffraction
                    ampl_far_ext_diff11, & ! <-  amplitude matrix (1,1) due to external diffraction
                    ampl_far_ext_diff12, & ! <-  amplitude matrix (1,2) due to external diffraction
                    ampl_far_ext_diff21, & ! <-  amplitude matrix (2,1) due to external diffraction
                    ampl_far_ext_diff22, & ! <-  amplitude matrix (2,2) due to external diffraction
                    mueller,             & !  -> 2d mueller matrix
                    mueller_1d,          & !  -> 1d mueller matrix
                    output_parameters,   & !  -> some output parameters
                    job_params)
                    
    ! call writeup(mueller, mueller_1d, theta_vals, phi_vals) ! write current mueller to file

    call summation(mueller, mueller_total, mueller_1d, mueller_1d_total,output_parameters,output_parameters_total)

    if((omp_get_wtime() - start)/3600D0 .gt. job_params%time_limit) then
        call make_cache_dir("cache/",cache_dir)
        call cache_remaining_orients_seq(cache_dir,i,num_remaining_orients,remaining_orients)
        call cache_job( job_params,                 & ! job parameters
                        i_loop,                     & ! current loop index
                        output_parameters_total,    & ! total output parameters
                        mueller_total,              & ! total 2d mueller
                        mueller_1d_total,           & ! total 1d mueller
                        cache_dir,                  &
                        geometry)
        stop
    end if

end do

! divide by no. of orientations
mueller_total = mueller_total / job_params%num_orients 
mueller_1d_total = mueller_1d_total / job_params%num_orients 
output_parameters_total%abs = output_parameters_total%abs / job_params%num_orients 
output_parameters_total%scatt = output_parameters_total%scatt / job_params%num_orients 
output_parameters_total%ext = output_parameters_total%ext / job_params%num_orients 
output_parameters_total%albedo = output_parameters_total%albedo / job_params%num_orients 
output_parameters_total%asymmetry = output_parameters_total%asymmetry / job_params%num_orients 
output_parameters_total%abs_eff = output_parameters_total%abs_eff / job_params%num_orients 
output_parameters_total%scatt_eff = output_parameters_total%scatt_eff / job_params%num_orients 
output_parameters_total%ext_eff = output_parameters_total%ext_eff / job_params%num_orients 
output_parameters_total%geo_cross_sec = output_parameters_total%geo_cross_sec / job_params%num_orients 
output_parameters_total%back_scatt = output_parameters_total%back_scatt / job_params%num_orients 

! writing to file
call write_outbins(output_dir,job_params%theta_vals,job_params%phi_vals)
call writeup(mueller_total, mueller_1d_total, output_dir, output_parameters_total, job_params) ! write to file

! clean up temporary files
call system("rm -r "//trim(output_dir)//"/tmp") ! remove directory for temp files

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
