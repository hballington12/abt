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
use mpi_pbt_mod

implicit none

! to do:
! add support to avoid crash if nan detected

! ########## variable declaration ##########

! shared
real(8) start, finish ! cpu timing variables
integer(8) i, j, k, i_loop ! counters

! input
character(len=*), parameter :: ifn = 'input.txt' ! input filename
character(len=255) :: my_rank_str ! string of my rank
character(len=255) :: my_log_dir ! log file location for each mpi process
integer result ! true if subdirectory was made, false if subdirectory was not made

! sr SDATIN
type(cc_hex_params_type) cc_hex_params ! parameters for C. Collier Gaussian Random hexagonal columns/plates
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
complex(8), dimension(:,:,:,:), allocatable:: ampl_far_beam ! total
real(8), dimension(:), allocatable :: theta_vals, phi_vals
complex(8), dimension(:,:,:,:), allocatable :: ampl_far_ext_diff ! total

! sr make_mueller
real(8), dimension(:,:,:), allocatable :: mueller, mueller_total, mueller_recv ! mueller matrices
real(8), dimension(:,:), allocatable :: mueller_1d, mueller_1d_total, mueller_1d_recv ! phi-integrated mueller matrices

! mpi (some of this is unused)
integer source, dest
integer status(MPI_STATUS_SIZE)
integer my_start, my_end, my_rank
real(8), dimension(:), allocatable :: alpha_vals, beta_vals, gamma_vals
logical i_finished_early, other_finished_early
logical is_job_cached
integer my_iter, total_iter, recv_iter, iter_done, iter_to_collect
integer send_request, recv_request
logical sent_request, cont
type(mpi_params) mpi

! sr finalise
type(output_parameters_type) output_parameters 
type(output_parameters_type) output_parameters_recv
type(output_parameters_type) output_parameters_total

real(8) max_area, max_edge_length
integer seed(1:8)
character(len=255) cache_dir ! cached files directory (if job stops early)
integer(8), dimension(:), allocatable :: remaining_orients
integer(8) num_remaining_orients

! ########## start ##########

! init mpi
call MPI_INIT(mpi%ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, mpi%rank, mpi%ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, mpi%p, mpi%ierr)

if(mpi%rank == 0) print*,'========== start pbt: mpi'
start = omp_get_wtime() ! get start time

! initialise some other stuff
mpi%tag = 1
total_iter = 0
mpi%flag = .true.
sent_request = .false.
i_finished_early = .false.
is_job_cached = .false.
iter_done = 0
seed = [0, 0, 0, 0, 0, 0, 0, 0] ! set the seed values

! print the command used to execute the program
if(mpi%rank == 0) call print_command()

! parse command line
call parse_command_line(job_params)

if(job_params%resume) then ! if resuming a cached job
    if(mpi%rank == 0) print*,'attempting to resume job using cache #',job_params%cache_id
    ! get cached data and continue
    call resume_job(job_params,num_remaining_orients,remaining_orients,mueller_total,mueller_1d_total,output_parameters_total)
    if(mpi%rank /= 0) then ! only rank 0 should keep summed parameters from the cache, reset vals for all other processes
        deallocate(mueller_total)
        deallocate(mueller_1d_total)
    end if
end if ! end if resuming a cached job

! set up job directory
if (mpi%rank .eq. 0) then ! if rank 0 process
    call make_dir(job_params%job_name,job_params%output_dir) ! get an appropriate name for the job directory
    print*,'output directory is "',trim(job_params%output_dir),'"'
    call system("mkdir "//trim(job_params%output_dir)//"/logs")
    call system("mkdir "//trim(job_params%output_dir)//"/tmp") ! make directory for temp files
end if ! end if rank 0 process

! synchronise the job directory across all processes
call mpi_send_output_dir(mpi,job_params)

if (mpi%rank .eq. 0) call write_job_params(job_params,job_params%output_dir) ! write job parameters to file

write(my_rank_str,*) mpi%rank
call StripSpaces(my_rank_str)
write(my_log_dir,*) trim(job_params%output_dir)//"/logs/log",trim(my_rank_str)
my_log_dir = adjustl(my_log_dir)

open(101,file=trim(my_log_dir),status="new") ! open global non-standard log file for important records

! get input particle information
call PDAL2( job_params, & ! <-  job parameters
            geometry)     !  -> particle geometry   

if (job_params%tri) then ! if triangulation enabled
    ! triangulate the particle
    call triangulate(   job_params,                 &
                        '-Q -q',                    & ! <-  extra flag inputs to triangle program
                        job_params%tri_roughness,   & ! <-  standard deviation for triangle roughness
                        mpi%rank,                   & ! <-  process rank, so the subroutine knows where to write temporary files to
                        job_params%output_dir,      & ! <-  output directory
                        geometry)                     !  -> triangulated geometry
    ! call merge_vertices(vert_in, face_ids, num_vert, 1D-1) ! merge vertices that are close enough
    ! call fix_collinear_vertices(vert_in, face_ids, num_vert, num_face, num_face_vert, apertures)
end if

! write geometry info to log file
call write_geometry_info(geometry)

if (mpi%rank .eq. 0) then ! if rank 0 process
    ! write unrotated particle to file (optional)            
    call PDAS(  job_params%output_dir,  & ! <-  output directory
                "unrotated",            & ! <-  filename
                geometry)                 ! <-  geometry
end if ! end if rank 0 process

if (mpi%rank .eq. 0) then ! if rank 0 process
    call RANDOM_SEED(put=seed) ! Set the seed for the random number generator
    ! initialise the euler angles to be used
    call init_loop( alpha_vals, & !  -> alpha eulers (set to 0 unless single orientation)
                    beta_vals,  & !  -> beta eulers
                    gamma_vals, & !  -> gamma eulers
                    job_params)   ! <-  job parameters
    if(job_params%output_eulers) then ! if euler angle enabled with flag -output_eulers
        ! output euler angles to file
        call output_eulers(alpha_vals,beta_vals,gamma_vals,job_params%output_dir,job_params)
    end if
end if ! end if rank 0 process

! synchronise the euler angles across all processes
call mpi_send_euler_vals(mpi,job_params,alpha_vals,beta_vals,gamma_vals)

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

! distribute the workload of the orientation loop across processes
call mpi_get_start_end(mpi,num_remaining_orients)

if (mpi%rank .eq. 0) print*,'starting orientation loop...'

do i = mpi%start, mpi%end ! start orientation loop

    i_loop = remaining_orients(i) ! get loop index

    if(job_params%debug >= 3) then
        if(mpi%rank == 0) print*,'rotating particle...'
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

    ! fast implementation of the incident beam
    call make_incident_beam(rotated_geometry,   & ! <-  geometry
                            beam_geometry,      & !  -> beam geometry
                            beam_inc)             !  -> incident beam

    if(job_params%debug >= 3) then
        if(mpi%rank == 0) print*,'computing near-field...'
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

    if(job_params%debug >= 3) then
        if(mpi%rank == 0) print*,'computing far-field...'
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
        if(mpi%rank == 0) print*,'computing mueller matrix and parameters...'
        write(101,*)'computing mueller matrix and parameters...'
    end if

    call finalise(  ampl_far_beam,      & ! <-  amplitude matrix due to beam diffraction
                    ampl_far_ext_diff,  & ! <-  amplitude matrix due to external diffraction
                    mueller,            & !  -> 2d mueller matrix
                    mueller_1d,         & !  -> 1d mueller matrix
                    output_parameters,  & !  -> some output parameters
                    job_params)           ! <-  job parameters

    ! sum the total mueller and output parameters
    call summation(mueller, mueller_total, mueller_1d, mueller_1d_total,output_parameters,output_parameters_total)

    if((omp_get_wtime() - start)/3600D0 .gt. job_params%time_limit .and. i /= mpi%end) then
        i_finished_early = .true. ! set logical which exits loop and starts caching routine
        if(mpi%rank == 0) print*,'time limit reached. caching job...'
    end if

    my_iter = int(i,kind=4) - mpi%start + 1 ! save progress for this rank
    total_iter = total_iter + 1 ! sum total progress
    num_remaining_orients = mpi%end - mpi%start + 1

    ! keep track of the progress across all mpi processes
    ! call mpi_track_progress(mpi,iter_done,total_iter,num_remaining_orients,sent_request,my_iter,recv_request,recv_iter)

    if(mpi%rank == 0) then
        if(num_remaining_orients > 1 .and. mod(total_iter,10) == 0) then
            print'(A,I8,A3,I8,A20,f8.4,A3)','orientation (rank 0): ',total_iter,' / ',num_remaining_orients,' (total progress: ',dble(total_iter)/dble(num_remaining_orients)*100,' %)'
            if(job_params%debug >= 2) then
                write(*,'(A,f10.2,A)') 'total time elapsed: ',omp_get_wtime()-start,' secs'
                write(*,'(A,f10.2,A)') 'average time per rotation: ',(omp_get_wtime()-start) / dble(total_iter+1),' secs'
            end if
            if(job_params%timing) then
                if (total_iter > 1) then 
                    print'(A20,F12.4,A5)','est. time remaining: '
                    call PROUST(nint(dble(num_remaining_orients-total_iter)*(omp_get_wtime()-start) / dble(total_iter+1)))
                end if
            end if
        end if
    end if

    if(i_finished_early) exit

end do ! end orientation loop

! wait for and collect progress from other processes
! call mpi_track_progress_cleanup(mpi,iter_done,total_iter,num_remaining_orients,sent_request,my_iter,recv_request,recv_iter,job_params, start)

call MPI_BARRIER(MPI_COMM_WORLD,mpi%ierr)

if (mpi%rank .eq. 0) print*,'end orientation loop'

! all processes send whether or not they have finished
if (mpi%rank .eq. 0) then ! if my rank is 0
    if(i_finished_early) is_job_cached = .true. ! if rank 0 didnt finish, the job is set to be cached
    do dest = 1, mpi%p-1 ! receive from each of the other processes
        call MPI_RECV(other_finished_early, 1, MPI_LOGICAL, dest, mpi%tag, MPI_COMM_WORLD,mpi%status, mpi%ierr)
        if(other_finished_early) is_job_cached = .true. ! if another process didnt finish, the job is set to be cached
    end do
else
    call MPI_SEND(i_finished_early, 1, MPI_LOGICAL, 0, mpi%tag, MPI_COMM_WORLD,mpi%status, mpi%ierr)
end if

call MPI_BARRIER(MPI_COMM_WORLD,mpi%ierr)

call MPI_Bcast(is_job_cached, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, mpi%ierr) ! broadcast job cache decision to all processes from rank 0

if(is_job_cached) then ! if the job is to be cached

    ! rank 0 makes cache directory and sends to other processes
    if (mpi%rank .eq. 0) then
        call make_cache_dir("cache/",cache_dir)
        print*,'cache directory is "',trim(cache_dir),'"'
        do dest = 1, mpi%p-1
            CALL MPI_SEND(cache_dir, 255, MPI_CHARACTER, dest, mpi%tag, MPI_COMM_WORLD, mpi%ierr)
        end do
    else
        call MPI_RECV(cache_dir,255,MPI_CHARACTER,0,mpi%tag,MPI_COMM_WORLD,mpi%status,mpi%ierr)
    end if

    ! make remaining orientations file with rank 0 process
    if(mpi%rank .eq. 0) then
        open(unit=10,file=trim(cache_dir)//"/orient_remaining.dat",status="new")
        close(10)
    end if
    ! 1 by 1, write remaining orientations to file...
    do j = 1, mpi%p ! for each process
        if(mpi%rank .eq. j-1) then ! if its my turn to write to the file
            if(i_finished_early) then ! if my rank didnt finish
                open(unit=10,file=trim(cache_dir)//"/orient_remaining.dat",status="old",access="append") ! open file in append mode
                    ! write my remaining orientations to the file
                    do k = i+1, mpi%end ! loop from one after my current loop index to my end index
                        write(10,*) remaining_orients(k) ! write my remaining orientations to file
                    end do
                ! print*,'mpi%rank:',mpi%rank,'wrote to file'
                close(10) ! close file
            end if
            call MPI_BARRIER(MPI_COMM_WORLD,mpi%ierr) ! meet other processes at meet point
        else ! if its not my turn to write to file
            call MPI_BARRIER(MPI_COMM_WORLD,mpi%ierr) ! do nothing and wait at meet point
        end if
    end do

    ! send to rank 0 process and sum
    call mpi_send_sum(  mpi%ierr,                       & ! mpi parameter
                        mpi%tag,                        & ! mpi parameter
                        mpi%p,                          & ! mpi parameter
                        mpi%status,                     & ! mpi parameter
                        mpi%rank,                    & ! process rank
                        mueller_1d_total,           & ! 1d mueller matrix total for each process
                        mueller_total,              & ! 2d mueller matrix total for each process
                        output_parameters_total)      ! output parameters total for each process

    ! then rank 0 writes to cached files
    if(mpi%rank .eq. 0) then
        call cache_job( job_params,                 & ! job parameters
                        i_loop,                     & ! current loop index
                        output_parameters_total,    & ! total output parameters
                        mueller_total,              & ! total 2d mueller
                        mueller_1d_total,           & ! total 1d mueller
                        cache_dir,                  &
                        geometry)
    end if

else ! else, if the job is not to be cached

    ! sum up across all mpi processes
    call mpi_send_sum(  mpi%ierr,                       & ! mpi parameter
                        mpi%tag,                        & ! mpi parameter
                        mpi%p,                          & ! mpi parameter
                        mpi%status,                     & ! mpi parameter
                        mpi%rank,                    & ! process rank
                        mueller_1d_total,           & ! 1d mueller matrix total for each process
                        mueller_total,              & ! 2d mueller matrix total for each process
                        output_parameters_total)      ! output parameters total for each process


    if (mpi%rank .eq. 0) then

        call divide_by_num_orientations(mueller_total,mueller_1d_total,output_parameters_total,job_params)
        
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
    end if

    close(101) ! close global non-standard output file

end if

call MPI_FINALIZE(mpi%ierr)

contains

end program main
