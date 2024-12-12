! mpi_pbt_mod.f90
! module containing various subroutines related to MPI
! also loads the mpif.h file

module mpi_pbt_mod

use types_mod
use omp_lib
use misc_submod

implicit none

include 'mpif.h'

type mpi_params
    ! mpi_params: contains information about the mpi parameters for each process
    ! this is declared here, rather than in the types module, because the types module should
    !   not be dependent on mpi implementation
    integer ierr ! fortran error code
    integer tag ! tag
    integer p ! number of processes
    logical flag ! flag
    integer rank ! my rank
    integer request ! request for non-blocking receive
    integer status(MPI_STATUS_SIZE)
    integer start ! orientation averaging loop start index
    integer end ! orientation averaging loop end index
end type mpi_params

contains



subroutine mpi_track_progress_cleanup(mpi,iter_done,total_iter,num_remaining_orients,sent_request,my_iter,recv_request,recv_iter,job_params, start)

    ! keeps track of the progress across all mpi processes after rank 0 has finished
    ! uses non-blocking send/receive routines to minimise slow-down

    type(mpi_params), intent(in) :: mpi
    integer, intent(inout) :: iter_done
    integer, intent(inout) :: total_iter
    integer, intent(in) :: my_iter
    integer(8), intent(in) :: num_remaining_orients
    logical, intent(inout) :: sent_request
    integer, intent(inout) :: recv_request
    integer, intent(inout) :: recv_iter
    type(job_parameters_type), intent(in) :: job_params
    real(8), intent(in) :: start

    integer send_request
    integer iter_to_collect
    logical cont
    

    integer, parameter :: update_every = 1 ! increase this to reduce the number of mpi sends

    ! Process 1 posts a non-blocking receive
    if (mpi%rank == 0) then
        print*,'rank 0 finished, waiting for other processes to finish...'
        iter_to_collect = num_remaining_orients-total_iter-(mpi%end-my_iter) ! progress left to collect
        ! if receive request exists, but not yet received, wait
        if(.not. mpi%flag) then
            call MPI_WAIT(recv_request, MPI_STATUS_IGNORE, mpi%ierr) ! wait for current receive to finish
            total_iter = total_iter + recv_iter ! update total progress
            iter_to_collect = num_remaining_orients-total_iter-(mpi%end-my_iter) ! progress left to collect
        end if
        do while (iter_to_collect > 0 .and. (omp_get_wtime() - start)/3600D0 .lt. job_params%time_limit ) ! while there is stuff left to receive, and the time limit has not been reached
            call MPI_IRECV(recv_iter, 1, MPI_INTEGER, MPI_ANY_SOURCE, mpi%tag, MPI_COMM_WORLD, recv_request, mpi%ierr)
            call MPI_WAIT(recv_request, MPI_STATUS_IGNORE, mpi%ierr) ! wait for current receive to finish
            total_iter = total_iter + recv_iter ! update total progress
            iter_to_collect = num_remaining_orients-total_iter-(mpi%end-my_iter) ! progress left to collect

            if(num_remaining_orients > 1 .and. mpi%rank == 0 .and. total_iter > 1) then
                print'(A15,I8,A3,I8,A20,f8.4,A3)','orientation: ',total_iter,' / ',num_remaining_orients,' (total progress: ',dble(total_iter)/dble(num_remaining_orients)*100,' %)'
                ! print*,'total time elapsed: ',omp_get_wtime()-start
                ! print*,'average time per rotation: ',(omp_get_wtime()-start) / dble(i_loop)
                if (total_iter .gt. 1) then 
                    print'(A20,F12.4,A5)','est. time remaining: '
                    call PROUST(nint(dble(num_remaining_orients-total_iter)*(omp_get_wtime()-start) / dble(total_iter+1)))
                end if
            end if
        end do ! end receive loop
    end if

end subroutine

subroutine mpi_track_progress(mpi,iter_done,total_iter,num_remaining_orients,sent_request,my_iter,recv_request,recv_iter)

    ! keeps track of the progress across all mpi processes
    ! uses non-blocking send/receive routines to minimise slow-down

    type(mpi_params), intent(in) :: mpi
    integer, intent(inout) :: iter_done
    integer, intent(inout) :: total_iter
    integer, intent(in) :: my_iter
    integer(8), intent(in) :: num_remaining_orients
    logical, intent(inout) :: sent_request
    integer, intent(inout) :: recv_request
    integer, intent(inout) :: recv_iter

    integer send_request
    integer iter_to_collect
    logical cont

    integer, parameter :: update_every = 1 ! increase this to reduce the number of mpi sends

    if (mpi%rank /= 0) then ! if not rank 0 process
        iter_done = iter_done + 1 ! update amount of work done
        if(mod(iter_done,update_every) == 0 .or. my_iter == (mpi%end - mpi%start + 1)) then
            ! send work done to rank 0
            call MPI_ISEND(iter_done, 1, MPI_INTEGER, 0, mpi%tag, MPI_COMM_WORLD, send_request, mpi%ierr)
            iter_done = 0
        end if
    end if

    ! Process 1 posts a non-blocking receive
    if (mpi%rank == 0) then ! if rank 0 process
        cont = .false. ! re-init for this orientation
        iter_to_collect = num_remaining_orients-total_iter-(mpi%end-my_iter) ! .calc progress left to collect
        do while (iter_to_collect > 0 .and. .not. cont) ! while there is progress to collect, and not ready to continue
            if(.not. sent_request) then ! if no send request has yet to be sent
                ! send a non-blocking request to receive progress
                call MPI_IRECV(recv_iter, 1, MPI_INTEGER, MPI_ANY_SOURCE, mpi%tag, MPI_COMM_WORLD, recv_request, mpi%ierr)
                sent_request = .true. ! receive request has been sent
            end if
            ! test for received data
            call MPI_TEST(recv_request, mpi%flag, MPI_STATUS_IGNORE, mpi%ierr)
            if(mpi%flag) then ! if data has been received
                sent_request = .false. ! keep track, so that a new request can be made to receive progress
                total_iter = total_iter + recv_iter ! update total progress
                iter_to_collect = num_remaining_orients-total_iter-(mpi%end-my_iter) ! calc progress left to collect
            else ! else, if no data was receive
                cont = .true. ! continue with our own work
            end if
        end do
    end if

end subroutine

subroutine mpi_get_start_end(mpi,num_remaining_orients)

    ! distributes the workload of the orientation loop across processes
    ! cant remember the exact maths behind this but it was designed so that
    !   the leftover angles arent just dumped onto 1 process

    type(mpi_params), intent(inout) :: mpi
    integer(8), intent(in) :: num_remaining_orients

    integer n1, n2

    if(mpi%p > num_remaining_orients) error stop "error: number of mpi processes must not be greater than number of orientations"
    n1 = int(num_remaining_orients / mpi%p)
    n2 = mod(num_remaining_orients,  mpi%p)
    mpi%start = mpi%rank*(n1+1)+1
    mpi%end = (mpi%rank+1)*(n1+1)
    if (mpi%rank .ge. n2) then
        mpi%start = mpi%start - mpi%rank + n2
        mpi%end = mpi%end - mpi%rank + n2 - 1
    end if

    write(101,*)'starting orientation loop... start: ',mpi%start,'end: ',mpi%end


end subroutine

subroutine mpi_send_euler_vals(mpi,job_params,alpha_vals,beta_vals,gamma_vals)

    ! sends the values of the euler angles from rank 0 to all other processes
    ! rank 0 process always generates the initial set of euler angles to prevent
    !   processes producing the same "random" angles due to computer architecture

    type(job_parameters_type), intent(in) :: job_params
    type(mpi_params), intent(in) :: mpi
    real(8), dimension(:), allocatable, intent(inout) :: alpha_vals, beta_vals, gamma_vals

    integer dest

    if (mpi%rank .eq. 0) then
        do dest = 1, mpi%p-1
            call MPI_SEND(alpha_vals,size(alpha_vals,1),MPI_REAL8,dest,mpi%tag,MPI_COMM_WORLD,mpi%ierr)
            call MPI_SEND(beta_vals,size(beta_vals,1),MPI_REAL8,dest,mpi%tag,MPI_COMM_WORLD,mpi%ierr)
            call MPI_SEND(gamma_vals,size(gamma_vals,1),MPI_REAL8,dest,mpi%tag,MPI_COMM_WORLD,mpi%ierr)
        end do
    else
        allocate(alpha_vals(1:job_params%num_orients))
        allocate(beta_vals(1:job_params%num_orients))
        allocate(gamma_vals(1:job_params%num_orients))
        call MPI_RECV(alpha_vals,size(alpha_vals,1),MPI_REAL8,0,mpi%tag,MPI_COMM_WORLD,mpi%status,mpi%ierr)
        call MPI_RECV(beta_vals,size(beta_vals,1),MPI_REAL8,0,mpi%tag,MPI_COMM_WORLD,mpi%status,mpi%ierr)
        call MPI_RECV(gamma_vals,size(gamma_vals,1),MPI_REAL8,0,mpi%tag,MPI_COMM_WORLD,mpi%status,mpi%ierr)
    end if

end subroutine

subroutine mpi_send_cache_dir(mpi,cache_dir)

    ! sends the string of the cache directory from rank 0 to all other processes
    character(len=255), intent(inout) :: cache_dir
    type(mpi_params), intent(in) :: mpi

    integer dest

    ! rank 0 process broadcasts job directory to other processes
    if (mpi%rank .eq. 0) then
        do dest = 1, mpi%p-1
            CALL MPI_SEND(cache_dir, 255, MPI_CHARACTER, dest, mpi%tag, MPI_COMM_WORLD, mpi%ierr)
        end do
    else
        call MPI_RECV(cache_dir,255,MPI_CHARACTER,0,mpi%tag,MPI_COMM_WORLD,mpi%status,mpi%ierr)
    end if 

end subroutine

subroutine mpi_send_output_dir(mpi,job_params)

    ! sends the string of the output directory from rank 0 to all other processes
    type(job_parameters_type), intent(inout) :: job_params
    type(mpi_params), intent(in) :: mpi

    integer dest

    ! rank 0 process broadcasts job directory to other processes
    if (mpi%rank .eq. 0) then
        do dest = 1, mpi%p-1
            CALL MPI_SEND(job_params%output_dir, 255, MPI_CHARACTER, dest, mpi%tag, MPI_COMM_WORLD, mpi%ierr)
        end do
    else
        call MPI_RECV(job_params%output_dir,255,MPI_CHARACTER,0,mpi%tag,MPI_COMM_WORLD,mpi%status,mpi%ierr)
    end if    

end subroutine

subroutine mpi_send_sum(ierr,                       & ! mpi parameter
                        tag,                        & ! mpi parameter
                        p,                          & ! mpi parameter
                        status,                     & ! mpi parameter
                        my_rank,                    & ! process rank
                        mueller_1d_total,           & ! 1d mueller matrix total for each process
                        mueller_total,              & ! 2d mueller matrix total for each process
                        output_parameters_total)      ! output parameters total for each process

    ! sends all stuff to rank 0 process, which then sums
 
    real(8), dimension(:,:), allocatable, intent(inout) :: mueller_1d_total ! phi-integrated mueller matrices
    real(8), dimension(:,:,:), allocatable, intent(inout) :: mueller_total ! mueller matrices
    type(output_parameters_type), intent(inout) :: output_parameters_total
    integer, intent(in) :: ierr
    integer, intent(in) :: tag
    integer, intent(in) :: p
    integer, intent(in) :: status(MPI_STATUS_SIZE)
    integer, intent(in) :: my_rank

    real(8), dimension(:,:), allocatable :: mueller_1d_recv ! phi-integrated mueller matrices
    real(8), dimension(:,:,:), allocatable :: mueller_recv ! mueller matrices
    type(output_parameters_type) output_parameters_recv
    integer source, dest

    if (my_rank .ne. 0) then ! if not rank 0 process, send mueller to rank 0
        call MPI_SEND(mueller_1d_total,size(mueller_1d_total,1)*size(mueller_1d_total,2),MPI_REAL8,0,tag,MPI_COMM_WORLD,ierr)
        call MPI_SEND(mueller_total,size(mueller_total,1)*size(mueller_total,2)*size(mueller_total,3),MPI_REAL8,0,tag,MPI_COMM_WORLD,ierr)
        call MPI_SEND(output_parameters_total%abs,1,MPI_REAL8,0,tag,MPI_COMM_WORLD,ierr)
        call MPI_SEND(output_parameters_total%scatt,1,MPI_REAL8,0,tag,MPI_COMM_WORLD,ierr)
        call MPI_SEND(output_parameters_total%ext,1,MPI_REAL8,0,tag,MPI_COMM_WORLD,ierr)
        call MPI_SEND(output_parameters_total%albedo,1,MPI_REAL8,0,tag,MPI_COMM_WORLD,ierr)
        call MPI_SEND(output_parameters_total%asymmetry,1,MPI_REAL8,0,tag,MPI_COMM_WORLD,ierr)
        call MPI_SEND(output_parameters_total%abs_eff,1,MPI_REAL8,0,tag,MPI_COMM_WORLD,ierr)
        call MPI_SEND(output_parameters_total%scatt_eff,1,MPI_REAL8,0,tag,MPI_COMM_WORLD,ierr)
        call MPI_SEND(output_parameters_total%ext_eff,1,MPI_REAL8,0,tag,MPI_COMM_WORLD,ierr)
        call MPI_SEND(output_parameters_total%geo_cross_sec,1,MPI_REAL8,0,tag,MPI_COMM_WORLD,ierr)
        call MPI_SEND(output_parameters_total%back_scatt,1,MPI_REAL8,0,tag,MPI_COMM_WORLD,ierr)

        call MPI_SEND(output_parameters_total%scatt_beam,1,MPI_REAL8,0,tag,MPI_COMM_WORLD,ierr)
        call MPI_SEND(output_parameters_total%scatt_ext_diff,1,MPI_REAL8,0,tag,MPI_COMM_WORLD,ierr)
        call MPI_SEND(output_parameters_total%asymmetry_beam,1,MPI_REAL8,0,tag,MPI_COMM_WORLD,ierr)
        call MPI_SEND(output_parameters_total%asymmetry_ext_diff,1,MPI_REAL8,0,tag,MPI_COMM_WORLD,ierr)
        call MPI_SEND(output_parameters_total%scatt_eff_beam,1,MPI_REAL8,0,tag,MPI_COMM_WORLD,ierr)
        call MPI_SEND(output_parameters_total%scatt_eff_ext_diff,1,MPI_REAL8,0,tag,MPI_COMM_WORLD,ierr)

        ! print*,'sent to rank 0. my rank = ',my_rank
    else ! if rank 0 process, receieve from all other ranks
        ! allocate some arrays to hold the received values
        print*,'start mpi summation...'
        allocate(mueller_1d_recv(1:size(mueller_1d_total,1),1:size(mueller_1d_total,2)))
        allocate(mueller_recv(1:size(mueller_total,1),1:size(mueller_total,2),1:size(mueller_total,3)))
        do source = 1,p-1 ! collect from other processes
            ! print*,'attempting to reveive from ',source,' my rank = ',my_rank
            call MPI_RECV(mueller_1d_recv,size(mueller_1d_recv,1)*size(mueller_1d_recv,2),MPI_REAL8,source,tag,MPI_COMM_WORLD,status,ierr)
            mueller_1d_total = mueller_1d_total + mueller_1d_recv ! sum
            call MPI_RECV(mueller_recv,size(mueller_recv,1)*size(mueller_recv,2)*size(mueller_recv,3),MPI_REAL8,source,tag,MPI_COMM_WORLD,status,ierr)
            mueller_total = mueller_total + mueller_recv ! sum        
            call MPI_RECV(output_parameters_recv%abs,1,MPI_REAL8,source,tag,MPI_COMM_WORLD,status,ierr)
            output_parameters_total%abs = output_parameters_total%abs + output_parameters_recv%abs ! sum  
            call MPI_RECV(output_parameters_recv%scatt,1,MPI_REAL8,source,tag,MPI_COMM_WORLD,status,ierr)
            output_parameters_total%scatt = output_parameters_total%scatt + output_parameters_recv%scatt ! sum  
            call MPI_RECV(output_parameters_recv%ext,1,MPI_REAL8,source,tag,MPI_COMM_WORLD,status,ierr)
            output_parameters_total%ext = output_parameters_total%ext + output_parameters_recv%ext ! sum  
            call MPI_RECV(output_parameters_recv%albedo,1,MPI_REAL8,source,tag,MPI_COMM_WORLD,status,ierr)
            output_parameters_total%albedo = output_parameters_total%albedo + output_parameters_recv%albedo ! sum  
            call MPI_RECV(output_parameters_recv%asymmetry,1,MPI_REAL8,source,tag,MPI_COMM_WORLD,status,ierr)
            output_parameters_total%asymmetry = output_parameters_total%asymmetry + output_parameters_recv%asymmetry ! sum  
            call MPI_RECV(output_parameters_recv%abs_eff,1,MPI_REAL8,source,tag,MPI_COMM_WORLD,status,ierr)
            output_parameters_total%abs_eff = output_parameters_total%abs_eff + output_parameters_recv%abs_eff ! sum  
            call MPI_RECV(output_parameters_recv%scatt_eff,1,MPI_REAL8,source,tag,MPI_COMM_WORLD,status,ierr)
            output_parameters_total%scatt_eff = output_parameters_total%scatt_eff + output_parameters_recv%scatt_eff ! sum  
            call MPI_RECV(output_parameters_recv%ext_eff,1,MPI_REAL8,source,tag,MPI_COMM_WORLD,status,ierr)
            output_parameters_total%ext_eff = output_parameters_total%ext_eff + output_parameters_recv%ext_eff ! sum  
            call MPI_RECV(output_parameters_recv%geo_cross_sec,1,MPI_REAL8,source,tag,MPI_COMM_WORLD,status,ierr)
            output_parameters_total%geo_cross_sec = output_parameters_total%geo_cross_sec + output_parameters_recv%geo_cross_sec ! sum  
            call MPI_RECV(output_parameters_recv%back_scatt,1,MPI_REAL8,source,tag,MPI_COMM_WORLD,status,ierr)
            output_parameters_total%back_scatt = output_parameters_total%back_scatt + output_parameters_recv%back_scatt ! sum  
            call MPI_RECV(output_parameters_recv%scatt_beam,1,MPI_REAL8,source,tag,MPI_COMM_WORLD,status,ierr)
            output_parameters_total%scatt_beam = output_parameters_total%scatt_beam + output_parameters_recv%scatt_beam ! sum   
            call MPI_RECV(output_parameters_recv%scatt_ext_diff,1,MPI_REAL8,source,tag,MPI_COMM_WORLD,status,ierr)
            output_parameters_total%scatt_ext_diff = output_parameters_total%scatt_ext_diff + output_parameters_recv%scatt_ext_diff ! sum   
            call MPI_RECV(output_parameters_recv%asymmetry_beam,1,MPI_REAL8,source,tag,MPI_COMM_WORLD,status,ierr)
            output_parameters_total%asymmetry_beam = output_parameters_total%asymmetry_beam + output_parameters_recv%asymmetry_beam ! sum   
            call MPI_RECV(output_parameters_recv%asymmetry_ext_diff,1,MPI_REAL8,source,tag,MPI_COMM_WORLD,status,ierr)
            output_parameters_total%asymmetry_ext_diff = output_parameters_total%asymmetry_ext_diff + output_parameters_recv%asymmetry_ext_diff ! sum   
            call MPI_RECV(output_parameters_recv%scatt_eff_beam,1,MPI_REAL8,source,tag,MPI_COMM_WORLD,status,ierr)
            output_parameters_total%scatt_eff_beam = output_parameters_total%scatt_eff_beam + output_parameters_recv%scatt_eff_beam ! sum   
            call MPI_RECV(output_parameters_recv%scatt_eff_ext_diff,1,MPI_REAL8,source,tag,MPI_COMM_WORLD,status,ierr)
            output_parameters_total%scatt_eff_ext_diff = output_parameters_total%scatt_eff_ext_diff + output_parameters_recv%scatt_eff_ext_diff ! sum  
            print*,'received data from rank: ',source
        end do
        print*,'end mpi summation.'
    end if

end subroutine

end module