program test
use input_mod
use types_mod

    implicit none

    type(job_parameters_type) job_params ! job parameters, contains wavelength, rbi, etc., see types mod for more details
    type(geometry_type) ref_geometry ! particle geometry data structure
    type(geometry_type) test_geometry ! particle geometry data structure

    call parse_command_line(job_params)

    job_params%debug = 3
    job_params%c_method = "read"
    job_params%cft = "obj"

    job_params%cfn = "../../cylinder1.obj" ! set cfn

    call PDAL2( job_params, &     ! <-  job parameters
                ref_geometry)         !  -> particle geometry            

    call PDAS(".","test",ref_geometry)
   
    ! job_params%cfn="jdskfnbkjsdfnbkl"
    call read_wavefront(job_params%cfn,test_geometry)

    call validate(ref_geometry,test_geometry)
    
contains

subroutine validate(ref,test)

    ! validate the geometry against the reference

    type(geometry_type), intent(in) :: ref, test
    integer i, j

    if (ref%nv /= test%nv) then
        print*,"nv:",ref%nv,test%nv
        stop "nv did not match"
    end if
    ! if (ref%nn /= test%nn) then
    !     print*,"nn:",ref%nn,test%nn
    !     stop "nn did not match"
    ! end if
    if (ref%nf /= test%nf) then
        print*,"nf:",ref%nf,test%nf
        stop "nf did not match"
    end if

    do i = 1, ref%nv
        do j = 1, 3
            if (ref%v(i,j) - test%v(i,j) > 1e-4) then
                print*,"i,j",i,j
                print*,"vertex=",ref%v(i,j),test%v(i,j)
                stop "vertex did not match"
            end if
        end do
    end do
    
    do i = 1, ref%nf
        do j = 1, ref%f(i)%nv
                if (ref%f(i)%vi(j) /= test%f(i)%vi(j)) then
                stop "face vertex did not match"
            end if
        end do
    end do
    
end subroutine


end program test