! input_mod.f90
! module containing various subroutines used to initiate aperture beam tracer

module input_mod

use misc_submod
use types_mod
use cc_hex_mod

implicit none

contains

subroutine validate_vertices(verts,norms,face_ids,num_face_vert,norm_ids)

    ! sr validate_vertices uses normals read from a .obj file to ensure that vertices
    ! are ordered in an anti-clockwise order as viewed from outside the particle

    ! for each face:
    ! rotate the face such that the normal is upward-facing
    ! compute the vector from the first vertex to each of the other vertices
    ! compute the angle between each vector and the positive x-axis
    ! order each vector by angle

    real(8), dimension(:,:) ,allocatable, intent(in) :: verts ! unique vertices
    real(8), dimension(:,:) ,allocatable, intent(in) :: norms ! unique vertices, face vertex IDs, face normals
    integer(8), dimension(:,:) ,allocatable, intent(inout) :: face_ids ! face vertex IDs (for after excess columns have been truncated)
    integer(8), dimension(:), allocatable, intent(in) :: num_face_vert ! number of vertices in each face
    integer(8), dimension(:), allocatable, intent(in) :: norm_ids ! face normal ID of each face

    integer(8) i, j ! counters
    integer(8) num_verts ! number of vertices in a given face
    real(8), dimension(:,:), allocatable :: vecs ! edge vectors for a given face
    real(8), dimension(:), allocatable :: angles ! edge vectors for a given face
    real(8), dimension(:,:), allocatable :: rotated_verts ! vertices of 1 face after rotating into xy plane with normal facing upwards
    real(8) v0(1:3,1:3) ! xyz coords of the first 3 vertices in the face
    real(8) rot(1:3,1:3) ! a rotation matrix
    real(8) com(1:3) ! centre of mass
    ! real(8) norm(1:3) ! face normal calculated from vertices
    real(8) rot_norm(1:3) ! rotated normal read from file
    integer, dimension(:), allocatable :: mapping ! mapping for quicksort algorithm
    integer(8), dimension(:), allocatable :: face_ids_temp ! temp array to hold face ids
    ! logical is_upwards

    ! print*,'======'

    allocate(face_ids_temp(1:maxval(num_face_vert)))

    do i = 1, size(face_ids,1) ! for each face
        
        num_verts = num_face_vert(i) ! get no. vertices in face
    
        face_ids_temp(1:num_verts) = face_ids(i,1:num_verts) ! save the face_ids temporarily

        if(allocated(vecs)) deallocate(vecs)
        allocate(vecs(1:num_verts-1,1:2)) ! allocate array to hold the vectors from the first vertex to each of the other vertices

        if(allocated(angles)) deallocate(angles)
        allocate(angles(1:num_verts-1)) ! allocate array to hold the vectors from the first vertex to each of the other vertices

        if(allocated(rotated_verts)) deallocate(rotated_verts)
        allocate(rotated_verts(1:num_verts,1:3))

        v0(1,1:3) = verts(face_ids(i,1),1:3) ! position of first vertex
        v0(2,1:3) = verts(face_ids(i,2),1:3) ! position of second vertex
        v0(3,1:3) = verts(face_ids(i,3),1:3) ! position of third vertex

        com = sum(v0,1)/3D0; ! get centre of mass

        v0(1,1:3) = v0(1,1:3) - com(1:3) ! move vertices to origin, ready for rotation matrix calc
        v0(2,1:3) = v0(2,1:3) - com(1:3)
        v0(3,1:3) = v0(3,1:3) - com(1:3)

        call get_rotation_matrix3(transpose(v0),rot) ! get rotation matrix to rotate into xy plane

        rot_norm = matmul(rot,norms(norm_ids(i),1:3)) ! rotate normal as read from file

        do j = 1, num_verts
            rotated_verts(j,1:3) = matmul(rot,verts(face_ids(i,j),1:3)) ! rotate vertices into xy plane
        end do

        if(dot_product(rot_norm,(/0D0,0D0,1D0/)) >= 0.99) then
            rotated_verts = - rotated_verts ! flip the vertices if facing down
        else if(dot_product(rot_norm,(/0D0,0D0,1D0/)) <= -0.99) then
            ! rotated_verts = - rotated_verts
        else
            print*,'error: bad normal found. check that all particle facets are planar'
            stop
        end if

        do j = 1, num_verts-1 ! for each vertex in this face (excluding the first vertex)
            vecs(j,1:2) = rotated_verts(j+1,1:2) - rotated_verts(1,1:2) ! get edge vector from 1st vertex to jth vertex
            angles(j) = atan2(vecs(j,2),vecs(j,1)) ! get anti-clockwise angle from +ive x-axis to vector
            if (angles(j) .gt. 0) angles(j) = 2*pi-angles(j) ! remap atan function
        end do

        call Qsort_real(angles,mapping) ! sort angles from lowest to highest ! modified with credit: A. Penttil� (see misc module)

        do j = 2, num_verts 
            face_ids(i,j) = face_ids_temp(mapping(j-1)+1) ! reorder vertices
        end do

        print*,'reordering:'
        do j = 1, num_verts
            print*,face_ids_temp(j),' --> ',face_ids(i,j)
        end do

    end do

    ! stop

end subroutine

subroutine make_angles(theta_vals,theta_vals_in,theta_splits_in,theta_vals_counter)

    ! sr read_theta,vals reads and makes the phi values
    ! taken from sr anglesplit

    ! important values:
    !   k is the total number of bins
    !   anglesout are the centres of each angular bin - size (1:k)
    !   anglesteps are the sizes of each angular bin - size (1:k)
    real(8), dimension(:), allocatable, intent(out) :: theta_vals
    real(8), dimension(:), intent(in) :: theta_vals_in(1:100)
    real(8), dimension(:), intent(in) :: theta_splits_in(1:100)
    integer(8), intent(in) ::  theta_vals_counter

    integer(8) nlines, i, j, jmax
    ! integer(8) io
    real(8), dimension(:) ,allocatable :: anglesint ,splitsint
    real(8), dimension(:) ,allocatable :: anglesout, anglesteps
    integer(8) kint

    if(allocated(anglesout)) deallocate(anglesout)
    if(allocated(anglesteps)) deallocate(anglesteps)
    allocate( anglesout(10000), anglesteps(10000) )    ! set up arrays for anglesint and splitsint
    nlines = 0  ! initialise line counter
    
    ! open(119, file = "theta_vals.txt", status = 'old', action = 'read')  ! open input file
    
    ! do  ! read in number of lines in input file
    !     read(119,*,iostat=io)
    !     if (io/=0) exit
    !     nlines = nlines + 1
    ! end do

    nlines = theta_vals_counter

    ! print*,'nlines = ',nlines
    allocate( anglesint(nlines) ,splitsint(nlines-1) )    ! set up arrays for anglesint and splitsint
    ! rewind(119)  ! rewind to top of input file
    

    do i = 1,nlines     ! read in anglesint and splitsint
        if(i .lt. nlines) then
            anglesint(i) = theta_vals_in(i)
            splitsint(i) = theta_splits_in(i)
            ! read(119,*) anglesint(i),splitsint(i)
        else
            anglesint(i) = theta_vals_in(i)
            ! read(119,*) anglesint(i)
        end if
    end do
    
    kint = 0
    do i = 1,nlines-1   ! for each line in the input file
        jmax = nint((anglesint(i+1)-anglesint(i))/splitsint(i))    ! compute the number of angle steps between one line and the next
        if(mod((anglesint(i+1)-anglesint(i))/splitsint(i),real(1)) .gt. 1e-3 .and. mod((anglesint(i+1)-anglesint(i))/splitsint(i),real(1)) .lt. 1-1e-3) then 
          print*,'bad angle choice, line:',i
          print*,'this should be close to integer:',(anglesint(i+1)-anglesint(i))/splitsint(i)
          print*,'should be less than 0.001',mod((anglesint(i+1)-anglesint(i))/splitsint(i),real(1))
          error stop     ! please check choice of bin splitting
        end if
        do j = 0,jmax
            if (i .ne. nlines-1 .and. j .eq. jmax) then
                ! do nothign
            else
            ! if (j .eq. 0. .and. i .ne. 1 .and. i .ne. nlines-1) then ! skip duplicates where 2 lines meet
            ! else if (i .ne. nlines-1 .and. j .eq. jmax) then ! bodge
            ! else
                kint = kint + 1
                anglesout(kint) = anglesint(i) + j*splitsint(i)    ! central angle of each angular bin
            end if
        end do
    end do

    ! close(119)

    allocate(theta_vals(1:kint))

    do i = 1, kint
        theta_vals(i) = anglesout(i)
        ! print*,'i',i,' theta:', theta_vals(i)
    end do
    ! stop
    end subroutine

subroutine parse_command_line(job_params)

integer i, j
character(len=255) :: arg ! a command line argument
integer my_status ! success code
! integer(8) ierror ! error status
logical finished
real(8), dimension(:) :: theta_vals_in(1:100), phi_vals_in(1:100)
real(8), dimension(:) :: theta_splits_in(1:100), phi_splits_in(1:100)
integer(8) theta_vals_counter, theta_splits_counter
integer(8) phi_vals_counter, phi_splits_counter

! output variables
character(255) cfn ! crystal filename
character(255) cft ! crystal file type
character(255) afn ! apertures filename
real(8) la ! wavelength
real(8) rbi ! real part of the refractive index
real(8) ibi ! imaginary part of the refractive index
integer(8) rec ! max number of internal beam recursions
character(100) rot_method ! rotation method
logical is_multithreaded ! whether or not code should use multithreading
integer(8) num_orients ! number of orientations
logical intellirot ! whether or not to use intelligent euler angle choices for orientation avergaing
character(100) c_method ! method of particle file input
character(255) job_name ! name of job
integer(8)  offs(1:2)
real(8)  eulers(1:3)
type(cc_hex_params_type) cc_hex_params ! parameters for C. Collier Gaussian Random hexagonal columns/plates
real(8), dimension(:), allocatable :: theta_vals, phi_vals

type(job_parameters_type), intent(out) :: job_params ! job parameters (see types_mod for more details)

! logicals to track required inputs
logical found_la
logical found_rbi
logical found_ibi
logical found_rec
logical found_c_method
logical found_cfn ! only required if c_method is read
logical found_afn ! only required if c_method is read
logical found_cft ! only required if c_method is read

! init and set default values
la = 0.532
rbi = 1.31
ibi = 0D0
rec = 8
found_la = .false.
found_rbi = .false.
found_ibi = .false.
found_rec = .false.
found_c_method = .false.
found_cfn = .false.
found_afn = .false.
found_cft = .false.
num_orients = 1
is_multithreaded = .true.
intellirot = .false.
c_method = "cc_hex"
cc_hex_params%l = 20D0
cc_hex_params%hr = 5D0
cc_hex_params%nfhr = 6
cc_hex_params%pfl = 10D0
cc_hex_params%nfpl = 12
cc_hex_params%pher = 1
cc_hex_params%pper = 1
cc_hex_params%nscales = 1
allocate(cc_hex_params%cls(1:cc_hex_params%nscales))
allocate(cc_hex_params%sds(1:cc_hex_params%nscales))
cc_hex_params%cls = 1
cc_hex_params%sds = 0
rot_method = "none"
eulers = (/0D0,0D0,0D0/) 
job_name = 'job'
afn = "(null)"
cfn = "(null)"
cft = "(null)"
theta_vals_in(1:2) = (/0D0,180D0/)
theta_vals_counter = 2
theta_splits_in(1) = 1D0
phi_vals_in(1:2) = (/0D0,360D0/)
phi_splits_in(1) = 1D0
phi_vals_counter = 2
call make_angles(theta_vals,theta_vals_in,theta_splits_in,theta_vals_counter)
theta_vals = theta_vals*pi/180d0
call make_angles(phi_vals,phi_vals_in,phi_splits_in,phi_vals_counter)
phi_vals = phi_vals*pi/180d0


job_params%suppress_2d = .false.
job_params%tri = .false.
job_params%tri_edge_length = huge(1d0)
job_params%tri_roughness = 0D0
job_params%tri_div = 1
job_params%time_limit = 1e6
job_params%resume = .false.
job_params%cache_id = -1
job_params%scaling = .false.
job_params%beta_lims = (/0D0,180D0/)
job_params%gamma_lims = (/0D0,360D0/)
job_params%output_eulers = .false.
job_params%debug = 1 ! default value is some debugging output
job_params%timing = .false. ! default is no timing
job_params%thresh_area = 1d-1 ! default area threshold is 0
job_params%thresh_energy = 1d-4 ! default energy threshold 1d-4
job_params%export_beam = .false. ! default is do not export the beam
job_params%refl = 10 ! default is max 10 total internal reflections
job_params%is_fast_diff = .false. ! default is no fast diffraction
job_params%is_fast = .true. ! default is to prioritise speed
job_params%disable_alpha = .false. ! default is dont disable alpha euler angle

! print*,'command_argument_count(): ',command_argument_count()
! print*,'parsing command line...'
i = 0
do while (i .lt. command_argument_count()) ! looping over command line args
    i = i + 1 ! update counter

    ! print*,'parsing arg #',i

    call get_command_argument(i,arg)
    ! print*,'command line argument #',i,': ','"',trim(arg),'"'
    ! print*,'"',trim(arg),'"'

    select case (arg) ! parse argument
        case ('-rot') ! if rotation specifier "rot" was included in command line
            ! print*,'found command line specifier "rot"'
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "rot"'
                stop
            else ! else, parse the rot specifier
                select case (arg) ! parse rot specifier
                case('none')
                    read(arg,*) rot_method
                    ! print*,'rot_method: ', trim(rot_method)
                case('euler')
                    read(arg,*) rot_method
                    ! print*,'rot_method: ', trim(rot_method)

                    i = i + 1 ! update counter to read the first euler angle
                    call get_command_argument(i,arg,status=my_status)
                    if (my_status .eq. 1) then ! if no argument found
                        print*,'error: failed to find first angle for "rot euler"'
                        stop
                    else
                        read(arg,*) eulers(1)
                        ! print*,'alpha: ',eulers(1)
                        ! do something
                    end if

                    i = i + 1 ! update counter to read the second euler angle
                    call get_command_argument(i,arg,status=my_status)
                    if (my_status .eq. 1) then ! if no argument found
                        print*,'error: failed to find second angle for "rot euler"'
                        stop
                    else
                        read(arg,*) eulers(2)
                        ! print*,'beta: ',eulers(2)
                    end if

                    i = i + 1 ! update counter to read the second euler angle
                    call get_command_argument(i,arg,status=my_status)
                    if (my_status .eq. 1) then ! if no argument found
                        print*,'error: failed to find third angle for "rot euler"'
                        stop
                    else
                        read(arg,*) eulers(3)
                        ! print*,'gamma: ',eulers(3)
                    end if
                
                case('off')
                    read(arg,*) rot_method
                    ! print*,'rot_method: ', trim(rot_method)
                    i = i + 1 ! update counter to read the first off value
                    call get_command_argument(i,arg,status=my_status)
                    if (my_status .eq. 1) then ! if no argument found
                        print*,'error: failed to find first value for "rot off"'
                        stop
                    else
                        read(arg,*) offs(1)
                        ! print*,'off(1): ',offs(1)
                    end if

                    i = i + 1 ! update counter to read the second off value
                    call get_command_argument(i,arg,status=my_status)
                    if (my_status .eq. 1) then ! if no argument found
                        print*,'error: failed to find second value for "rot off"'
                        stop
                    else
                        read(arg,*) offs(2)
                        ! print*,'off(2): ',offs(2)
                    end if
                    
                case('multi')
                    read(arg,*) rot_method
                    ! print*,'rot_method: ', trim(rot_method)
                    i = i + 1 ! update counter to read the number of orientations
                    call get_command_argument(i,arg,status=my_status)
                    if (my_status .eq. 1) then ! if no argument found
                        print*,'error: failed to find value for "rot multi"'
                        stop
                    else
                        read(arg,*) num_orients ! do something
                        ! print*,'number of orientations: ',num_orients
                    end if

                case default
                    print*,'error: "',trim(arg),'" is an invalid specifier for command line speicifer "rot"'
                    stop
                end select
            end if

        case ('-lambda')
            ! print*,'found command line specifier "lambda"'
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "lambda"'
                stop
            else ! else, parse the specifier
                read(arg,*) la
                ! print*,'lambda: ',la
                found_la = .true.
            end if

        case ('-rbi')
            ! print*,'found command line specifier "rbi"'
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "rbi"'
                stop
            else ! else, parse the specifier
                read(arg,*) rbi
                ! print*,'rbi: ',rbi
                found_rbi = .true.
            end if

        case ('-ibi')
            ! print*,'found command line specifier "ibi"'
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "ibi"'
                stop
            else ! else, parse the specifier
                read(arg,*) ibi
                ! print*,'ibi: ',ibi
                found_ibi = .true.
            end if

        case ('-cmethod')
            ! print*,'found command line specifier "cmethod"'
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "cmethod"'
                stop
            else ! else, parse the specifier
                select case (arg)
                    case('read')
                        read(arg,*) c_method
                        ! print*,'c_method: ', trim(c_method)
                    case('cc_hex')
                        read(arg,*) c_method
                        ! print*,'c_method: ', trim(c_method)
                    case default
                        print*,'invalid cmethod'
                        stop
                end select
                found_c_method = .true.
            end if            

        case ('-cc_hex_l')
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "cc_hex_l"'
                stop
            else
                read(arg,*) cc_hex_params%l
                ! print*,'cc_hex_params%l: ', cc_hex_params%l
            end if

        case ('-cc_hex_hr')
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "cc_hex_hr"'
                stop
            else
                read(arg,*) cc_hex_params%hr
                ! print*,'cc_hex_params%hr: ', cc_hex_params%hr
            end if            

        case ('-cc_hex_nfhr')
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "cc_hex_nfhr"'
                stop
            else
                read(arg,*) cc_hex_params%nfhr
                ! print*,'cc_hex_params%nfhr: ', cc_hex_params%nfhr
            end if  

        case ('-cc_hex_pfl')
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "cc_hex_pfl"'
                stop
            else
                read(arg,*) cc_hex_params%pfl
                ! print*,'cc_hex_params%pfl: ', cc_hex_params%pfl
            end if  

        case ('-cc_hex_nfpl')
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "cc_hex_nfpl"'
                stop
            else
                read(arg,*) cc_hex_params%nfpl
                ! print*,'cc_hex_params%nfpl: ', cc_hex_params%nfpl
            end if  

        case ('-cc_hex_pher')
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "cc_hex_pher"'
                stop
            else
                read(arg,*) cc_hex_params%pher
                ! print*,'cc_hex_params%pher: ', cc_hex_params%pher
            end if  

        case ('-cc_hex_pper')
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "cc_hex_pper"'
                stop
            else
                read(arg,*) cc_hex_params%pper
                ! print*,'cc_hex_params%pper: ', cc_hex_params%pper
            end if  

        case ('-cc_hex_nscales')
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "cc_hex_nscales"'
                stop
            else
                read(arg,*) cc_hex_params%nscales
                ! print*,'cc_hex_params%nscales: ', cc_hex_params%nscales
                if(allocated(cc_hex_params%cls)) deallocate(cc_hex_params%cls)
                if(allocated(cc_hex_params%sds)) deallocate(cc_hex_params%sds)
                allocate(cc_hex_params%cls(1:cc_hex_params%nscales))
                allocate(cc_hex_params%sds(1:cc_hex_params%nscales))
            end if  

        case ('-cc_hex_cls')

                if (.not. allocated(cc_hex_params%cls)) then
                    print*,'error, cc_hex_nscales must be specified before cc_hex_cls'
                    stop
                end if
                do j = 1, cc_hex_params%nscales ! for each roughness scale
                    i = i + 1 ! update counter to read the rotation method
                    call get_command_argument(i,arg,status=my_status)
                    if (my_status .eq. 1) then ! if no argument found
                        print*,'error: no option found for "cc_hex_cls"'
                        stop
                    else
                        read(arg,*) cc_hex_params%cls(j)
                        ! print*,'cc_hex_params%cls(',j,'): ', cc_hex_params%cls(j)
                    end if
                end do

        case ('-cc_hex_sds')

            if (.not. allocated(cc_hex_params%sds)) then
                print*,'error, cc_hex_nscales must be specified before cc_hex_sds'
                stop
            end if
            do j = 1, cc_hex_params%nscales ! for each roughness scale
                i = i + 1 ! update counter to read the rotation method
                call get_command_argument(i,arg,status=my_status)
                if (my_status .eq. 1) then ! if no argument found
                    print*,'error: no option found for "cc_hex_sds"'
                    stop
                else
                    read(arg,*) cc_hex_params%sds(j)
                    ! print*,'cc_hex_params%sds(',j,'): ', cc_hex_params%sds(j)
                end if
            end do



        case ('-cft')
            ! print*,'found command line specifier "cft"'
            i = i + 1 ! update counter to read the next arg
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "cft"'
                stop
            else ! else, parse the specifier
                select case (arg)
                case('obj')
                    read(arg,*) cft
                    ! print*,'cft: ', trim(cft)
                    found_cft = .true.
                case('mrt')
                    read(arg,*) cft
                    ! print*,'cft: ', trim(cft)
                    found_cft = .true.
                case default
                    print*,'error: invalid particle file type'
                    stop
                end select
            end if

        case ('-cfn')
            ! print*,'found command line specifier "cfn"'
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "cfn"'
                stop
            else ! else, parse the specifier
                ! read(arg,*) cfn
                write(cfn,'(A)') arg
                ! print*,'cfn: ', trim(cfn)
                ! print*,'arg:',arg
                found_cfn = .true.




            end if

        case ('-afn')
            ! print*,'found command line specifier "afn"'
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "afn"'
                stop
            else ! else, parse the specifier
                ! read(arg,*) afn
                write(afn,'(A)') arg
                ! print*,'afn: ', trim(afn)
                found_afn = .true.
            end if

        case ('-rec')
            ! print*,'found command line specifier "rec"'
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "rec"'
                stop
            else ! else, parse the specifier
                read(arg,*) rec
                ! print*,'rec: ', rec
                found_rec = .true.
            end if

        case ('-refl')
            ! print*,'found command line specifier "rec"'
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "refl"'
                stop
            else ! else, parse the specifier
                read(arg,*) job_params%refl
                ! print*,'rec: ', rec
                found_rec = .true.
            end if

        case ('-jobname')
            ! print*,'found command line specifier "jobname"'
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "jobname"'
                stop
            else ! else, parse the specifier
                read(arg,'(A)') job_name
                ! print*,'job name: ', trim(job_name)
            end if

        case ('-theta')
            finished = .false. ! init
            theta_vals_counter = 0 ! number of theta values read
            theta_splits_counter = 0 ! number of theta splits read
            j = 0 ! counts the number of values read
            ! print*,'theta values:'
            do while (.not. finished) ! while we havent reached
                i = i + 1 ! update counter to read the rotation method
                j = j + 1
                call get_command_argument(i,arg,status=my_status) ! try to get the next value
                if (my_status .eq. 1 .and. j .le. 3) then ! if no argument found and we havent yet read enough values
                    print*,'error: not enough values specified for theta"'
                    stop
                else ! else, parse the specifier
                    ! print*,'found an arg: "',trim(arg),'"'
                    ! print*,'len(trim(arg))',len(trim(arg))
                    if(arg(1:1) .eq. '-' .or. len(trim(arg)) .eq. 0) then ! if we found another flag or nothing else, try to finish
                        if (mod(j,2) .eq. 1) then ! if we found an even number of theta values
                            print*,'error: an odd number of theta values is required'
                            stop
                        else
                            i = i - 1 ! move counter back so we can parse the next argument properly
                            ! print*,'finished reading theta'
                            finished = .true.
                        end if
                    else ! else, if we found a value
                        ! print*,'j',j
                        if (mod(j,2) .eq. 1) then
                            theta_vals_counter = theta_vals_counter + 1
                            ! print*,'found a theta value: ',trim(arg)
                            read(arg,*) theta_vals_in(theta_vals_counter)
                        else
                            theta_splits_counter = theta_splits_counter + 1
                            ! print*,'found a theta split: ',trim(arg)
                            read(arg,*) theta_splits_in(theta_splits_counter)
                        end if
                    end if
                end if 
            end do
            ! print*,'theta values read: ',theta_vals_in(1:theta_vals_counter)
            ! print*,'theta splits read: ',theta_splits_in(1:theta_splits_counter)

            if(allocated(theta_vals)) deallocate(theta_vals)
            call make_angles(theta_vals,theta_vals_in,theta_splits_in,theta_vals_counter)
            ! convert theta vals to rad
            theta_vals = theta_vals*pi/180d0

        case ('-phi')
            finished = .false. ! init
            phi_vals_counter = 0 ! number of phi values read
            phi_splits_counter = 0 ! number of phi splits read
            j = 0 ! counts the number of values read
            ! print*,'phi values:'
            do while (.not. finished) ! while we havent reached
                i = i + 1 ! update counter to read the rotation method
                j = j + 1
                call get_command_argument(i,arg,status=my_status) ! try to get the next value
                if (my_status .eq. 1 .and. j .le. 3) then ! if no argument found and we havent yet read enough values
                    print*,'error: not enough values specified for phi"'
                    stop
                else ! else, parse the specifier
                    ! print*,'found an arg: "',trim(arg),'"'
                    ! print*,'len(trim(arg))',len(trim(arg))
                    if(arg(1:1) .eq. '-' .or. len(trim(arg)) .eq. 0) then ! if we found another flag or nothing else, try to finish
                        if (mod(j,2) .eq. 1) then ! if we found an even number of theta values
                            print*,'error: an odd number of phi values is required'
                            stop
                        else
                            i = i - 1 ! move counter back so we can parse the next argument properly
                            ! print*,'finished reading theta'
                            finished = .true.
                        end if
                    else ! else, if we found a value
                        ! print*,'j',j
                        if (mod(j,2) .eq. 1) then
                            phi_vals_counter = phi_vals_counter + 1
                            ! print*,'found a phi value: ',trim(arg)
                            read(arg,*) phi_vals_in(phi_vals_counter)
                        else
                            phi_splits_counter = phi_splits_counter + 1
                            ! print*,'found a phi split: ',trim(arg)
                            read(arg,*) phi_splits_in(phi_splits_counter)
                        end if
                    end if
                end if 
            end do
            ! print*,'phi values read: ',phi_vals_in(1:phi_vals_counter)
            ! print*,'phi splits read: ',phi_splits_in(1:phi_splits_counter)
            ! stop
            if(allocated(phi_vals)) deallocate(phi_vals)
            call make_angles(phi_vals,phi_vals_in,phi_splits_in,phi_vals_counter)
            ! convert phi vals to rad
            phi_vals = phi_vals*pi/180d0

            ! numerical fixes due to divide by 0 in contour integral
            ! do i = 1, size(phi_vals)
            !     if(abs(phi_vals(i)*180.0/pi) .lt. 0.000001) phi_vals(i) = phi_vals(i) + 0.00001*pi/180.0
            !     if(abs(phi_vals(i)*180.0/pi - 360.0) .lt. 0.000001) phi_vals(i) = phi_vals(i) + 0.00001*pi/180.0
            ! end do

        case ('-mt')
            ! print*,'found command line specifier "mt"'
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "mt"'
                stop
            else
                ! print*,'arg: "',trim(arg),'"'
                if(trim(arg) == "0") then
                    is_multithreaded = .false.
                    ! print*,'multithreading: disabled'
                else if(trim(arg) == "1") then
                    is_multithreaded = .true.
                    ! print*,'multithreading: enabled'
                else
                    error stop "invalid argument for flag -mt"
                end if
            end if

        case ('-no2d')
            ! print*,'suppress 2d outputs: enabled'
            job_params%suppress_2d = .true.

        case ('-fast_diff')
            ! print*,'fast diffraction: enabled'
            job_params%is_fast_diff = .true.

        case ('-speed')
            ! print*,'prioritising for speed or memory: speed'
            job_params%is_fast = .true.

        case ('-memory')
            ! print*,'prioritising for speed or memory: memory'
            job_params%is_fast = .false.

        case ('-disable_alpha')
            ! print*,'disabling alpha euler angle'
            job_params%disable_alpha = .true.

        case ('-tri')
            ! print*,'found command line specifier "mt"'
            ! print*,'automatic triangulation: enabled'
            job_params%tri = .true.

        case ('-scaling')
            ! print*,'found command line specifier "mt"'
            ! print*,'diffraction energy scaling: enabled'
            job_params%scaling = .true.

        case ('-resume')
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "resume"'
                stop
            else
                ! print*,'resume from cached data: enabled'
                job_params%resume = .true.
                read(arg,*) job_params%cache_id
                ! print*,'job_params%cache_id: ', job_params%cache_id
            end if

        case ('-tri_edge')
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "tri_edge"'
                stop
            else
                read(arg,*) job_params%tri_edge_length
                ! print*,'job_params%tri_edge: ', job_params%tri_edge_length
            end if  

        case ('-tri_rough')
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "tri_rough"'
                stop
            else
                read(arg,*) job_params%tri_roughness
                ! print*,'job_params%tri_roughness: ', job_params%tri_roughness
            end if 

        case ('-tri_div')
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "tri_div"'
                stop
            else
                read(arg,*) job_params%tri_div
                ! print*,'job_params%tri_roughness: ', job_params%tri_roughness
            end if 

        case ('-intellirot')
            ! print*,'found command line specifier "intellirot"'
            intellirot = .true.
            ! do something

        case ('-time_limit')
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "time_limit"'
                stop
            else
                read(arg,*) job_params%time_limit
                ! print*,'job_params%time_limit: ', job_params%time_limit,' hours'
            end if  

        case ('-beta_min')
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "beta_min"'
                stop
            else
                read(arg,*) job_params%beta_lims(1)
                ! print*,'job_params%beta_lims(1): ', job_params%beta_lims(1)
            end if  

        case ('-beta_max')
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "beta_max"'
                stop
            else
                read(arg,*) job_params%beta_lims(2)
                ! print*,'job_params%beta_lims(2): ', job_params%beta_lims(2)
            end if  

        case ('-gamma_min')
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "gamma_min"'
                stop
            else
                read(arg,*) job_params%gamma_lims(1)
                ! print*,'job_params%gamma_lims(1): ', job_params%gamma_lims(1)
            end if  

        case ('-gamma_max')
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "gamma_max"'
                stop
            else
                read(arg,*) job_params%gamma_lims(2)
                ! print*,'job_params%gamma_lims(2): ', job_params%gamma_lims(2)
            end if 

        case ('-output_eulers')
            ! print*,'found command line specifier "output_eulers"'
            job_params%output_eulers = .true.
            ! do something

        case ('-debug')
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "debug"'
                stop
            else
                read(arg,*) job_params%debug
                ! print*,'job_params%debug: ', job_params%debug
            end if 
            
        case ('-timing')
            ! print*,'found command line specifier "timing"'
            job_params%timing = .true.
            ! print*,'timing: enabled'
            ! do something

        case ('-export_beam')
            print*,'found command line specifier "export_beam"'
            job_params%export_beam = .true.
            i = i + 1 ! update counter to read the rotation method
            call get_command_argument(i,arg,status=my_status)
            if (my_status .eq. 1) then ! if no argument found
                print*,'error: no option found for "export_beam"'
                stop
            else
                ! print*,'arg: "',trim(arg),'"'
                if(trim(arg(1:1)) == "-") then ! if found next flag, skip
                    ! print*,'found next flag, recycling'
                    i = i - 1
                else if(trim(arg(1:len(trim(arg)))) == "rec") then
                    ! print*,'found export_beam option "rec"'
                    job_params%export_beam_rec = .true. ! set to export by recursion number
                    i = i + 1 ! go to next command line argument
                    call get_command_argument(i,arg,status=my_status) ! get next arg
                    if (my_status .eq. 1 .or. trim(arg(1:1)) == "-") then ! if no argument (or next flag) found
                        print*,'error: no value found for "-export_beam rec <value> [<value>]"'
                        stop
                    else ! else, if found first number
                        read(arg,*) job_params%export_beam_lims(2) ! set it as the end recursion
                        ! print*,'read first value for -export_beam rec <value> [<value>]:',job_params%export_beam_lims(2)
                        ! attempt to read second number
                        i = i + 1 ! go to next command line argument
                        call get_command_argument(i,arg,status=my_status) ! get next arg
                        if (my_status .eq. 1 .or. len(trim(arg)) .eq. 0 .or. trim(arg(1:1)) == "-") then ! if found next flag, or didnt find a number
                            ! print*,'didnt find second value for -export_beam rec <value> [<value>]'
                            job_params%export_beam_lims(1) = 1 ! set the start index to 1
                            i = i - 1
                        else ! if found argument
                            job_params%export_beam_lims(1) = job_params%export_beam_lims(2) ! set the first value we read as the start
                            read(arg,*) job_params%export_beam_lims(2) ! and read the second value as the end
                            ! print*,'found 2 values:',job_params%export_beam_lims(1:2)
                        end if
                    end if
                else if(trim(arg(1:len(trim(arg)))) == "num") then
                    ! print*,'found export_beam option "num"'
                    job_params%export_beam_rec = .false. ! set to export by beam number
                    i = i + 1 ! go to next command line argument
                    call get_command_argument(i,arg,status=my_status) ! get next arg
                    if (my_status .eq. 1 .or. trim(arg(1:1)) == "-") then ! if no argument (or next flag) found
                        print*,'error: no value found for "-export_beam num <value> [<value>]"'
                        stop
                    else ! else, if found first number
                        read(arg,*) job_params%export_beam_lims(2) ! set it as the end recursion
                        ! print*,'read first value for -export_beam num <value> [<value>]:',job_params%export_beam_lims(2)
                        ! attempt to read second number
                        i = i + 1 ! go to next command line argument
                        call get_command_argument(i,arg,status=my_status) ! get next arg
                        if (my_status .eq. 1 .or. len(trim(arg)) .eq. 0 .or. trim(arg(1:1)) == "-") then ! if found next flag, or didnt find a number
                            ! print*,'didnt find second value for -export_beam num <value> [<value>]'
                            job_params%export_beam_lims(1) = 1 ! set the start index to 1
                            ! print*,'found next flag, recycling'
                            i = i - 1
                        else ! if found argument
                            job_params%export_beam_lims(1) = job_params%export_beam_lims(2) ! set the first value we read as the start
                            read(arg,*) job_params%export_beam_lims(2) ! and read the second value as the end
                            ! print*,'found 2 values:',job_params%export_beam_lims(1:2)
                        end if
                    end if
                else
                    error stop "invalid argument for flag -export_beam"
                end if
            end if

        case default ! if argument was unrecognised
            print '(2a, /)', 'unrecognised command-line option: ', arg
            stop
    end select
    ! print*,'i:',i
end do ! end loop over command line args

! if cfn found but cft not found, attempt to guess filetype from the extension
if (.not. found_cft .and. found_cfn) then
    ! if particle filename is long enough, look for file extensions...
    if(len(trim(cfn)) .gt. 4) then
        if(cfn(len(trim(cfn))-3:len(trim(cfn))) .eq. ".obj") then
            cft = "obj"
            ! if(job_params%debug >= 2) then
            !     print*,'guessed obj particle file type'
            ! end if
            found_cft = .true.
        else if(cfn(len(trim(cfn))-3:len(trim(cfn))) .eq. ".cry") then
            cft = "mrt"
            ! if(job_params%debug >= 2) then
            !     print*,'guessed mrt file type'
            ! end if
            found_cft = .true.
        end if
    end if
    if(len(trim(cfn)) .gt. 8) then
        if(cfn(len(trim(cfn))-7:len(trim(cfn))) .eq. ".crystal") then
            cft = "mrt"
            ! if(job_params%debug >= 2) then
            !     print*,'guessed mrt file type'
            ! end if
            found_cft = .true.
        end if
    end if

    ! if no file extension was recognised, stop
    if(found_cft .eqv. .false.) then
        print*,'error: a particle filename was given (-cfn) but the filetype could not be identified. &
        & supported file types are wavefront (obj) (.obj extension) and macke ray tracing (mrt) style (.cry &
        & or .crystal extensions). it is advised to specify the filetype explicitly using the -cft flag (eg. "-cft obj")'
        error stop ""
    end if
end if

! if cfn given and cft given (or guessed), set particle input method to read
if (found_cft .and. found_cfn) c_method = "read"

! if(rot_method(1:len(trim(rot_method))) .ne. 'multi' .and. job_params%is_fast_diff) print*,'warning, fast diffraction is not recommended for fixed orientation computations'

job_params%cfn = cfn
job_params%cft = cft
job_params%afn = afn
job_params%la = la
job_params%rbi = rbi
job_params%ibi = ibi
job_params%rec = rec
job_params%rot_method = rot_method
job_params%is_multithreaded = is_multithreaded
job_params%num_orients = num_orients
job_params%intellirot = intellirot
job_params%c_method = c_method
job_params%job_name = job_name
job_params%offs = offs
job_params%eulers = eulers
job_params%cc_hex_params = cc_hex_params
job_params%theta_vals = theta_vals
job_params%phi_vals = phi_vals

if(job_params%export_beam) then ! if beam exporting enabled, check values
    write(101,*)'beam exporting enabled'
    if(job_params%export_beam_lims(1) > job_params%export_beam_lims(2)) then 
        error stop "beam export limits must be equal or increasing values"
        stop
    end if
    if(job_params%export_beam_rec) then
        write(101,*)'exporting by recursion number:',job_params%export_beam_lims(1:2)
        ! if(job_params%export_beam_lims(2) > job_params%rec) then 
        !     job_params%export_beam_lims(2) = job_params%rec        
        !     print*,'changed'
        ! end if
    else
        ! print*,'exporting by beam number:',job_params%export_beam_lims(1:2)
    end if
end if

! print*,'job settings:'

! print*,'wavelength: ',job_params%la
! print*,'refractive index (real): ',job_params%rbi
! print*,'refractive index (imag): ',job_params%ibi
! print*,'beam recursions: ',job_params%rec
! print*,'total internal reflections: ',job_params%refl
! print*,'particle rotation method: ',job_params%rot_method
! print*,'number of orientations: ',job_params%num_orients
! print*,'intelligent orientations: ',job_params%intellirot
! print*,'particle input method: ',job_params%c_method
! print*,'job_name: ',job_params%job_name
! print*,'multithreading: ',job_params%is_multithreaded
! print*,'logging level: ',job_params%debug,'/ 3'
! print*,'extra timings: ',job_params%timing
! print*,'automatic triangulation: ',job_params%tri
! print*,'fast diffraction',job_params%is_fast_diff
! if(found_cfn) print*,'particle filename: ',job_params%cfn
! if(found_cft) print*,'particle filetype: ',job_params%cft



! print*,'=========='

! stop



    end subroutine

subroutine area_stats(face_areas)

real(8), dimension(:), allocatable, intent(in) :: face_areas ! area of each facet
integer(8) i, num_facets
real(8) min_area, max_area, total_area, avg_area

! init
min_area = face_areas(1)
max_area = face_areas(1)
total_area = 0

num_facets = size(face_areas,1)

do i = 1, num_facets

    total_area = total_area + face_areas(i)
    if(face_areas(i) .gt. max_area) max_area = face_areas(i)
    if(face_areas(i) .lt. min_area) min_area = face_areas(i)

end do

avg_area = total_area / num_facets

print*,'max facet area:      ',max_area
print*,'min facet area:      ',min_area
print*,'total surface area: ',total_area
print*,'avg. facet area:     ',avg_area

write(101,*)'max facet area:      ',max_area
write(101,*)'min facet area:      ',min_area
write(101,*)'total surface area: ',total_area
write(101,*)'avg. facet area:     ',avg_area

end subroutine

subroutine init_loop(   alpha_vals, &
                        beta_vals, &
                        gamma_vals, &
                        job_params)

    ! subroutine to pick angles that will be looped over

    integer(8) num_orients ! number of orientations
    logical intellirot ! whether or not to use intelligent euler angle choices for orientation avergaing
    type(job_parameters_type), intent(in) :: job_params ! job parameters
    real(8), dimension(:), allocatable, intent(out) :: alpha_vals, beta_vals, gamma_vals
    integer(8) num_angles, leftover_angles, num_beta_angles, num_gamma_angles
    real(8), allocatable, dimension(:) :: beta_intelli_vals, gamma_intelli_vals
    integer(8) i, j, k, counter
    real(8) rand, beta_spacing, gamma_spacing, h, w

    intellirot = job_params%intellirot
    num_orients = job_params%num_orients

    allocate(alpha_vals(1:num_orients))
    allocate(beta_vals(1:num_orients))
    allocate(gamma_vals(1:num_orients))

    ! print*,'job_params%beta_lims',job_params%beta_lims
    ! print*,'job_params%gamma_lims',job_params%gamma_lims

        ! check the validity of beta and gamma lims
    if(job_params%beta_lims(1) .gt. job_params%beta_lims(2)) then
        print*,'error: lower beta limit was greater than the upper beta limit'
        stop
    else if(job_params%beta_lims(1) .lt. -1D-4 .or. job_params%beta_lims(1) .gt. 180D0) then
        print*,'error: lower beta limit must lie in the range 0 to 180'
        stop
    else if(job_params%beta_lims(2) .lt. -0.0001 .or. job_params%beta_lims(2) .gt. 180D0) then
        print*,'error: upper beta limit must lie in the range 0 to 180'
        stop
    end if
    if(job_params%gamma_lims(1) .gt. job_params%gamma_lims(2)) then
        print*,'error: lower gamma limit was greater than the upper gamma limit'
        stop
    else if(job_params%gamma_lims(1) .lt. -1D-4 .or. job_params%gamma_lims(1) .gt. 360D0) then
        print*,'error: lower gamma limit must lie in the range 0 to 180'
        stop
    else if(job_params%gamma_lims(2) .lt. -0.0001 .or. job_params%gamma_lims(2) .gt. 360D0) then
        print*,'error: upper gamma limit must lie in the range 0 to 180'
        stop
    end if

    ! get beta (h) and gamma (w) limits when mapped to number in range 0 to 1
    
    h = (job_params%beta_lims(2) - job_params%beta_lims(1)) / 180D0
    ! print*,'h=',h
    w = (job_params%gamma_lims(2) - job_params%gamma_lims(1)) / 360D0
    ! print*,'w=',w

    if (intellirot) then

        ! the spacing for beta and gamma must be uniformly distributed
        ! this needs special care if using particle symmetry to reduce the range of angles needed

        ! print*,'job_params%beta_lims',job_params%beta_lims
        ! print*,'job_params%gamma_lims',job_params%gamma_lims

        if(w < 1d-5) then
            num_beta_angles = num_orients
            num_gamma_angles = 1
        else if(h < 1d-5) then
            num_beta_angles = 1
            num_gamma_angles = num_orients
        else
            num_gamma_angles = floor((-1D0+sqrt(1D0+(4D0*h*num_orients/w)))/(2D0*h/w))
            num_beta_angles = floor(num_orients/((-1D0+sqrt(1D0+(4D0*h*num_orients/w)))/(2D0*h/w)))
        end if

        leftover_angles = num_orients - num_beta_angles*num_gamma_angles
        ! print*,'number of (leftover) random euler angles: ',leftover_angles

        ! stop

        ! stop


        allocate(beta_intelli_vals(1:num_beta_angles)) ! allocate array to hold intelligent euler angles
        allocate(gamma_intelli_vals(1:num_gamma_angles)) ! allocate array to hold intelligent euler angles
        if(num_angles .eq. 1) then
            beta_intelli_vals(1) = 0.5 ! set to middle if less than than 8 orientations specified
            gamma_intelli_vals(1) = 0.5 ! set to middle if less than than 8 orientations specified
        else
            beta_spacing = h/real(num_beta_angles-1)
            do i = 1, num_beta_angles ! for each entry, linear interpolate from 0 to 1
                beta_intelli_vals(i) = beta_spacing * (i-1) + &
                    ! beta_spacing / 4D0 + & ! with small shift to avoid normal incidence
                    job_params%beta_lims(1) / 180D0 ! note that beta_lims(1) should be in the range 0 to 180 deg.
                    
                ! print*,'beta_intelli_vals(i)',beta_intelli_vals(i)
            end do 
            gamma_spacing = w/real(num_gamma_angles)
            do i = 1, num_gamma_angles ! for each entry, linear interpolate from 0 to 1
                gamma_intelli_vals(i) = gamma_spacing * (i-1) + &
                    job_params%gamma_lims(1) / 360D0 ! note that gamma_lims(1) should be in the range 0 to 360 deg.
                ! print*,'gamma_intelli_vals(i)',gamma_intelli_vals(i)
            end do                        
        end if
        ! stop
        ! loop through and assign intelligent angles
        counter = 0
        do j = 1,num_beta_angles
            do k = 1,num_gamma_angles
                counter = counter + 1 ! count how many orientations we have set up
                alpha_vals(counter) = 0D0
                beta_vals(counter) = beta_intelli_vals(j)
                gamma_vals(counter) = gamma_intelli_vals(k)
            end do
        end do
        ! stop
        ! fix numerical errors with a very small amount of extra rotation (probably not needed anymore following fixes to contour integral fnc)
        ! alpha_vals(1:counter) = abs(alpha_vals(1:counter) - 0.0001)
        ! beta_vals(1:counter) = abs(beta_vals(1:counter) - 0.0001)
        ! gamma_vals(1:counter) = abs(gamma_vals(1:counter) - 0.0001)
        ! stop
        ! fill in remainining angles with random numbers
        do i = 1, leftover_angles
            counter = counter + 1
            call random_number(rand)
            ! alpha_vals(counter) = rand
            alpha_vals(counter) = 0D0
            call random_number(rand)
            ! beta_vals(counter) = rand
            beta_vals(counter) = rand*h + job_params%beta_lims(1)/180D0
            call random_number(rand)
            ! gamma_vals(counter) = rand
            gamma_vals(counter) = rand*w + job_params%gamma_lims(1)/360D0
        end do

        ! print intelligent euler angles
        ! do i = 1, num_orients
        !     print'(A,f6.4,A,f6.4,A,f6.4)','alpha: ',alpha_vals(i),' beta: ',beta_vals(i),' gamma: ',gamma_vals(i)
        ! end do

    else
        do i = 1, size(alpha_vals,1) ! loop here so that the angles are reproducable regardless of number of orientations
            ! random alpha value
            if (job_params%disable_alpha) then
                ! for random orientation, alpha has no effect on the 1d patterns, so set it to a constant
                alpha_vals(i) = 0D0
            else
                call random_number(alpha_vals(i))
            end if
            call random_number(rand)
            beta_vals(i) = rand*h + job_params%beta_lims(1)/180D0
            call random_number(rand)
            gamma_vals(i) = rand*w + job_params%gamma_lims(1)/360D0
        end do
    end if

    ! scaling

    ! beta_vals = beta_vals * &
    !     (job_params%beta_lims(2) - job_params%beta_lims(1))/180D0 + & ! scale it back if needed
    !     job_params%beta_lims(1)/180D0 ! shift it to minimum point

        ! print*,'scaling factor: ',(job_params%beta_lims(2) - job_params%beta_lims(1))/360D0     
        ! print*,'scaling shift: ',job_params%beta_lims(1)/360D0
        
    ! gamma_vals = gamma_vals * &
    !     (job_params%gamma_lims(2) - job_params%gamma_lims(1))/360D0 + & ! scale it back if needed
    !     job_params%gamma_lims(1)/360D0 ! shift it to minimum point




    ! stop

    end subroutine

subroutine PROT(ifn,rot_method,verts)

    ! rotates particle

    character(len=*), intent(in) :: ifn
    character(100), intent(in) :: rot_method ! rotation method
    real(8), dimension(:,:), allocatable, intent(inout) :: verts ! unique vertices    
    integer(8) offs(1:2)
    real(8) eulers(1:3)
    real(8) vec(1:3) ! off rotation vector
    real(8) hilf0, hilf1
    real(8) rot1(1:3,1:3), rot2(1:3,1:3), rot(1:3,1:3)
    integer(8) i
    real(8) s1, s2, s3, c1, c2, c3
    real(8) rand
    integer(8) num_vals

    print*,'========== start sr PROT'

    print*,'rotation method: "',rot_method(1:len(trim(rot_method))),'"'

    if(rot_method(1:len(trim(rot_method))) .eq. 'none') then
        ! do nothing
    else if(rot_method(1:len(trim(rot_method))) .eq. 'off') then
        num_vals = 2
        call read_input_vals(ifn,"rot off",offs,num_vals)
        ! print*,'off values: ', offs(1:2)

        if(offs(1) .eq. 30 .and. offs(2) .eq. 0) then
            print*,'off setting: 30x0'
            vec = (/-0.866025,-0.5,0.0/)
        else if(offs(1) .eq. 30 .and. offs(2) .eq. 10) then
            print*,'off setting: 30x10'
            vec = (/-0.866025,-0.492404,0.0868240/)
        else if(offs(1) .eq. 30 .and. offs(2) .eq. 20) then
            print*,'off setting: 30x20'
            vec = (/-0.866025,-0.469846,0.171010/)
        else if(offs(1) .eq. 30 .and. offs(2) .eq. 30) then
            print*,'off setting: 30x30'
            vec = (/-0.866025,-0.433013,0.25/)
        end if

        hilf0 = vec(2)/vec(1)
        hilf1 = cos(atan(hilf0))

        rot1(1,1) = hilf1
        rot1(1,2) = cos(pi/2-atan(hilf0))
        rot1(1,3) = 0
        rot1(2,1) = cos(pi/2+atan(hilf0))
        rot1(2,2) = hilf1
        rot1(2,3) = 0
        rot1(3,1) = 0
        rot1(3,2) = 0
        rot1(3,3) = 1

        rot2(1,1) = cos(pi-acos(vec(3)))
        rot2(1,2) = 0
        rot2(1,3) = cos(pi/2+acos(vec(3)))
        rot2(2,1) = 0
        rot2(2,2) = 1
        rot2(2,3) = 0
        rot2(3,1) = cos(pi/2-acos(vec(3)))
        rot2(3,2) = 0
        rot2(3,3) = cos(pi-acos(vec(3)))

        rot = matmul(rot2,rot1)

        do i = 1, size(verts,1) ! for each vertex
            verts(i,1:3) = matmul(rot,verts(i,1:3)) ! rotate
        end do

        ! print*,'verts(2,1:3)',verts(2,1:3)

    else if(rot_method(1:len(trim(rot_method))) .eq. 'euler') then
        num_vals = 3
        call read_input_vals_real(ifn,"rot euler",eulers,num_vals)
        print*,'alpha:',eulers(1)
        print*,'beta:',eulers(2)
        print*,'gamma:',eulers(3)

        eulers = eulers*pi/180 ! convert to rad

        ! mishchenko rotation
        s1 = sin(eulers(1))
        s2 = sin(eulers(2))
        s3 = sin(eulers(3))
        c1 = cos(eulers(1))
        c2 = cos(eulers(2))
        c3 = cos(eulers(3))

        ! make rotation matrix
        rot(1,1) = c1*c2*c3 - s1*s3
        rot(1,2) = -c1*c2*s3 - s1*c3
        rot(1,3) = c1*s2
        rot(2,1) = s1*c2*c3 + c1*s3
        rot(2,2) = -s1*c2*s3 + c1*c3
        rot(2,3) = s1*s2
        rot(3,1) = -s2*c3
        rot(3,2) = s2*s3
        rot(3,3) = c2

        do i = 1, size(verts,1) ! for each vertex
            verts(i,1:3) = matmul(rot,verts(i,1:3)) ! rotate
        end do
        
    else if(rot_method(1:len(trim(rot_method))) .eq. 'multi') then
        call random_number(rand)
        eulers(1) = 2*pi*(rand)

        call random_number(rand)
        eulers(2) = acos(1.0 - 2.0*rand)

        call random_number(rand)
        eulers(3) = 2*pi*(rand)

        print*,'alpha:',eulers(1)*180/pi
        print*,'beta:',eulers(2)*180/pi
        print*,'gamma:',eulers(3)*180/pi

        ! mishchenko rotation
        s1 = sin(eulers(1))
        s2 = sin(eulers(2))
        s3 = sin(eulers(3))
        c1 = cos(eulers(1))
        c2 = cos(eulers(2))
        c3 = cos(eulers(3))

        ! make rotation matrix
        rot(1,1) = c1*c2*c3 - s1*s3
        rot(1,2) = -c1*c2*s3 - s1*c3
        rot(1,3) = c1*s2
        rot(2,1) = s1*c2*c3 + c1*s3
        rot(2,2) = -s1*c2*s3 + c1*c3
        rot(2,3) = s1*s2
        rot(3,1) = -s2*c3
        rot(3,2) = s2*s3
        rot(3,3) = c2

        do i = 1, size(verts,1) ! for each vertex
            verts(i,1:3) = matmul(rot,verts(i,1:3)) ! rotate
        end do
    end if

    ! stop

    print*,'========== end sr PROT'

end subroutine

subroutine UN_PROT_CC(verts)

    ! unrotates cc_hex particle so it can be rotated into the "off" positions

    real(8), dimension(:,:), allocatable, intent(inout) :: verts ! unique vertices    
    ! integer(8) offs(1:2)
    real(8) eulers(1:3)
    ! real(8) vec(1:3) ! off rotation vector
    ! real(8) hilf0, hilf1
    real(8) rot(1:3,1:3)
    integer(8) i
    real(8) s1, s2, s3, c1, c2, c3
    ! real(8) rand

    print*,'========== start sr PROT_CC'

    eulers(1) = 0.0
    eulers(2) = 90.0
    eulers(3) = 0.0

    print*,'undoing second pre-rotation...'

    print*,'alpha:',eulers(1)
    print*,'beta:',eulers(2)
    print*,'gamma:',eulers(3)

    eulers = eulers*pi/180 ! convert to rad

    ! mishchenko rotation
    s1 = sin(eulers(1))
    s2 = sin(eulers(2))
    s3 = sin(eulers(3))
    c1 = cos(eulers(1))
    c2 = cos(eulers(2))
    c3 = cos(eulers(3))

    ! make rotation matrix
    rot(1,1) = -c1*c2*s3 + c1*c3
    rot(1,2) = -s1*c2*c3 - c1*s3
    rot(1,3) = s2*s1
    rot(2,1) = c1*c2*s3 + s1*c3
    rot(2,2) = c1*c2*c3 - s1*s3
    rot(2,3) = -s2*c1
    rot(3,1) = s2*s3
    rot(3,2) = s2*c3
    rot(3,3) = c2

    do i = 1, size(verts,1) ! for each vertex
        verts(i,1:3) = matmul(transpose(rot),verts(i,1:3)) ! rotate
    end do

    ! #########################
    
    eulers(1) = 90.0
    eulers(2) = 0.0
    eulers(3) = 0.0

    print*,'undoing first pre-rotation...'

    print*,'alpha:',eulers(1)
    print*,'beta:',eulers(2)
    print*,'gamma:',eulers(3)

    eulers = eulers*pi/180 ! convert to rad

    ! mishchenko rotation
    s1 = sin(eulers(1))
    s2 = sin(eulers(2))
    s3 = sin(eulers(3))
    c1 = cos(eulers(1))
    c2 = cos(eulers(2))
    c3 = cos(eulers(3))

    ! make rotation matrix
    rot(1,1) = -c1*c2*s3 + c1*c3
    rot(1,2) = -s1*c2*c3 - c1*s3
    rot(1,3) = s2*s1
    rot(2,1) = c1*c2*s3 + s1*c3
    rot(2,2) = c1*c2*c3 - s1*s3
    rot(2,3) = -s2*c1
    rot(3,1) = s2*s3
    rot(3,2) = s2*c3
    rot(3,3) = c2

    do i = 1, size(verts,1) ! for each vertex
        verts(i,1:3) = matmul(transpose(rot),verts(i,1:3)) ! rotate
    end do

    ! stop

    print*,'========== end sr PROT_CC'

end subroutine

subroutine PROT_CC(verts)

    ! rotates cc_hex particle so that prism axis is aligned with z axis

    real(8), dimension(:,:), allocatable, intent(inout) :: verts ! unique vertices    
    ! integer(8) offs(1:2)
    real(8) eulers(1:3)
    ! real(8) vec(1:3) ! off rotation vector
    ! real(8) hilf0, hilf1
    real(8) rot(1:3,1:3)
    integer(8) i
    real(8) s1, s2, s3, c1, c2, c3
    ! real(8) rand

    write(101,*)'========== start sr PROT_CC'

    eulers(1) = 90.0
    eulers(2) = 0.0
    eulers(3) = 0.0

    write(101,*)'first pre-rotation...'

    write(101,*)'alpha:',eulers(1)
    write(101,*)'beta:',eulers(2)
    write(101,*)'gamma:',eulers(3)

    eulers = eulers*pi/180 ! convert to rad

    ! mishchenko rotation
    s1 = sin(eulers(1))
    s2 = sin(eulers(2))
    s3 = sin(eulers(3))
    c1 = cos(eulers(1))
    c2 = cos(eulers(2))
    c3 = cos(eulers(3))

    ! make rotation matrix
    rot(1,1) = -c1*c2*s3 + c1*c3
    rot(1,2) = -s1*c2*c3 - c1*s3
    rot(1,3) = s2*s1
    rot(2,1) = c1*c2*s3 + s1*c3
    rot(2,2) = c1*c2*c3 - s1*s3
    rot(2,3) = -s2*c1
    rot(3,1) = s2*s3
    rot(3,2) = s2*c3
    rot(3,3) = c2

    do i = 1, size(verts,1) ! for each vertex
        verts(i,1:3) = matmul(rot,verts(i,1:3)) ! rotate
    end do

    ! #########################

    eulers(1) = 0.0
    eulers(2) = 90.0
    eulers(3) = 0.0

    write(101,*)'second pre-rotation...'

    write(101,*)'alpha:',eulers(1)
    write(101,*)'beta:',eulers(2)
    write(101,*)'gamma:',eulers(3)

    eulers = eulers*pi/180 ! convert to rad

    ! mishchenko rotation
    s1 = sin(eulers(1))
    s2 = sin(eulers(2))
    s3 = sin(eulers(3))
    c1 = cos(eulers(1))
    c2 = cos(eulers(2))
    c3 = cos(eulers(3))

    ! make rotation matrix
    rot(1,1) = -c1*c2*s3 + c1*c3
    rot(1,2) = -s1*c2*c3 - c1*s3
    rot(1,3) = s2*s1
    rot(2,1) = c1*c2*s3 + s1*c3
    rot(2,2) = c1*c2*c3 - s1*s3
    rot(2,3) = -s2*c1
    rot(3,1) = s2*s3
    rot(3,2) = s2*c3
    rot(3,3) = c2

    do i = 1, size(verts,1) ! for each vertex
        verts(i,1:3) = matmul(rot,verts(i,1:3)) ! rotate
    end do


    ! stop

    write(101,*)'========== end sr PROT_CC'

end subroutine

subroutine PROT_MPI(alpha_vals,         & ! list of values for euler alpha angle in range 0 to 1 (for multirot only)
                    beta_vals,          & ! list of values for beta alpha angle in range 0 to 1 (for multirot only)
                    gamma_vals,         & ! list of values for gamma alpha angle in range 0 to 1 (for multirot only)
                    loop_index,         & ! current loop index that each process is on
                    job_params,         &
                    geometry,           &
                    rot_geometry)

    ! rotates particle
    ! modified to ensure different mpi processes have different 

    character(100) rot_method ! rotation method
    real(8), dimension(:,:), allocatable :: verts_rot ! unique vertices    
    real(8), dimension(:), allocatable, intent(in) :: alpha_vals, beta_vals, gamma_vals
    integer(8) offs(1:2)
    real(8) eulers(1:3)
    real(8) vec(1:3) ! off rotation vector
    real(8) hilf0, hilf1
    real(8) rot1(1:3,1:3), rot2(1:3,1:3), rot(1:3,1:3)
    integer(8) i
    real(8) s1, s2, s3, c1, c2, c3
    real(8) rand
    integer(8), intent(in) :: loop_index
    integer(8) num_orients ! number of orientations
    type(job_parameters_type), intent(in) :: job_params
    type(geometry_type), intent(in) :: geometry
    type(geometry_type), intent(out) :: rot_geometry

    rot_method = job_params%rot_method
    num_orients = job_params%num_orients
    eulers = job_params%eulers
    offs = job_params%offs

    ! if(job_params%debug >= 1) then 
    !     print*,'========== start sr PROT_MPI'
    !     write(101,*)'======================================================'
    !     write(101,*)'======================================================'
    !     write(101,*)'orientation: ',loop_index,' / ',num_orients
    !     print*,'rotation method: "',rot_method(1:len(trim(rot_method))),'"'
    ! end if

    allocate(verts_rot(1:geometry%nv,1:3)) ! allocate array for rotated vertices

    if(rot_method(1:len(trim(rot_method))) .eq. 'none') then
        ! do nothing
        verts_rot = geometry%v
    else if(rot_method(1:len(trim(rot_method))) .eq. 'off') then
        ! call read_input_vals(ifn,"rot off",offs,2)
        ! print*,'off values: ', offs(1:2)

        verts_rot = geometry%v

        call UN_PROT_CC(verts_rot)

        if(offs(1) .eq. 30 .and. offs(2) .eq. 0) then
            ! print*,'off setting: 30x0'
            ! write(101,*)'off setting: "30x0"'
            vec = (/-0.866025,-0.5,0.0/)
        else if(offs(1) .eq. 30 .and. offs(2) .eq. 10) then
            ! print*,'off setting: 30x10'
            ! write(101,*)'off setting: "30x10"'
            vec = (/-0.866025,-0.492404,0.0868240/)
        else if(offs(1) .eq. 30 .and. offs(2) .eq. 20) then
            ! print*,'off setting: 30x20'
            ! write(101,*)'off setting: "30x20"'
            vec = (/-0.866025,-0.469846,0.171010/)
        else if(offs(1) .eq. 30 .and. offs(2) .eq. 30) then
            ! print*,'off setting: 30x30'
            ! write(101,*)'off setting: "30x30"'
            vec = (/-0.866025,-0.433013,0.25/)
        end if

        hilf0 = vec(2)/vec(1)
        hilf1 = cos(atan(hilf0))

        rot1(1,1) = hilf1
        rot1(1,2) = cos(pi/2-atan(hilf0))
        rot1(1,3) = 0.0
        rot1(2,1) = cos(pi/2+atan(hilf0))
        rot1(2,2) = hilf1
        rot1(2,3) = 0.0
        rot1(3,1) = 0.0
        rot1(3,2) = 0.0
        rot1(3,3) = 1.0

        rot2(1,1) = cos(pi-acos(vec(3)))
        rot2(1,2) = 0.0
        rot2(1,3) = cos(pi/2+acos(vec(3)))
        rot2(2,1) = 0.0
        rot2(2,2) = 1.0
        rot2(2,3) = 0.0
        rot2(3,1) = cos(pi/2-acos(vec(3)))
        rot2(3,2) = 0.0
        rot2(3,3) = cos(pi-acos(vec(3)))

        rot = matmul(rot2,rot1)

        do i = 1, size(verts_rot,1) ! for each vertex
            verts_rot(i,1:3) = matmul(rot,verts_rot(i,1:3)) ! rotate
        end do

        ! print*,'verts(2,1:3)',verts(2,1:3)

    else if(rot_method(1:len(trim(rot_method))) .eq. 'euler') then
        ! call read_input_vals_real(ifn,"rot euler",eulers,3)
        ! print*,'alpha:',eulers(1)
        ! write(101,*)'alpha:',eulers(1)
        ! print*,'beta: ',eulers(2)
        ! write(101,*)'beta: ',eulers(2)
        ! print*,'gamma:',eulers(3)
        ! write(101,*)'gamma:',eulers(3)

        if(abs(eulers(2)) < 1d-4) eulers(2) = 1d-4 ! fix

        eulers = eulers*pi/180.0 ! convert to rad

        ! mishchenko rotation
        s1 = sin(eulers(1))
        s2 = sin(eulers(2))
        s3 = sin(eulers(3))
        c1 = cos(eulers(1))
        c2 = cos(eulers(2))
        c3 = cos(eulers(3))

        ! make rotation matrix
        rot(1,1) = c1*c2*c3 - s1*s3
        rot(1,2) = -c1*c2*s3 - s1*c3
        rot(1,3) = c1*s2
        rot(2,1) = s1*c2*c3 + c1*s3
        rot(2,2) = -s1*c2*s3 + c1*c3
        rot(2,3) = s1*s2
        rot(3,1) = -s2*c3
        rot(3,2) = s2*s3
        rot(3,3) = c2

        do i = 1, geometry%nv ! for each vertex
            verts_rot(i,1:3) = matmul(rot,geometry%v(i,1:3)) ! rotate
        end do
        
    else if(rot_method(1:len(trim(rot_method))) .eq. 'multi') then

        ! print*,'rot method was multi'

        rand = alpha_vals(loop_index)    
        eulers(1) = 2*pi*(rand)

        rand = beta_vals(loop_index) ! unscaled by job limits
        eulers(2) = acos(1.0 - 2.0*rand)

        rand = gamma_vals(loop_index) 
        eulers(3) = 2*pi*(rand)

        if(job_params%debug >= 1) then 
            write(101,*)'orientation: ',loop_index
            ! print*,'alpha:',eulers(1)*180.0/pi
            write(101,*)'alpha:',eulers(1)*180.0/pi
            ! print*,'beta: ',eulers(2)*180.0/pi
            write(101,*)'beta: ',eulers(2)*180.0/pi
            ! print*,'gamma:',eulers(3)*180.0/pi
            write(101,*)'gamma:',eulers(3)*180.0/pi
        end if

        ! mishchenko rotation
        s1 = sin(eulers(1))
        s2 = sin(eulers(2))
        s3 = sin(eulers(3))
        c1 = cos(eulers(1))
        c2 = cos(eulers(2))
        c3 = cos(eulers(3))

        ! make rotation matrix
        rot(1,1) = c1*c2*c3 - s1*s3
        rot(1,2) = -c1*c2*s3 - s1*c3
        rot(1,3) = c1*s2
        rot(2,1) = s1*c2*c3 + c1*s3
        rot(2,2) = -s1*c2*s3 + c1*c3
        rot(2,3) = s1*s2
        rot(3,1) = -s2*c3
        rot(3,2) = s2*s3
        rot(3,3) = c2

        do i = 1, geometry%nv ! for each vertex
            verts_rot(i,1:3) = matmul(rot,geometry%v(i,1:3)) ! rotate
        end do
    end if

    ! make a copy of the original geometry
    rot_geometry = geometry
    ! reassign the rotated vertices
    rot_geometry%v(:,:) = verts_rot(:,:)
    ! recompute the midpoints
    call compute_geometry_midpoints(rot_geometry)
    ! recompute normals
    call compute_geometry_normals(rot_geometry)
    ! recompute aperture parameters
    call compute_geometry_apertures(rot_geometry)
    ! recompute aperture parameters
    call compute_geometry_edge_vectors(rot_geometry)
    ! stop
    ! if(job_params%debug >= 1) then 
    !     print*,'========== end sr PROT_MPI'
    ! end if

end subroutine

subroutine read_input_vals(fn,string,vals,num_vals)

    ! read_input_vals reads values from a line in a text file
    ! a string containing the first part of the line is used to determine which line should be read from
    ! the number of values read from the line is determined by an input integer
    ! to do: add custom delimiter

    character(len=*), intent(in) :: fn ! filename to read from
    character(len=*), intent(in) :: string ! first part of the line to read from
    integer(8), dimension(:) :: vals ! values to be read
    integer(8) num_vals ! number of values to read

    integer(8), parameter :: max_line_length = 150 ! max number of characters in a line of thecrystal file (might need increasing if faces have many vertices)
    character(max_line_length) line ! a line in a file
    integer(8) i, io ! counting variables
    integer(8) num_lines
    logical success
    integer(8) j, k, val_count ! number chars since last delimiter

    open(10,file = fn, status = 'old')

    num_lines = 0  ! initialise line counter
    success = .false.
    val_count = 0

    do
        read(10,*,iostat=io)
        if (io/=0) exit
        num_lines = num_lines + 1
    end do
    
    rewind(10) ! back to top of file
    
    do i = 1,num_lines
        read(10,"(a)",iostat=io)line
        if (line(1:len(string)+1) .eq. string//' ') then
            ! print*,'match found on line:',i
            k = 0
            do j = len(string)+2, len(line) ! read through characters in line
                if(line(j:j) .ne. ' ') then ! if character wasnt space delimiter
                    k = k + 1 ! update characters counted since last space delimiter
                    ! print*,j,line(j:j)
                else ! if delimiter found
                    if(k .gt. 0 .and. val_count .lt. num_vals) then
                        val_count = val_count + 1
                        read(line(j-k:j),*) vals(val_count)
                        ! print*,'value found: ',vals(val_count)
                    end if
                    k = 0
                end if
            end do

        end if
    
    end do

    close(10)

end subroutine

subroutine read_input_vals_real(fn,string,vals,num_vals)

    ! read_input_vals reads values from a line in a text file
    ! a string containing the first part of the line is used to determine which line should be read from
    ! the number of values read from the line is determined by an input integer
    ! to do: add custom delimiter

    character(len=*), intent(in) :: fn ! filename to read from
    character(len=*), intent(in) :: string ! first part of the line to read from
    real(8), dimension(:) :: vals ! values to be read
    integer(8) num_vals ! number of values to read

    integer(8), parameter :: max_line_length = 150 ! max number of characters in a line of thecrystal file (might need increasing if faces have many vertices)
    character(max_line_length) line ! a line in a file
    integer(8) i, io ! counting variables
    integer(8) num_lines
    logical success
    integer(8) j, k, val_count ! number chars since last delimiter

    open(10,file = fn, status = 'old')

    num_lines = 0  ! initialise line counter
    success = .false.
    val_count = 0

    do
        read(10,*,iostat=io)
        if (io/=0) exit
        num_lines = num_lines + 1
    end do
    
    rewind(10) ! back to top of file
    
    do i = 1,num_lines
        read(10,"(a)",iostat=io)line
        if (line(1:len(string)+1) .eq. string//' ') then
            ! print*,'match found on line:',i
            k = 0
            do j = len(string)+2, len(line) ! read through characters in line
                if(line(j:j) .ne. ' ') then ! if character wasnt space delimiter
                    k = k + 1 ! update characters counted since last space delimiter
                    ! print*,j,line(j:j)
                else ! if delimiter found
                    if(k .gt. 0 .and. val_count .lt. num_vals) then
                        val_count = val_count + 1
                        read(line(j-k:j),*) vals(val_count)
                        ! print*,'value found: ',vals(val_count)
                    end if
                    k = 0
                end if
            end do

        end if
    
    end do

    close(10)

end subroutine

subroutine readApertures(afn,apertures, face_ids)

! sr readApertures reads the apertures filename
! will exit if the apertures file does not have the correct number of lines

integer(8), dimension(:), allocatable, intent(out) :: apertures
character(100), intent(in) :: afn ! crystal filename
integer(8), dimension(:,:), allocatable, intent(in) :: face_ids

integer(8), parameter :: max_line_length = 150 ! max number of characters in a line of thecrystal file (might need increasing if faces have many vertices)
character(max_line_length) line ! a line in a file
integer(8) i, io ! counting variables
integer(8) num_lines
    
! print*,'========== start sr readApertures'

! read number of lines in aperture file
num_lines = 0
print*,'opening apertures file:',afn
open(unit = 10, file = afn, status = 'old')
do  ! scan through the lines in the crystal file...
    read(10,"(a)",iostat=io)line
    if (io/=0) exit
    num_lines = num_lines + 1
end do
   
!print*,'number of lines in apertures file: ',num_lines

if(num_lines .ne. size(face_ids,1)) then
    print*,'number of lines in aperture file did not match number of faces. exiting...'
    print*,'num faces:',size(face_ids,1)
    print*,'num_lines:',num_lines
    stop
end if

allocate(apertures(num_lines)) ! allocate apertures array (with same size as face_ids)

rewind(10)
do  i = 1,num_lines ! read aperture from file
    read(10,*) apertures(i)
    !print*,'i',i,' aperture: ',apertures(i)
end do

close(10)

! print*,'========== end sr readApertures'

end subroutine

subroutine read_apertures(afn,geometry)

    character(100), intent(in) :: afn ! crystal filename
    type(geometry_type), intent(inout) :: geometry

    integer(8) i, io ! counting variables
    integer(8) num_lines

    num_lines = 0
    print*,'opening apertures file:',afn
    open(unit = 10, file = afn, status = 'old')
    do  ! scan through the lines in the crystal file...
        read(10,"(a)",iostat=io)
        if (io/=0) exit
        num_lines = num_lines + 1
    end do

    if(num_lines .ne. geometry%nf) then
        print*,'number of lines in aperture file did not match number of faces. exiting...'
        print*,'num faces:',geometry%nf
        print*,'num_lines:',num_lines
        stop
    end if

    rewind(10)
    do  i = 1,num_lines ! read aperture from file
        read(10,*) geometry%f(i)%ap
    end do

close(10)

end subroutine

subroutine make_incident_beam(geometry, beam_geometry, beam_inc)

! subroutine makeIncidentBeam makes a simple square incident beam wavefront at a location above the maximum z value of the particle (currently set to 1000)
! the width and length of the wavefront is set larger than the maximum x and y vertex values of the particle (full illumination)

type(geometry_type), intent(in) :: geometry
real(8), allocatable, dimension(:,:) :: beamV, beamN
integer(8), allocatable, dimension(:,:) :: beamF1
integer(8), allocatable, dimension(:) :: beamF2
real(8), dimension(:,:) ,allocatable :: verts
real(8), dimension(:,:) ,allocatable :: beamMidpoints
! integer(8), dimension(:,:) ,allocatable, intent(in) :: face_ids
type(geometry_type), intent(out) :: beam_geometry
type(beam_type), intent(out) :: beam_inc

real(8) min_x, min_y, max_x, max_y, min_z, max_z, fac
integer(8) i, j

allocate(verts(1:geometry%nv,1:3))
verts(:,:) = geometry%v(:,:)

! print*,'========== start sr makeIncidentBeam'

! allocate arrays
! assume square incident beam, 4 vertices, 1 facet, 1 normal
allocate(beamV(1:4,1:3)) ! 4 vertices, xyz coords
allocate(beamF1(1:1,1:4)) ! 1 facet, 4 vertices
allocate(beamN(1:1,1:3)) ! 1 vertex, xyz normal components
allocate(beamF2(1:1)) ! 1 facet

! get the max xyz and min xy coordinates
max_x = maxval(verts(1:size(verts,1),1))
min_x = minval(verts(1:size(verts,1),1))
max_y = maxval(verts(1:size(verts,1),2))
min_y = minval(verts(1:size(verts,1),2))
max_z = maxval(verts(1:size(verts,1),3))
min_z = minval(verts(1:size(verts,1),3))

! print*,'particle max x: ',max_x
! print*,'particle min x: ',min_x
! print*,'particle max y: ',max_y
! print*,'particle min y: ',min_y
! print*,'particle max z: ',max_z
! print*,'particle min z: ',min_z

! use the max/min coords to define the beam
beamF1(1:1,1) = 1
beamF1(1:1,2) = 2
beamF1(1:1,3) = 3
beamF1(1:1,4) = 4
beamN(1:1,1) = 0
beamN(1:1,2) = 0
beamN(1:1,3) = -1
beamF2(1:1) = 1

! full illumination quick implementation
! looping anti-clockwise around the incident beam
fac = 1.1 ! stretch factor
beamV(1,1) = min_x*fac
beamV(1,2) = min_y*fac
beamV(2,1) = min_x*fac
beamV(2,2) = max_y*fac
beamV(3,1) = max_x*fac
beamV(3,2) = max_y*fac
beamV(4,1) = max_x*fac
beamV(4,2) = min_y*fac
! beamV(1:4,3) = max_z*fac
beamV(1:4,3) = 1000 ! changed for comparing with Matlab

!print*,'using truncated initial beam for testing purposes'
!! partial illumination for test implementation
!beamV(1,1) = -3.5
!beamV(1,2) = -10
!beamV(2,1) = -3.5
!beamV(2,2) = 10
!beamV(3,1) = 3.5
!beamV(3,2) = 10
!beamV(4,1) = 3.5
!beamV(4,2) = -10
!beamV(1:4,3) = 50

! make beam midpoints array
allocate(beamMidpoints(1:1,1:3))
beamMidpoints(1,1:3) = sum(beamV(beamF1(1,1:4),1:3),1)/4

! allocate(ampl_beam(1:2,1:2,1:1)) ! 4 (2x2) components, 1 facet
! ampl_beam = 0
! ampl_beam(1,1,1) = 1 ! set diagonal elements to 1
! ampl_beam(2,2,1) = 1

! print'(A16,f6.4,A,f6.4,A,f6.4,A,f6.4,A)',' beam ampl in: (',real(ampl_beam(1,1,1)),' + ',imag(ampl_beam(1,1,1)),'i, ',real(ampl_beam(1,2,1)),' + ',imag(ampl_beam(1,2,1)),'i)'
! print'(A16,f6.4,A,f6.4,A,f6.4,A,f6.4,A)','               (',real(ampl_beam(2,1,1)),' + ',imag(ampl_beam(2,1,1)),'i, ',real(ampl_beam(2,2,1)),' + ',imag(ampl_beam(2,2,1)),'i)'

! print'(A,f10.4,f10.4,f10.4)',' beam verts 1: ', beamV(1,1), beamV(1,2), beamV(1,3)
! print'(A,f10.4,f10.4,f10.4)',' beam verts 2: ', beamV(2,1), beamV(2,2), beamV(2,3)
! print'(A,f10.4,f10.4,f10.4)',' beam verts 3: ', beamV(3,1), beamV(3,2), beamV(3,3)
! print'(A,f10.4,f10.4,f10.4)',' beam verts 4: ', beamV(4,1), beamV(4,2), beamV(4,3)

! print'(A,f10.4,f10.4,f10.4)',' beam midpoint: ', beamMidpoints(1,1), beamMidpoints(1,2), beamMidpoints(1,3)

! print*,'========== end sr makeIncidentBeam'

beam_geometry%nf = size(beamF1,1)
beam_geometry%nv = size(beamV,1)
beam_geometry%nn = size(beamN,1)

allocate(beam_geometry%f(1:beam_geometry%nf))
allocate(beam_geometry%v(1:beam_geometry%nv,1:3))
allocate(beam_geometry%n(1:beam_geometry%nn,1:3))

do i = 1, beam_geometry%nf
    beam_geometry%f(i)%nv = 4
    allocate(beam_geometry%f(i)%vi(1:beam_geometry%f(i)%nv)) ! just 1 face with 4 vertices
    do j = 1, beam_geometry%f(i)%nv
        beam_geometry%f(i)%vi(j) = beamF1(i,j)
    end do
    beam_geometry%f(i)%mid(:) = beamMidpoints(i,:)
    beam_geometry%f(i)%ni = beamF2(i)
end do

do i = 1, beam_geometry%nn
    beam_geometry%n(i,:) = beamN(i,:)
end do

do i = 1, beam_geometry%nv
    beam_geometry%v(i,:) = beamV(i,:)
end do

! stop

! initialise the incident beam structure
beam_inc%prop(:) = (/0D0,0D0,-1D0/) ! propagation direction
beam_inc%nf_in = beam_geometry%nf ! number of facets
beam_inc%is_int = .false. ! incidence is, by default, external
beam_inc%id = 0
beam_inc%scatt_in = 0D0
beam_inc%scatt_out = 0D0
beam_inc%proj_area_in = 0D0
beam_inc%rec = 0 ! incidence is the 0th recursion
beam_inc%po = 0 ! by default, no outgoing power for the first beam
beam_inc%refl = 0 ! no total internal reflections
allocate(beam_inc%field_in(1:beam_inc%nf_in))
do i = 1, beam_inc%nf_in ! for each facet in the incident beam
    beam_inc%field_in(i)%ampl(:,:) = 0D0 ! init amplitude matrix
    beam_inc%field_in(i)%ampl(1,1) = 1D0 ! set amplitude matrix
    beam_inc%field_in(i)%ampl(2,2) = 1D0 
    beam_inc%field_in(i)%e_perp(:) = (/1D0,0D0,0D0/) ! set e-perp vector
    beam_inc%field_in(i)%fi = i ! set facet id in beam geometry
end do



end subroutine

!subroutine midPointsAndAreas(face_ids, verts, Midpoints, faceAreas)
!
!! subroutine midPointsAndAreas computes midpoints and areas of each facet (assumes triangle facets)
!
!integer(8), dimension(:,:) ,allocatable, intent(in) :: face_ids
!real(8), dimension(:,:), allocatable, intent(in) :: verts
!real(8), dimension(:,:), allocatable, intent(out) :: Midpoints
!real(8), dimension(:), allocatable, intent(out) :: faceAreas
!real(8), dimension(1:3) :: vecA, vecB, AcrossB
!real(8) temp_area
!
!integer(8) i,j
!
!print*,'========== start sr midPointsAndAreas'
!
!allocate(Midpoints(size(face_ids,1),3))
!    
!do i = 1, size(face_ids,1) ! for each face
!    Midpoints(i,1:3) = sum(verts(face_ids(i,1:3),1:3),1)/3
!end do
!
!! allocate faceAreas array
!allocate(faceAreas(1:size(face_ids,1)))
!faceAreas = 0 ! initialise
!
!do i = 1, size(face_ids,1) ! for each face
!    do j = 1,3 ! for each vertex in the face
!        vecA = verts(face_ids(i,j),1:3) - Midpoints(i,1:3) ! vector from midpoint to vertex j
!        if (j .eq. 3) then
!            vecB = verts(face_ids(i,1),1:3) - Midpoints(i,1:3) ! vector from midpoint to vertex j+1
!        else
!            vecB = verts(face_ids(i,j+1),1:3) - Midpoints(i,1:3) ! vector from midpoint to vertex j+1
!        end if
!        call cross(vecA,vecB,AcrossB,.false.) ! cross product, no normalisation, calculates parallelepid area
!        temp_area = sqrt(AcrossB(1)**2 + AcrossB(2)**2 + AcrossB(3)**2)/2 ! triangle area is half the above area
!        faceAreas(i) = faceAreas(i) + temp_area ! add to facet area
!    end do
!    !print*,'i',i
!    !print*,'face area',faceAreas(i)
!end do
!
!print*,'========== end sr midPointsAndAreas'
!
!end subroutine

subroutine SDATIN(  ifn,                & ! input filename
                    la,                 & ! wavelength
                    rbi,                & ! real refractive index
                    ibi,                & ! imaginary refractive index
                    rec,                & ! number of beam recursions
                    rot_method,         & ! rotation method
                    is_multithreaded,   & ! multithreading
                    num_orients,        & ! number of orientations
                    intellirot,         & ! intelligent multiple rotations method
                    c_method)             ! method of particle file input (read or cc crystal generation)
    
    ! sr SDATIN reads the input file

    integer(8), intent(out) :: rec ! max number of internal beam recursions
    real(8), intent(out) :: la, rbi, ibi ! wavelength, real and imaginary parts of the refractive index
    character(len=*), intent(in) :: ifn
    character(100), intent(out) :: rot_method ! rotation method
    character(100), intent(out) :: c_method ! method of particle file input
    logical, intent(out) :: is_multithreaded ! whether or not multithreaded subroutines should be used (where appropriate)
    logical, intent(out) :: intellirot ! whether or not to use intelligent euler angle choices for orientation avergaing
    integer(8), intent(out) :: num_orients ! number of orientations

    ! subroutine PDAS reads inputs from the input file
    ! cfn - crystal filename
    ! la - wavelength
    ! rbi - real part of refractive index
    ! ibi - imaginary part of refractive index
    ! aps - number of aperturesread_real

    is_multithreaded = .false. ! assume no multithreading, unless read from input file
    intellirot = .false. ! assume no intelligent euler angle choices, unless read from input file
    
    print*,'========== start sr SDATIN'
    write(101,*)'======================================================'
    write(101,*)'=====================JOB SETTINGS====================='
    write(101,*)'======================================================'


    ! cfn = read_string(ifn,"cfn") ! get crystal filename
    ! call StripSpaces(cfn) ! remove leading spaces
    ! print*,'particle filename: "',cfn(1:len(trim(cfn))),'"'
    ! write(101,*)'particle filename:      "',cfn(1:len(trim(cfn))),'"'

    c_method = read_string(ifn,"cmethod") ! get crystal filename
    call StripSpaces(c_method) ! remove leading spaces
    print*,'particle input method: "',c_method(1:len(trim(c_method))),'"'
    write(101,*)'particle input method:      "',c_method(1:len(trim(c_method))),'"'

    ! cft = read_string(ifn,"cft") ! get crystal filename
    ! call StripSpaces(cft) ! remove leading spaces
    ! print*,'particle file type: "',cft(1:len(trim(cft))),'"'  
    ! write(101,*)'particle file type:     "',cft(1:len(trim(cft))),'"' 

    ! afn = read_string(ifn,"afn") ! get crystal filename
    ! call StripSpaces(afn) ! remove leading spaces
    ! print*,'apertures filename: "',afn(1:len(trim(afn))),'"'    
    ! write(101,*)'apertures filename:     "',afn(1:len(trim(afn))),'"'    
    
    la = read_real(ifn,"lambda")
    print*,'lambda: ',la
    write(101,*)'lambda:                ',la
    
    rbi = read_real(ifn,"rbi")
    print*,'refractive index real: ',rbi
    write(101,*)'refractive index real:',rbi
    
    ibi = read_real(ifn,"ibi")
    print*,'refractive index imag: ',ibi
    write(101,*)'refractive index imag: ',ibi

    rec = read_int(ifn,"rec")
    print*,'max beam recursions: ',rec
    write(101,*)'max beam recursions:   ',rec
 
    rot_method = readString2(ifn,"rot") ! get rotation method
    if( rot_method(1:len(trim(rot_method))) .eq. 'off' .or. &
        rot_method(1:len(trim(rot_method))) .eq. 'euler' .or. &
        rot_method(1:len(trim(rot_method))) .eq. 'multi' .or. &
        rot_method(1:len(trim(rot_method))) .eq. 'none') then
        ! print*,'rotation method: "',rot_method(1:len(trim(rot_method))),'"'
    else
        print*,'Error, "',rot_method(1:len(trim(rot_method))),'" is not a valid option for rotation method'
        stop
    end if

    is_multithreaded = read_flag(ifn,"mt") ! check input file for multithread option

    intellirot = read_flag(ifn,"intellirot") ! check input file for intelligent euler angle choices for orientation avergaing

    if(rot_method(1:len(trim(rot_method))) .eq. 'multi') then
        num_orients = read_int(ifn,"rot multi")
    else
        num_orients = 1
    end if

    print*,'num_orients: ',num_orients
    write(101,*)'num_orients:           ',num_orients

    print*,'rotation method: "',rot_method(1:len(trim(rot_method))),'"'
    write(101,*)'rotation method:        "',rot_method(1:len(trim(rot_method))),'"'


    if (is_multithreaded) then
        print*,'multithreading: enabled'
        write(101,*)'multithreading:         ','enabled'
    else
        print*,'multithreading: disabled'
        write(101,*)'multithreading:         ','disabled'
    end if

    if (intellirot .and. num_orients .gt. 1) then
        print*,'multirot method: intelligent'
        write(101,*)'multirot method:        intelligent'
    else if (.not. intellirot .and. num_orients .gt. 1) then
        print*,'multirot method: random'
        write(101,*)'multirot method:        random'
    end if

    print*,'========== end sr SDATIN'
    
end subroutine

subroutine PDAL2(   job_params,     &
                    geometry)

! subroutine PDAL2 reads a file containing the particle geometry
! accepted file types: "obj" - .obj files, "mrt" - macke ray-tracing style

! the inputs are:
! cfn = particle filename
! cft = particle filetype
! the outputs are:
! num_vert      = number of unique particle vertices
! num_face      = number of crystal faces
! verts         = num_vert x 3 array of real values equal to the vertex coordinates, 
!   where each row corresponds to a unique vertex and each column corresponds to the x, y, and z components
! num_face_vert = num_face array of integers equal to the number of vertices in each face, 
!   where each row corresponds to each face
! face_ids      = num_face x num_face_vert_max array of integers equal to the vertex IDs, 
!   where each row corresponds to each face and each column corresponds to the vertices in the face.
!   A vertex ID is used to point to the row of the vertex in verts
        
    character(len=100) cfn ! particle filename
    character(len=100) cft ! particle filetype
    character(100) afn ! apertures filename
    integer(8) :: num_vert, num_face ! number of unique vertices, number of faces
    integer(8) :: num_norm ! number of face normals
    integer(8), dimension(:), allocatable :: num_face_vert ! number of vertices in each face
    integer(8), dimension(:), allocatable :: norm_ids ! face normal ID of each face
    real(8), dimension(:,:) ,allocatable :: verts ! unique vertices
    real(8), dimension(:,:) ,allocatable :: norms ! unique vertices, face vertex IDs, face normals
    integer(8), dimension(:,:) ,allocatable :: face_ids_temp ! temporary array to hold face vertex IDs
    integer(8), dimension(:,:) , allocatable :: face_ids ! face vertex IDs (for after excess columns have been truncated)
    character(100) c_method ! method of particle file input
    integer(8), dimension(:), allocatable :: apertures ! taken as parents parent facets
    type(cc_hex_params_type) cc_hex_params ! parameters for C. Collier Gaussian Random hexagonal columns/plates
    type(job_parameters_type), intent(inout) :: job_params ! parameters for C. Collier Gaussian Random hexagonal columns/plates
    type(geometry_type), intent(out) :: geometry ! the particle geometry data structure

    integer(8), parameter :: num_face_vert_max_in = 50 ! max number of vertices per face
    integer(8), parameter :: max_line_length = 1000 ! max number of characters in a line of thecrystal file (might need increasing if faces have many vertices)
    character(max_line_length) line ! a line in a file
    integer(8) face_string_length
    integer(8) entry_count, delim_count, digits_to_read
    logical is_current_char_slash
    integer(8) vertex_count
    integer(8) num_face_vert_max
    ! logical has_end_of_line_reached
    integer(8) i, io, j, k, m, o ! counting variables
    real(8), dimension(:), allocatable :: faceAreas ! area of each facet
    real(8), dimension(:,:), allocatable :: Midpoints ! face midpoints
    logical auto_apertures ! whether or noth automatic aperture asignment should be used
    character(len=32) cache_id_string
    integer seed(1:8)

    c_method = job_params%c_method
    cc_hex_params = job_params%cc_hex_params
    cft = job_params%cft
    cfn = job_params%cfn
    afn = job_params%afn
    auto_apertures = .false.

    if(job_params%resume .eqv. .true.) then

        write(cache_id_string,*) job_params%cache_id

        c_method = "read"
        cft = "mrt"
        cfn = "cache/"//adjustl(trim(cache_id_string)//"/unrotated.cry")
        afn = "cache/"//adjustl(trim(cache_id_string)//"/apertures.dat")

    end if

    if(trim(afn) .eq. "(null)") auto_apertures = .true.

    ! print*,'particle input method: "',c_method(1:len(trim(c_method))),'"'
    ! write(101,*)'======================================================'
    ! write(101,*)'=================PARTICLE INFORMATION================='
    ! write(101,*)'======================================================'

    if(c_method(1:len(trim(c_method))) .eq. "read") then ! if particle is to be read from file

        ! cfn = read_string(ifn,"cfn") ! get crystal filename
        ! call StripSpaces(cfn) ! remove leading spaces
        ! print*,'particle filename: "',cfn(1:len(trim(cfn))),'"'
        write(101,*)'particle filename:      "',cfn(1:len(trim(cfn))),'"'

        ! cft = read_string(ifn,"cft") ! get crystal filename
        ! call StripSpaces(cft) ! remove leading spaces
        ! print*,'particle file type: "',cft(1:len(trim(cft))),'"'  
        write(101,*)'particle file type:     "',cft(1:len(trim(cft))),'"'

        ! afn = read_string(ifn,"afn") ! get crystal filename
        ! call StripSpaces(afn) ! remove leading spaces
        ! print*,'apertures filename: "',afn(1:len(trim(afn))),'"'
        write(101,*)'apertures filename:     "',afn(1:len(trim(afn))),'"'

        write(101,*)'crystal file type: "',trim(cft),'"'
        ! write(101,*)'particle filename:      "',cfn(1:len(trim(cfn))),'"'    
        ! write(101,*)'particle file type:     "',trim(cft),'"'

        if (trim(cft) .eq. 'obj') then

            call read_wavefront(job_params%cfn,geometry)

        else if (trim(cft) .eq. 'mrt') then ! if macke ray-tracing style geometry file

            ! print*,'opening crystal file'
            open(unit = 10, file = cfn, status = 'old')
            read(10, *) num_face
            ! print*,'number of unique faces: ',num_face

            allocate(face_ids_temp(num_face,num_face_vert_max_in)) ! allocate an array for the vertex IDs in each face
            allocate(num_face_vert(num_face)) ! allocate array for the number of vertices in each face

            m = 0 ! counter number of vertices assigned to faces
            num_face_vert_max = 0
            do i = 1, num_face  ! scan through the face lines in crystal file
                read(10,"(a)",iostat=io)line
                if (io/=0) exit
                
                read(line,*) j
                num_face_vert(i) = j

                ! print*,'face',i,'had',num_face_vert(i),'vertices'

                if (j .gt. num_face_vert_max) num_face_vert_max = j

                if (j .gt. num_face_vert_max_in) then
                    print*,'face',i,' has',j,' vertices which is greater than the max value of',num_face_vert_max_in
                    print*,'Please increase the max vertices per face "num_face_vert_max_in"'
                    stop
                end if

                ! add to face array
                do k = 1, j
                    m = m + 1 ! update vertex counter
                    face_ids_temp(i,j-k+1) = m
                end do


            end do

            ! print*,'finished reading number of vertices in each face'

            num_vert = m

            ! print*,'total vertices to be read: ',num_vert

            allocate(face_ids(num_face,num_face_vert_max)) ! allocate an array for the vertex IDs in each face
            face_ids(1:num_face,1:num_face_vert_max) = face_ids_temp(1:num_face,1:num_face_vert_max)

            allocate(verts(num_vert,3)) ! allocate an array for the crystal vertex coorindates

            k = 0 ! counter to count how many vertices have been read
            do i = 1, num_face
                do j = 1, num_face_vert(i)
                    read(10,"(a)",iostat=io)line
                    if (io/=0) exit
                    k = k + 1 ! update vertex counter
                    read(line,*) verts(k,1), verts(k,2), verts(k,3)
                end do
            end do

            if (k .ne. num_vert) then
                print*,'error: expected to read', num_vert,'vertices but found',k,'vertices'
                stop
            else
                ! print*,'read expected number of vertices'
            end if

            ! print*,'succesfully finished reading mrt particle file'
                

            ! do i = 1, num_face ! for each face
            ! do i = 1, 10 ! for each face
            !         !print*,'vertices for this face: '
            !    print*,'vertex IDs for this face: '
            !    print*,face_ids(i,1:num_face_vert_max)
            !    ! print*,'vertex coordinates for this face: '
            !    ! print*,transpose(verts(face_ids(i,1:num_face_vert(i)),1:3))
            !    !print*,'midpoint: '
            !    !print*,sum(verts(face_ids(i,1:num_face_vert(i)),1:3),1)/num_face_vert(i)
            ! !    Midpoints(i,1:3) = sum(verts(face_ids(i,1:num_face_vert(i)),1:3),1)/num_face_vert(i)
            ! end do
            ! stop

            close(10)
            ! now put everything into a data strucute (work in progress)
            ! vertices
            allocate(geometry%v(1:num_vert,1:3))
            geometry%nv = num_vert
            do i = 1, geometry%nv
                geometry%v(i,:) = verts(i,:)
            end do

            ! faces
            allocate(geometry%f(1:num_face))
            geometry%nf = num_face
            do i = 1, geometry%nf
                allocate(geometry%f(i)%vi(1:num_face_vert(i)))
                do j = 1, num_face_vert(i)
                    geometry%f(i)%vi(j) = face_ids(i,j)
                end do
                geometry%f(i)%nv = num_face_vert(i)
            end do

        else
            print*,'error: particle geometry file type "',trim(cft),'" is not supported.'
            stop
        end if

        if (auto_apertures .eqv. .true.) then
            ! print*,'number of faces:',size(face_ids,1)
            if(geometry%nf > 50) print*,'warning: automatic aperture assignment for large numbers of input faces is not well supported.'
            do i = 1, geometry%nf
                geometry%f(i)%ap = i
            end do
        else
            ! call readApertures(afn, apertures, face_ids) ! read aperture assignments from file
            call read_apertures(afn, geometry)
        end if

    else if(c_method(1:len(trim(c_method))) .eq. "cc_hex") then ! if particle is to be generated according to Chris Collier hex method
        write(101,*)'attempting to make cc crystal'
        seed = [0, 0, 0, 0, 0, 0, 0, 0] ! Set the seed values
        call RANDOM_SEED(put=seed) ! Set the seed for the random number generator
        call CC_HEX_MAIN(cc_hex_params,geometry)
        call PROT_CC(geometry%v) ! align prism axis with z axis
    else
        print*,'error: ',c_method(1:len(trim(c_method))),' is not a valid method of particle file input'
        stop
    end if

    geometry%na = maxval(geometry%f(:)%ap) ! total number of apertures
    allocate(geometry%ap(1:geometry%na)) ! allocate

    call compute_geometry_midpoints(geometry)
    call compute_geometry_areas(geometry)

    call compute_geometry_com(geometry)
    if(job_params%debug >= 2) write(101,*)'centre of mass: ',geometry%com(:)
    if(job_params%debug >= 2) write(101,*)'translating centre of mass to origin...'
    call move_geometry_to_origin(geometry)

    call compute_geometry_midpoints(geometry) ! recompute after translating to origin
    call compute_geometry_normals(geometry)
    call compute_geometry_apertures(geometry)
    call compute_geometry_edge_vectors(geometry)

end subroutine

subroutine read_wavefront(cfn,geometry)

    type(geometry_type), intent(out) :: geometry 
    character(len=*), intent(in) :: cfn

    character(:), allocatable :: line ! a line in a file
    integer io, num_vert, num_norm, num_face, vi, ni, fi

    num_vert = 0
    num_norm = 0
    num_face = 0
    vi = 0
    ni = 0
    fi = 0

    allocate(character(len=get_max_line_len(cfn)) :: line)

    open(unit=10, file=cfn, status="old")

    do ! scan lines
        read(10,"(a)",iostat=io) line
        if (io/=0) exit

        line = adjustl(line) ! remove leading white space

        if (line(1:2) == "v ") then
            num_vert = num_vert + 1
        else if (line(1:2) == "vn") then
            num_norm = num_norm + 1
        else if (line(1:2) == "f ") then
            num_face = num_face + 1
        end if

        ! print*,"line=",trim(line)," iostat=",io

    end do

    print*,"num vertices: ",num_vert    
    print*,"num normals: ",num_norm    
    print*,"num faces: ",num_face    

    allocate(geometry%v(1:num_vert,1:3))
    allocate(geometry%n(1:num_face,1:3)) ! 1 normal per face
    allocate(geometry%f(1:num_face))
    geometry%nv = num_vert
    geometry%nn = num_face
    geometry%nf = num_face

    rewind(10)

    do ! scan lines
        read(10,"(a)",iostat=io) line
        if (io/=0) exit

        line = adjustl(line) ! remove leading white space

        if (line(1:2) == "v ") then ! read vertex
            vi = vi + 1
            geometry%v(vi,1:3) = read_wavefront_triplet(line)
        else if (line(1:2) == "f ") then ! read face
            fi = fi + 1
            call read_wavefront_face(line,geometry%f(fi))
        end if
    end do

    close(10)

    print*,"finished reading geometry"

end subroutine

subroutine read_wavefront_face(line,f)

    character(len=*), intent(in) :: line
    type(facet_type), intent(out) :: f
    integer iostat, i, nv, ii, start, end
    character(1) char
    logical ready

    nv = 0

    ! prescan - get number of vertices in face
    do i = 2, len_trim(line) ! ignore f line-leading char
        if (line(i:i) == ' ') then
            ready = .TRUE. ! ready to read next vertex part
            cycle
        else 
            if (ready) then
                nv = nv + 1
                ii = i ! mark start
                ready = .FALSE.
            end if
        end if
    end do

    f%nv = nv
    allocate(f%vi(nv))
    nv = 0 ! reset vertex counter

    ! read vertex
    i = 1 ! skip over leading f
    do 
        i = i + 1
        char = line(i:i)
        if (char /= ' ') then
            start = i
            do 
                i = i + 1
                char = line(i:i)
                if (char == ' ' .OR. i > len_trim(line)) then
                    end = i - 1
                    nv = nv + 1
                    f%vi(nv) = rip_vertex_id_from_part(line(start:end))
                    exit
                end if
            end do
        end if
        if (i > len_trim(line)) exit
    end do

end subroutine

function rip_vertex_id_from_part(part) result(vertex_id)

    character(len=*), intent(in) :: part
    integer :: vertex_id
    integer i

    do i = 1, len(part)
        if (part(i:i) == '/') then
            exit
        end if
    end do

    read(part(1:i-1),*) vertex_id

end function

function read_wavefront_triplet(line) result(vertex)

    character(len=*), intent(in) :: line
    real :: vertex(1:3)
    integer io

    read(line(3:), *,iostat=io) vertex(1:3)

    if (io /= 0) then
        print*,"Error reading line."
        stop
    end if

    if (any(isnan(vertex(:)))) then
        print*,"Detected NaN in line."
        stop
    end if

end function

integer function get_max_line_len(filename)

    character(len=*), intent(in) :: filename
    integer iostat, length, unit
    character(1) char

    unit = 10
    get_max_line_len = 0
    length = 0

    open(unit=unit,file=filename,status="old",action="read",iostat=iostat)
    if (iostat /= 0) then
        print*,"Error opening file: '",trim(filename),"'"
        stop
    end if

    do
        read(unit,'(A1)',advance="no",iostat=iostat) char
        if (length > get_max_line_len) then
            get_max_line_len = length
        end if
        length = length + 1

        if (iostat == -1) then ! end of file
            exit
        else if (iostat == -2) then ! end of line 
            length = 0
            continue
        else if (iostat /= 0) then ! exit
            exit
        end if
    end do 

    close(unit)

end function

character(100) function read_string(ifn,var)

! function read_string(ifn,var) reads the input file defined by variable ifn
! the output is a string contained in the variable var

    character(len=*), intent(in) :: ifn ! input filename
    character(len=*), intent(in) :: var ! variable to read
    character(100) output ! output variable
    character(100) line
    integer(8) num_lines, io, i
    logical success

    open(10,file = ifn, status = 'old')

    num_lines = 0  ! initialise line counter
    success = .false.

    do
        read(10,*,iostat=io)
        if (io/=0) exit
        num_lines = num_lines + 1
    end do

    rewind(10) ! back to top of file

    do i = 1,num_lines
        read(10,"(a)",iostat=io)line
        if (line(1:len(var)+1) .eq. var//' ') then
            output = line(len(var)+1:len(line))
            success = .true.
        end if

    end do

    close(10)

    if (.not. success) then
        print*,'Error: "',var,'" input argument not found'
        stop
    end if

    read_string = output

end function

character(100) function read_optional_string(ifn,var)

! function read_optional_string(ifn,var) reads the input file and looks for an optional flag
! if the flag is found, the flag is read and the output is a string contained in the variable var
! if the flag is not found, the output is a string: "#flagnotfound#" with trailing spaces in the variable var

    character(len=*), intent(in) :: ifn ! input filename
    character(len=*), intent(in) :: var ! variable to read
    character(100) output ! output variable
    character(100) line
    integer(8) num_lines, io, i
    logical success

    open(10,file = ifn, status = 'old')

    num_lines = 0  ! initialise line counter
    success = .false.

    do
        read(10,*,iostat=io)
        if (io/=0) exit
        num_lines = num_lines + 1
    end do

    rewind(10) ! back to top of file

    do i = 1,num_lines
        read(10,"(a)",iostat=io)line
        if (line(1:len(var)+1) .eq. var//' ') then
            output = line(len(var)+1:len(line))
            success = .true.
        end if

    end do

    close(10)

    if (.not. success) then
        output = "#flagnotfound#"
    end if

    read_optional_string = output

end function

character(100) function readString2(ifn,var)

! function read_string(ifn,var) reads the input file defined by variable ifn
! the output is a string contained in the variable var
! only reads until the first space (useful for more complicated inputs)

    character(len=*), intent(in) :: ifn ! input filename
    character(len=*), intent(in) :: var ! variable to read
    character(100) output ! output variable
    character(100) line
    integer(8) num_lines, io, i, j
    logical success, success2

    open(10,file = ifn, status = 'old')

    num_lines = 0  ! initialise line counter
    success = .false.
    success2 = .false.

    do
        read(10,*,iostat=io)
        if (io/=0) exit
        num_lines = num_lines + 1
    end do

    rewind(10) ! back to top of file

    do i = 1,num_lines
        read(10,"(a)",iostat=io)line
        if (line(1:len(var)+1) .eq. var//' ') then ! if match found
            j = 0
            do while (success2 .eqv. .false.)
                j = j + 1
                ! print*,line(len(var)+1+j:len(var)+1+j)
                if(line(len(var)+1+j:len(var)+1+j) .eq. ' ') success2 = .true.
            end do
            ! print*,'j',j
            output = line(len(var)+2:len(var)+j)
            success = .true.
        end if

    end do

    close(10)

    if (.not. success) then
        print*,'no input for rotation detected. Assuming no rotation required...'
        output = "none"
    end if

    readString2 = output

end function

real(8) function read_real(ifn,var)

! function read_string(ifn,var) reads the input file defined by variable ifn
! the output is a string contained in the variable var

character(len=*), intent(in) :: ifn ! input filename
character(len=*), intent(in) :: var ! variable to read
character(100) string_to_convert
real(8) output ! output variable
character(100) line
integer(8) num_lines, io, i
logical success

open(10,file = ifn, status = 'old')

num_lines = 0  ! initialise line counter
success = .false.

do
    read(10,*,iostat=io)
    if (io/=0) exit
    num_lines = num_lines + 1
end do

rewind(10) ! back to top of file

do i = 1,num_lines
    read(10,"(a)",iostat=io)line
    if (line(1:len(var)+1) .eq. var//' ') then
        string_to_convert = line(len(var)+1:len(line))
        call StripSpaces(string_to_convert)
        read(string_to_convert(1:len(trim(string_to_convert))),*) output
        success = .true.
    end if

end do

close(10)

if (.not. success) then
    print*,'Error getting crystal filename: "',var,'" argument not found'
    stop
end if

read_real = output

end function

integer(8) function read_int(ifn,var)

! function read_string(ifn,var) reads the input file defined by variable ifn
! the output is a string contained in the variable var

character(len=*), intent(in) :: ifn ! input filename
character(len=*), intent(in) :: var ! variable to read
character(100) string_to_convert
integer(8) output ! output variable
character(100) line
integer(8) num_lines, io, i
logical success

open(10,file = ifn, status = 'old')

num_lines = 0  ! initialise line counter
success = .false.

do
    read(10,*,iostat=io)
    if (io/=0) exit
    num_lines = num_lines + 1
end do

rewind(10) ! back to top of file

do i = 1,num_lines
    read(10,"(a)",iostat=io)line
    if (line(1:len(var)+1) .eq. var//' ') then
        string_to_convert = line(len(var)+1:len(line))
        call StripSpaces(string_to_convert)
        read(string_to_convert(1:len(trim(string_to_convert))),*) output
        success = .true.
    end if

end do

close(10)

if (.not. success) then
    print*,'Error getting crystal filename: "',var,'" argument not found'
    stop
end if

read_int = output

end function

logical function read_flag(ifn,var)

! function read_string(ifn,var) reads the input file defined by variable ifn
! the output is a string contained in the variable var

character(len=*), intent(in) :: ifn ! input filename
character(len=*), intent(in) :: var ! variable to read
! character(100) string_to_convert
! integer(8) output ! output variable
character(100) line
integer(8) num_lines, io, i
logical success

open(10,file = ifn, status = 'old')

read_flag = .false. ! assume false
num_lines = 0  ! initialise line counter
success = .false.

do
    read(10,*,iostat=io)
    if (io/=0) exit
    num_lines = num_lines + 1
end do

rewind(10) ! back to top of file

do i = 1,num_lines
    read(10,"(a)",iostat=io)line
    if (line(1:len(var)+1) .eq. var//' ') then
        ! string_to_convert = line(len(var)+1:len(line))
        ! call StripSpaces(string_to_convert)
        ! read(string_to_convert(1:len(trim(string_to_convert))),*) output
        print*,'enabled optional flag: "'//var//'"'
        read_flag = .true.
        success = .true.
    end if

end do

close(10)
! stop
! if (.not. success) then
!     print*,'Error getting crystal filename: "',var,'" argument not found'
!     stop
! end if

! read_int = output

end function

end module input_mod
