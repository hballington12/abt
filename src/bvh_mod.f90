! bvh_mod.f90
! submodule for bounding volume hierarchies

module bvh_mod

    use types_mod
    use misc_submod

    implicit none

    contains 

    subroutine cast_ray(r,o,cid_start,geometry,intsct,fi,mask)

        ! sr cast_ray
        ! overview:
        ! casts a ray with propagation direction r from ray starting coordinate o
        ! cid_start specifies the cell in which the ray starts
        ! geometry is the geometry data strucure, which contains the bvh
        ! intsct returns true if an intersection is found, and false if the ray is outgoing
        ! fi returns the face index of the intersection if an intersection is found, otherwise returns 0
        ! mask is an optional logical array - if mask(i) is false, then facet i is excluded from the intersection check
        ! provides several times speedup over searching all facets
        ! this is done by utilising the bvh to reduce the number of facets that need to be looked at        

        real(8), intent(in) :: r(1:3) ! ray propagation direction
        real(8), intent(in) :: o(1:3) ! ray origin
        type(geometry_type), intent(in) :: geometry ! geoemtry structure
        integer(8), intent(in) :: cid_start ! starting cell id
        logical, intent(out) :: intsct ! whether or not an intersection was found
        integer(8), intent(out) :: fi ! intersected facet id
        logical, dimension(:), allocatable, intent(in), optional :: mask ! logical array of facets to ignore

        logical done ! whether or not we are done
        integer(8) i, j ! counters
        integer(8) pid ! a parent cell id
        integer(8) di ! a depth index (1 to number of depth levels in bvh)
        integer(8) ci ! a cell index (1 to number of cells at current depth)
        integer(8) chid ! a child cell id
        integer(8) cid ! a cell id
        type(bvh_cell_type) cell, child_cell ! a cell, a child cell
        logical, dimension(:), allocatable :: checked ! whether or not this cell has been checked
        real(8) dist ! a distance to an intersection
        integer(8), dimension(:), allocatable :: fis ! facet ids which found intersection
        real(8), dimension(:), allocatable :: distances ! distances to facet ids with intersection

        ! initialise
        allocate(checked(1:geometry%bvh%nc_total))
        allocate(fis(1:geometry%nf))
        allocate(distances(1:geometry%nf))
        checked(:) = .false.
        fi = 0
        intsct = .false.

        ! cast ray
        ! input: ray direction, ray origin, origin cell id
        ! output: intersection, facet id
        cid = cid_start ! starting cell
        di = geometry%bvh%cp(cid)%di ! get the depth of this cell
        ci = geometry%bvh%cp(cid)%ci ! get the cell index of this cell
        done = .false. ! init
        do while (.not. done) ! do while not finished casting ray
            cell = geometry%bvh%depth(di)%cell(ci) ! get cell from bvh
            cid = cell%id ! get cell id
            intsct = .false. ! reset
            ! print*,'cell:',cell%id,'parent=',cell%pid,'nch',cell%nch
            if(cell%nch > 0) then ! if cell has childeren
                do i = 1, cell%nch ! for each child cell
                    chid = cell%chid(i) ! get child cell id
                    if(.not. checked(chid)) then ! if child cell has not been checked
                        child_cell = geometry%bvh%depth(geometry%bvh%cp(chid)%di)%cell(geometry%bvh%cp(chid)%ci) ! get child cell
                        call bvh_cell_intersection_check(child_cell,o(:),r(:),intsct) ! see if the ray passes through this cell
                    end if
                    if(intsct) then ! if the ray passes through this cell... and then traverse down (see below)
                        exit ! break out the do loop if intersection found, could sort by distance at a later date...
                    else
                        checked(cid) = .true. ! if no intersection, cell has been checked
                    end if
                end do
                if(intsct) then ! if the ray passes through a child cell
                    ! traverse down to the child cell
                    di = geometry%bvh%cp(chid)%di ! new depth is child depth
                    ci = geometry%bvh%cp(chid)%ci ! new cell index is child index
                else ! else, if ray did not pass through a child cell
                    if(cell%id /= 1) then ! if not root cell
                        ! traverse up
                        pid = geometry%bvh%depth(di)%cell(ci)%pid
                        di = geometry%bvh%cp(pid)%di
                        ci = geometry%bvh%cp(pid)%ci
                    else ! if root cell
                        ! print*,'reached cell root. ray must be outgoing. stopping...'
                        done = .true. ! finished
                    end if
                end if
            else ! if cell has no children
                if(checked(cid)) then ! if cell has been checked
                    ! traverse up
                    pid = geometry%bvh%depth(di)%cell(ci)%pid ! get parent cell id
                    di = geometry%bvh%cp(pid)%di ! new depth is parent depth
                    ci = geometry%bvh%cp(pid)%ci ! new cell index is parent index
                else ! if cell has not been checked
                    ! check for geometry intersection
                    j = 0 ! init counter for number of intersections found
                    do i = 1, size(cell%fi(:),1) ! for each face inside this cell
                        ! look for intersection with this facet
                        if(present(mask)) then ! if mask present
                            if(mask(cell%fi(i)) .eqv. .false.) then ! if this facet is masked
                                ! no check
                            else ! else, if this facet is unmasked
                                call ray_polygon_intersection_check(geometry%v(geometry%f(cell%fi(i))%vi(:),:), &
                                geometry%f(cell%fi(i))%evec(:,:), &
                                geometry%n(geometry%f(cell%fi(i))%ni,:), &
                                r(:), o(:), intsct, dist)
                                if(intsct) then ! if ray intersected with this face
                                j = j + 1 ! update counter
                                distances(j) = dist ! save distance to intersection
                                fis(j) = cell%fi(i) ! save facet id
                                end if
                            end if
                        else ! else, if no mask present
                            call ray_polygon_intersection_check(geometry%v(geometry%f(cell%fi(i))%vi(:),:), &
                                                                geometry%f(cell%fi(i))%evec(:,:), &
                                                                geometry%n(geometry%f(cell%fi(i))%ni,:), &
                                                                r(:), o(:), intsct, dist)
                            if(intsct) then ! if ray intersected with this face
                                j = j + 1 ! update counter
                                distances(j) = dist ! save distance to intersection
                                fis(j) = cell%fi(i) ! save facet id
                            end if
                        end if
                    end do
                    if (j == 0) then ! if no intersection found
                        if(cell%id /= 1) then ! if not root cell
                            checked(cid) = .true. ! cell has now been checked
                            ! traverse up
                            pid = geometry%bvh%depth(di)%cell(ci)%pid ! get parent cell id
                            di = geometry%bvh%cp(pid)%di ! new depth is parent depth
                            ci = geometry%bvh%cp(pid)%ci ! new cell index is parent index
                        else ! if root cell
                            ! print*,'no intersection and at cell root. ray is outgoing...'
                            intsct = .false. ! no intersection found
                            done = .true. ! finished
                        end if
                    else if(j == 1) then ! if 1 intersection found
                        ! print*,'ending... intersection found with facet',fis(1),'distance travelled to intersection:',distances(1)
                        fi = fis(1) ! get facet id
                        intsct = .true. ! intersection found
                        done = .true. ! finished
                    else ! if more than 1 intersection found, find location of shortest distance
                        ! print*,'multiple intersections... closest facet was:',fis(minloc(distances(1:j)))
                        fi = fis(minloc(distances(1:j),1)) ! get the id of the closest facet
                        intsct = .true. ! intersection found
                        done = .true. ! finished
                    end if
                end if
            end if
        end do

    end subroutine

    subroutine ray_polygon_intersection_check(v,e,n,r,o,intsct,d)

        real(8), dimension(:,:), intent(in) :: v ! polygon vertices, N x 3, ordered anti-clockwise from outside
        real(8), dimension(:,:), intent(in) :: e ! polygon edge vectors, N x 3, ordered anti-clockwise from outside
        real(8), intent(in) :: n(1:3) ! polygon normal, facing outwards
        real(8), intent(in) :: r(1:3) ! ray propagation direction
        real(8), intent(in) :: o(1:3) ! ray origin
        real(8), intent(out) :: d
        logical, intent(out) :: intsct ! whether or not there was an intersection

        integer nv, i, j
        real(8) rdotn
        ! real(8) d
        real(8) p(1:3) ! vector to intersection with plane
        real(8) rr(1:3) ! vector from vertex to intersection with plane
        real(8) en(1:3) ! edge normal vector

        intsct = .false. ! init
        nv = size(v,1) ! get number of vertices in polygon
        rdotn = dot_product(r(:),n(:)) ! dot product of ray and polygon normal
        d = dot_product(v(1,:)-o(:),n(:)) / rdotn ! get signed distance to plane intersection
        if(d > 1d-6) then ! if intersection is in forwards direction
            j = 0 ! init counter
            p(:) = d*r(:) + o(:) ! vector to intersection with plane
            do i = 1, nv ! for each vertex in polygon
                call cross(e(i,:),n(:),en(:),.false.) ! get en, the edge normal
                rr(:) = p(:) - v(i,:) ! get vector from vertex to intersection with plane
                if(dot_product(en(:),rr(:)) <= 0d0) j = j + 1 ! if edge check was true
            end do
            if(j == nv) then ! if edge check was true for all edges
                ! print*,'intersection found' ! intersection was within bounded polygon
                intsct = .true.
            end if
        end if

    end subroutine

    subroutine bvh_cell_intersection_check(cell,ray_origin,ray,intersection)
        type(bvh_cell_type), intent(in) :: cell
        real(8), intent(in) :: ray_origin(1:3)
        real(8), intent(in) :: ray(1:3)
        logical, intent(out) :: intersection
    
        integer :: i
        real(8) :: dist

        intersection = .false.
    
        do i = 1, 6 ! for each face in the cell

            call ray_polygon_intersection_check(cell%f(i)%v(:,:),cell%f(i)%e(:,:),cell%f(i)%c(1:3),ray(:),ray_origin(:),intersection, dist)
            if(intersection) exit

        end do

    end subroutine bvh_cell_intersection_check
    
    
    subroutine test_bvh(job_params,geometry)

        type(job_parameters_type), intent(in) :: job_params
        type(geometry_type), intent(in) :: geometry

        type(bvh_cell_type) cell, child_cell
        integer(8) fi, i, pid, di, ci, cid, chid, nf, fi2
        integer j
        real(8) vec(1:3), ray_origin(1:3)
        real(8) dist
        logical done
        logical intersection
        logical, dimension(:), allocatable :: checked
        integer(8), dimension(:), allocatable :: fis ! facet ids which found intersection
        real(8), dimension(:), allocatable :: distances ! distances to facet ids with intersection
        real(8) start, finish

        print*,'testing bvh...'

        ! init
        allocate(checked(1:geometry%bvh%nc_total))
        allocate(fis(1:geometry%nf))
        allocate(distances(1:geometry%nf))
        print*,'geometry%bvh%nc_total',geometry%bvh%nc_total
        checked(:) = .false. ! init

        fi = 6000 ! start facet
        print*,'start facet is:',fi

        vec(:) = (/0,0,-1/)
        call normalise_vec(vec)
        print*,'propagation direction is:',vec(:)

        call cpu_time(start)
        print*,'num facets',geometry%nf
        do i = 1, geometry%nf ! test

            fi = i

            ray_origin = geometry%f(fi)%mid(:)
            ! print*,'ray origin is:',ray_origin(:)

            ! find cell in which the ray originates
            di = geometry%bvh%fp(fi)%di
            ci = geometry%bvh%fp(fi)%ci
            ! print*,'starting cell depth index: di',di
            ! print*,'starting cell cell index: ci',ci
            cell = geometry%bvh%depth(di)%cell(ci)
            cid = cell%id
            ! print*,'smallest cell containing this point is cell id:',cid,'at depth:',di

            call cast_ray(vec(:),ray_origin(:),cid,geometry,intersection,fi2)
            ! print*,'ray cast found intersection?',intersection,'fi',fi2

        end do
        call cpu_time(finish)
        print'(a,f9.6)','time taken with bvh: ',finish-start

        ! call cpu_time(start)
        ! ! test compatison without bvh
        ! j = 0 ! init counter for number of intersections found
        ! do i = 1, geometry%nf ! for each face in geometry
        !     ! print*,'vertices for this face are:',geometry%v(geometry%f(cell%fi(i))%vi(:),:)
        !     call ray_polygon_intersection_check(geometry%v(geometry%f(i)%vi(:),:), &
        !                                         geometry%f(i)%evec(:,:), &
        !                                         geometry%n(geometry%f(i)%ni,:), &
        !                                         vec(:), ray_origin(:), intersection, dist)
        !     if(intersection) then
        !         ! print*,'found intersection with facet ',i,'!!!'
        !         j = j + 1 ! update counter
        !         distances(j) = dist ! distance to intersection
        !         fis(j) = i ! save facet id
        !     end if
        ! end do
        ! if (j == 0) then ! if no intersection found
        !     if(cell%id /= 1) then ! if not root cell
        !         print*,'no intersection.'
        !     end if
        ! else if(j == 1) then ! if 1 intersection found
        !     print*,'ending... intersection found with facet',fis(1),'distance travelled to intersection:',distances(1)
        ! else ! if more than 1 intersection found, find location of shortest distance
        !     print*,'closest facet intersection was:',fis(minloc(distances(1:j)))
        ! end if
        ! call cpu_time(finish)

        ! print*,'time taken without bvh:',finish-start

            

        ! stop

    end subroutine

    subroutine make_bvh(job_params,geometry)

        ! make bvh
        ! makes the bounding volume hierarchy
        ! wraps a box around the geometry, which forms a cell
        ! then subdivides the cell and determines which vertices of the geometry are in each of the new cells
        ! then shrink the new cells to reduce the volume
        ! this process repeats recursively until a maximum depth has been reached,
        !   or until a maximum number of vertices in each cell is less than a threshold number,
        !   or until the cells can no longer be subdivided without having less than 3 vertices in a cell

        integer(8), parameter :: max_depth = 20 ! must be 1 or greater
        ! integer(8), parameter :: max_num_verts = 4096 ! must be 3 or greater
        ! integer(8), parameter :: max_num_verts = 2048 ! must be 3 or greater
        ! integer(8), parameter :: max_num_verts = 1024 ! must be 3 or greater
        ! integer(8), parameter :: max_num_verts = 512 ! must be 3 or greater
        ! integer(8), parameter :: max_num_verts = 256 ! must be 3 or greater
        ! integer(8), parameter :: max_num_verts = 128 ! must be 3 or greater
        integer(8), parameter :: max_num_verts = 64 ! must be 3 or greater
        ! integer(8), parameter :: max_num_verts = 32 ! must be 3 or greater
        ! integer(8), parameter :: max_num_verts = 16 ! must be 3 or greater
        ! integer(8), parameter :: max_num_verts = 8 ! must be 3 or greater
        ! integer(8), parameter :: max_num_verts = 4 ! must be 3 or greater
        ! integer(8), parameter :: max_num_verts = 3 ! must be 3 or greater

        type(job_parameters_type), intent(in) :: job_params
        type(geometry_type), intent(inout) :: geometry
        type(bvh_type) bvh ! bounding volume tree
        type(bvh_cell_type) cell_a, cell_b, cell

        integer(8) i, j, k, nc, nv, dim, ncc, m, vi, nf, fi, nch, pid
        integer(8), dimension(:), allocatable :: mapping ! points to a position in the parent depth
        logical success
        integer(8) actual_max_depth ! max depth actually reached
        integer(8) nc_total ! total number of cells
        integer(8), dimension(:), allocatable :: fi_cache ! cache for facet ids
        integer(8), dimension(:), allocatable :: fi_uniq ! unique, sorted facet ids
        real(8) mid(1:3) ! a midpoint
        logical result

        type vertex_mapping_type
            integer(8), dimension(:), allocatable :: fi
            integer(8) nf
        end type vertex_mapping_type
        type(vertex_mapping_type), dimension(:), allocatable :: vmap

        actual_max_depth = 1

        if(job_params%debug >= 1) write(101,*)'start make bvh...'
        if(job_params%debug >= 2) then
            write(101,*)'bvh max depth:',max_depth
            write(101,*)'bvh max vertices threshold:',max_num_verts            
        end if

        allocate(bvh%depth(1:max_depth+1)) ! allocate depths for each level of depth in bvh tree, where index 1 is the outer cell

        ! make the first cell
        call make_first_cell(bvh,geometry)
        nc_total = 1

        do i = 2, max_depth ! for each level of depth
            nc = size(bvh%depth(i-1)%cell(:),1)
            ncc = 0 ! init counter
            do j = 1, nc
                nv = bvh%depth(i-1)%cell(j)%nv
                if(nv > max_num_verts) ncc = ncc + 1
            end do
            if(allocated(mapping)) deallocate(mapping)
            allocate(mapping(1:ncc))
            ncc = 0 ! reset
            do j = 1, nc
                nv = bvh%depth(i-1)%cell(j)%nv
                if(nv > max_num_verts) then
                    ncc = ncc + 1
                    mapping(ncc) = j ! save position of cell with enough vertices in parent depth
                end if
            end do
            allocate(bvh%depth(i)%cell(1:ncc*2)) ! allocate space to hold new cells at this depth (each cell at the depth above can be split into 2)
            bvh%depth(i)%cell(:)%nv = 0 ! init the number of vertices in each cell
            do k = 1, ncc ! for each cell of the depth level above
                j = mapping(k)
                nv = bvh%depth(i-1)%cell(j)%nv
                if(nv > max_num_verts) then ! if cell had enough vertices, attempt split
                    ! determine along which dimension the cell should be split
                    call determine_split_dim_1(bvh%depth(i-1)%cell(j),dim)
                    ! split the cell along the chosen dimension
                    call split_cell(geometry,bvh%depth(i-1)%cell(j),dim,cell_a,cell_b,success)
                    ! if split failed, try to split along a different dimension
                    if(.not. success) call split_cell(geometry,bvh%depth(i-1)%cell(j),mod(dim,3)+1,cell_a,cell_b,success)
                    if(.not. success) call split_cell(geometry,bvh%depth(i-1)%cell(j),mod(dim+1,3)+1,cell_a,cell_b,success)
                    if(success) then ! if successfully split the cell
                        ! assign cell ids
                        nc_total = nc_total + 1
                        cell_a%id = nc_total
                        nc_total = nc_total + 1
                        cell_b%id = nc_total
                        ! allocate space to hold the children cell ids
                        allocate(bvh%depth(i-1)%cell(j)%chid(1:2)) ! cell has 2 children
                        bvh%depth(i-1)%cell(j)%nch = 2 ! cell has 2 children
                        bvh%depth(i-1)%cell(j)%chid(1) = cell_a%id ! save child cell 1
                        bvh%depth(i-1)%cell(j)%chid(2) = cell_b%id ! save child cell 2
                        ! shrink the cells
                        call shrink_cell(cell_a,geometry)
                        call shrink_cell(cell_b,geometry)
                        ! add the cells to this depth
                        bvh%depth(i)%cell(2*k-1) = cell_a
                        bvh%depth(i)%cell(2*k) = cell_b
                    end if
                    ! also update the maximum depth reached with meaningful cells
                    if(i > actual_max_depth) actual_max_depth = i
                end if
            end do
        end do

        ! bvh made, now assign to the geometry, while pruning cells with no vertices
        nc_total = 0 ! init
        allocate(geometry%bvh%depth(1:actual_max_depth)) ! allocate space for each level of depth reached
        geometry%bvh%nd = actual_max_depth ! save number of depth levels
        do i = 1, actual_max_depth ! for each depth level in bvh
            ! get the number of cells with number of vertices greater than 0
            nc = size(bvh%depth(i)%cell(:),1) ! number of cells in depth
            ncc = 0 ! init counter to count number of cells with number of vertices greater than 0
            do j = 1, nc
                if(bvh%depth(i)%cell(j)%nv > 0) ncc = ncc + 1
            end do
            geometry%bvh%depth(i)%nc = ncc ! save number of cells at this depth
            nc_total = nc_total + ncc
            allocate(geometry%bvh%depth(i)%cell(1:ncc)) ! allocate space to hold cells
            ncc = 0 ! reset counter
            do j = 1, nc
                if(bvh%depth(i)%cell(j)%nv > 0) then
                    ncc = ncc + 1
                    geometry%bvh%depth(i)%cell(ncc) = bvh%depth(i)%cell(j)
                end if
            end do
        end do

        ! add cell pointer information
        allocate(geometry%bvh%cp(1:nc_total))
        do i = 1, geometry%bvh%nd ! for each depth level
            do j = 1, geometry%bvh%depth(i)%nc ! for each cell
                cell = geometry%bvh%depth(i)%cell(j) ! get cell
                geometry%bvh%cp(cell%id)%di = i ! set cell depth index to i
                geometry%bvh%cp(cell%id)%ci = j ! set cell index to j
            end do
        end do

        if(job_params%debug >= 2) write(101,*) 'bvh actual max depth:',actual_max_depth
        if(job_params%debug >= 2) write(101,*) 'bvh total cells:',nc_total
        geometry%bvh%nc_total = nc_total

        ! also add information about the facets with vertices in each cell
        ! first, get a mapping for each vertex that connects it with the facets
        allocate(vmap(1:geometry%nv)) ! allocate structure to for each vertex in geometry
        do i = 1, geometry%nf ! for each facet
            do j = 1, geometry%f(i)%nv ! for each vertex in the facet
                vi = geometry%f(i)%vi(j) ! get vertex id
                vmap(vi)%nf = vmap(vi)%nf + 1 ! update counter for this vertex
            end do
        end do
        do i = 1, geometry%nv ! for each vertex
            allocate(vmap(i)%fi(1:vmap(i)%nf)) ! allocate space to hold the facet ids that it is part of
        end do
        vmap(:)%nf = 0 ! reset counter
        do i = 1, geometry%nf ! for each facet
            do j = 1, geometry%f(i)%nv ! for each vertex in the facet
                vi = geometry%f(i)%vi(j) ! get vertex id
                vmap(vi)%nf = vmap(vi)%nf + 1 ! update counter
                vmap(vi)%fi(vmap(vi)%nf) = i ! save facet id
            end do
        end do
        ! second, add the facet information to the bvh
        do i = 1, size(geometry%bvh%depth(:),1) ! for each level of depth in the bvh
            do j = 1, size(geometry%bvh%depth(i)%cell(:),1) ! for each cell at this depth
                ! determine the number of unique facets that have vertices in this cell
                cell = geometry%bvh%depth(i)%cell(j) ! get cell
                if(cell%nch == 0) then ! if cell has no children
                    ! make a list of facets for each vertex
                    nf = 0 ! init counter for number of facets in this cell
                    do k = 1 , cell%nv ! for each vertex in this cell
                        vi = cell%vi(k) ! get the vertex id
                        nf = nf + vmap(vi)%nf ! sum the number of faces that use the vertex
                    end do
                    if(allocated(fi_cache)) deallocate(fi_cache)
                    allocate(fi_cache(1:nf))
                    nf = 0 ! reset counter
                    do k = 1 , cell%nv ! for each vertex in this cell
                        vi = cell%vi(k) ! get the vertex id
                        do m = 1, vmap(vi)%nf ! for each face that uses this vertex
                            nf = nf + 1 ! update counter
                            fi_cache(nf) = vmap(vi)%fi(m) ! save facet id to temporary array
                        end do
                    end do
                    call unique_int(fi_cache,fi_uniq) ! remove duplicates
                    nf = size(fi_uniq,1) ! get number of unique facet ids in this cell
                    geometry%bvh%depth(i)%cell(j)%nf = nf ! save number of unique faces
                    allocate(geometry%bvh%depth(i)%cell(j)%fi(1:nf)) ! allocate space
                    do k = 1, nf ! for each unique facet in this cell
                        geometry%bvh%depth(i)%cell(j)%fi(k) = fi_uniq(k) ! save face ids
                    end do
                end if
            end do
        end do

        ! make the facet pointer array
        allocate(geometry%bvh%fp(1:geometry%nf))
        do i = 1, geometry%nf ! for each facet in the geometry
            mid(:) = geometry%f(i)%mid(:) ! get midpoint
            pid = 0 ! init
            do j = 1, geometry%bvh%nd ! for each depth level
                do k = 1, geometry%bvh%depth(j)%nc ! for each cell
                    if(geometry%bvh%depth(j)%cell(k)%pid == pid) then ! if correct parent
                        call is_vertex_in_cell(geometry%bvh%depth(j)%cell(k),mid,result)
                        ! print*,'is vertex in cell?',result,'depth=',j,'cell id',geometry%bvh%depth(j)%cell(k)%id,'parent id:',geometry%bvh%depth(j)%cell(k)%pid,'num children:',geometry%bvh%depth(j)%cell(k)%nch
                        if(result) then
                            geometry%bvh%fp(i)%di = j
                            geometry%bvh%fp(i)%ci = k
                            pid = geometry%bvh%depth(j)%cell(k)%id
                            exit
                        end if
                    end if
                end do
            end do
            ! print*,'smallest cell containing the midpoint of facet:',i,'was at depth:',geometry%bvh%fp(i)%di,'and cell index:',geometry%bvh%fp(i)%ci
        end do

        ! make the bvh faces
        do i = 1, size(geometry%bvh%depth(:),1) ! for each level of depth in the bvh
            do j = 1, size(geometry%bvh%depth(i)%cell(:),1) ! for each cell at this depth
                cell = geometry%bvh%depth(i)%cell(j) ! get cell
                ! make faces (bit messy)
                ! plane coefficients
                cell%f(1)%c(1:4) = (/0d0,-1d0,0d0,cell%ymin/)
                cell%f(2)%c(1:4) = (/0d0,0d0,1d0,cell%zmax/)
                cell%f(3)%c(1:4) = (/1d0,0d0,0d0,cell%xmax/)
                cell%f(4)%c(1:4) = (/-1d0,0d0,0d0,cell%xmin/)
                cell%f(5)%c(1:4) = (/0d0,0d0,-1d0,cell%zmin/)
                cell%f(6)%c(1:4) = (/0d0,1d0,0d0,cell%ymax/)
                ! vertices
                cell%f(1)%v(1,1:3) = (/cell%xmax,cell%ymin,cell%zmin/)
                cell%f(1)%v(2,1:3) = (/cell%xmin,cell%ymin,cell%zmin/)
                cell%f(1)%v(3,1:3) = (/cell%xmax,cell%ymin,cell%zmax/)
                cell%f(1)%v(4,1:3) = (/cell%xmin,cell%ymin,cell%zmax/)
                cell%f(2)%v(1,1:3) = (/cell%xmin,cell%ymin,cell%zmax/)
                cell%f(2)%v(2,1:3) = (/cell%xmax,cell%ymin,cell%zmax/)
                cell%f(2)%v(3,1:3) = (/cell%xmax,cell%ymax,cell%zmax/)
                cell%f(2)%v(4,1:3) = (/cell%xmin,cell%ymax,cell%zmax/)
                cell%f(3)%v(1,1:3) = (/cell%xmax,cell%ymin,cell%zmax/)
                cell%f(3)%v(2,1:3) = (/cell%xmax,cell%ymin,cell%zmin/)
                cell%f(3)%v(3,1:3) = (/cell%xmax,cell%ymax,cell%zmin/)
                cell%f(3)%v(4,1:3) = (/cell%xmax,cell%ymax,cell%zmax/)
                cell%f(4)%v(1,1:3) = (/cell%xmin,cell%ymin,cell%zmin/)
                cell%f(4)%v(2,1:3) = (/cell%xmin,cell%ymin,cell%zmax/)
                cell%f(4)%v(3,1:3) = (/cell%xmin,cell%ymax,cell%zmax/)
                cell%f(4)%v(4,1:3) = (/cell%xmin,cell%ymax,cell%zmin/)
                cell%f(5)%v(1,1:3) = (/cell%xmin,cell%ymin,cell%zmin/)
                cell%f(5)%v(2,1:3) = (/cell%xmin,cell%ymax,cell%zmin/)
                cell%f(5)%v(3,1:3) = (/cell%xmax,cell%ymax,cell%zmin/)
                cell%f(5)%v(4,1:3) = (/cell%xmax,cell%ymin,cell%zmin/)
                cell%f(6)%v(1,1:3) = (/cell%xmax,cell%ymax,cell%zmax/)
                cell%f(6)%v(2,1:3) = (/cell%xmax,cell%ymax,cell%zmin/)
                cell%f(6)%v(3,1:3) = (/cell%xmin,cell%ymax,cell%zmin/)
                cell%f(6)%v(4,1:3) = (/cell%xmin,cell%ymax,cell%zmax/)
                ! edge vectors
                cell%f(1)%e(1,1:3) = cell%f(1)%v(2,1:3) - cell%f(1)%v(1,1:3)
                cell%f(1)%e(2,1:3) = cell%f(1)%v(3,1:3) - cell%f(1)%v(2,1:3)
                cell%f(1)%e(3,1:3) = cell%f(1)%v(4,1:3) - cell%f(1)%v(3,1:3)
                cell%f(1)%e(4,1:3) = cell%f(1)%v(1,1:3) - cell%f(1)%v(4,1:3)
                cell%f(2)%e(1,1:3) = cell%f(2)%v(2,1:3) - cell%f(2)%v(1,1:3)
                cell%f(2)%e(2,1:3) = cell%f(2)%v(3,1:3) - cell%f(2)%v(2,1:3)
                cell%f(2)%e(3,1:3) = cell%f(2)%v(4,1:3) - cell%f(2)%v(3,1:3)
                cell%f(2)%e(4,1:3) = cell%f(2)%v(1,1:3) - cell%f(2)%v(4,1:3)
                cell%f(3)%e(1,1:3) = cell%f(3)%v(2,1:3) - cell%f(3)%v(1,1:3)
                cell%f(3)%e(2,1:3) = cell%f(3)%v(3,1:3) - cell%f(3)%v(2,1:3)
                cell%f(3)%e(3,1:3) = cell%f(3)%v(4,1:3) - cell%f(3)%v(3,1:3)
                cell%f(3)%e(4,1:3) = cell%f(3)%v(1,1:3) - cell%f(3)%v(4,1:3)
                cell%f(4)%e(1,1:3) = cell%f(4)%v(2,1:3) - cell%f(4)%v(1,1:3)
                cell%f(4)%e(2,1:3) = cell%f(4)%v(3,1:3) - cell%f(4)%v(2,1:3)
                cell%f(4)%e(3,1:3) = cell%f(4)%v(4,1:3) - cell%f(4)%v(3,1:3)
                cell%f(4)%e(4,1:3) = cell%f(4)%v(1,1:3) - cell%f(4)%v(4,1:3)
                cell%f(5)%e(1,1:3) = cell%f(5)%v(2,1:3) - cell%f(5)%v(1,1:3)
                cell%f(5)%e(2,1:3) = cell%f(5)%v(3,1:3) - cell%f(5)%v(2,1:3)
                cell%f(5)%e(3,1:3) = cell%f(5)%v(4,1:3) - cell%f(5)%v(3,1:3)
                cell%f(5)%e(4,1:3) = cell%f(5)%v(1,1:3) - cell%f(5)%v(4,1:3)
                cell%f(6)%e(1,1:3) = cell%f(6)%v(2,1:3) - cell%f(6)%v(1,1:3)
                cell%f(6)%e(2,1:3) = cell%f(6)%v(3,1:3) - cell%f(6)%v(2,1:3)
                cell%f(6)%e(3,1:3) = cell%f(6)%v(4,1:3) - cell%f(6)%v(3,1:3)
                cell%f(6)%e(4,1:3) = cell%f(6)%v(1,1:3) - cell%f(6)%v(4,1:3)

                geometry%bvh%depth(i)%cell(j) = cell
            end do
        end do

        if(job_params%debug >= 2) write(101,*)'finished making bvh'

    end subroutine

    subroutine is_vertex_in_cell(cell,vertex,result)

        type(bvh_cell_type), intent(in) :: cell
        real(8), intent(in) :: vertex(1:3)
        logical, intent(out) :: result

        result = .false. ! assume not inside cell

        if( (vertex(1) > cell%xmin .and. vertex(1) < cell%xmax) .and. &
            (vertex(2) > cell%ymin .and. vertex(2) < cell%ymax) .and. &
            (vertex(3) > cell%zmin .and. vertex(3) < cell%zmax)) then
            result = .true.
        end if
            
    end subroutine

    subroutine export_bvh(bvh,dir)

        ! writes the bvh to a file

        character(len=*), intent(in) :: dir
        type(bvh_type), intent(in) :: bvh

        type(geometry_type) bvh_geom ! bvh geometry struct
        type(bvh_cell_type) cell ! a bvh cell
        integer(8) i, j, k
        integer(8) nv, nf ! number of vertices, faces
        integer(8) nb, nc, nc_total ! number of depths, cells, cells total

        nc_total = 0 ! init
        nb = size(bvh%depth(:),1) ! get num depths
        ! print*,'nb:',nb
        do i = 1, nb ! for each depth
            nc = size(bvh%depth(i)%cell(:),1) ! get num cells in this depth
            ! print*,'nc:',nc
            do j = 1, nc ! for each cell in this depth
                if(bvh%depth(i)%cell(j)%nv > 0) nc_total = nc_total + 1
            end do
        end do
        nv = nc_total*8 ! total vertices
        nf = nc_total*6 ! total faces
        
        ! print*,'bvh total number of cells:',nc_total
        ! print*,'bvh total number of vertices:',nv
        ! print*,'bvh total number of faces:',nf
        bvh_geom%nv = nv
        bvh_geom%nf = nf
        bvh_geom%nn = 0 ! cba to put normals in right now

        allocate(bvh_geom%v(1:nv,1:3))
        allocate(bvh_geom%f(1:nf))
        do i = 1, nf
            bvh_geom%f(i)%nv = 4
            allocate(bvh_geom%f(i)%vi(1:4))
        end do

        nv = 0 ! reset counter
        nf = 0 ! reset counter
        do i = 1, nb ! for each depth
            nc = size(bvh%depth(i)%cell(:),1) ! get num cells in this depth
            do j = 1, nc ! for each cell in this depth
                cell = bvh%depth(i)%cell(j) ! get the cell
                if(cell%nv > 0) then
                    bvh_geom%v(nv+1,:) = (/cell%xmin,cell%ymin,cell%zmin/)
                    bvh_geom%v(nv+2,:) = (/cell%xmin,cell%ymin,cell%zmax/)
                    bvh_geom%v(nv+3,:) = (/cell%xmax,cell%ymin,cell%zmax/)
                    bvh_geom%v(nv+4,:) = (/cell%xmax,cell%ymin,cell%zmin/)
                    bvh_geom%v(nv+5,:) = (/cell%xmin,cell%ymax,cell%zmin/)
                    bvh_geom%v(nv+6,:) = (/cell%xmin,cell%ymax,cell%zmax/)
                    bvh_geom%v(nv+7,:) = (/cell%xmax,cell%ymax,cell%zmax/)
                    bvh_geom%v(nv+8,:) = (/cell%xmax,cell%ymax,cell%zmin/)
                    bvh_geom%f(nf+1)%vi(:) = (/nv+1,nv+4,nv+3,nv+2/)
                    bvh_geom%f(nf+2)%vi(:) = (/nv+2,nv+3,nv+7,nv+6/)
                    bvh_geom%f(nf+3)%vi(:) = (/nv+3,nv+4,nv+8,nv+7/)
                    bvh_geom%f(nf+4)%vi(:) = (/nv+1,nv+2,nv+6,nv+5/)
                    bvh_geom%f(nf+5)%vi(:) = (/nv+1,nv+5,nv+8,nv+4/)
                    bvh_geom%f(nf+6)%vi(:) = (/nv+7,nv+8,nv+5,nv+6/)
                    nf = nf + 6
                    nv = nv + 8
                end if
            end do
        end do

        call PDAS(dir,"bvh",bvh_geom)


    end subroutine

    subroutine shrink_cell(cell,geometry)

        ! shrink a cell around the extreme coordinates of the vertices in the cell

        type(bvh_cell_type), intent(inout) :: cell
        type(geometry_type), intent(in) :: geometry

        integer(8) i, vi, j
        real(8) maximum(1:3), minimum(1:3)

        ! print*,'minimum x in:',cell%xmin
        ! print*,'maximum x in:',cell%xmax
        ! print*,'minimum y in:',cell%ymin
        ! print*,'maximum y in:',cell%ymax
        ! print*,'minimum z in:',cell%zmin
        ! print*,'maximum z in:',cell%zmax

        ! init
        vi = cell%vi(1) ! get first cell vertex id
        minimum = geometry%v(vi,:)
        maximum = geometry%v(vi,:)

        do i = 1, cell%nv ! for each vertex in cell
            vi = cell%vi(i) ! get vertex id
            do j = 1, 3 ! for each dimension
                if(geometry%v(vi,j) > maximum(j)) then ! check for new maximum value
                    maximum(j) = geometry%v(vi,j)
                else if(geometry%v(vi,j) < minimum(j)) then ! check for new minimum value
                    minimum(j) = geometry%v(vi,j)
                end if
            end do
        end do

        ! save new extreme coordinates
        cell%xmin = minimum(1)
        cell%xmax = maximum(1)
        cell%ymin = minimum(2)
        cell%ymax = maximum(2)
        cell%zmin = minimum(3)
        cell%zmax = maximum(3)

        ! print*,'minimum x in:',minimum(1)
        ! print*,'maximum x in:',maximum(1)
        ! print*,'minimum y in:',minimum(2)
        ! print*,'maximum y in:',maximum(2)
        ! print*,'minimum z in:',minimum(3)
        ! print*,'maximum z in:',maximum(3)

    end subroutine

    subroutine split_cell(geometry,cell,dim,cell_a,cell_b,success)

        ! splits cell along the dimension dim, returning 2 new cells, cell_a and cell_b

        type(geometry_type), intent(in) :: geometry
        type(bvh_cell_type), intent(in) :: cell
        type(bvh_cell_type), intent(out) :: cell_a
        type(bvh_cell_type), intent(out) :: cell_b
        integer(8), intent(in) :: dim
        logical, intent(out) :: success
        
        real(8) xmid, ymid, zmid, mid
        integer(8) nv_a, nv_b ! counts the vertices to be put into each cell
        integer(8) i, vi

        success = .true. ! init
        
        ! set the new cell extreme coordinates
        if(dim == 1) then ! if split along x
            mid = (cell%xmax + cell%xmin)/2d0
            cell_a%xmin = cell%xmin
            cell_a%xmax = mid
            cell_a%ymin = cell%ymin
            cell_a%ymax = cell%ymax
            cell_a%zmin = cell%zmin
            cell_a%zmax = cell%zmax
            cell_b%xmin = mid
            cell_b%xmax = cell%xmax
            cell_b%ymin = cell%ymin
            cell_b%ymax = cell%ymax
            cell_b%zmin = cell%zmin
            cell_b%zmax = cell%zmax
        else if(dim == 2) then
            mid = (cell%ymax + cell%ymin)/2d0
            cell_a%xmin = cell%xmin
            cell_a%xmax = cell%xmax
            cell_a%ymin = cell%ymin
            cell_a%ymax = mid
            cell_a%zmin = cell%zmin
            cell_a%zmax = cell%zmax
            cell_b%xmin = cell%xmin
            cell_b%xmax = cell%xmax
            cell_b%ymin = mid
            cell_b%ymax = cell%ymax
            cell_b%zmin = cell%zmin
            cell_b%zmax = cell%zmax
        else if(dim == 3) then
            mid = (cell%zmax + cell%zmin)/2d0
            cell_a%xmin = cell%xmin
            cell_a%xmax = cell%xmax
            cell_a%ymin = cell%ymin
            cell_a%ymax = cell%ymax
            cell_a%zmin = cell%zmin
            cell_a%zmax = mid
            cell_b%xmin = cell%xmin
            cell_b%xmax = cell%xmax
            cell_b%ymin = cell%ymin
            cell_b%ymax = cell%ymax
            cell_b%zmin = mid
            cell_b%zmax = cell%zmax
        end if

        ! determine which cell each vertex belongs to
        nv_a = 0 ! set counter
        nv_b = 0 ! set counter
        ! get number of vertices in each of the new cells
        do i = 1, cell%nv ! for each vertex
            vi = cell%vi(i) ! get the vertex id
            if(geometry%v(vi,dim) < mid) then ! if component of vertex less than the split point
                nv_a = nv_a + 1 ! update number of vertices in cell a
            else ! if component greater then or equal to the split point
                nv_b = nv_b + 1 ! update number of vertices in cell b
            end if
        end do
        if(allocated(cell_a%vi)) deallocate(cell_a%vi)
        if(allocated(cell_b%vi)) deallocate(cell_b%vi)
        allocate(cell_a%vi(1:nv_a)) ! allocate space in cell_a to hold new vertices
        allocate(cell_b%vi(1:nv_b)) ! allocate space in cell_b to hold new vertices
        cell_a%nv = nv_a ! save the number of vertices in cell a
        cell_b%nv = nv_b ! save the number of vertices in cell b
        ! assign vertices to each of the new cells
        nv_a = 0 ! reset counter
        nv_b = 0 ! reset counter
        do i = 1, cell%nv ! for each vertex
            vi = cell%vi(i) ! get the vertex id
            if(geometry%v(vi,dim) < mid) then ! if component of vertex less than the split point
                nv_a = nv_a + 1 ! update counter
                cell_a%vi(nv_a) = vi ! put it in cell a
            else ! if component greater then or equal to the split point
                nv_b = nv_b + 1 ! update counter
                cell_b%vi(nv_b) = vi ! put it in cell a
            end if
        end do

        cell_a%pid = cell%id ! save parent cell id
        cell_b%pid = cell%id ! save parent cell id
        cell_a%nch = 0 ! init
        cell_b%nch = 0 ! init
        cell_a%nf = 0 ! init
        cell_b%nf = 0 ! init

        ! print*,'number of vertices in cell a:',nv_a
        ! print*,'number of vertices in cell b:',nv_b

        if(nv_a < 3) success = .false.
        if(nv_b < 3) success = .false.

        ! print*,'vertices in cell a:'
        ! do i = 1, nv_a
        !     print*,cell_a%vi(i)
        ! end do
        ! print*,'vertices in cell b:'
        ! do i = 1, nv_b
        !     print*,cell_b%vi(i)
        ! end do

    end subroutine

    subroutine determine_split_dim_1(cell,dim)

        ! choose split dimension based off of cell largest dimension

        type(bvh_cell_type), intent(in) :: cell
        integer(8), intent(out) :: dim
        real(8) xdim, ydim, zdim

        xdim = cell%xmax - cell%xmin 
        ydim = cell%ymax - cell%ymin
        zdim = cell%zmax - cell%zmin

        if(xdim > ydim .and. xdim > zdim) then ! if xdim is largest
            dim = 1
        else if(xdim <= ydim .and. ydim > zdim) then ! if ydim is largest
            dim = 2
        else
            dim = 3
        end if
        
    end subroutine

    subroutine make_first_cell(bvh,geometry)

        ! make the first cell in the bounding volume hierarchy

        type(geometry_type), intent(in) :: geometry
        type(bvh_type), intent(inout) :: bvh
        integer(8) i
        real(8), parameter :: stretch = 1d-2

        ! initialise the outer bounding cell
        allocate(bvh%depth(1)%cell(1:1)) ! allocate just 1 cell for the outer bounding cell
        bvh%depth(1)%cell(1)%id = 1 ! first cell has id 1
        bvh%depth(1)%cell(1)%pid = 0 ! first cell has no parent
        bvh%depth(1)%cell(1)%pi = 0 ! first cell has no parent
        bvh%depth(1)%cell(1)%nch = 0 ! initialise with no children
        bvh%depth(1)%cell(1)%nv = geometry%nv ! first cell contains all vertices
        allocate(bvh%depth(1)%cell(1)%vi(1:geometry%nv)) ! allocate array to hold all vertex ids
        do i = 1, geometry%nv ! for each vertex
            bvh%depth(1)%cell(1)%vi(i) = i ! save vertex ids
        end do
        ! save extreme coordinates
        bvh%depth(1)%cell(1)%xmin = minval(geometry%v(:,1)) - stretch
        bvh%depth(1)%cell(1)%xmax = maxval(geometry%v(:,1)) + stretch
        bvh%depth(1)%cell(1)%ymin = minval(geometry%v(:,2)) - stretch
        bvh%depth(1)%cell(1)%ymax = maxval(geometry%v(:,2)) + stretch
        bvh%depth(1)%cell(1)%zmin = minval(geometry%v(:,3)) - stretch
        bvh%depth(1)%cell(1)%zmax = maxval(geometry%v(:,3)) + stretch

    end subroutine

end module bvh_mod