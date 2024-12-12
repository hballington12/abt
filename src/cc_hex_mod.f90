!create a rough surface
!adapted from Appendix A of JQSRT 64 (2000) 201-218, Muinonen & Saarinen
!written by Chris Collier (except subroutine RNDG)
module cc_hex_mod
    
    use types_mod
    
    IMPLICIT NONE
    
    CONTAINS
    
    ! ################ START MAIN #################
    
    SUBROUTINE CC_HEX_MAIN(cc_hex_params,geometry)
        REAL(8) :: L, hradius, pflength
        INTEGER :: nfhr, nfpl, pfhfer, pfpfer, nscales
        REAL(8), DIMENSION(:), ALLOCATABLE :: clen, stdev
        type(cc_hex_params_type), intent(in) :: cc_hex_params ! parameters for C. Collier Gaussian Random hexagonal columns/plates
        
        !output
        ! INTEGER :: nf
        ! INTEGER, DIMENSION(:), ALLOCATABLE :: nv
        REAL(8), DIMENSION(:,:), ALLOCATABLE :: p
        
        !other
        REAL(8) :: pi, targetdiff, ystep, devmax, as, z, zd, offset
        ! REAL(8) :: x0, y0
        INTEGER :: m, m2, j1, szfc2, szfc1, nphr, sz1t, szfc3, pfhfb, s, e, v1, v2, szp1, szp
        ! INTEGER :: j2
        INTEGER :: szpfr1, szpfr2, cmv, nf1s, nf16bf, nf12pf
        INTEGER, PARAMETER :: fncl=200
        REAL(8), DIMENSION(:,:), ALLOCATABLE :: fc1, fc2, fc3
        INTEGER, DIMENSION(:), ALLOCATABLE :: pfr1, pfr2, pfr1b, pfr2b
        CHARACTER(fncl) :: fn
        INTEGER :: O1 ! HB 23/1/23 offset test
        
        ! HB integration with abt
        integer(8), dimension(:), allocatable :: apertures ! taken as parents parent facets
        real(8), dimension(:,:), allocatable :: vertices
        integer(8), dimension(:,:), allocatable :: face_ids
        integer face_counter, vert_counter ! counter for tracking number of faces
        integer k1, i, j 
        type(geometry_type), intent(out) :: geometry
        
        !read in input values
        ! OPEN(UNIT=3, FILE='vals.in', STATUS='OLD')
        !READ(UNIT=3, FMT=*) filename
        ! READ(UNIT=3, FMT=*) L
        L = cc_hex_params%l
        ! READ(UNIT=3, FMT=*) hradius
        hradius = cc_hex_params%hr
        ! READ(UNIT=3, FMT=*) nfhr
        nfhr = cc_hex_params%nfhr
        ! READ(UNIT=3, FMT=*) pflength
        pflength = cc_hex_params%pfl
        ! READ(UNIT=3, FMT=*) nfpl
        nfpl = cc_hex_params%nfpl
        ! READ(UNIT=3, FMT=*) pfhfer
        pfhfer = cc_hex_params%pher
        ! READ(UNIT=3, FMT=*) pfpfer
        pfpfer = cc_hex_params%pper
        ! READ(UNIT=3, FMT=*) nscales
        nscales = cc_hex_params%nscales
        ALLOCATE(clen(nscales), stdev(nscales))
        DO j1=1,nscales
            ! READ(UNIT=3, FMT=*) clen(j1)
            clen(j1) = cc_hex_params%cls(j1)
            ! READ(UNIT=3, FMT=*) stdev(j1)
            stdev(j1) = cc_hex_params%sds(j1)
            
        END DO
        ! CLOSE(UNIT=3)
        
        !check nfpl has a value divisible by 4
        IF (MOD(nfpl,4) .NE. 0) THEN
            STOP 'number of subfacets along the prism facet length must be divisible by 4'
        END IF
        
        !set various things
        offset=10.*hradius*pflength      !offset to deal with roughness generation edge effects
        z=0.                                            !zero (in REAL(8) as opposed to REAL(4))
        pi=4.*ATAN(1.)                                  !pi
        szfc1=(2*nfhr+1)*nfpl/4 + ((nfhr+1)*(nfhr+2))/2 !number of rows of 1st coordinate array
        targetdiff=10.**(-6.0)       !maximum difference between correlation function and its expansion
        ystep=hradius/REAL(nfhr)      !gap between adjacent vertices
        nphr=nfhr+1          !number of vertices along hexagon radius
        sz1t=(nphr**2+nphr)/2       !number of unique coordinate positions in one basal facet "triangle"
        pfhfb=sz1t-nphr         !last vertex in in the basal facet that isn't shared with the prism facet
        nf16bf=nfhr**2         !number of subfacets in 1/6th basal facet
        nf12pf=nfhr*nfpl-nfpl/2       !number of subfacets in 1/2th prism facet
        nf1s=nf16bf+nf12pf        !number of subfacets in 1/6th basal facet and 1/2 prism facet
        szp1=3*nf1s          !number of coordinate rows in Macke-format 1/6th basal facet and 1/2th prism facet
        szp=12*szp1+6*nfpl*3       !number of coordinate rows in the whole crystal
        CALL filename(L, hradius, nfhr, pflength, nfpl, pfhfer, pfpfer, nscales, clen, stdev, fn, fncl)
        fn=TRIM(fn)//'_pf.cry'
        ! OPEN(UNIT=3, FILE=TRIM(fn), STATUS='REPLACE') !output file
        
        !coordinates for 1/12th of the crystal
        write(101,*) "Generating part of the crystal"
        ALLOCATE(fc1(szfc1,3))
        CALL coordgen(offset, nfhr, nfpl, hradius, pflength, szfc1, fc1)
        
        !calculate rough surface(s)
        write(101,*) "Generating roughness"
        DO j1=1,nscales
            CALL GRFStrip(L, clen(j1), stdev(j1), targetdiff, fc1, szfc1)
        END DO
        
        fc1(:,1)=fc1(:,1)-fc1(sz1t,1)
        DEALLOCATE(clen, stdev)
        fc1(:,2)=fc1(:,2)-MINVAL(fc1(:,2))
        
        !make most negative z the base z value
        devmax=0.0
        DO j1=1,szfc1
            IF (fc1(j1,3)<devmax) THEN
                devmax=fc1(j1,3)
            END IF
        END DO
        fc1(:,3)=fc1(:,3)-devmax
        
        !correct for where the joins will be
        !done by making the roughness on the boundary equal to the roughness adjacent to it
        !boundaries on basal facets
        m=3
        DO j1=3,nfhr
            fc1(m+1,3)=(fc1(m+2,3)+fc1(m+1,3))/2.
            fc1(m+j1,3)=(fc1(m+j1-1,3)+fc1(m+j1,3))/2.
            m=m+j1
        END DO
        !boundaries in the middle of prism facets
        IF (MOD(nfpl,2) .EQ. 0) THEN
            fc1(szfc1,3)=(fc1(szfc1-nfhr-1,3)+fc1(szfc1,3))/2.
            fc1(szfc1-nfhr,3)=fc1(szfc1-2*nfhr,3)
            DO j1=szfc1-nfhr+1,szfc1-1
                fc1(j1,3)=((fc1(j1-nfhr,3) + fc1(j1-nfhr-1,3))/2.+fc1(j1,3))/2.
            END DO
        ELSE IF (MOD(nfpl,2) .EQ. 1) THEN
            DO j1=szfc1-nfhr+1,szfc1-1
                fc1(j1,3)=((fc1(j1-nfhr,3) + fc1(j1-nfhr-1,3))/2.+fc1(j1,3))/2.
            END DO
        END IF
        
        !rotate basal facet into position and do rotations on and near the boundary between the prism facet and the basal facet
        write(101,*) "Doing basal facet-prism facet boundary rotations"
        szfc2=szfc1*2
        ALLOCATE(fc2(szfc2,3))
        fc2(1:szfc1,:)=fc1(1:szfc1,:)
        DEALLOCATE(fc1)
        CALL roty(fc2(pfhfb+1:sz1t,:), nphr, -pi/4., z) !half-rotate the boundary
        as=pi/(4.*REAL(pfhfer+1))
        m=pfhfb
        m2=sz1t
        DO j1=1,pfhfer !do rotations either side of the boundary
            !rotation on the prism facet
            IF (MOD(j1,2) .EQ. 1) THEN
                s=m2+1
                e=m2+nphr-1
                CALL roty(fc2(s:e,:), nphr-1, -as*REAL(pfhfer+1-j1), fc2(s,1))
                m2=m2+nphr-1
            ELSE IF (MOD(j1,2) .EQ. 0) THEN
                s=m2+1
                e=m2+nphr
                CALL roty(fc2(s:e,:), nphr, -as*REAL(pfhfer+1-j1), fc2(s,1))
                m2=m2+nphr
            END IF
            !rotation on the basal facet
            s=m-(nphr-j1)+1
            CALL roty(fc2(s:m,:), nphr-j1, as*REAL(pfhfer-j1+1), fc2(s,1))
            m=m-(nphr-j1)
        END DO
        CALL roty(fc2(1:pfhfb,:), pfhfb, -pi/2., z) !rotate the basal facet part into position
        
        !correct y position of end of each partially rotated vertex row
        m=sz1t
        DO j1=1,pfhfer+1
            v1=m-(nphr-j1)
            zd=fc2(v1,3)-fc2(1,3)
            fc2(v1,2)=fc2(1,2)-zd*TAN(pi/6.)
            v2=m
            zd=fc2(v2,3)-fc2(1,3)
            fc2(v2,2)=fc2(1,2)+zd/SQRT(3.)
            m=m-(nphr-j1+1)
        END DO
        
        !perform edge rotations on the prism facet (where it will border other prism facets)
        write(101,*) "Doing prism facet-prism facet boundary rotations"
        szpfr1=CEILING(REAL(nfpl/4))
        szpfr2=FLOOR(REAL(nfpl/4))
        ALLOCATE(pfr1(szpfr1), pfr2(szpfr2))
        !points with -ve y
        m=1
        as=pi/(6.*REAL(pfpfer+1))
        CALL getpfr(pfr2, szpfr2, sz1t+nphr, nphr) !get row number of coordinates on the boundary 
        CALL rotx2(fc2, szfc2, pfr2, szpfr2, pi/6., z)    !rotate the boundary
        DO j1=1,pfpfer                              !rotate other points
            IF (MOD(j1,2) .EQ. 1) THEN
                CALL getpfr(pfr1, szpfr1, sz1t+(j1+1)/2, nphr)
                CALL rotx2(fc2, szfc2, pfr1, szpfr1, as*REAL(pfpfer+1-j1), fc2(pfr1(1),2))
            ELSE IF (MOD(j1,2) .EQ. 0) THEN
                CALL getpfr(pfr2, szpfr2, sz1t+nphr+j1/2, nphr)
                CALL rotx2(fc2, szfc2, pfr2, szpfr2, as*REAL(pfpfer+1-j1), fc2(pfr2(1),2))
            END IF
        END DO
        !points with +ve y
        CALL getpfr(pfr2, szpfr2, sz1t+2*nphr-1, nphr)   !get row number of coordinates on the boundary 
        CALL rotx2(fc2, szfc2, pfr2, szpfr2, -pi/6., fc2(pfr2(1),2))  !rotate the boundary
        DO j1=1,pfpfer                                          !rotate other points
            IF (MOD(j1,2) .EQ. 1) THEN
                CALL getpfr(pfr1, szpfr1, sz1t+nphr-(j1+1)/2, nphr)
                CALL rotx2(fc2, szfc2, pfr1, szpfr1, -as*REAL(pfpfer+1-j1), fc2(pfr1(1),2))
            ELSE IF (MOD(j1,2) .EQ. 0) THEN
                CALL getpfr(pfr2, szpfr2, sz1t+2*nphr-j1/2-1, nphr)
                CALL rotx2(fc2, szfc2, pfr2, szpfr2, -as*REAL(pfpfer+1-j1), fc2(pfr2(1),2))
            END IF
        END DO
        
        !make another 1/6th basal facet and 1/2th prism facet and rotate into position
        write(101,*) "Duplicating to generate a half-crystal"
        fc2(szfc1+1:2*szfc1,:)=fc2(1:szfc1,:)
        CALL rotx(fc2(szfc1+1:2*szfc1,:), szfc1, pi, z)
        fc2(szfc1+1:2*szfc1,3)=-fc2(szfc1+1:2*szfc1,3)
        CALL rotx(fc2(szfc1+1:2*szfc1,:), szfc1, pi/3., z)
        
        !duplicate and rotate this to create 4 more half prism facets and complete the basal facet
        szfc3=6*szfc1
        ALLOCATE(fc3(szfc3,3))
        fc3(1:szfc2,:)=fc2(1:szfc2,:)
        DEALLOCATE(fc2)
        fc3(:,2)=fc3(:,2)-fc3(1,2)
        fc3(:,3)=fc3(:,3)-fc3(1,3)
        fc3(szfc2+1:2*szfc2,:)=fc3(1:szfc2,:)
        fc3(2*szfc2+1:3*szfc2,:)=fc3(1:szfc2,:)
        CALL rotx(fc3(szfc2+1:2*szfc2,:), szfc2, -4.*pi/3., z)
        CALL rotx(fc3(2*szfc2+1:3*szfc2,:), szfc2, 4.*pi/3., z)
        
        !convert into Macke format
        write(101,*) "Converting to Macke format"
        ALLOCATE(p(szp,3))
        DO j1=1,6
            cmv=MOD(j1,2) !if cmv=0, convmacke reverses surface normals
            CALL convmacke(fc3((j1-1)*szfc1+1:j1*szfc1,:), szfc1, nfhr, nfpl, p((j1-1)*szp1+1:j1*szp1,:), szp1, cmv)
        END DO
        
        !create subfacets to join each prism facet
        write(101,*) 'Filling gaps between prism facets'
        DEALLOCATE(pfr2)
        ALLOCATE(pfr2b(szpfr2+1))
        ALLOCATE(pfr1b(szpfr1))
        m=szp1*6
        DO j1=1,5
            IF (MOD(j1,2) .EQ. 1) THEN
                CALL getpfr(pfr2b, szpfr2+1, (j1-1)*szfc1+sz1t-nphr+1, nphr)
                CALL getpfr(pfr1, szpfr1, (j1-1)*szfc1+sz1t+1, nphr)
                CALL getpfr(pfr1b, szpfr1, j1*szfc1+sz1t+1, nphr)
                CALL fillboundary (nfpl, pfr2b, szpfr2+1, pfr1, pfr1b, szpfr1, fc3, szfc3, p, szp, m)
            ELSE IF (MOD(j1,2) .EQ. 0) THEN
                CALL getpfr(pfr2b, szpfr2+1, (j1-1)*szfc1+sz1t, nphr)
                CALL getpfr(pfr1, szpfr1, (j1-1)*szfc1+sz1t+nphr-1, nphr)
                CALL getpfr(pfr1b, szpfr1, j1*szfc1+sz1t+nphr-1, nphr)
                CALL fillboundary (nfpl, pfr2b, szpfr2+1, pfr1, pfr1b, szpfr1, fc3, szfc3, p, szp, m)
            END IF
        END DO
        !join the first and last prism facets
        CALL getpfr(pfr2b, szpfr2+1, 5*szfc1+sz1t, nphr)
        CALL getpfr(pfr1, szpfr1, 5*szfc1+sz1t+nphr-1, nphr)
        CALL getpfr(pfr1b, szpfr1, sz1t+nphr-1, nphr)
        CALL fillboundary (nfpl, pfr2b, szpfr2+1, pfr1, pfr1b, szpfr1, fc3, szfc3, p, szp, m)
        DEALLOCATE(pfr1, pfr2b, pfr1b)
        
        !reverse normals
        CALL revnorm(p, szp)
        
        !mirror to get 2nd basal facet and 2nd half of all prism facets
        write(101,*) "Duplicating the half-crystal to create the whole crystal"
        p(1:szp/2,1)=p(1:szp/2,1)-MAXVAL(p(1:szp/2,1))
        p(szp/2+1:szp,:)=p(1:szp/2,:)
        p(szp/2+1:szp,1)=-p(szp/2+1:szp,1)
        CALL revnorm(p(szp/2+1:szp,:), szp/2)
        
        !!save fc3 for visualisation
        !OPEN(UNIT=2, FILE='coords.dat', STATUS='REPLACE')
        !DO j1=1,szfc3
        ! WRITE(UNIT=2, FMT=*) fc3(j1,1), '', fc3(j1,2), '', fc3(j1,3)
        !END DO
        !CLOSE(UNIT=2)
        
        
        !output the crystal file
        ! write(101,*) "Not saving the crystal"
        !format is:
        !row 1: number of parent facets
        !rows 2-9: number of vertices for each parent facet
        !rows 10-45: vertices for parent facets
        !row 46: number of subfacets
        !then number of vertices for each subfacet with the parent facet number
        !finally, coordinates for each vertex
        !save the centre of each parent facet
        ! WRITE(UNIT=3, FMT=*) 8 !total number of parent facets
        ! WRITE(UNIT=3, FMT=*) 6 !vertices on 1st basal facet
        ! WRITE(UNIT=3, FMT=*) 6 !vertices on 2nd basal facet
        ! WRITE(UNIT=3, FMT=*) 4 !vertices on 1st prism facet
        ! WRITE(UNIT=3, FMT=*) 4 !vertices on 2nd prism facet
        ! WRITE(UNIT=3, FMT=*) 4 !vertices on 3rd prism facet
        ! WRITE(UNIT=3, FMT=*) 4 !vertices on 4th prism facet
        ! WRITE(UNIT=3, FMT=*) 4 !vertices on 5th prism facet
        ! WRITE(UNIT=3, FMT=*) 4 !vertices on 6th prism facet
        
        O1 = 2 ! HB offset test value (original value was +3)
        
        DO j1=1,6 !1st parent basal facet
            m=6*szp1+(j1-1)*nfpl*3/2
            ! WRITE(UNIT=3, FMT=*) p(m+O1,1), '', p(m+O1,2), '', p(m+O1,3)
        END DO
        DO j1=1,6 !2nd parent basal facet
            m=6*szp1+(6-j1)*nfpl*3/2
            ! WRITE(UNIT=3, FMT=*) -p(m+O1,1), '', p(m+O1,2), '', p(m+O1,3)
        END DO
        !1st parent prism facet
        m=6*szp1+5*nfpl*3/2
        ! WRITE(UNIT=3, FMT=*) p(m+O1,1), '', p(m+O1,2), '', p(m+O1,3)
        ! WRITE(UNIT=3, FMT=*) -p(m+O1,1), '', p(m+O1,2), '', p(m+O1,3)
        m=6*szp1
        ! WRITE(UNIT=3, FMT=*) -p(m+O1,1), '', p(m+O1,2), '', p(m+O1,3)
        ! WRITE(UNIT=3, FMT=*) p(m+O1,1), '', p(m+O1,2), '', p(m+O1,3)
        DO j1=1,5 !other parent prism facets
            m=6*szp1+(j1-1)*nfpl*3/2
            ! WRITE(UNIT=3, FMT=*) p(m+O1,1), '', p(m+O1,2), '', p(m+O1,3)
            ! WRITE(UNIT=3, FMT=*) -p(m+O1,1), '', p(m+O1,2), '', p(m+O1,3)
            ! WRITE(UNIT=3, FMT=*) -p(m+nfpl*3/2+O1,1), '', p(m+nfpl*3/2+O1,2), '', p(m+nfpl*3/2+O1,3)
            ! WRITE(UNIT=3, FMT=*) p(m+nfpl*3/2+O1,1), '', p(m+nfpl*3/2+O1,2), '', p(m+nfpl*3/2+O1,3)
        END DO
        
        allocate(apertures(1:szp/3)) ! HB allocate an array to hold the apertures (each aperture is set as the parent)
        allocate(vertices(1:szp,1:3)) ! HB allocate an array to hold the vertices (szp is total vertices)
        allocate(face_ids(1:szp/3,1:3)) ! HB allocate an array to hold the face_ids (number of vertices / 3)
        face_counter = 0 ! set counter
        vert_counter = 0 ! set counter
        
        ! WRITE(UNIT=3, FMT=*) szp/3  !total number of subfacets
        
        !write facet numbers and parent facets
        !loop through facets
        DO j1=1,nf16bf
            face_counter = face_counter + 1
            apertures(face_counter) = 1
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! write(101,*)'apertures(',face_counter,')',apertures(face_counter)
            ! write(101,*)'face_ids(',face_counter,',1:3)',face_ids(face_counter,1), face_ids(face_counter,2), face_ids(face_counter,3)
            ! WRITE(UNIT=3, FMT=*) 3, 1
        END DO
        DO j1=nf16bf+1,nf1s
            face_counter = face_counter + 1
            apertures(face_counter) = 3
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 3
        END DO
        DO j1=1,nf16bf
            face_counter = face_counter + 1
            apertures(face_counter) = 1
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 1
        END DO
        DO j1=nf16bf+1,nf1s
            face_counter = face_counter + 1
            apertures(face_counter) = 4
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 4
        END DO
        DO j1=1,nf16bf
            face_counter = face_counter + 1
            apertures(face_counter) = 1
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 1
        END DO
        DO j1=nf16bf+1,nf1s
            face_counter = face_counter + 1
            apertures(face_counter) = 5
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 5
        END DO
        DO j1=1,nf16bf
            face_counter = face_counter + 1
            apertures(face_counter) = 1
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 1
        END DO
        DO j1=nf16bf+1,nf1s
            face_counter = face_counter + 1
            apertures(face_counter) = 6
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 6
        END DO
        DO j1=1,nf16bf
            face_counter = face_counter + 1
            apertures(face_counter) = 1
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 1
        END DO
        DO j1=nf16bf+1,nf1s
            face_counter = face_counter + 1
            apertures(face_counter) = 7
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 7
        END DO
        DO j1=1,nf16bf
            face_counter = face_counter + 1
            apertures(face_counter) = 1
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 1
        END DO
        DO j1=nf16bf+1,nf1s
            face_counter = face_counter + 1
            apertures(face_counter) = 8
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 8
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 3
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 3
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 4
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 4
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 4
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 4
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 5
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 5
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 5
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 5
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 6
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 6
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 6
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 6
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 7
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 7
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 7
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 7
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 8
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 8
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 8
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 8
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 3
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 3
        END DO
        DO j1=1,nf16bf
            face_counter = face_counter + 1
            apertures(face_counter) = 2
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 2
        END DO
        DO j1=nf16bf+1,nf1s
            face_counter = face_counter + 1
            apertures(face_counter) = 3
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 3
        END DO
        DO j1=1,nf16bf
            face_counter = face_counter + 1
            apertures(face_counter) = 2
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 2
        END DO
        DO j1=nf16bf+1,nf1s
            face_counter = face_counter + 1
            apertures(face_counter) = 4
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 4
        END DO
        DO j1=1,nf16bf
            face_counter = face_counter + 1
            apertures(face_counter) = 2
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 2
        END DO
        DO j1=nf16bf+1,nf1s
            face_counter = face_counter + 1
            apertures(face_counter) = 5
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 5
        END DO
        DO j1=1,nf16bf
            face_counter = face_counter + 1
            apertures(face_counter) = 2
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 2
        END DO
        DO j1=nf16bf+1,nf1s
            face_counter = face_counter + 1
            apertures(face_counter) = 6
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 6
        END DO
        DO j1=1,nf16bf
            face_counter = face_counter + 1
            apertures(face_counter) = 2
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 2
        END DO
        DO j1=nf16bf+1,nf1s
            face_counter = face_counter + 1
            apertures(face_counter) = 7
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 7
        END DO
        DO j1=1,nf16bf
            face_counter = face_counter + 1
            apertures(face_counter) = 2
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 2
        END DO
        DO j1=nf16bf+1,nf1s
            face_counter = face_counter + 1
            apertures(face_counter) = 8
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 8
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 3
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 3
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 4
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 4
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 4
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 4
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 5
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 5
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 5
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 5
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 6
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 6
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 6
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 6
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 7
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 7
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 7
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 7
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 8
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 8
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 8
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 8
        END DO
        DO j1=1,nfpl/4
            face_counter = face_counter + 1
            apertures(face_counter) = 3
            do k1 = 1, 3
                vert_counter = vert_counter + 1
                face_ids(face_counter,3-k1+1) = vert_counter
            end do
            ! WRITE(UNIT=3, FMT=*) 3, 3
        END DO
        
        !position of each vertex
        DO j1=1,szp
            vertices(j1,1:3) = (/p(j1,1),p(j1,2),p(j1,3)/)
            ! WRITE(UNIT=3, FMT=*) p(j1,1), '', p(j1,2), '', p(j1,3)
        END DO
        
        ! CLOSE(UNIT=3) 

        geometry%nv = size(vertices,1)
        geometry%nf = size(face_ids,1)
        allocate(geometry%f(geometry%nf))
        allocate(geometry%v(1:geometry%nv,1:3))

        geometry%f(:)%nv = 3
        do i = 1, geometry%nv
            geometry%v(i,:) = vertices(i,:)
        end do

        do i = 1, geometry%nf
            allocate(geometry%f(i)%vi(3))
            do j = 1, 3
                geometry%f(i)%vi(j) = face_ids(i,j)
            end do
            geometry%f(i)%ap = apertures(i)
        end do 
        
        ! write(101,*) 'Output filename is:', TRIM(fn)
        
        DEALLOCATE(p, fc3)
        
        
    END SUBROUTINE
    
    
    
    
    
    
    
    
    
    
    !output a coordinate array organised in Macke format
    SUBROUTINE convmacke(fc, szfc, nfhr, nfpl, p, szp, cmv)
        
        
        INTEGER, INTENT(IN) :: szfc, nfhr, nfpl, szp, cmv
        REAL(8), INTENT(IN), DIMENSION(szfc,3) :: fc
        REAL(8), INTENT(OUT), DIMENSION(szp,3) :: p
        INTEGER :: m1, m2, j1, j2
        
        !create subfacets within the triangle
        m1=0 !start of the points row at the bottom of the current subfacets row
        m2=1 !position in the output coordinate array
        DO j1=1,nfhr
            !triangles pointing down
            DO j2=1,j1
                p(m2,:)=fc(m1+j2,:)
                p(m2+1,:)=fc(m1+j1+j2,:)
                p(m2+2,:)=fc(m1+j1+j2+1,:)
                m2=m2+3
            END DO
            !triangles pointing up
            DO j2=1,j1-1
                p(m2,:)=fc(m1+j2,:)
                p(m2+1,:)=fc(m1+j1+j2+1,:)
                p(m2+2,:)=fc(m1+j2+1,:)
                m2=m2+3
            END DO
            m1=m1+j1
        END DO
        !create subfacets within the half prism facet
        DO j1=1,nfpl/2
            IF (MOD(j1,2) .EQ. 0) THEN
                !triangles pointing down
                DO j2=1,nfhr
                    p(m2,:)=fc(m1+j2,:)
                    p(m2+1,:)=fc(m1+(nfhr+1)+j2-1,:)
                    p(m2+2,:)=fc(m1+(nfhr+1)+j2,:)
                    m2=m2+3
                END DO
                !triangles pointing up
                DO j2=1,nfhr-1
                    p(m2,:)=fc(m1+j2+1,:)
                    p(m2+1,:)=fc(m1+j2,:)
                    p(m2+2,:)=fc(m1+j2+(nfhr+1),:)
                    m2=m2+3
                END DO
                m1=m1+nfhr
            ELSE IF (MOD(j1,2) .EQ. 1) THEN
                !triangles pointing down
                DO j2=1,nfhr-1
                    p(m2,:)=fc(m1+j2+1,:)
                    p(m2+1,:)=fc(m1+(nfhr+1)+j2,:)
                    p(m2+2,:)=fc(m1+(nfhr+1)+j2+1,:)
                    m2=m2+3
                END DO
                !triangles pointing up
                DO j2=1,nfhr
                    p(m2,:)=fc(m1+j2+1,:)
                    p(m2+1,:)=fc(m1+j2,:)
                    p(m2+2,:)=fc(m1+(nfhr+1)+j2,:)
                    m2=m2+3
                END DO
                m1=m1+nfhr+1
            END IF
        END DO
        
        !reverse surface normals if cmv=0
        IF(cmv .EQ. 0) THEN
            CALL revnorm(p, szp)
        END IF
        
    END SUBROUTINE convmacke
    
    !reverse surface normals for Macke format triangular subfacets
    SUBROUTINE revnorm(p, szp)
        
        
        INTEGER, INTENT(IN) :: szp
        REAL(8), DIMENSION(szp,3), INTENT(INOUT) :: p
        REAL(8), DIMENSION(szp,3) :: ptemp
        INTEGER :: m, j1
        
        ptemp=p
        
        m=0
        DO j1=1,szp/3
            p(m+2,:)=ptemp(m+3,:)
            p(m+3,:)=ptemp(m+2,:)
            m=m+3
        END DO
        
    END SUBROUTINE revnorm
    
    !rotx2 - do rotations around the x-axis for non-consecutive coordinate rows
    !necessary because some compilers don't like vector references in arrays (e.g. p(v,:), where v is an array of integers)
    SUBROUTINE rotx2(p, szp, v, szv, angle, ydev)
        
        
        INTEGER, INTENT(IN) :: szp, szv
        REAL(8), INTENT(INOUT), DIMENSION(szp,3) :: p
        REAL(8), INTENT(IN) :: angle, ydev
        INTEGER, INTENT(IN), DIMENSION(szv) :: v
        REAL(8), DIMENSION(szv,3) ::ptemp1, ptemp2
        INTEGER :: j1
        
        DO j1=1,szv
            ptemp1(j1,:)=p(v(j1),:)
        END DO
        
        ptemp2=ptemp1
        ptemp2(:,2)=ptemp2(:,2)-ydev
        
        ptemp1(:,2) = ptemp2(:,2)*COS(angle) - ptemp2(:,3)*SIN(angle)+ydev
        ptemp1(:,3) = ptemp2(:,2)*SIN(angle) + ptemp2(:,3)*COS(angle)
        
        DO j1=1,szv
            p(v(j1),:)=ptemp1(j1,:)
        END DO
        
    END SUBROUTINE rotx2
    
    !rotate around the x axis
    SUBROUTINE rotx(p, szp, angle, ydev)
        
        
        INTEGER, INTENT(IN) :: szp
        REAL(8), INTENT(INOUT), DIMENSION(szp,3) :: p
        REAL(8), INTENT(IN) :: angle, ydev
        REAL(8), DIMENSION(szp,3) ::ptemp
        
        ptemp=p
        ptemp(:,2)=ptemp(:,2)-ydev
        
        p(:,2) = ptemp(:,2)*COS(angle) - ptemp(:,3)*SIN(angle)+ydev
        p(:,3) = ptemp(:,2)*SIN(angle) + ptemp(:,3)*COS(angle)
        
    END SUBROUTINE rotx
    
    !rotate around the y axis
    SUBROUTINE roty(p, szp, angle, xdev)
        
        
        INTEGER, INTENT(IN) :: szp
        REAL(8), INTENT(INOUT), DIMENSION(szp,3) :: p
        REAL(8), INTENT(IN) :: angle, xdev
        REAL(8), DIMENSION(szp,3) ::ptemp
        
        ptemp=p
        ptemp(:,1)=ptemp(:,1)-xdev
        
        p(:,1) = ptemp(:,3)*SIN(angle) + ptemp(:,1)*COS(angle)+xdev
        p(:,3) = ptemp(:,3)*COS(angle) - ptemp(:,1)*SIN(angle)
        
    END SUBROUTINE roty
    
    !Generate a Gaussian random surface
    SUBROUTINE GRFStrip(L, clen, stdev, targetdiff, facetcoords, szfc)
        
        
        INTEGER, INTENT(IN) :: szfc
        REAL(8), INTENT(IN) :: L, clen, stdev, targetdiff
        REAL(8), INTENT(INOUT), DIMENSION(szfc,3) :: facetcoords
        COMPLEX(8) :: i, zxy, ipKxqKy
        COMPLEX(8), ALLOCATABLE, DIMENSION(:,:) :: zpq
        INTEGER :: j1, itlim, dq0, p, q, pqval, n
        ! INTEGER :: dp0
        REAL(8) :: K, pi, p1, p1sq, p2, cc, zpqr, zpqi, sdzr, sdzi, Kx, Ky, rn, pKx, psq
        ! REAL(8) :: ccp1, ccp2, diff, diffold
        REAL :: rnd1, rnd2
        
        pi=4.*ATAN(1.)
        i=CMPLX(0.,1.)
        K=pi/L
        p1=SQRT(pi/2.)*clen/L   !precalculate parts of the function
        p1sq=p1**2.      ! "   " " "    "
        p2=-0.5*(pi**2.)*((clen/L)**2.) ! "   " " "    "
        
        !find iteration limit
        CALL iterationlimit(clen, L, targetdiff, itlim)
        pqval=itlim+1
        
        !calculate all z_pq
        ALLOCATE(zpq(2*itlim+1,2*itlim+1))
        !calculate z_pq for p=0, q=0
        CALL RNDG(rnd1)
        rn=rnd1
        zpq(pqval,pqval)=p1*stdev*rn
        !calculate z_pq for p=0 q=/=0
        DO q=1,itlim
            cc=p1sq*2.*EXP((REAL(q)**2.)*p2)
            sdzr=SQRT((1./4.)*cc)*stdev 
            sdzi=sdzr
            CALL RNDG(rnd1)
            rn=rnd1
            zpqr=rn*sdzr
            CALL RNDG(rnd2)
            rn=rnd2
            zpqi=rn*sdzi
            zpq(pqval,pqval+q)=zpqr+i*zpqi
            zpq(pqval,pqval-q)=zpqr-i*zpqi
        END DO
        !calculate z_pq for all other p,q
        DO p=1,itlim
            psq=REAL(p)**2.
            DO q=-itlim,itlim
                dq0=0
                IF (q==0) THEN
                    dq0=1
                END IF
                !correlation coefficients
                cc=2.*REAL(2-dq0)*p1sq*EXP(p2*(psq + REAL(q)**2.))
                !std dev of the real & imag parts of z_pq
                sdzr=SQRT((1./8.)*REAL(1+dq0)*cc)*stdev
                sdzi=sdzr
                !calculate z_pq & z_-p-q & save into array zpq
                CALL RNDG(rnd1)
                rn=rnd1
                zpqr=rn*sdzr
                CALL RNDG(rnd2)
                rn=rnd2
                zpqi=rn*sdzi
                zpq(pqval+p,pqval+q)=zpqr+i*zpqi
                zpq(pqval-p,pqval-q)=zpqr-i*zpqi
            END DO
        END DO
        !write(101,*) 'Coefficient values set'
        
        !calculate z_xy for all points
        n=0
        DO j1=1,szfc
            zxy=0.
            n=n+1
            Kx=K*facetcoords(n,1)
            Ky=K*facetcoords(n,2)
            !p=0, q=0
            zxy=zpq(pqval,pqval)
            !p=0, q=/=0
            DO q=1,itlim
                !add contributions for z_0q & z_0-q
                ipKxqKy=i*REAL(q)*Ky
                zxy=zxy+zpq(pqval,pqval+q)*EXP(ipKxqKy)+zpq(pqval,pqval-q)*EXP(-ipKxqKy)
            END DO
            !remaining p,q
            DO p=1,itlim
                pKx=REAL(p)*Kx
                DO q=-itlim,itlim
                    !add contributions for z_pq & z_-p-q
                    ipKxqKy=i*(pKx + (REAL(q)*Ky))
                    zxy=zxy+zpq(pqval+p,pqval+q)*EXP(ipKxqKy)+zpq(pqval-p,pqval-q)*EXP(-ipKxqKy)
                END DO
            END DO
            facetcoords(n,3)=facetcoords(n,3)+REAL(zxy)
        END DO
        DEALLOCATE(zpq)
        
    END SUBROUTINE GRFStrip
    
    !calculate the iteration limit by getting the correlation expansion to agree (within tolerance "targetdiff") with the correlation function
    SUBROUTINE iterationlimit(clen, L, targetdiff, itlim)
        
        
        REAL(8), INTENT(IN) :: clen, L, targetdiff
        INTEGER, INTENT(OUT) :: itlim
        REAL(8) :: pi, K, p1, p1sq, p2, cc, ccp1, ccp2, diffold, diff
        INTEGER :: j1, p, q, dp0, dq0
        
        pi=4.*ATAN(1.)
        K=pi/L
        
        !calculate the limits in p & q
        itlim=0       !iteration limit
        p1=SQRT(pi/2.)*clen/L   !precalculate parts of the function
        p1sq=p1**2.      ! "   " " "    "
        p2=-0.5*(pi**2.)*((clen/L)**2.) ! "   " " "    "
        j1=0
        diff=0.
        DO !loop through setting a new max p,q (j1) each time
            cc=0.
            DO p=0,j1
                dp0=0
                IF (p==0) THEN
                    dp0=1
                END IF
                ccp1=REAL(2-dp0)*p1sq*EXP((REAL(p)**2.)*p2)
                DO q=0,j1
                    dq0=0
                    IF (q==0) THEN
                        dq0=1
                    END IF
                    !calculate cc
                    ccp2=REAL(2-dq0)*EXP((REAL(q)**2.)*p2)
                    cc=cc+ccp1*ccp2
                END DO
            END DO
            !calculate the difference between the correlation function and the correlation expansion
            diffold=diff
            diff=ABS(1.-cc)
            IF (diff .LT. targetdiff) THEN
                itlim=j1
                EXIT
            END IF
            IF (cc > 1.02) THEN
                STOP 'Iteration limit will never be found (summation greater than 1). Exiting...'
            END IF
            IF (diff .EQ. diffold) THEN
                STOP 'Iteration limit will never be found (summation not increasing). Exiting...'
            END IF
            j1=j1+1
        END DO
        
    END SUBROUTINE iterationlimit
    
    ! Gaussian random number generation:
    ! RNDG: Gaussian distribution with zero mean and unit standard deviation
    subroutine RNDG(r1)
        
        ! Returns a normally distributed random deviate with zero mean and 
        ! unit variance. Version 2002-12-16.
        ! Copyright (C) 2002 Karri Muinonen
        
        
        integer :: flg,irnd
        ! integer :: xrandom
        real :: q1,q2,r1,r2
        ! real :: RNDU
        save flg, r2
        data flg/0/
        common irnd
        
        if (flg.eq.1) then
            r1=r2
            flg=0
            return
        endif
        
        flg=0
        q1 = 0.
        
        do while ((q1.ge.1. .or. q1.le.0.))
            CALL RANDOM_NUMBER(r1)
            CALL RANDOM_NUMBER(r2)
            r1=2.*r1-1.
            r2=2.*r2-1.
            q1=r1**2.+r2**2.
        end do
        
        q2=sqrt(-2.*log(q1)/q1)
        r1=r1*q2
        r2=r2*q2
        flg=1
        
        end
        
        !generate the initial unroughened surface
        SUBROUTINE coordgen(offset, nfhr, nfpl, hradius, pflength, szfc, fc)
            
            
            INTEGER, INTENT(IN) :: szfc, nfhr, nfpl
            REAL(8), INTENT(IN) :: hradius, pflength, offset
            REAL(8), DIMENSION(szfc,3), INTENT(INOUT) :: fc
            INTEGER :: j1, j2, n
            REAL(8) :: x, y, xstep1, xstep2, ystep, x0, y0
            
            !calculate initial variables
            ystep=hradius/REAL(nfhr)
            x0=offset
            y0=offset
            
            !generate 1/6th of a basal facet
            y=y0
            xstep1=ystep*SQRT(3.)/2.
            n=0
            DO j1=1,nfhr+1      !loop through the vertex rows
                x=x0+REAL(j1-1)*xstep1   !x position of the row
                y=y0-REAL(j1-1)*ystep/2.  !y position of the start of the row
                DO j2=1,j1      !loop through all vertices in the row
                    n=n+1      !identify the vertex
                    fc(n,1)=x     !x position of the vertex
                    fc(n,2)=y+REAL(j2-1)*ystep !y position of the vertex
                    fc(n,3)=0.     !z position
                END DO
            END DO
            
            !generate half a prism facet attached to the basal facet part
            xstep2=pflength/REAL(nfpl)
            DO j1=1,nfpl/2   !loop through rows
                x=x0+REAL(nfhr)*xstep1+REAL(j1)*xstep2
                IF (MOD(j1,2) .EQ. 1) THEN
                    y=y0-REAL(nfhr-1)*ystep/2.
                    DO j2=1,nfhr
                        n=n+1
                        fc(n,1)=x
                        fc(n,2)=y+REAL(j2-1)*ystep
                        fc(n,3)=0.
                    END DO
                ELSE
                    y=y0-REAL(nfhr)*ystep/2.
                    DO j2=1,nfhr+1
                        n=n+1
                        fc(n,1)=x
                        fc(n,2)=y+REAL(j2-1)*ystep
                        fc(n,3)=0.
                    END DO
                END IF
            END DO
            
        END SUBROUTINE coordgen
        
        !work out which points on a prism facet-prism facet boundary to rotate
        SUBROUTINE getpfr(pfr, szpfr, s, nphr)
            
            INTEGER, INTENT(IN) :: szpfr, s, nphr
            INTEGER, DIMENSION(szpfr), INTENT(INOUT) :: pfr
            INTEGER :: j1
            
            pfr(1)=s
            DO j1=1,szpfr-1
                pfr(j1+1)=s+j1*(2*nphr-1)
            END DO
            
        END SUBROUTINE getpfr
        
        !create subfacets that join together neighbouring prism facets
        SUBROUTINE fillboundary (nfpl, pfr2, szpfr2, pfr1, pfr1b, szpfr1, fc3, szfc3, p, szp, m)
            
            INTEGER, INTENT(IN) :: nfpl, szpfr2, szpfr1, szfc3, szp
            INTEGER, INTENT(INOUT) :: m
            INTEGER, INTENT(IN), DIMENSION(szpfr2) :: pfr2
            INTEGER, INTENT(IN), DIMENSION(szpfr1) :: pfr1, pfr1b
            REAL(8), INTENT(IN), DIMENSION(szfc3,3) :: fc3
            REAL(8), INTENT(INOUT), DIMENSION(szp,3) :: p
            INTEGER :: j1
            
            DO j1=1,nfpl/4
                p(m+1,:)=fc3(pfr2(j1+1),:)
                p(m+2,:)=fc3(pfr1(j1),:)
                p(m+3,:)=fc3(pfr2(j1),:)
                m=m+3
            END DO
            DO j1=1,nfpl/4
                p(m+1,:)=fc3(pfr2(j1),:)
                p(m+2,:)=fc3(pfr1b(j1),:)
                p(m+3,:)=fc3(pfr2(j1+1),:)
                m=m+3
            END DO
            
        END SUBROUTINE fillboundary
        
        !create the output filename
        SUBROUTINE filename(L, hradius, nfhr, pflength, nfpl, pfhfer, pfpfer, nscales, clen, stdev, fn, fncl)
            
            INTEGER, INTENT(IN) :: nfhr, nfpl, pfhfer, pfpfer, nscales, fncl
            REAL(8), INTENT(IN) :: hradius, pflength, L
            REAL(8), INTENT(IN), DIMENSION(nscales) :: clen, stdev
            CHARACTER(len=20) :: string, string2
            CHARACTER(len=fncl), INTENT(OUT) :: fn
            INTEGER :: j1
            
            fn='grc'
            WRITE(string, '(I0)') nscales
            fn=TRIM(fn)//TRIM(string)//'sc'
            DO j1=1,nscales
                WRITE(string2, '(I0)') j1
                WRITE(string, '(F0.2)') clen(j1)
                fn=TRIM(fn)//'_CL'//TRIM(string2)//'_'//TRIM(string)
                WRITE(string, '(F0.2)') stdev(j1)
                fn=TRIM(fn)//'_SD'//TRIM(string2)//'_'//TRIM(string)
            END DO
            WRITE(string, '(F0.1)') hradius
            fn=TRIM(fn)//'hr'//TRIM(string)
            WRITE(string, '(I0)') nfhr
            fn=TRIM(fn)//'nfhr'//TRIM(string)
            WRITE(string, '(F0.1)') pflength
            fn=TRIM(fn)//'pfl'//TRIM(string)
            WRITE(string, '(I0)') nfpl
            fn=TRIM(fn)//'nfpl'//TRIM(string)
            WRITE(string, '(I0)') pfhfer
            fn=TRIM(fn)//'pher'//TRIM(string)
            WRITE(string, '(I0)') pfpfer
            fn=TRIM(fn)//'pper'//TRIM(string)
            WRITE(string, '(F0.1)') L
            fn=TRIM(fn)//'L'//TRIM(string)
            
        END SUBROUTINE
        
    END module cc_hex_mod
    
    ! ################ END MAIN #################
    
    