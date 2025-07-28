MODULE module_gad_iea15

REAL, PARAMETER :: pi=3.1415926535897932384626433

CONTAINS

!===============================================================================
!
!  IEA 15 MW Turbine
!
!===============================================================================

!=========================================================================

FUNCTION calc_rpm_iea15( x ) result( rpm )

!_________________________________________________________________________
!
! Data from IEA 15 MW
! x =  wind speed
!_________________________________________________________________________


  REAL, INTENT( in ) :: x

  INTEGER, PARAMETER :: nws = 50

  REAL, DIMENSION( 0:nws-1 ) :: ws
  REAL, DIMENSION( 0:nws-1 ) :: rp

  REAL :: rpm
  INTEGER :: i

  rpm = 0.0

  rp(0) = 5.0
  rp(1) = 5.0
  rp(2) = 5.0
  rp(3) = 5.0
  rp(4) = 5.0
  rp(5) = 5.0
  rp(6) = 5.0
  rp(7) = 5.0
  rp(8) = 5.0
  rp(9) = 5.0
  rp(10) = 5.0
  rp(11) = 5.086081797
  rp(12) = 5.195446076
  rp(13) = 5.27648885
  rp(14) = 5.329022767
  rp(15) = 5.35292638
  rp(16) = 5.357708331
  rp(17) = 5.391164791
  rp(18) = 5.453218417
  rp(19) = 5.543725753
  rp(20) = 5.662477568
  rp(21) = 5.809199332
  rp(22) = 5.983551858
  rp(23) = 6.185132081
  rp(24) = 6.413473991
  rp(25) = 6.668049714
  rp(26) = 6.948270725
  rp(27) = 7.253489215
  rp(28) = 7.499240933
  rp(29) = 7.499240933
  rp(30) = 7.499240933
  rp(31) = 7.499240933
  rp(32) = 7.499240933
  rp(33) = 7.499240933
  rp(34) = 7.499240933
  rp(35) = 7.499240933
  rp(36) = 7.499240933
  rp(37) = 7.499240933
  rp(38) = 7.499240933
  rp(39) = 7.499240933
  rp(40) = 7.499240933
  rp(41) = 7.499240933
  rp(42) = 7.499240933
  rp(43) = 7.499240933
  rp(44) = 7.499240933
  rp(45) = 7.499240933
  rp(46) = 7.499240933
  rp(47) = 7.499240933
  rp(48) = 7.499240933
  rp(49) = 7.499240933

  ws(0) = 3.0
  ws(1) = 3.54953237
  ws(2) = 4.067900771
  ws(3) = 4.553906848
  ws(4) = 5.006427063
  ws(5) = 5.424415288
  ws(6) = 5.806905228
  ws(7) = 6.153012649
  ws(8) = 6.461937428
  ws(9) = 6.732965398
  ws(10) = 6.965470002
  ws(11) = 7.158913742
  ws(12) = 7.312849418
  ws(13) = 7.426921164
  ws(14) = 7.500865272
  ws(15) = 7.534510799
  ws(16) = 7.541241633
  ws(17) = 7.58833327
  ws(18) = 7.675676842
  ws(19) = 7.803070431
  ws(20) = 7.970219531
  ws(21) = 8.176737731
  ws(22) = 8.422147605
  ws(23) = 8.70588182
  ws(24) = 9.027284445
  ws(25) = 9.385612468
  ws(26) = 9.780037514
  ws(27) = 10.20964776
  ws(28) = 10.65843263
  ws(29) = 10.67345004
  ws(30) = 11.17037214
  ws(31) = 11.6992653
  ws(32) = 12.25890683
  ws(33) = 12.84800295
  ws(34) = 13.46519181
  ws(35) = 14.10904661
  ws(36) = 14.77807889
  ws(37) = 15.470742
  ws(38) = 16.18543466
  ws(39) = 16.92050464
  ws(40) = 17.67425264
  ws(41) = 18.44493615
  ws(42) = 19.23077353
  ws(43) = 20.02994808
  ws(44) = 20.8406123
  ws(45) = 21.66089211
  ws(46) = 22.4888912
  ws(47) = 23.32269542
  ws(48) = 24.1603772
  ws(49) = 25.0

  DO i = 1,nws-1
     
     IF ( ( x .GE. ws(i-1) ) .AND. ( x .LT. ws(i) ) ) THEN
        rpm = rp(i-1) + (x - ws(i-1))*( ( rp(i) - rp(i-1) )/( ws(i) - ws(i-1) ) )
        GOTO 1
     ENDIF

  END DO
  
  IF ( x .LT. ws(0) ) rpm = rp(0)
  IF ( x .EQ. ws(nws-1) ) rpm = rp(nws-1)

1 CONTINUE
 
!PRINT*,x,rpm
!_________________________________________________________________________

END FUNCTION calc_rpm_iea15
!_________________________________________________________________________
 
!=========================================================================

FUNCTION calc_pitch_iea15( x ) RESULT( pitch )

!_________________________________________________________________________
!
! Data from IEA 15 MW
! x = wind speeds
!_________________________________________________________________________
 
  REAL, INTENT( IN ) :: x

  INTEGER, PARAMETER :: nws = 50

  REAL, DIMENSION( 0:nws-1 ) :: ws 
  REAL, DIMENSION( 0:nws-1 ) :: p

  REAL :: pitch
  INTEGER :: i

  pitch = 0.0

  p(0) = 3.920293066
  p(1) = 3.913016973
  p(2) = 3.709535473
  p(3) = 3.347852063
  p(4) = 2.905271743
  p(5) = 2.410742282
  p(6) = 1.897891096
  p(7) = 1.38709008
  p(8) = 0.891713024
  p(9) = 0.425207394
  p(10) = 0.000469162
  p(11) = 0.0
  p(12) = 0.0
  p(13) = 0.0
  p(14) = 0.0
  p(15) = 0.0
  p(16) = 0.0
  p(17) = 0.0
  p(18) = 0.0
  p(19) = 0.0
  p(20) = 0.0
  p(21) = 0.0
  p(22) = 0.0
  p(23) = 0.0
  p(24) = 0.0
  p(25) = 0.0
  p(26) = 0.0
  p(27) = 0.0
  p(28) = 0.0
  p(29) = 0.0
  p(30) = 0.511974392
  p(31) = 3.723733263
  p(32) = 5.396486368
  p(33) = 6.766694447
  p(34) = 7.989590009
  p(35) = 9.124292675
  p(36) = 10.20005088
  p(37) = 11.23370987
  p(38) = 12.23548853
  p(39) = 13.21207233
  p(40) = 14.16785471
  p(41) = 15.10569973
  p(42) = 16.02744622
  p(43) = 16.93422766
  p(44) = 17.82668205
  p(45) = 18.70508213
  p(46) = 19.56943768
  p(47) = 20.41950905
  p(48) = 21.25504455
  p(49) = 22.07544699
  p(50) = 22.88018135

  ws(0) = 3.0
  ws(1) = 3.54953237
  ws(2) = 4.067900771
  ws(3) = 4.553906848
  ws(4) = 5.006427063
  ws(5) = 5.424415288
  ws(6) = 5.806905228
  ws(7) = 6.153012649
  ws(8) = 6.461937428
  ws(9) = 6.732965398
  ws(10) = 6.965470002
  ws(11) = 7.158913742
  ws(12) = 7.312849418
  ws(13) = 7.426921164
  ws(14) = 7.500865272
  ws(15) = 7.534510799
  ws(16) = 7.541241633
  ws(17) = 7.58833327
  ws(18) = 7.675676842
  ws(19) = 7.803070431
  ws(20) = 7.970219531
  ws(21) = 8.176737731
  ws(22) = 8.422147605
  ws(23) = 8.70588182
  ws(24) = 9.027284445
  ws(25) = 9.385612468
  ws(26) = 9.780037514
  ws(27) = 10.20964776
  ws(28) = 10.65843263
  ws(29) = 10.67345004
  ws(30) = 11.17037214
  ws(31) = 11.6992653
  ws(32) = 12.25890683
  ws(33) = 12.84800295
  ws(34) = 13.46519181
  ws(35) = 14.10904661
  ws(36) = 14.77807889
  ws(37) = 15.470742
  ws(38) = 16.18543466
  ws(39) = 16.92050464
  ws(40) = 17.67425264
  ws(41) = 18.44493615
  ws(42) = 19.23077353
  ws(43) = 20.02994808
  ws(44) = 20.8406123
  ws(45) = 21.66089211
  ws(46) = 22.4888912
  ws(47) = 23.32269542
  ws(48) = 24.1603772
  ws(49) = 25.0


  DO i = 1,nws-1

     IF ( ( x .GT. ws(i-1) ) .AND. ( x .LE. ws(i) ) ) THEN 
        pitch = p(i-1) + (x - ws(i-1))*( ( p(i) - p(i-1) )/( ws(i) - ws(i-1) ) )
        GOTO 2
     ENDIF
     
  ENDDO

  IF ( x .LE. ws(0) ) pitch = p(0)
  IF ( x .GT. ws(nws-1) ) pitch = p(nws-1)

2 CONTINUE
  
!PRINT*,x,pitch
!_________________________________________________________________________

END FUNCTION calc_pitch_iea15

!=========================================================================

FUNCTION calc_fnft_iea15( v0_upstream, v0, rho, blades, bladelen, diameter,           &
                         radius, pitchdeg, rpm, dscale, cscale,          &
          		 tip_corr_opt, axial_ind_opt, tang_ind_opt, 	 & ! PKJ/JDM
                         an_fixed, at_fixed,                             & ! PKJ/JDM
 			 g_glauert, c1_shen, c2_shen, c3_shen) 		 & ! PKJ/JDM
                         RESULT( fnft )
!_________________________________________________________________________
!
!  --v0_upstream - "upstream" time-averaged velocity normal to the turbine rotor plane, [m/s]
!  --v0       - "local" instantaneous velocity normal to the turbine rotor plane, [m/s]
!  --rho      - air density, [kg/m^3]
!  --blades   - number of turbine blades, [non-dimensional]
!  --diameter - turbine rotor diameter, [m]
!  --xcoord   - streamwise coordinate of the grid point with respect to
!               local coordinate system with the origin at the turbine hub, [m]
!  --ycoord   - cross-stream coordinate of the grid point with respect to
!               local coordinate system with the origin at the turbine hub, [m]
!  --zcoord   - vertical coordinate of the grid point with respect to
!               local coordinate system with the origin at the turbine hub, [m]
!
!  --fnft     - one-dimensional array (two members) with FUNCTION output
!               specific normal and tangential forces per unit area, [N/m^2]
!
     real, intent( in ) :: v0_upstream, v0, rho, blades, bladelen, diameter
     real, intent( in ) :: dscale, cscale 
     real, intent( in ) :: radius
     real, intent( in ) :: rpm
     real, intent( in ) :: pitchdeg
     integer, intent (in) :: tip_corr_opt, axial_ind_opt, tang_ind_opt !PKJ/JDM
     real, intent( in ) ::   an_fixed, at_fixed !PKJ/JDM
     real, intent( in ) ::   g_glauert, c1_shen, c2_shen, c3_shen !PKJ/JDM
     real, dimension( 2 )  :: fnft
     real               :: rps, omega 
     real               :: radmax, radmin
     real               :: an, at, a
     real               :: anold, atold
     real               :: cord
     real               :: twist
     real               :: xi, xideg, beta, betadeg, psi, psideg
     real               :: vn, vt
     real               :: cl, cd
     real               :: cn, ct
     real               :: solid, F, c1, c2, c3, lambda, g, Y1, Y2
     real               :: fac1, fac2, tiploss, hubloss, lossfac
     real               :: vr
     real               :: lift, drag
     real               :: fn, ft
     real               :: epsi, epscrit
     real               :: cd_nac
     integer            :: iter, itmax
!
     rps   = rpm/60.0
     omega = 2.0*pi*rps
!
     radmax = 0.5*diameter
     radmin = radmax - bladelen
!
!    print*,'radmax', radmax, 'radmin', radmin

  fnft(1) = 0.0
  fnft(2) = 0.0

  cl = 0.0
  cd = 0.0

  vn = 0.0
  vt = 0.0

  F = 0.0 ! loss_factor used in Glauert as well as Shen correction
  a = 1.0/3.0

  an = an_fixed
  at = at_fixed

  betadeg = 0.0
  psideg = 0.0
  ! constants used for shen correction
  c1 = c1_shen
  c2 = c2_shen
  c3 = c3_shen 

  ! tip speed ratio
  lambda = omega*radmax/v0_upstream
  !print*,'In calc_fnft, at function initialization..'
  !print*,'an:', an, 'at:', at, 'c1: ', c1, 'c2: ', c2, 'c3: ', c3, 'lambda: ', lambda

  IF (radius .LT. radmax) THEN !JDM
     
     IF (radius .GT. radmin) THEN !JDM ! MSG CHANGED FROM .GE. TO .GT.

        iter = 0
        epsi = 1.0        
        epscrit = 1.e-5
        itmax   = 20
!
!    --Incidence angle

        xideg = calc_twist_iea15( radius/dscale ) + pitchdeg
        xi    = xideg*pi/180.0
!
!    --Cord length 

        cord  = cscale * calc_chord_iea15( radius/dscale )
! 	print*,'Twist + Pitch: xi (deg)', xideg, 'xi (rad)', xi
!
!    --Compute induction factors an and at iteratively

        do while ( ( epsi .gt. epscrit ) .and. ( iter .lt. itmax ) ) 
!
           iter = iter + 1
           anold = an
           atold = at
!
!       --Normal and tangential velocity components

           vn = v0*( 1.0 - an )
           vt = omega*radius*( 1.0 + at ) 
 
!       --Advance angle 
  
           psi    = atan2(vn,vt)
           psideg = psi*180.0/pi

!       --Incidence angle  
 
           beta    = psi - xi
           betadeg = beta*180.0/pi

!       --Lift and drag coefficients  
     
           cl = calc_cl_iea15( radius/dscale, betadeg )
           cd = calc_cd_iea15( radius/dscale, betadeg )
  
!       --Normal and tangential force coefficients     
  
           cn = cl*cos( psi ) + cd*sin( psi )
           ct = cl*sin( psi ) - cd*cos( psi )

!       --Solidity factor

           solid = cord*blades / ( 2.0*pi*radius )

           !print*,'iter',iter
           !print*,'v0,  omega, radius',v0,omega,radius
           !print*,'psi, psideg, AoA ',psi, psideg, betadeg
           !print*,'Before tip correction: cl, cd, cn, ct, solidity',cl, cd, cn, ct, solid
           !print*,'Before tip correction: an, vn, at, vt',an,vn,at,vt

           !--Tip and hub loss

!          When psi < 0, fac1 and fac 2 can give bizarre answers, and resulting in tip and hub loss terms - NaN.
!          Some logic should be added to keep things from going crazy
           
!          RSA update: the argument of acos should have an abs. value <= 1. When psi
!          is negative, fac1 and/or fac2 are negative, causing exp(-fac) to be a
!          large positive number, causing acos to return NaN. When psi is positive, fac1 and fac2 are
!          positive, such that exp(-fac) is small and positive, which is ok.
!          Thus, we have added logic around the calls to calc_fnft above such
!          that when v0 < 0 (ie psi < 0), this function is not called and the
!          force is set to 0. The turbine should still yaw into place eventually.

           if ( abs( psi ) .gt. 0.001 ) then
             if ( psi .lt. (pi-0.01)) then

                IF ( tip_corr_opt .EQ. 1 ) THEN ! Glauert tip correction
                   g = g_glauert

                   fac1 = blades / 2.0*( radmax - radius ) / ( radius*sin( abs(psi) ) )
                   fac2 = blades / 2.0*( radius - radmin ) / ( radius*sin( abs(psi) ) )

                   tiploss = (2.0/pi)*acos( min (exp( -g*fac1 ), 1.0) )
                   hubloss = (2.0/pi)*acos( min (exp( -g*fac2 ), 1.0) )

                   F = tiploss*hubloss

		   IF ( axial_ind_opt .EQ. 1 ) THEN
                      an = ( 1.0 + 4.0*F*sin( psi )*sin( psi )/ ( solid*cn ) )**(-1.0) ! update an
		   ENDIF

		   IF ( tang_ind_opt .EQ. 1 ) THEN
                      at = (-1.0 + 4.0*F*sin( psi )*cos( psi )/ ( solid*ct ) )**(-1.0) ! update at
		   ENDIF

                   !print*,'tip_corr_opt =',tip_corr_opt,'axial_ind_opt=',axial_ind_opt,'tang_ind_opt=',tang_ind_opt,'g=',g,
                   !print*,'fac1=',fac1,'fac2=',fac2, 'tiploss=',tiploss,'hubloss=',hubloss,'lossfac=',lossfac
                   !print*,'After tip correction: an, at',an,at
                   !print '(a14,2X,4F15.12)','an, at, vn,vt',an,at,vn,vt
                   !print*,'------'

		ELSEIF (tip_corr_opt .EQ. 2 ) THEN !  Shen et al. (2005) tip correction
		   g = exp( -c1*( blades*lambda - c2 ) ) + c3

		   fac1 = blades/ 2.0 *( radmax - radius ) / ( radius*sin( abs(psi) ) )
		   F1 = (2.0/pi)*acos( min (exp( -g*fac1 ), 1.0) )
                   F  = F1

		   IF ( axial_ind_opt .EQ. 1 ) THEN
		      Y1 = 4.0*F*sin( psi )*sin( psi )/ ( solid*cn*F1 )
                      an = (2.0 + Y1 - sqrt(4.0*Y1*(1.0 - F) + Y1*Y1)) / (2.0 * (1.0 + F*Y1)) ! update an
                      IF (an .GT. 0.33) THEN
                         an = ( 1.0 + 4.0*F*sin( psi )*sin( psi )/ ( solid*cn ))**(-1.0)
                      ENDIF
		   ENDIF

		   IF ( tang_ind_opt .EQ. 1 ) THEN
		      Y2 = 4.0*F*sin( psi )*cos( psi )/ ( solid*ct*F1 )
                      at = (-1.0 + (1.0 - a*F)*Y2/(1.0 -a) )**(-1.0) ! update at
		   ENDIF

                   !print*,'tip_corr_opt =',tip_corr_opt,'axial_ind_opt=',axial_ind_opt,'tang_ind_opt=',tang_ind_opt, & 
                   !     'g=',g,'fac1=',fac1,'F=',F, 'cn=',cn,'ct=',ct
                   !print*,'After tip correction: an, at',an,at

		ENDIF ! tip_corr_opt

	        if (isnan(at) .or. isnan(an)) then
		  print*,'iter',iter
	          print*,'v0,  omega, radius',v0,omega,radius
	          print*,'psi, psideg, AoA ',psi, psideg, betadeg
	          print*,'cl, cd, cn, ct, solidity',cl, cd, cn, ct, solid
                  print*,'an, vn, at, vt',an,vn,at,vt
                  print*,'g =',g,'psi=',psi,'solid=',solid, & 
                        'F=',F,'fac1=',fac1,'radius=',radius
	          print*,'------'
	        endif ! Check for nan

            else

             an = 0.0
             at = 0.0

            endif ! if ( psi .lt. (pi-0.01))

          else

             an = 0.0
             at = 0.0

          endif ! if ( abs( psi ) .gt. 0.001 )
!       --Convergence criteria

           epsi = sqrt( ( anold - an )**2.0 + ( atold - at )**2.0 )
  
        end do ! do while ( ( epsi .gt. epscrit ) .and. ( iter .lt. itmax ) )

!    --Normal and tangential velocity components

       vn = v0*( 1.0 - an )

       vt = omega*radius*( 1.0 + at ) 

!    --Relative velocity

       vr = sqrt( vn*vn + vt*vt ) 
! 
!    --Advance angle   

       psi    = atan2(vn,vt)
       psideg = psi*180.0/pi
        if (isnan(psi)) then
                print*,'vn=',vn,'vt=',vt,'v0=',v0,'an=',an,'omega=',omega,'radius=',radius,'at=',at
        endif
!
!    --Incidence angle   

       beta    = psi - xi
       betadeg = beta*180.0/pi

! print*,'outside of iteration'
! print*,'v0, vn, at, vt',v0,vn,at,vt
! print*,'psideg, betadeg',psideg,betadeg
! print*,'------'

!    --Lift and drag coefficients     
  
       cl = calc_cl_iea15( radius/dscale, betadeg )
       cd = calc_cd_iea15( radius/dscale, betadeg )

!    --Lift and drag forces

       lift = 0.5*rho*vr*vr*cord*cl 
       drag = 0.5*rho*vr*vr*cord*cd 

!    --Specific normal and tangential forces

       fn = lift*cos( psi ) + drag*sin( psi )
       ft = lift*sin( psi ) - drag*cos( psi )

!       print*,'psi',psideg
!    --Makes the forces in Kg/(s^2 m)

       fn = blades*fn/(2.0*pi*radius)
       ft = blades*ft/(2.0*pi*radius)

       if( fn .lt. 0.0 ) fn = 0.0
       if( ft .lt. 0.0 ) ft = 0.0
        if (isnan(fn)) then
                print*,'fn is NaN, psi=',psi,'lift=',lift,'drag=',drag
        endif
        if (isnan(ft)) then
                print*,'ft is NaN, psi=',psi,'lift=',lift,'drag=',drag
        endif
       fnft(1) = fn
       fnft(2) = ft              

!print*,'radius,radmax,radmin',radius,radmax,radmin
!print*,'radius - radmin',radius - radmin
!print*,'fac1,fac2',fac1,fac2
!print*,'psi,sin(psi)',psi,sin(psi)
!print*,'tiploss,hubloss',tiploss,hubloss
!print*,'cl,cd',cl,cd
!print*,'cn,ct',cn,ct
!print*,'fn,ft',fn,ft


     ENDIF !radius .GE. radmin

  ENDIF   !radius .LT. radmax

END FUNCTION calc_fnft_iea15

!=========================================================================

FUNCTION calc_twist_iea15( x ) RESULT( twist )
!_________________________________________________________________________
!
! Computes blade twist as a function of radial distance from hub center
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: x

  INTEGER, PARAMETER :: ns = 53

  REAL, DIMENSION( 0:ns-1 ) :: r  !TL: r [0,1] means ralative distance within a blade
  REAL, DIMENSION( 0:ns-1 ) :: t

  REAL :: twist  !has unit of degree [0-180] 
  INTEGER :: i
  REAL, PARAMETER :: bladelen = 117.0

  twist = 0.0
  
  r(0) = 0.0
  r(1) = 0.020408163
  r(2) = 0.040816327
  r(3) = 0.06122449
  r(4) = 0.081632653
  r(5) = 0.102040816
  r(6) = 0.12244898
  r(7) = 0.142857143
  r(8) = 0.163265306
  r(9) = 0.183673469
  r(10) = 0.204081633
  r(11) = 0.224489796
  r(12) = 0.244897959
  r(13) = 0.265306122
  r(14) = 0.285714286
  r(15) = 0.306122449
  r(16) = 0.326530612
  r(17) = 0.346938776
  r(18) = 0.367346939
  r(19) = 0.387755102
  r(20) = 0.408163265
  r(21) = 0.428571429
  r(22) = 0.448979592
  r(23) = 0.469387755
  r(24) = 0.489795918
  r(25) = 0.510204082
  r(26) = 0.530612245
  r(27) = 0.551020408
  r(28) = 0.571428571
  r(29) = 0.591836735
  r(30) = 0.612244898
  r(31) = 0.632653061
  r(32) = 0.653061224
  r(33) = 0.673469388
  r(34) = 0.693877551
  r(35) = 0.714285714
  r(36) = 0.734693878
  r(37) = 0.755102041
  r(38) = 0.775510204
  r(39) = 0.795918367
  r(40) = 0.816326531
  r(41) = 0.836734694
  r(42) = 0.857142857
  r(43) = 0.87755102
  r(44) = 0.897959184
  r(45) = 0.918367347
  r(46) = 0.93877551
  r(47) = 0.959183673
  r(48) = 0.979591837
  r(49) = 0.985
  r(50) = 0.99
  r(51) = 0.995
  r(52) = 1.0

  r = r * bladelen + 3.97
 
  t(0) = 15.59455302
  t(1) = 15.58773861
  t(2) = 15.41083468
  t(3) = 14.94855563
  t(4) = 14.25845481
  t(5) = 13.39714164
  t(6) = 12.42201948
  t(7) = 11.39457132
  t(8) = 10.37097608
  t(9) = 9.403999875
  t(10) = 8.551522201
  t(11) = 7.83315304
  t(12) = 7.191354972
  t(13) = 6.551629061
  t(14) = 5.933995702
  t(15) = 5.346089613
  t(16) = 4.796333947
  t(17) = 4.296593563
  t(18) = 3.846955195
  t(19) = 3.445342057
  t(20) = 3.07691391
  t(21) = 2.733562457
  t(22) = 2.412234674
  t(23) = 2.111690772
  t(24) = 1.828396455
  t(25) = 1.558783421
  t(26) = 1.30243687
  t(27) = 1.064427371
  t(28) = 0.843446166
  t(29) = 0.636564187
  t(30) = 0.4370289
  t(31) = 0.239656793
  t(32) = 0.039677688
  t(33) = -0.172828124
  t(34) = -0.407083831
  t(35) = -0.680377614
  t(36) = -0.99929361
  t(37) = -1.320525256
  t(38) = -1.623298762
  t(39) = -1.884368146
  t(40) = -2.086230093
  t(41) = -2.163998202
  t(42) = -2.175841166
  t(43) = -2.155303775
  t(44) = -2.102909522
  t(45) = -2.018403421
  t(46) = -1.896743214
  t(47) = -1.724325763
  t(48) = -1.508125114
  t(49) = -1.442971489
  t(50) = -1.379466387
  t(51) = -1.312657952
  t(52) = -1.242387706

  
  DO i = 1,ns-1
 
     IF ( ( x .GT. r(i-1) ) .AND. ( x .LE. r(i) ) ) THEN
        twist = t(i-1) + (x - r(i-1))*( ( t(i) - t(i-1) )/( r(i) - r(i-1) ) )
        goto 1
     ENDIF

1 CONTINUE

     IF ( x .LT. r(0) ) THEN
        PRINT*,'—————————————'
        PRINT*,'radius < blade root in calc_twist_iea15.pro'
	PRINT*, 'radius: ', x
        PRINT*,'—————————————'
        STOP
     ENDIF
     
     IF ( x .GT. r(ns-1) ) THEN
        PRINT*,'—————————————'
        PRINT*,'radius >  blade length in calc_twist_iea15.pro'
	PRINT*,'radius: ', x
        PRINT*,'—————————————'
        STOP
     ENDIF 
     
  ENDDO
  
!PRINT*,x,twist
  
!_________________________________________________________________________
  
END FUNCTION calc_twist_iea15
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_chord_iea15( x ) RESULT( chord )
!_________________________________________________________________________
!
! Data from IEA 15 MW
! x = radial distance from hub center
!_________________________________________________________________________
 

  REAL, INTENT( IN ) :: x

  INTEGER, PARAMETER :: ns = 53

  REAL, DIMENSION( 0:ns-1 ) :: r
  REAL, DIMENSION( 0:ns-1 ) :: c

  REAL :: chord 
  INTEGER :: i
  REAL, PARAMETER :: bladelen = 117.0
  
  chord = 0.0
  
  r(0) = 0.0
  r(1) = 0.020408163
  r(2) = 0.040816327
  r(3) = 0.06122449
  r(4) = 0.081632653
  r(5) = 0.102040816
  r(6) = 0.12244898
  r(7) = 0.142857143
  r(8) = 0.163265306
  r(9) = 0.183673469
  r(10) = 0.204081633
  r(11) = 0.224489796
  r(12) = 0.244897959
  r(13) = 0.265306122
  r(14) = 0.285714286
  r(15) = 0.306122449
  r(16) = 0.326530612
  r(17) = 0.346938776
  r(18) = 0.367346939
  r(19) = 0.387755102
  r(20) = 0.408163265
  r(21) = 0.428571429
  r(22) = 0.448979592
  r(23) = 0.469387755
  r(24) = 0.489795918
  r(25) = 0.510204082
  r(26) = 0.530612245
  r(27) = 0.551020408
  r(28) = 0.571428571
  r(29) = 0.591836735
  r(30) = 0.612244898
  r(31) = 0.632653061
  r(32) = 0.653061224
  r(33) = 0.673469388
  r(34) = 0.693877551
  r(35) = 0.714285714
  r(36) = 0.734693878
  r(37) = 0.755102041
  r(38) = 0.775510204
  r(39) = 0.795918367
  r(40) = 0.816326531
  r(41) = 0.836734694
  r(42) = 0.857142857
  r(43) = 0.87755102
  r(44) = 0.897959184
  r(45) = 0.918367347
  r(46) = 0.93877551
  r(47) = 0.959183673
  r(48) = 0.979591837
  r(49) = 0.985
  r(50) = 0.99
  r(51) = 0.995
  r(52) = 1.0

  r = r * bladelen + 3.97  !radmin = 3.97 m

  c(0) = 5.2
  c(1) = 5.208839942
  c(2) = 5.237887092
  c(3) = 5.293325313
  c(4) = 5.367339855
  c(5) = 5.452092684
  c(6) = 5.540031729
  c(7) = 5.621824261
  c(8) = 5.692531175
  c(9) = 5.742610891
  c(10) = 5.764836827
  c(11) = 5.75611953
  c(12) = 5.703098513
  c(13) = 5.604676022
  c(14) = 5.471559127
  c(15) = 5.322778014
  c(16) = 5.166482288
  c(17) = 5.019421327
  c(18) = 4.885807889
  c(19) = 4.767959675
  c(20) = 4.65456608
  c(21) = 4.541031052
  c(22) = 4.428175578
  c(23) = 4.316958877
  c(24) = 4.207880736
  c(25) = 4.101646187
  c(26) = 3.998712335
  c(27) = 3.899408676
  c(28) = 3.803172544
  c(29) = 3.709389454
  c(30) = 3.617111742
  c(31) = 3.525634918
  c(32) = 3.434082671
  c(33) = 3.341933111
  c(34) = 3.248678448
  c(35) = 3.15610968
  c(36) = 3.064580005
  c(37) = 2.972992647
  c(38) = 2.880705107
  c(39) = 2.786969377
  c(40) = 2.691030939
  c(41) = 2.591965556
  c(42) = 2.489323648
  c(43) = 2.383917231
  c(44) = 2.275923816
  c(45) = 2.165466732
  c(46) = 2.052625083
  c(47) = 1.937753327
  c(48) = 1.819662967
  c(49) = 1.779921673
  c(50) = 1.707787195
  c(51) = 1.472482673
  c(52) = 0.5


  DO i = 1,ns-1
     IF ( ( x .GE. r(i-1) ) .AND. ( x .LT. r(i) ) ) THEN
        chord = c(i-1) + (x - r(i-1))*( ( c(i) - c(i-1) )/( r(i) - r(i-1) ) )
        GOTO 1
     ENDIF
  ENDDO

  IF ( x .LT. r(0) ) chord = 0.0 !No airfoil here, just a cylinder
  IF ( x .EQ. r(ns-1) ) chord = c(ns-1)
  
  IF ( x .GT. r(ns-1) ) THEN 
     PRINT*,'—————————————'
     PRINT*,'radius > blade length in calc_chord_iea15.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  
1 CONTINUE
  
!PRINT*,x,chord
   
!_________________________________________________________________________
  
END FUNCTION calc_chord_iea15
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cl_iea15( x,y ) RESULT( cl )
!_________________________________________________________________________
!
! Calls appropriate airfoil section table depending upon radial distance
! from blade root. Data from IEA 15 MW
!
! calc_cl1_iea15 uses data from circular
! calc_cl2_iea15 uses data from SNL-FFA-W3-500
! calc_cl3_iea15 uses data from FFA-W3-360
! calc_cl4_iea15 uses data from FFA-W3-330blend
! calc_cl5_iea15 uses data from FFA-W3-301
! calc_cl6_iea15 uses data from FFA-W3-270blend
! calc_cl7_iea15 uses data from FFA-W3-241
! calc_cl8_iea15 uses data from FFA-W3-211
!
! x = radial distance from hub center
! y = angle of attack
! hub diameter = 7.94 m
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: x,y

  INTEGER, PARAMETER :: ns = 10

  REAL, DIMENSION( 0:ns-1 ) :: r

  REAL :: cl

  INTEGER :: i

  cl = 0.0

  r(0) = 3.97
  r(1) = 6.31
  r(2) = 21.52
  r(3) = 32.65492706
  r(4) = 42.44474223
  r(5) = 55.35398353
  r(6) = 66.87755463
  r(7) = 78.64029586
  r(8) = 94.26403072
  r(9) = 120.97

  IF ( ( x .GT. r( 0) ) .AND. ( x .LE. r( 1) ) ) THEN
       cl = calc_cl1_iea15(y)
     ELSEIF ( ( x .GT. r( 1) ) .AND. ( x .LE. r( 2) ) ) THEN
       cl = calc_cl2_iea15(y)
     ELSEIF ( ( x .GT. r( 2) ) .AND. ( x .LE. r( 3) ) ) THEN
       cl = calc_cl3_iea15(y)
     ELSEIF ( ( x .GT. r( 3) ) .AND. ( x .LE. r( 4) ) ) THEN
       cl = calc_cl4_iea15(y)
     ELSEIF ( ( x .GT. r( 4) ) .AND. ( x .LE. r( 5) ) ) THEN
       cl = calc_cl5_iea15(y)
     ELSEIF ( ( x .GT. r( 5) ) .AND. ( x .LE. r( 6) ) ) THEN
       cl = calc_cl6_iea15(y)
     ELSEIF ( ( x .GT. r( 6) ) .AND. ( x .LE. r( 7) ) ) THEN
       cl = calc_cl7_iea15(y)
     ELSEIF ( ( x .GT. r( 7) ) .AND. ( x .LE. r( 8) ) ) THEN
       cl = calc_cl8_iea15(y)  
     ELSEIF ( ( x .GT. r( 8) ) .AND. ( x .LE. r( 9) ) ) THEN
       cl = calc_cl8_iea15(y)  
  ENDIF
  
  IF ( x .LT. r(0) ) THEN 
     PRINT*,'—————————————'
     PRINT*,'radius less than blade root in calc_cl_iea15.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  IF ( x .GT. r(ns-1) ) THEN 
     PRINT*,'—————————————'
     PRINT*,'radius greater than blade length in calc_cl_iea15.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  
!_________________________________________________________________________
  
END FUNCTION calc_cl_iea15
!_________________________________________________________________________


FUNCTION calc_cl1_iea15( y ) RESULT( cl1 )
!_________________________________________________________________________
!
! Data from IEA 15 MW: circular
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL :: cl1

  cl1 = 0.0001
!_________________________________________________________________________
  
END FUNCTION calc_cl1_iea15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl2_iea15( y ) RESULT( cl2 )
!_________________________________________________________________________
!
! Data from IEA 15 MW: SNL-FFA-W3-500
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y
  INTEGER, PARAMETER :: na = 199

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl2

  cl2 = 0.0
!                alpha    CL   
  cl(0:1,  0) = [-180.00,  0.0000]
  cl(0:1,  1) = [-170.00,  0.4419]
  cl(0:1,  2) = [-160.00,  0.8837]
  cl(0:1,  3) = [-150.00,  0.9674]
  cl(0:1,  4) = [-140.00,  0.7801]
  cl(0:1,  5) = [-130.00,  0.6293]
  cl(0:1,  6) = [-120.00,  0.4785]
  cl(0:1,  7) = [-110.00,  0.3189]
  cl(0:1,  8) = [-100.00,  0.1553]
  cl(0:1,  9) = [ -90.00,  0.0000]
  cl(0:1, 10) = [ -80.00, -0.1553]
  cl(0:1, 11) = [ -70.00, -0.3189]
  cl(0:1, 12) = [ -60.00, -0.4784]
  cl(0:1, 13) = [ -50.00, -0.6293]
  cl(0:1, 14) = [ -40.00, -0.7801]
  cl(0:1, 15) = [ -30.00, -0.9674]
  cl(0:1, 16) = [ -20.00, -1.0281]
  cl(0:1, 17) = [ -19.75, -1.0243]
  cl(0:1, 18) = [ -19.25, -1.0052]
  cl(0:1, 19) = [ -19.00, -0.9971]
  cl(0:1, 20) = [ -18.75, -1.0052]
  cl(0:1, 21) = [ -18.50, -0.9995]
  cl(0:1, 22) = [ -18.25, -0.9908]
  cl(0:1, 23) = [ -18.00, -0.9815]
  cl(0:1, 24) = [ -17.50, -0.9764]
  cl(0:1, 25) = [ -17.25, -0.9705]
  cl(0:1, 26) = [ -17.00, -0.9655]
  cl(0:1, 27) = [ -16.75, -0.9662]
  cl(0:1, 28) = [ -16.50, -0.9544]
  cl(0:1, 29) = [ -16.25, -0.9444]
  cl(0:1, 30) = [ -16.00, -0.9405]
  cl(0:1, 31) = [ -15.75, -0.9433]
  cl(0:1, 32) = [ -15.50, -0.9330]
  cl(0:1, 33) = [ -15.25, -0.9211]
  cl(0:1, 34) = [ -14.75, -0.9158]
  cl(0:1, 35) = [ -14.50, -0.9070]
  cl(0:1, 36) = [ -14.25, -0.8959]
  cl(0:1, 37) = [ -14.00, -0.8926]
  cl(0:1, 38) = [ -13.75, -0.8808]
  cl(0:1, 39) = [ -13.50, -0.8722]
  cl(0:1, 40) = [ -13.25, -0.8660]
  cl(0:1, 41) = [ -13.00, -0.8626]
  cl(0:1, 42) = [ -12.75, -0.8489]
  cl(0:1, 43) = [ -12.50, -0.8363]
  cl(0:1, 44) = [ -12.25, -0.8363]
  cl(0:1, 45) = [ -12.00, -0.8271]
  cl(0:1, 46) = [ -11.75, -0.8141]
  cl(0:1, 47) = [ -11.50, -0.8004]
  cl(0:1, 48) = [ -11.00, -0.7890]
  cl(0:1, 49) = [ -10.75, -0.7862]
  cl(0:1, 50) = [ -10.50, -0.7747]
  cl(0:1, 51) = [ -10.25, -0.7701]
  cl(0:1, 52) = [ -10.00, -0.7674]
  cl(0:1, 53) = [  -9.75, -0.7506]
  cl(0:1, 54) = [  -9.50, -0.7290]
  cl(0:1, 55) = [  -9.25, -0.7095]
  cl(0:1, 56) = [  -9.00, -0.6855]
  cl(0:1, 57) = [  -8.75, -0.6590]
  cl(0:1, 58) = [  -8.50, -0.6319]
  cl(0:1, 59) = [  -8.25, -0.6019]
  cl(0:1, 60) = [  -8.00, -0.5718]
  cl(0:1, 61) = [  -7.75, -0.5424]
  cl(0:1, 62) = [  -7.50, -0.5098]
  cl(0:1, 63) = [  -7.25, -0.4767]
  cl(0:1, 64) = [  -7.00, -0.4454]
  cl(0:1, 65) = [  -6.75, -0.4142]
  cl(0:1, 66) = [  -6.50, -0.3791]
  cl(0:1, 67) = [  -6.25, -0.3460]
  cl(0:1, 68) = [  -6.00, -0.3144]
  cl(0:1, 69) = [  -5.75, -0.2817]
  cl(0:1, 70) = [  -5.50, -0.2461]
  cl(0:1, 71) = [  -5.25, -0.2133]
  cl(0:1, 72) = [  -5.00, -0.1827]
  cl(0:1, 73) = [  -4.75, -0.1494]
  cl(0:1, 74) = [  -4.50, -0.1158]
  cl(0:1, 75) = [  -4.25, -0.0837]
  cl(0:1, 76) = [  -4.00, -0.0529]
  cl(0:1, 77) = [  -3.75, -0.0225]
  cl(0:1, 78) = [  -3.50,  0.0089]
  cl(0:1, 79) = [  -3.25,  0.0392]
  cl(0:1, 80) = [  -3.00,  0.0686]
  cl(0:1, 81) = [  -2.75,  0.0974]
  cl(0:1, 82) = [  -2.50,  0.1260]
  cl(0:1, 83) = [  -2.25,  0.1555]
  cl(0:1, 84) = [  -2.00,  0.1853]
  cl(0:1, 85) = [  -1.75,  0.2146]
  cl(0:1, 86) = [  -1.50,  0.2430]
  cl(0:1, 87) = [  -1.25,  0.2713]
  cl(0:1, 88) = [  -1.00,  0.3006]
  cl(0:1, 89) = [  -0.75,  0.3295]
  cl(0:1, 90) = [  -0.50,  0.3578]
  cl(0:1, 91) = [  -0.25,  0.3857]
  cl(0:1, 92) = [   0.00,  0.4135]
  cl(0:1, 93) = [   0.23,  0.4425]
  cl(0:1, 94) = [   0.47,  0.4715]
  cl(0:1, 95) = [   0.70,  0.5003]
  cl(0:1, 96) = [   0.94,  0.5286]
  cl(0:1, 97) = [   1.17,  0.5567]
  cl(0:1, 98) = [   1.40,  0.5850]
  cl(0:1, 99) = [   1.64,  0.6135]
  cl(0:1,100) = [   1.87,  0.6417]
  cl(0:1,101) = [   2.11,  0.6697]
  cl(0:1,102) = [   2.34,  0.6975]
  cl(0:1,103) = [   2.57,  0.7251]
  cl(0:1,104) = [   2.81,  0.7528]
  cl(0:1,105) = [   3.04,  0.7807]
  cl(0:1,106) = [   3.28,  0.8083]
  cl(0:1,107) = [   3.51,  0.8358]
  cl(0:1,108) = [   3.74,  0.8631]
  cl(0:1,109) = [   3.98,  0.8902]
  cl(0:1,110) = [   4.21,  0.9173]
  cl(0:1,111) = [   4.45,  0.9444]
  cl(0:1,112) = [   4.68,  0.9713]
  cl(0:1,113) = [   4.91,  0.9981]
  cl(0:1,114) = [   5.15,  1.0249]
  cl(0:1,115) = [   5.38,  1.0515]
  cl(0:1,116) = [   5.62,  1.0779]
  cl(0:1,117) = [   5.85,  1.1041]
  cl(0:1,118) = [   6.08,  1.1302]
  cl(0:1,119) = [   6.32,  1.1560]
  cl(0:1,120) = [   6.55,  1.1818]
  cl(0:1,121) = [   6.79,  1.2076]
  cl(0:1,122) = [   7.02,  1.2334]
  cl(0:1,123) = [   7.25,  1.2589]
  cl(0:1,124) = [   7.49,  1.2841]
  cl(0:1,125) = [   7.72,  1.3088]
  cl(0:1,126) = [   7.96,  1.3331]
  cl(0:1,127) = [   8.19,  1.3570]
  cl(0:1,128) = [   8.42,  1.3810]
  cl(0:1,129) = [   8.66,  1.4054]
  cl(0:1,130) = [   8.89,  1.4295]
  cl(0:1,131) = [   9.12,  1.4531]
  cl(0:1,132) = [   9.88,  1.5154]
  cl(0:1,133) = [  10.64,  1.5749]
  cl(0:1,134) = [  11.40,  1.6151]
  cl(0:1,135) = [  12.15,  1.6443]
  cl(0:1,136) = [  12.91,  1.6824]
  cl(0:1,137) = [  13.67,  1.7146]
  cl(0:1,138) = [  14.42,  1.7362]
  cl(0:1,139) = [  15.18,  1.7627]
  cl(0:1,140) = [  15.94,  1.7706]
  cl(0:1,141) = [  16.69,  1.7639]
  cl(0:1,142) = [  17.45,  1.7604]
  cl(0:1,143) = [  18.21,  1.7251]
  cl(0:1,144) = [  18.97,  1.7035]
  cl(0:1,145) = [  19.72,  1.6784]
  cl(0:1,146) = [  20.48,  1.6505]
  cl(0:1,147) = [  21.24,  1.6227]
  cl(0:1,148) = [  21.99,  1.6067]
  cl(0:1,149) = [  22.75,  1.5972]
  cl(0:1,150) = [  23.51,  1.5892]
  cl(0:1,151) = [  24.26,  1.5815]
  cl(0:1,152) = [  25.02,  1.5563]
  cl(0:1,153) = [  25.78,  1.5272]
  cl(0:1,154) = [  26.53,  1.4982]
  cl(0:1,155) = [  27.29,  1.4691]
  cl(0:1,156) = [  28.05,  1.4401]
  cl(0:1,157) = [  28.81,  1.4110]
  cl(0:1,158) = [  29.56,  1.3820]
  cl(0:1,159) = [  30.32,  1.3622]
  cl(0:1,160) = [  31.08,  1.3424]
  cl(0:1,161) = [  31.83,  1.3225]
  cl(0:1,162) = [  32.59,  1.3027]
  cl(0:1,163) = [  33.35,  1.2829]
  cl(0:1,164) = [  34.10,  1.2631]
  cl(0:1,165) = [  34.86,  1.2433]
  cl(0:1,166) = [  35.62,  1.2234]
  cl(0:1,167) = [  36.38,  1.2036]
  cl(0:1,168) = [  37.13,  1.1838]
  cl(0:1,169) = [  37.89,  1.1640]
  cl(0:1,170) = [  38.65,  1.1442]
  cl(0:1,171) = [  39.40,  1.1243]
  cl(0:1,172) = [  40.16,  1.1064]
  cl(0:1,173) = [  40.92,  1.0905]
  cl(0:1,174) = [  41.67,  1.0745]
  cl(0:1,175) = [  42.43,  1.0586]
  cl(0:1,176) = [  43.19,  1.0426]
  cl(0:1,177) = [  43.94,  1.0267]
  cl(0:1,178) = [  44.70,  1.0107]
  cl(0:1,179) = [  45.46,  0.9948]
  cl(0:1,180) = [  46.22,  0.9788]
  cl(0:1,181) = [  46.97,  0.9628]
  cl(0:1,182) = [  47.73,  0.9469]
  cl(0:1,183) = [  48.49,  0.9309]
  cl(0:1,184) = [  49.24,  0.9150]
  cl(0:1,185) = [  50.00,  0.8990]
  cl(0:1,186) = [  60.00,  0.6836]
  cl(0:1,187) = [  70.00,  0.4556]
  cl(0:1,188) = [  80.00,  0.2219]
  cl(0:1,189) = [  90.00,  0.0000]
  cl(0:1,190) = [ 100.00, -0.1553]
  cl(0:1,191) = [ 110.00, -0.3189]
  cl(0:1,192) = [ 120.00, -0.4784]
  cl(0:1,193) = [ 130.00, -0.6293]
  cl(0:1,194) = [ 140.00, -0.7801]
  cl(0:1,195) = [ 150.00, -0.9674]
  cl(0:1,196) = [ 160.00, -0.8837]
  cl(0:1,197) = [ 170.00, -0.4418]
  cl(0:1,198) = [ 180.00,  0.0000]  
  
  IF ( y .EQ. cl(0,0) ) cl2 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl2 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cl(0,0) ) THEN 
     cl2 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl3_iea15.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl2 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl3_iea15.pro'
  ENDIF
!_________________________________________________________________________
  
END FUNCTION calc_cl2_iea15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl3_iea15( y ) RESULT( cl3 )
!_________________________________________________________________________
!
! Data from IEA 15 MW: FFA-W3-360
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 120

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl3
  INTEGER :: i

  cl3 = 0.0

!                alpha    CL   
  cl(0:1,  0) = [-180.00,  0.0000]
  cl(0:1,  1) = [-177.71,  0.0718]
  cl(0:1,  2) = [-175.43,  0.1436]
  cl(0:1,  3) = [-173.14,  0.2153]
  cl(0:1,  4) = [-170.86,  0.2871]
  cl(0:1,  5) = [-168.57,  0.3589]
  cl(0:1,  6) = [-166.29,  0.4307]
  cl(0:1,  7) = [-164.00,  0.5025]
  cl(0:1,  8) = [-161.71,  0.5743]
  cl(0:1,  9) = [-159.43,  0.6460]
  cl(0:1, 10) = [-157.14,  0.7178]
  cl(0:1, 11) = [-154.86,  0.7896]
  cl(0:1, 12) = [-152.57,  0.8614]
  cl(0:1, 13) = [-150.29,  0.9332]
  cl(0:1, 14) = [-148.00,  1.0049]
  cl(0:1, 15) = [-143.86,  0.9190]
  cl(0:1, 16) = [-139.71,  0.8441]
  cl(0:1, 17) = [-135.57,  0.7748]
  cl(0:1, 18) = [-131.43,  0.7079]
  cl(0:1, 19) = [-127.29,  0.6412]
  cl(0:1, 20) = [-123.14,  0.5734]
  cl(0:1, 21) = [-119.00,  0.5039]
  cl(0:1, 22) = [-114.86,  0.4326]
  cl(0:1, 23) = [-110.71,  0.3598]
  cl(0:1, 24) = [-106.57,  0.2860]
  cl(0:1, 25) = [-102.43,  0.2121]
  cl(0:1, 26) = [ -98.29,  0.1390]
  cl(0:1, 27) = [ -94.14,  0.0679]
  cl(0:1, 28) = [ -90.00,  0.0000]
  cl(0:1, 29) = [ -85.86, -0.0679]
  cl(0:1, 30) = [ -81.71, -0.1390]
  cl(0:1, 31) = [ -77.57, -0.2121]
  cl(0:1, 32) = [ -73.43, -0.2860]
  cl(0:1, 33) = [ -69.29, -0.3598]
  cl(0:1, 34) = [ -65.14, -0.4326]
  cl(0:1, 35) = [ -61.00, -0.5039]
  cl(0:1, 36) = [ -56.86, -0.5734]
  cl(0:1, 37) = [ -52.71, -0.6412]
  cl(0:1, 38) = [ -48.57, -0.7079]
  cl(0:1, 39) = [ -44.43, -0.7748]
  cl(0:1, 40) = [ -40.29, -0.8441]
  cl(0:1, 41) = [ -36.14, -0.9190]
  cl(0:1, 42) = [ -32.00, -1.0049]
  cl(0:1, 43) = [ -28.00, -1.1131]
  cl(0:1, 44) = [ -24.00, -1.0542]
  cl(0:1, 45) = [ -20.00, -0.9825]
  cl(0:1, 46) = [ -18.00, -0.9417]
  cl(0:1, 47) = [ -16.00, -0.8933]
  cl(0:1, 48) = [ -14.00, -0.8547]
  cl(0:1, 49) = [ -12.00, -0.8235]
  cl(0:1, 50) = [ -10.00, -0.7954]
  cl(0:1, 51) = [  -8.00, -0.6365]
  cl(0:1, 52) = [  -6.00, -0.3909]
  cl(0:1, 53) = [  -4.00, -0.1307]
  cl(0:1, 54) = [  -2.00,  0.1617]
  cl(0:1, 55) = [  -1.00,  0.3112]
  cl(0:1, 56) = [   0.00,  0.4596]
  cl(0:1, 57) = [   1.00,  0.6057]
  cl(0:1, 58) = [   2.00,  0.7487]
  cl(0:1, 59) = [   3.00,  0.8886]
  cl(0:1, 60) = [   4.00,  1.0254]
  cl(0:1, 61) = [   5.00,  1.1588]
  cl(0:1, 62) = [   6.00,  1.2882]
  cl(0:1, 63) = [   7.00,  1.4128]
  cl(0:1, 64) = [   8.00,  1.5309]
  cl(0:1, 65) = [   9.00,  1.6406]
  cl(0:1, 66) = [  10.00,  1.7393]
  cl(0:1, 67) = [  11.00,  1.8197]
  cl(0:1, 68) = [  12.00,  1.8706]
  cl(0:1, 69) = [  13.00,  1.8922]
  cl(0:1, 70) = [  14.00,  1.8791]
  cl(0:1, 71) = [  15.00,  1.8811]
  cl(0:1, 72) = [  16.00,  1.8636]
  cl(0:1, 73) = [  18.00,  1.7332]
  cl(0:1, 74) = [  20.00,  1.5936]
  cl(0:1, 75) = [  24.00,  1.4671]
  cl(0:1, 76) = [  28.00,  1.4483]
  cl(0:1, 77) = [  32.00,  1.4356]
  cl(0:1, 78) = [  36.14,  1.3128]
  cl(0:1, 79) = [  40.29,  1.2058]
  cl(0:1, 80) = [  44.43,  1.1069]
  cl(0:1, 81) = [  48.57,  1.0113]
  cl(0:1, 82) = [  52.71,  0.9159]
  cl(0:1, 83) = [  56.86,  0.8191]
  cl(0:1, 84) = [  61.00,  0.7198]
  cl(0:1, 85) = [  65.14,  0.6180]
  cl(0:1, 86) = [  69.29,  0.5140]
  cl(0:1, 87) = [  73.43,  0.4086]
  cl(0:1, 88) = [  77.57,  0.3030]
  cl(0:1, 89) = [  81.71,  0.1986]
  cl(0:1, 90) = [  85.86,  0.0970]
  cl(0:1, 91) = [  90.00,  0.0000]
  cl(0:1, 92) = [  94.14, -0.0679]
  cl(0:1, 93) = [  98.29, -0.1390]
  cl(0:1, 94) = [ 102.43, -0.2121]
  cl(0:1, 95) = [ 106.57, -0.2860]
  cl(0:1, 96) = [ 110.71, -0.3598]
  cl(0:1, 97) = [ 114.86, -0.4326]
  cl(0:1, 98) = [ 119.00, -0.5039]
  cl(0:1, 99) = [ 123.14, -0.5734]
  cl(0:1,100) = [ 127.29, -0.6412]
  cl(0:1,101) = [ 131.43, -0.7079]
  cl(0:1,102) = [ 135.57, -0.7748]
  cl(0:1,103) = [ 139.71, -0.8441]
  cl(0:1,104) = [ 143.86, -0.9190]
  cl(0:1,105) = [ 148.00, -1.0049]
  cl(0:1,106) = [ 150.29, -0.9332]
  cl(0:1,107) = [ 152.57, -0.8614]
  cl(0:1,108) = [ 154.86, -0.7896]
  cl(0:1,109) = [ 157.14, -0.7178]
  cl(0:1,110) = [ 159.43, -0.6460]
  cl(0:1,111) = [ 161.71, -0.5743]
  cl(0:1,112) = [ 164.00, -0.5025]
  cl(0:1,113) = [ 166.29, -0.4307]
  cl(0:1,114) = [ 168.57, -0.3589]
  cl(0:1,115) = [ 170.86, -0.2871]
  cl(0:1,116) = [ 173.14, -0.2153]
  cl(0:1,117) = [ 175.43, -0.1436]
  cl(0:1,118) = [ 177.71, -0.0718]
  cl(0:1,119) = [ 180.00,  0.0000]
  
  IF ( y .EQ. cl(0,0) ) cl3 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl3 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cl(0,0) ) THEN 
     cl3 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl3_iea15.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl3 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl3_iea15.pro'
  ENDIF
  
!print,y,cl3
  
!_________________________________________________________________________
  
END FUNCTION calc_cl3_iea15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl4_iea15( y ) RESULT( cl4 )
!_________________________________________________________________________
!
! Data from IEA 15 FFA-W3-330blend
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 120

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl4
  INTEGER :: i

  cl4 = 0.0

!                alpha    CL   
  cl(0:1,  0) = [-180.00,  0.0000]
  cl(0:1,  1) = [-177.71,  0.0696]
  cl(0:1,  2) = [-175.43,  0.1392]
  cl(0:1,  3) = [-173.14,  0.2088]
  cl(0:1,  4) = [-170.86,  0.2784]
  cl(0:1,  5) = [-168.57,  0.3480]
  cl(0:1,  6) = [-166.29,  0.4176]
  cl(0:1,  7) = [-164.00,  0.4872]
  cl(0:1,  8) = [-161.71,  0.5568]
  cl(0:1,  9) = [-159.43,  0.6264]
  cl(0:1, 10) = [-157.14,  0.6960]
  cl(0:1, 11) = [-154.86,  0.7656]
  cl(0:1, 12) = [-152.57,  0.8352]
  cl(0:1, 13) = [-150.29,  0.9048]
  cl(0:1, 14) = [-148.00,  0.9744]
  cl(0:1, 15) = [-143.86,  0.8941]
  cl(0:1, 16) = [-139.71,  0.8238]
  cl(0:1, 17) = [-135.57,  0.7584]
  cl(0:1, 18) = [-131.43,  0.6948]
  cl(0:1, 19) = [-127.29,  0.6308]
  cl(0:1, 20) = [-123.14,  0.5653]
  cl(0:1, 21) = [-119.00,  0.4978]
  cl(0:1, 22) = [-114.86,  0.4282]
  cl(0:1, 23) = [-110.71,  0.3568]
  cl(0:1, 24) = [-106.57,  0.2841]
  cl(0:1, 25) = [-102.43,  0.2110]
  cl(0:1, 26) = [ -98.29,  0.1385]
  cl(0:1, 27) = [ -94.14,  0.0678]
  cl(0:1, 28) = [ -90.00,  0.0000]
  cl(0:1, 29) = [ -85.86, -0.0678]
  cl(0:1, 30) = [ -81.71, -0.1385]
  cl(0:1, 31) = [ -77.57, -0.2110]
  cl(0:1, 32) = [ -73.43, -0.2841]
  cl(0:1, 33) = [ -69.29, -0.3568]
  cl(0:1, 34) = [ -65.14, -0.4282]
  cl(0:1, 35) = [ -61.00, -0.4978]
  cl(0:1, 36) = [ -56.86, -0.5653]
  cl(0:1, 37) = [ -52.71, -0.6308]
  cl(0:1, 38) = [ -48.57, -0.6948]
  cl(0:1, 39) = [ -44.43, -0.7584]
  cl(0:1, 40) = [ -40.29, -0.8238]
  cl(0:1, 41) = [ -36.14, -0.8941]
  cl(0:1, 42) = [ -32.00, -0.9744]
  cl(0:1, 43) = [ -28.00, -1.1631]
  cl(0:1, 44) = [ -24.00, -1.1489]
  cl(0:1, 45) = [ -20.00, -1.0945]
  cl(0:1, 46) = [ -18.00, -1.0580]
  cl(0:1, 47) = [ -16.00, -1.0228]
  cl(0:1, 48) = [ -14.00, -0.9981]
  cl(0:1, 49) = [ -12.00, -0.9852]
  cl(0:1, 50) = [ -10.00, -0.8958]
  cl(0:1, 51) = [  -8.00, -0.6754]
  cl(0:1, 52) = [  -6.00, -0.4325]
  cl(0:1, 53) = [  -4.00, -0.1588]
  cl(0:1, 54) = [  -2.00,  0.1346]
  cl(0:1, 55) = [  -1.00,  0.2801]
  cl(0:1, 56) = [   0.00,  0.4239]
  cl(0:1, 57) = [   1.00,  0.5652]
  cl(0:1, 58) = [   2.00,  0.7041]
  cl(0:1, 59) = [   3.00,  0.8407]
  cl(0:1, 60) = [   4.00,  0.9750]
  cl(0:1, 61) = [   5.00,  1.1068]
  cl(0:1, 62) = [   6.00,  1.2360]
  cl(0:1, 63) = [   7.00,  1.3622]
  cl(0:1, 64) = [   8.00,  1.4842]
  cl(0:1, 65) = [   9.00,  1.6010]
  cl(0:1, 66) = [  10.00,  1.7101]
  cl(0:1, 67) = [  11.00,  1.8096]
  cl(0:1, 68) = [  12.00,  1.8947]
  cl(0:1, 69) = [  13.00,  1.9570]
  cl(0:1, 70) = [  14.00,  1.9858]
  cl(0:1, 71) = [  15.00,  1.9926]
  cl(0:1, 72) = [  16.00,  1.9962]
  cl(0:1, 73) = [  18.00,  1.9640]
  cl(0:1, 74) = [  20.00,  1.8118]
  cl(0:1, 75) = [  24.00,  1.5607]
  cl(0:1, 76) = [  28.00,  1.4680]
  cl(0:1, 77) = [  32.00,  1.3920]
  cl(0:1, 78) = [  36.14,  1.2773]
  cl(0:1, 79) = [  40.29,  1.1769]
  cl(0:1, 80) = [  44.43,  1.0835]
  cl(0:1, 81) = [  48.57,  0.9925]
  cl(0:1, 82) = [  52.71,  0.9011]
  cl(0:1, 83) = [  56.86,  0.8076]
  cl(0:1, 84) = [  61.00,  0.7112]
  cl(0:1, 85) = [  65.14,  0.6118]
  cl(0:1, 86) = [  69.29,  0.5097]
  cl(0:1, 87) = [  73.43,  0.4059]
  cl(0:1, 88) = [  77.57,  0.3015]
  cl(0:1, 89) = [  81.71,  0.1979]
  cl(0:1, 90) = [  85.86,  0.0968]
  cl(0:1, 91) = [  90.00,  0.0000]
  cl(0:1, 92) = [  94.14, -0.0678]
  cl(0:1, 93) = [  98.29, -0.1385]
  cl(0:1, 94) = [ 102.43, -0.2110]
  cl(0:1, 95) = [ 106.57, -0.2841]
  cl(0:1, 96) = [ 110.71, -0.3568]
  cl(0:1, 97) = [ 114.86, -0.4282]
  cl(0:1, 98) = [ 119.00, -0.4978]
  cl(0:1, 99) = [ 123.14, -0.5653]
  cl(0:1,100) = [ 127.29, -0.6308]
  cl(0:1,101) = [ 131.43, -0.6948]
  cl(0:1,102) = [ 135.57, -0.7584]
  cl(0:1,103) = [ 139.71, -0.8238]
  cl(0:1,104) = [ 143.86, -0.8941]
  cl(0:1,105) = [ 148.00, -0.9744]
  cl(0:1,106) = [ 150.29, -0.9048]
  cl(0:1,107) = [ 152.57, -0.8352]
  cl(0:1,108) = [ 154.86, -0.7656]
  cl(0:1,109) = [ 157.14, -0.6960]
  cl(0:1,110) = [ 159.43, -0.6264]
  cl(0:1,111) = [ 161.71, -0.5568]
  cl(0:1,112) = [ 164.00, -0.4872]
  cl(0:1,113) = [ 166.29, -0.4176]
  cl(0:1,114) = [ 168.57, -0.3480]
  cl(0:1,115) = [ 170.86, -0.2784]
  cl(0:1,116) = [ 173.14, -0.2088]
  cl(0:1,117) = [ 175.43, -0.1392]
  cl(0:1,118) = [ 177.71, -0.0696]
  cl(0:1,119) = [ 180.00,  0.0000] 
  IF ( y .EQ. cl(0,0) ) cl4 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl4 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cl(0,0) ) THEN 
     cl4 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl4_iea15.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl4 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl4_iea15.pro'
  ENDIF
  
!print,y,cl4
  
!_________________________________________________________________________
  
END FUNCTION calc_cl4_iea15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl5_iea15( y ) RESULT( cl5 )
!_________________________________________________________________________
!
! Data from IEA 15 MW: FFA-W3-301
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 120

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl5
  INTEGER :: i

  cl5 = 0.0

!                alpha    CL  
  cl(0:1,  0) = [-180.00,  0.0000]
  cl(0:1,  1) = [-177.73,  0.0651]
  cl(0:1,  2) = [-175.44,  0.1302]
  cl(0:1,  3) = [-173.15,  0.1952]
  cl(0:1,  4) = [-170.86,  0.2603]
  cl(0:1,  5) = [-168.56,  0.3254]
  cl(0:1,  6) = [-166.27,  0.3905]
  cl(0:1,  7) = [-163.98,  0.4556]
  cl(0:1,  8) = [-161.69,  0.5207]
  cl(0:1,  9) = [-159.45,  0.5857]
  cl(0:1, 10) = [-157.16,  0.6508]
  cl(0:1, 11) = [-154.87,  0.7159]
  cl(0:1, 12) = [-152.58,  0.7810]
  cl(0:1, 13) = [-150.29,  0.8461]
  cl(0:1, 14) = [-147.99,  0.9111]
  cl(0:1, 15) = [-143.87,  0.8426]
  cl(0:1, 16) = [-139.69,  0.7819]
  cl(0:1, 17) = [-135.56,  0.7245]
  cl(0:1, 18) = [-131.44,  0.6675]
  cl(0:1, 19) = [-127.31,  0.6093]
  cl(0:1, 20) = [-123.13,  0.5487]
  cl(0:1, 21) = [-119.00,  0.4853]
  cl(0:1, 22) = [-114.88,  0.4191]
  cl(0:1, 23) = [-110.70,  0.3506]
  cl(0:1, 24) = [-106.57,  0.2802]
  cl(0:1, 25) = [-102.44,  0.2088]
  cl(0:1, 26) = [ -98.26,  0.1375]
  cl(0:1, 27) = [ -94.14,  0.0675]
  cl(0:1, 28) = [ -90.01,  0.0000]
  cl(0:1, 29) = [ -85.83, -0.0675]
  cl(0:1, 30) = [ -81.70, -0.1375]
  cl(0:1, 31) = [ -77.58, -0.2088]
  cl(0:1, 32) = [ -73.45, -0.2802]
  cl(0:1, 33) = [ -69.27, -0.3506]
  cl(0:1, 34) = [ -65.15, -0.4191]
  cl(0:1, 35) = [ -61.02, -0.4853]
  cl(0:1, 36) = [ -56.84, -0.5487]
  cl(0:1, 37) = [ -52.71, -0.6093]
  cl(0:1, 38) = [ -48.59, -0.6675]
  cl(0:1, 39) = [ -44.40, -0.7245]
  cl(0:1, 40) = [ -40.28, -0.7819]
  cl(0:1, 41) = [ -36.15, -0.8426]
  cl(0:1, 42) = [ -32.03, -0.9111]
  cl(0:1, 43) = [ -28.02, -1.1035]
  cl(0:1, 44) = [ -24.01, -1.1074]
  cl(0:1, 45) = [ -20.00, -1.1182]
  cl(0:1, 46) = [ -17.99, -1.1233]
  cl(0:1, 47) = [ -15.99, -1.1187]
  cl(0:1, 48) = [ -13.98, -1.1162]
  cl(0:1, 49) = [ -11.97, -1.0959]
  cl(0:1, 50) = [ -10.03, -0.9177]
  cl(0:1, 51) = [  -8.02, -0.6931]
  cl(0:1, 52) = [  -6.02, -0.4540]
  cl(0:1, 53) = [  -4.01, -0.1778]
  cl(0:1, 54) = [  -2.01,  0.1048]
  cl(0:1, 55) = [  -0.97,  0.2438]
  cl(0:1, 56) = [   0.00,  0.3811]
  cl(0:1, 57) = [   0.97,  0.5166]
  cl(0:1, 58) = [   2.01,  0.6504]
  cl(0:1, 59) = [   2.98,  0.7827]
  cl(0:1, 60) = [   4.01,  0.9133]
  cl(0:1, 61) = [   4.98,  1.0421]
  cl(0:1, 62) = [   6.02,  1.1687]
  cl(0:1, 63) = [   6.99,  1.2930]
  cl(0:1, 64) = [   8.02,  1.4139]
  cl(0:1, 65) = [   9.00,  1.5309]
  cl(0:1, 66) = [  10.03,  1.6421]
  cl(0:1, 67) = [  11.00,  1.7457]
  cl(0:1, 68) = [  11.97,  1.8389]
  cl(0:1, 69) = [  13.01,  1.9176]
  cl(0:1, 70) = [  13.98,  1.9741]
  cl(0:1, 71) = [  15.01,  1.9992]
  cl(0:1, 72) = [  15.99,  1.9938]
  cl(0:1, 73) = [  17.99,  1.9172]
  cl(0:1, 74) = [  20.00,  1.7368]
  cl(0:1, 75) = [  24.01,  1.4732]
  cl(0:1, 76) = [  28.02,  1.3602]
  cl(0:1, 77) = [  32.03,  1.3016]
  cl(0:1, 78) = [  36.15,  1.2037]
  cl(0:1, 79) = [  40.28,  1.1170]
  cl(0:1, 80) = [  44.40,  1.0350]
  cl(0:1, 81) = [  48.59,  0.9536]
  cl(0:1, 82) = [  52.71,  0.8704]
  cl(0:1, 83) = [  56.84,  0.7838]
  cl(0:1, 84) = [  61.02,  0.6933]
  cl(0:1, 85) = [  65.15,  0.5988]
  cl(0:1, 86) = [  69.27,  0.5008]
  cl(0:1, 87) = [  73.45,  0.4002]
  cl(0:1, 88) = [  77.58,  0.2983]
  cl(0:1, 89) = [  81.70,  0.1965]
  cl(0:1, 90) = [  85.83,  0.0964]
  cl(0:1, 91) = [  90.01,  0.0000]
  cl(0:1, 92) = [  94.14, -0.0675]
  cl(0:1, 93) = [  98.26, -0.1375]
  cl(0:1, 94) = [ 102.44, -0.2088]
  cl(0:1, 95) = [ 106.57, -0.2802]
  cl(0:1, 96) = [ 110.70, -0.3506]
  cl(0:1, 97) = [ 114.88, -0.4191]
  cl(0:1, 98) = [ 119.00, -0.4853]
  cl(0:1, 99) = [ 123.13, -0.5487]
  cl(0:1,100) = [ 127.31, -0.6093]
  cl(0:1,101) = [ 131.44, -0.6675]
  cl(0:1,102) = [ 135.56, -0.7245]
  cl(0:1,103) = [ 139.69, -0.7819]
  cl(0:1,104) = [ 143.87, -0.8426]
  cl(0:1,105) = [ 147.99, -0.9111]
  cl(0:1,106) = [ 150.29, -0.8461]
  cl(0:1,107) = [ 152.58, -0.7810]
  cl(0:1,108) = [ 154.87, -0.7159]
  cl(0:1,109) = [ 157.16, -0.6508]
  cl(0:1,110) = [ 159.45, -0.5857]
  cl(0:1,111) = [ 161.69, -0.5207]
  cl(0:1,112) = [ 163.98, -0.4556]
  cl(0:1,113) = [ 166.27, -0.3905]
  cl(0:1,114) = [ 168.56, -0.3254]
  cl(0:1,115) = [ 170.86, -0.2603]
  cl(0:1,116) = [ 173.15, -0.1952]
  cl(0:1,117) = [ 175.44, -0.1302]
  cl(0:1,118) = [ 177.73, -0.0651]
  cl(0:1,119) = [ 180.00,  0.0000]

  IF ( y .EQ. cl(0,0) ) cl5 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl5 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cl(0,0) ) THEN 
     cl5 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl5_iea15.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl5 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl5_iea15.pro'
  ENDIF
  
!print,y,cl5
  
!_________________________________________________________________________
  
END FUNCTION calc_cl5_iea15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl6_iea15( y ) RESULT( cl6 )
!_________________________________________________________________________
!
! Data from IEA 15 MW: FFA-W3-270blend
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 120

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl6
  INTEGER :: i

  cl6 = 0.0

!                alpha    CL  
  cl(0:1,  0) = [-180.00,  0.0000]
  cl(0:1,  1) = [-177.73,  0.0621]
  cl(0:1,  2) = [-175.44,  0.1243]
  cl(0:1,  3) = [-173.15,  0.1864]
  cl(0:1,  4) = [-170.86,  0.2485]
  cl(0:1,  5) = [-168.56,  0.3106]
  cl(0:1,  6) = [-166.27,  0.3728]
  cl(0:1,  7) = [-163.98,  0.4349]
  cl(0:1,  8) = [-161.69,  0.4970]
  cl(0:1,  9) = [-159.45,  0.5592]
  cl(0:1, 10) = [-157.16,  0.6213]
  cl(0:1, 11) = [-154.87,  0.6834]
  cl(0:1, 12) = [-152.58,  0.7455]
  cl(0:1, 13) = [-150.29,  0.8077]
  cl(0:1, 14) = [-147.99,  0.8698]
  cl(0:1, 15) = [-143.87,  0.8166]
  cl(0:1, 16) = [-139.69,  0.7681]
  cl(0:1, 17) = [-135.56,  0.7204]
  cl(0:1, 18) = [-131.44,  0.6710]
  cl(0:1, 19) = [-127.31,  0.6183]
  cl(0:1, 20) = [-123.13,  0.5616]
  cl(0:1, 21) = [-119.00,  0.5006]
  cl(0:1, 22) = [-114.88,  0.4354]
  cl(0:1, 23) = [-110.70,  0.3665]
  cl(0:1, 24) = [-106.57,  0.2947]
  cl(0:1, 25) = [-102.44,  0.2210]
  cl(0:1, 26) = [ -98.26,  0.1464]
  cl(0:1, 27) = [ -94.14,  0.0723]
  cl(0:1, 28) = [ -90.01,  0.0000]
  cl(0:1, 29) = [ -85.83, -0.0723]
  cl(0:1, 30) = [ -81.70, -0.1464]
  cl(0:1, 31) = [ -77.58, -0.2210]
  cl(0:1, 32) = [ -73.45, -0.2947]
  cl(0:1, 33) = [ -69.27, -0.3665]
  cl(0:1, 34) = [ -65.15, -0.4354]
  cl(0:1, 35) = [ -61.02, -0.5006]
  cl(0:1, 36) = [ -56.84, -0.5616]
  cl(0:1, 37) = [ -52.71, -0.6183]
  cl(0:1, 38) = [ -48.59, -0.6710]
  cl(0:1, 39) = [ -44.40, -0.7204]
  cl(0:1, 40) = [ -40.28, -0.7681]
  cl(0:1, 41) = [ -36.15, -0.8166]
  cl(0:1, 42) = [ -32.03, -0.8698]
  cl(0:1, 43) = [ -28.02, -1.0984]
  cl(0:1, 44) = [ -24.01, -1.0834]
  cl(0:1, 45) = [ -20.00, -1.0699]
  cl(0:1, 46) = [ -17.99, -1.0545]
  cl(0:1, 47) = [ -15.99, -1.0343]
  cl(0:1, 48) = [ -13.98, -1.0836]
  cl(0:1, 49) = [ -11.97, -1.0949]
  cl(0:1, 50) = [ -10.03, -0.9266]
  cl(0:1, 51) = [  -8.02, -0.6968]
  cl(0:1, 52) = [  -6.02, -0.4363]
  cl(0:1, 53) = [  -4.01, -0.1625]
  cl(0:1, 54) = [  -2.01,  0.1071]
  cl(0:1, 55) = [  -0.97,  0.2399]
  cl(0:1, 56) = [   0.00,  0.3716]
  cl(0:1, 57) = [   0.97,  0.5021]
  cl(0:1, 58) = [   2.01,  0.6314]
  cl(0:1, 59) = [   2.98,  0.7595]
  cl(0:1, 60) = [   4.01,  0.8864]
  cl(0:1, 61) = [   4.98,  1.0117]
  cl(0:1, 62) = [   6.02,  1.1343]
  cl(0:1, 63) = [   6.99,  1.2554]
  cl(0:1, 64) = [   8.02,  1.3738]
  cl(0:1, 65) = [   9.00,  1.4884]
  cl(0:1, 66) = [  10.03,  1.5978]
  cl(0:1, 67) = [  11.00,  1.7001]
  cl(0:1, 68) = [  11.97,  1.7919]
  cl(0:1, 69) = [  13.01,  1.8678]
  cl(0:1, 70) = [  13.98,  1.9269]
  cl(0:1, 71) = [  15.01,  1.9090]
  cl(0:1, 72) = [  15.99,  1.8855]
  cl(0:1, 73) = [  17.99,  1.7211]
  cl(0:1, 74) = [  20.00,  1.5474]
  cl(0:1, 75) = [  24.01,  1.3718]
  cl(0:1, 76) = [  28.02,  1.3361]
  cl(0:1, 77) = [  32.03,  1.2426]
  cl(0:1, 78) = [  36.15,  1.1666]
  cl(0:1, 79) = [  40.28,  1.0973]
  cl(0:1, 80) = [  44.40,  1.0291]
  cl(0:1, 81) = [  48.59,  0.9585]
  cl(0:1, 82) = [  52.71,  0.8833]
  cl(0:1, 83) = [  56.84,  0.8023]
  cl(0:1, 84) = [  61.02,  0.7151]
  cl(0:1, 85) = [  65.15,  0.6220]
  cl(0:1, 86) = [  69.27,  0.5236]
  cl(0:1, 87) = [  73.45,  0.4211]
  cl(0:1, 88) = [  77.58,  0.3157]
  cl(0:1, 89) = [  81.70,  0.2091]
  cl(0:1, 90) = [  85.83,  0.1032]
  cl(0:1, 91) = [  90.01,  0.0000]
  cl(0:1, 92) = [  94.14, -0.0723]
  cl(0:1, 93) = [  98.26, -0.1464]
  cl(0:1, 94) = [ 102.44, -0.2210]
  cl(0:1, 95) = [ 106.57, -0.2947]
  cl(0:1, 96) = [ 110.70, -0.3665]
  cl(0:1, 97) = [ 114.88, -0.4354]
  cl(0:1, 98) = [ 119.00, -0.5006]
  cl(0:1, 99) = [ 123.13, -0.5616]
  cl(0:1,100) = [ 127.31, -0.6183]
  cl(0:1,101) = [ 131.44, -0.6710]
  cl(0:1,102) = [ 135.56, -0.7204]
  cl(0:1,103) = [ 139.69, -0.7681]
  cl(0:1,104) = [ 143.87, -0.8166]
  cl(0:1,105) = [ 147.99, -0.8698]
  cl(0:1,106) = [ 150.29, -0.8077]
  cl(0:1,107) = [ 152.58, -0.7455]
  cl(0:1,108) = [ 154.87, -0.6834]
  cl(0:1,109) = [ 157.16, -0.6213]
  cl(0:1,110) = [ 159.45, -0.5592]
  cl(0:1,111) = [ 161.69, -0.4970]
  cl(0:1,112) = [ 163.98, -0.4349]
  cl(0:1,113) = [ 166.27, -0.3728]
  cl(0:1,114) = [ 168.56, -0.3106]
  cl(0:1,115) = [ 170.86, -0.2485]
  cl(0:1,116) = [ 173.15, -0.1864]
  cl(0:1,117) = [ 175.44, -0.1243]
  cl(0:1,118) = [ 177.73, -0.0621]
  cl(0:1,119) = [ 180.00,  0.0000]

  IF ( y .EQ. cl(0,0) ) cl6 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl6 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cl(0,0) ) THEN 
     cl6 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl6_iea15.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl6 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl6_iea15.pro'
  ENDIF
  
!print,y,cl6
  
!_________________________________________________________________________
  
END FUNCTION calc_cl6_iea15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl7_iea15( y ) RESULT( cl7 )
!_________________________________________________________________________
!
! Data from  IEA 15 MW: FFA-W3-241
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 120

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl7
  INTEGER :: i

  cl7 = 0.0

!                alpha    CL  
  cl(0:1,  0) = [-180.00,  0.0000]
  cl(0:1,  1) = [-177.73,  0.0582]
  cl(0:1,  2) = [-175.44,  0.1164]
  cl(0:1,  3) = [-173.15,  0.1745]
  cl(0:1,  4) = [-170.86,  0.2327]
  cl(0:1,  5) = [-168.56,  0.2909]
  cl(0:1,  6) = [-166.27,  0.3491]
  cl(0:1,  7) = [-163.98,  0.4072]
  cl(0:1,  8) = [-161.69,  0.4654]
  cl(0:1,  9) = [-159.45,  0.5236]
  cl(0:1, 10) = [-157.16,  0.5818]
  cl(0:1, 11) = [-154.87,  0.6400]
  cl(0:1, 12) = [-152.58,  0.6981]
  cl(0:1, 13) = [-150.29,  0.7563]
  cl(0:1, 14) = [-147.99,  0.8145]
  cl(0:1, 15) = [-143.87,  0.7792]
  cl(0:1, 16) = [-139.69,  0.7451]
  cl(0:1, 17) = [-135.56,  0.7088]
  cl(0:1, 18) = [-131.44,  0.6683]
  cl(0:1, 19) = [-127.31,  0.6225]
  cl(0:1, 20) = [-123.13,  0.5708]
  cl(0:1, 21) = [-119.00,  0.5131]
  cl(0:1, 22) = [-114.88,  0.4496]
  cl(0:1, 23) = [-110.70,  0.3812]
  cl(0:1, 24) = [-106.57,  0.3085]
  cl(0:1, 25) = [-102.44,  0.2327]
  cl(0:1, 26) = [ -98.26,  0.1550]
  cl(0:1, 27) = [ -94.14,  0.0770]
  cl(0:1, 28) = [ -90.01,  0.0000]
  cl(0:1, 29) = [ -85.83, -0.0770]
  cl(0:1, 30) = [ -81.70, -0.1550]
  cl(0:1, 31) = [ -77.58, -0.2327]
  cl(0:1, 32) = [ -73.45, -0.3085]
  cl(0:1, 33) = [ -69.27, -0.3812]
  cl(0:1, 34) = [ -65.15, -0.4496]
  cl(0:1, 35) = [ -61.02, -0.5131]
  cl(0:1, 36) = [ -56.84, -0.5708]
  cl(0:1, 37) = [ -52.71, -0.6225]
  cl(0:1, 38) = [ -48.59, -0.6683]
  cl(0:1, 39) = [ -44.40, -0.7088]
  cl(0:1, 40) = [ -40.28, -0.7451]
  cl(0:1, 41) = [ -36.15, -0.7792]
  cl(0:1, 42) = [ -32.03, -0.8145]
  cl(0:1, 43) = [ -28.02, -1.0778]
  cl(0:1, 44) = [ -24.01, -1.1269]
  cl(0:1, 45) = [ -20.00, -1.1448]
  cl(0:1, 46) = [ -17.99, -1.1280]
  cl(0:1, 47) = [ -15.99, -1.0939]
  cl(0:1, 48) = [ -13.98, -1.0596]
  cl(0:1, 49) = [ -11.97, -1.0312]
  cl(0:1, 50) = [ -10.03, -0.9371]
  cl(0:1, 51) = [  -8.02, -0.6738]
  cl(0:1, 52) = [  -6.02, -0.4039]
  cl(0:1, 53) = [  -4.01, -0.1423]
  cl(0:1, 54) = [  -2.01,  0.1158]
  cl(0:1, 55) = [  -0.97,  0.2438]
  cl(0:1, 56) = [   0.00,  0.3711]
  cl(0:1, 57) = [   0.97,  0.4977]
  cl(0:1, 58) = [   2.01,  0.6233]
  cl(0:1, 59) = [   2.98,  0.7480]
  cl(0:1, 60) = [   4.01,  0.8714]
  cl(0:1, 61) = [   4.98,  0.9932]
  cl(0:1, 62) = [   6.02,  1.1133]
  cl(0:1, 63) = [   6.99,  1.2304]
  cl(0:1, 64) = [   8.02,  1.3450]
  cl(0:1, 65) = [   9.00,  1.4541]
  cl(0:1, 66) = [  10.03,  1.5591]
  cl(0:1, 67) = [  11.00,  1.6578]
  cl(0:1, 68) = [  11.97,  1.7483]
  cl(0:1, 69) = [  13.01,  1.8267]
  cl(0:1, 70) = [  13.98,  1.8883]
  cl(0:1, 71) = [  15.01,  1.9258]
  cl(0:1, 72) = [  15.99,  1.9272]
  cl(0:1, 73) = [  17.99,  1.8006]
  cl(0:1, 74) = [  20.00,  1.6309]
  cl(0:1, 75) = [  24.01,  1.4334]
  cl(0:1, 76) = [  28.02,  1.2880]
  cl(0:1, 77) = [  32.03,  1.1636]
  cl(0:1, 78) = [  36.15,  1.1132]
  cl(0:1, 79) = [  40.28,  1.0644]
  cl(0:1, 80) = [  44.40,  1.0126]
  cl(0:1, 81) = [  48.59,  0.9548]
  cl(0:1, 82) = [  52.71,  0.8893]
  cl(0:1, 83) = [  56.84,  0.8154]
  cl(0:1, 84) = [  61.02,  0.7330]
  cl(0:1, 85) = [  65.15,  0.6424]
  cl(0:1, 86) = [  69.27,  0.5445]
  cl(0:1, 87) = [  73.45,  0.4407]
  cl(0:1, 88) = [  77.58,  0.3324]
  cl(0:1, 89) = [  81.70,  0.2215]
  cl(0:1, 90) = [  85.83,  0.1100]
  cl(0:1, 91) = [  90.01,  0.0000]
  cl(0:1, 92) = [  94.14, -0.0770]
  cl(0:1, 93) = [  98.26, -0.1550]
  cl(0:1, 94) = [ 102.44, -0.2327]
  cl(0:1, 95) = [ 106.57, -0.3085]
  cl(0:1, 96) = [ 110.70, -0.3812]
  cl(0:1, 97) = [ 114.88, -0.4496]
  cl(0:1, 98) = [ 119.00, -0.5131]
  cl(0:1, 99) = [ 123.13, -0.5708]
  cl(0:1,100) = [ 127.31, -0.6225]
  cl(0:1,101) = [ 131.44, -0.6683]
  cl(0:1,102) = [ 135.56, -0.7088]
  cl(0:1,103) = [ 139.69, -0.7451]
  cl(0:1,104) = [ 143.87, -0.7792]
  cl(0:1,105) = [ 147.99, -0.8145]
  cl(0:1,106) = [ 150.29, -0.7563]
  cl(0:1,107) = [ 152.58, -0.6981]
  cl(0:1,108) = [ 154.87, -0.6400]
  cl(0:1,109) = [ 157.16, -0.5818]
  cl(0:1,110) = [ 159.45, -0.5236]
  cl(0:1,111) = [ 161.69, -0.4654]
  cl(0:1,112) = [ 163.98, -0.4072]
  cl(0:1,113) = [ 166.27, -0.3491]
  cl(0:1,114) = [ 168.56, -0.2909]
  cl(0:1,115) = [ 170.86, -0.2327]
  cl(0:1,116) = [ 173.15, -0.1745]
  cl(0:1,117) = [ 175.44, -0.1164]
  cl(0:1,118) = [ 177.73, -0.0582]
  cl(0:1,119) = [ 180.00,  0.0000]

  IF ( y .EQ. cl(0,0) ) cl7 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl7 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cl(0,0) ) THEN 
     cl7 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl7_iea15.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl7 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl7_iea15.pro'
  ENDIF
  
!print,y,cl7
  
!_________________________________________________________________________
  
END FUNCTION calc_cl7_iea15
!_________________________________________________________________________

!=========================================================================

Function calc_cl8_iea15( y ) RESULT( cl8 )
!_________________________________________________________________________
!
! Data from IEA 15 MW: FFA-W3-211
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 120

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl8
  INTEGER :: i

  cl8 = 0.0

!                alpha    CL  
  cl(0:1,  0) = [-180.00,  0.0000]
  cl(0:1,  1) = [-177.71,  0.0540]
  cl(0:1,  2) = [-175.43,  0.1081]
  cl(0:1,  3) = [-173.14,  0.1621]
  cl(0:1,  4) = [-170.86,  0.2161]
  cl(0:1,  5) = [-168.57,  0.2701]
  cl(0:1,  6) = [-166.29,  0.3242]
  cl(0:1,  7) = [-164.00,  0.3782]
  cl(0:1,  8) = [-161.71,  0.4322]
  cl(0:1,  9) = [-159.43,  0.4862]
  cl(0:1, 10) = [-157.14,  0.5403]
  cl(0:1, 11) = [-154.86,  0.5943]
  cl(0:1, 12) = [-152.57,  0.6483]
  cl(0:1, 13) = [-150.29,  0.7023]
  cl(0:1, 14) = [-148.00,  0.7564]
  cl(0:1, 15) = [-143.86,  0.7319]
  cl(0:1, 16) = [-139.71,  0.7066]
  cl(0:1, 17) = [-135.57,  0.6776]
  cl(0:1, 18) = [-131.43,  0.6433]
  cl(0:1, 19) = [-127.29,  0.6028]
  cl(0:1, 20) = [-123.14,  0.5555]
  cl(0:1, 21) = [-119.00,  0.5016]
  cl(0:1, 22) = [-114.86,  0.4413]
  cl(0:1, 23) = [-110.71,  0.3754]
  cl(0:1, 24) = [-106.57,  0.3048]
  cl(0:1, 25) = [-102.43,  0.2306]
  cl(0:1, 26) = [ -98.29,  0.1541]
  cl(0:1, 27) = [ -94.14,  0.0768]
  cl(0:1, 28) = [ -90.00,  0.0000]
  cl(0:1, 29) = [ -85.86, -0.0768]
  cl(0:1, 30) = [ -81.71, -0.1541]
  cl(0:1, 31) = [ -77.57, -0.2306]
  cl(0:1, 32) = [ -73.43, -0.3048]
  cl(0:1, 33) = [ -69.29, -0.3754]
  cl(0:1, 34) = [ -65.14, -0.4413]
  cl(0:1, 35) = [ -61.00, -0.5016]
  cl(0:1, 36) = [ -56.86, -0.5555]
  cl(0:1, 37) = [ -52.71, -0.6028]
  cl(0:1, 38) = [ -48.57, -0.6433]
  cl(0:1, 39) = [ -44.43, -0.6776]
  cl(0:1, 40) = [ -40.29, -0.7066]
  cl(0:1, 41) = [ -36.14, -0.7319]
  cl(0:1, 42) = [ -32.00, -0.7564]
  cl(0:1, 43) = [ -28.00, -0.8564]
  cl(0:1, 44) = [ -24.00, -1.1829]
  cl(0:1, 45) = [ -20.00, -1.2360]
  cl(0:1, 46) = [ -18.00, -1.2254]
  cl(0:1, 47) = [ -16.00, -1.2048]
  cl(0:1, 48) = [ -14.00, -1.1833]
  cl(0:1, 49) = [ -12.00, -1.1009]
  cl(0:1, 50) = [ -10.00, -0.8821]
  cl(0:1, 51) = [  -8.00, -0.6298]
  cl(0:1, 52) = [  -6.00, -0.3767]
  cl(0:1, 53) = [  -4.00, -0.1218]
  cl(0:1, 54) = [  -2.00,  0.1281]
  cl(0:1, 55) = [  -1.00,  0.2519]
  cl(0:1, 56) = [   0.00,  0.3754]
  cl(0:1, 57) = [   1.00,  0.4983]
  cl(0:1, 58) = [   2.00,  0.6205]
  cl(0:1, 59) = [   3.00,  0.7420]
  cl(0:1, 60) = [   4.00,  0.8624]
  cl(0:1, 61) = [   5.00,  0.9811]
  cl(0:1, 62) = [   6.00,  1.0966]
  cl(0:1, 63) = [   7.00,  1.2090]
  cl(0:1, 64) = [   8.00,  1.3168]
  cl(0:1, 65) = [   9.00,  1.4221]
  cl(0:1, 66) = [  10.00,  1.5236]
  cl(0:1, 67) = [  11.00,  1.6199]
  cl(0:1, 68) = [  12.00,  1.7094]
  cl(0:1, 69) = [  13.00,  1.7868]
  cl(0:1, 70) = [  14.00,  1.8429]
  cl(0:1, 71) = [  15.00,  1.8531]
  cl(0:1, 72) = [  16.00,  1.8095]
  cl(0:1, 73) = [  18.00,  1.6603]
  cl(0:1, 74) = [  20.00,  1.5615]
  cl(0:1, 75) = [  24.00,  1.4333]
  cl(0:1, 76) = [  28.00,  1.2906]
  cl(0:1, 77) = [  32.00,  1.0805]
  cl(0:1, 78) = [  36.14,  1.0455]
  cl(0:1, 79) = [  40.29,  1.0094]
  cl(0:1, 80) = [  44.43,  0.9680]
  cl(0:1, 81) = [  48.57,  0.9190]
  cl(0:1, 82) = [  52.71,  0.8611]
  cl(0:1, 83) = [  56.86,  0.7936]
  cl(0:1, 84) = [  61.00,  0.7165]
  cl(0:1, 85) = [  65.14,  0.6304]
  cl(0:1, 86) = [  69.29,  0.5363]
  cl(0:1, 87) = [  73.43,  0.4355]
  cl(0:1, 88) = [  77.57,  0.3295]
  cl(0:1, 89) = [  81.71,  0.2202]
  cl(0:1, 90) = [  85.86,  0.1096]
  cl(0:1, 91) = [  90.00,  0.0000]
  cl(0:1, 92) = [  94.14, -0.0768]
  cl(0:1, 93) = [  98.29, -0.1541]
  cl(0:1, 94) = [ 102.43, -0.2306]
  cl(0:1, 95) = [ 106.57, -0.3048]
  cl(0:1, 96) = [ 110.71, -0.3754]
  cl(0:1, 97) = [ 114.86, -0.4413]
  cl(0:1, 98) = [ 119.00, -0.5016]
  cl(0:1, 99) = [ 123.14, -0.5555]
  cl(0:1,100) = [ 127.29, -0.6028]
  cl(0:1,101) = [ 131.43, -0.6433]
  cl(0:1,102) = [ 135.57, -0.6776]
  cl(0:1,103) = [ 139.71, -0.7066]
  cl(0:1,104) = [ 143.86, -0.7319]
  cl(0:1,105) = [ 148.00, -0.7564]
  cl(0:1,106) = [ 150.29, -0.7023]
  cl(0:1,107) = [ 152.57, -0.6483]
  cl(0:1,108) = [ 154.86, -0.5943]
  cl(0:1,109) = [ 157.14, -0.5403]
  cl(0:1,110) = [ 159.43, -0.4862]
  cl(0:1,111) = [ 161.71, -0.4322]
  cl(0:1,112) = [ 164.00, -0.3782]
  cl(0:1,113) = [ 166.29, -0.3242]
  cl(0:1,114) = [ 168.57, -0.2701]
  cl(0:1,115) = [ 170.86, -0.2161]
  cl(0:1,116) = [ 173.14, -0.1621]
  cl(0:1,117) = [ 175.43, -0.1081]
  cl(0:1,118) = [ 177.71, -0.0540]
  cl(0:1,119) = [ 180.00,  0.0000]

  IF ( y .EQ. cl(0,0) ) cl8 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl8 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cl(0,0) ) THEN 
     cl8 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl8_iea15.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl8 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl8_iea15.pro'
  ENDIF
  
!print,y,cl8
  
!_________________________________________________________________________
  
END FUNCTION calc_cl8_iea15
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cd_iea15( x,y ) RESULT( cd )
!_________________________________________________________________________
!
! Calls appropriate airfoil section table depending upon radial distance
! from blade root. Data from IEA 15 MW
!
! calc_cl1_iea15 uses data from circular
! calc_cl2_iea15 uses data from SNL-FFA-W3-500
! calc_cl3_iea15 uses data from FFA-W3-360
! calc_cl4_iea15 uses data from FFA-W3-330blend
! calc_cl5_iea15 uses data from FFA-W3-301
! calc_cl6_iea15 uses data from FFA-W3-270blend
! calc_cl7_iea15 uses data from FFA-W3-241
! calc_cl8_iea15 uses data from FFA-W3-211
!
! x = radial distance from hub center
! y = angle of attack
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: x,y

  INTEGER, PARAMETER :: ns = 10

  REAL, DIMENSION( 0:ns-1 ) :: r

  REAL :: cd
  INTEGER :: i

  cd = 0.0

  r(0) = 3.97
  r(1) = 6.31
  r(2) = 21.52
  r(3) = 32.65492706
  r(4) = 42.44474223
  r(5) = 55.35398353
  r(6) = 66.87755463
  r(7) = 78.64029586
  r(8) = 94.26403072
  r(9) = 120.97

  IF ( ( x .GT. r( 0) ) .AND. (x .LE. r( 1) ) ) THEN
       cd = calc_cd1_iea15(y)
     ELSEIF ( ( x .GT. r( 1) ) .AND. (x .LE. r( 2) ) ) THEN
       cd = calc_cd2_iea15(y)
     ELSEIF ( ( x .GT. r( 2) ) .AND. (x .LE. r( 3) ) ) THEN
       cd = calc_cd3_iea15(y)
     ELSEIF ( ( x .GT. r( 3) ) .AND. (x .LE. r( 4) ) ) THEN
       cd = calc_cd4_iea15(y)
     ELSEIF ( ( x .GT. r( 4) ) .AND. (x .LE. r( 5) ) ) THEN
       cd = calc_cd5_iea15(y)
     ELSEIF ( ( x .GT. r( 5) ) .AND. (x .LE. r( 6) ) ) THEN
       cd = calc_cd6_iea15(y)
     ELSEIF ( ( x .GT. r( 6) ) .AND. (x .LE. r( 7) ) ) THEN
       cd = calc_cd7_iea15(y)
     ELSEIF ( ( x .GT. r( 7) ) .AND. (x .LE. r( 8) ) ) THEN
       cd = calc_cd8_iea15(y)  
     ELSEIF ( ( x .GT. r( 8) ) .AND. (x .LE. r( 9) ) ) THEN
       cd = calc_cd8_iea15(y)  
  ENDIF
  
  IF ( x .LT. r(0) ) THEN
     PRINT*,'—————————————'
     PRINT*,'radius less than blade root in calc_cd_iea15.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  
  IF ( x .GT. r(ns-1) ) THEN
     PRINT*,'—————————————'
     PRINT*,'radius greater than blade length in calc_cd_iea15.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
!_________________________________________________________________________
  
END FUNCTION calc_cd_iea15
!_________________________________________________________________________

FUNCTION calc_cd1_iea15( y ) RESULT( cd1 )
!_________________________________________________________________________
!
! Data from IEA 15 MW: circular
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL :: cd1

  cd1 = 0.35
!_________________________________________________________________________
  
END FUNCTION calc_cd1_iea15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd2_iea15( y ) RESULT( cd2 )
!_________________________________________________________________________
!
! Data from IEA 15 MW: SNL-FFA-W3-500
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 199

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd2

  cd2 = 0.0

!               alpha    CD
  cd(0:1,  0) = [-180.00,  0.0844]
  cd(0:1,  1) = [-170.00,  0.0844]
  cd(0:1,  2) = [-160.00,  0.1268]
  cd(0:1,  3) = [-150.00,  0.2927]
  cd(0:1,  4) = [-140.00,  0.4970]
  cd(0:1,  5) = [-130.00,  0.7161]
  cd(0:1,  6) = [-120.00,  0.9246]
  cd(0:1,  7) = [-110.00,  1.0985]
  cd(0:1,  8) = [-100.00,  1.2182]
  cd(0:1,  9) = [ -90.00,  1.2707]
  cd(0:1, 10) = [ -80.00,  1.2182]
  cd(0:1, 11) = [ -70.00,  1.0985]
  cd(0:1, 12) = [ -60.00,  0.9246]
  cd(0:1, 13) = [ -50.00,  0.7161]
  cd(0:1, 14) = [ -40.00,  0.4970]
  cd(0:1, 15) = [ -30.00,  0.2927]
  cd(0:1, 16) = [ -20.00,  0.1499]
  cd(0:1, 17) = [ -19.75,  0.1472]
  cd(0:1, 18) = [ -19.25,  0.1447]
  cd(0:1, 19) = [ -19.00,  0.1433]
  cd(0:1, 20) = [ -18.75,  0.1403]
  cd(0:1, 21) = [ -18.50,  0.1386]
  cd(0:1, 22) = [ -18.25,  0.1373]
  cd(0:1, 23) = [ -18.00,  0.1360]
  cd(0:1, 24) = [ -17.50,  0.1322]
  cd(0:1, 25) = [ -17.25,  0.1306]
  cd(0:1, 26) = [ -17.00,  0.1290]
  cd(0:1, 27) = [ -16.75,  0.1268]
  cd(0:1, 28) = [ -16.50,  0.1258]
  cd(0:1, 29) = [ -16.25,  0.1246]
  cd(0:1, 30) = [ -16.00,  0.1229]
  cd(0:1, 31) = [ -15.75,  0.1206]
  cd(0:1, 32) = [ -15.50,  0.1195]
  cd(0:1, 33) = [ -15.25,  0.1185]
  cd(0:1, 34) = [ -14.75,  0.1150]
  cd(0:1, 35) = [ -14.50,  0.1138]
  cd(0:1, 36) = [ -14.25,  0.1127]
  cd(0:1, 37) = [ -14.00,  0.1110]
  cd(0:1, 38) = [ -13.75,  0.1100]
  cd(0:1, 39) = [ -13.50,  0.1089]
  cd(0:1, 40) = [ -13.25,  0.1075]
  cd(0:1, 41) = [ -13.00,  0.1059]
  cd(0:1, 42) = [ -12.75,  0.1051]
  cd(0:1, 43) = [ -12.50,  0.1042]
  cd(0:1, 44) = [ -12.25,  0.1023]
  cd(0:1, 45) = [ -12.00,  0.1013]
  cd(0:1, 46) = [ -11.75,  0.1004]
  cd(0:1, 47) = [ -11.50,  0.0997]
  cd(0:1, 48) = [ -11.00,  0.0971]
  cd(0:1, 49) = [ -10.75,  0.0956]
  cd(0:1, 50) = [ -10.50,  0.0948]
  cd(0:1, 51) = [ -10.25,  0.0940]
  cd(0:1, 52) = [ -10.00,  0.0925]
  cd(0:1, 53) = [  -9.75,  0.0917]
  cd(0:1, 54) = [  -9.50,  0.0912]
  cd(0:1, 55) = [  -9.25,  0.0902]
  cd(0:1, 56) = [  -9.00,  0.0895]
  cd(0:1, 57) = [  -8.75,  0.0891]
  cd(0:1, 58) = [  -8.50,  0.0887]
  cd(0:1, 59) = [  -8.25,  0.0879]
  cd(0:1, 60) = [  -8.00,  0.0875]
  cd(0:1, 61) = [  -7.75,  0.0873]
  cd(0:1, 62) = [  -7.50,  0.0868]
  cd(0:1, 63) = [  -7.25,  0.0864]
  cd(0:1, 64) = [  -7.00,  0.0862]
  cd(0:1, 65) = [  -6.75,  0.0860]
  cd(0:1, 66) = [  -6.50,  0.0856]
  cd(0:1, 67) = [  -6.25,  0.0853]
  cd(0:1, 68) = [  -6.00,  0.0852]
  cd(0:1, 69) = [  -5.75,  0.0850]
  cd(0:1, 70) = [  -5.50,  0.0847]
  cd(0:1, 71) = [  -5.25,  0.0846]
  cd(0:1, 72) = [  -5.00,  0.0845]
  cd(0:1, 73) = [  -4.75,  0.0843]
  cd(0:1, 74) = [  -4.50,  0.0842]
  cd(0:1, 75) = [  -4.25,  0.0840]
  cd(0:1, 76) = [  -4.00,  0.0840]
  cd(0:1, 77) = [  -3.75,  0.0839]
  cd(0:1, 78) = [  -3.50,  0.0838]
  cd(0:1, 79) = [  -3.25,  0.0838]
  cd(0:1, 80) = [  -3.00,  0.0838]
  cd(0:1, 81) = [  -2.75,  0.0838]
  cd(0:1, 82) = [  -2.50,  0.0838]
  cd(0:1, 83) = [  -2.25,  0.0838]
  cd(0:1, 84) = [  -2.00,  0.0838]
  cd(0:1, 85) = [  -1.75,  0.0837]
  cd(0:1, 86) = [  -1.50,  0.0837]
  cd(0:1, 87) = [  -1.25,  0.0838]
  cd(0:1, 88) = [  -1.00,  0.0838]
  cd(0:1, 89) = [  -0.75,  0.0838]
  cd(0:1, 90) = [  -0.50,  0.0838]
  cd(0:1, 91) = [  -0.25,  0.0838]
  cd(0:1, 92) = [   0.00,  0.0838]
  cd(0:1, 93) = [   0.23,  0.0839]
  cd(0:1, 94) = [   0.47,  0.0839]
  cd(0:1, 95) = [   0.70,  0.0839]
  cd(0:1, 96) = [   0.94,  0.0840]
  cd(0:1, 97) = [   1.17,  0.0840]
  cd(0:1, 98) = [   1.40,  0.0841]
  cd(0:1, 99) = [   1.64,  0.0841]
  cd(0:1,100) = [   1.87,  0.0842]
  cd(0:1,101) = [   2.11,  0.0842]
  cd(0:1,102) = [   2.34,  0.0843]
  cd(0:1,103) = [   2.57,  0.0843]
  cd(0:1,104) = [   2.81,  0.0844]
  cd(0:1,105) = [   3.04,  0.0845]
  cd(0:1,106) = [   3.28,  0.0846]
  cd(0:1,107) = [   3.51,  0.0846]
  cd(0:1,108) = [   3.74,  0.0847]
  cd(0:1,109) = [   3.98,  0.0847]
  cd(0:1,110) = [   4.21,  0.0848]
  cd(0:1,111) = [   4.45,  0.0849]
  cd(0:1,112) = [   4.68,  0.0850]
  cd(0:1,113) = [   4.91,  0.0851]
  cd(0:1,114) = [   5.15,  0.0852]
  cd(0:1,115) = [   5.38,  0.0853]
  cd(0:1,116) = [   5.62,  0.0853]
  cd(0:1,117) = [   5.85,  0.0854]
  cd(0:1,118) = [   6.08,  0.0856]
  cd(0:1,119) = [   6.32,  0.0857]
  cd(0:1,120) = [   6.55,  0.0858]
  cd(0:1,121) = [   6.79,  0.0859]
  cd(0:1,122) = [   7.02,  0.0860]
  cd(0:1,123) = [   7.25,  0.0861]
  cd(0:1,124) = [   7.49,  0.0862]
  cd(0:1,125) = [   7.72,  0.0864]
  cd(0:1,126) = [   7.96,  0.0865]
  cd(0:1,127) = [   8.19,  0.0867]
  cd(0:1,128) = [   8.42,  0.0869]
  cd(0:1,129) = [   8.66,  0.0870]
  cd(0:1,130) = [   8.89,  0.0871]
  cd(0:1,131) = [   9.12,  0.0873]
  cd(0:1,132) = [   9.88,  0.0879]
  cd(0:1,133) = [  10.64,  0.0886]
  cd(0:1,134) = [  11.40,  0.0895]
  cd(0:1,135) = [  12.15,  0.0912]
  cd(0:1,136) = [  12.91,  0.0930]
  cd(0:1,137) = [  13.67,  0.0954]
  cd(0:1,138) = [  14.42,  0.0989]
  cd(0:1,139) = [  15.18,  0.1024]
  cd(0:1,140) = [  15.94,  0.1076]
  cd(0:1,141) = [  16.69,  0.1144]
  cd(0:1,142) = [  17.45,  0.1211]
  cd(0:1,143) = [  18.21,  0.1310]
  cd(0:1,144) = [  18.97,  0.1399]
  cd(0:1,145) = [  19.72,  0.1492]
  cd(0:1,146) = [  20.48,  0.1591]
  cd(0:1,147) = [  21.24,  0.1691]
  cd(0:1,148) = [  21.99,  0.1778]
  cd(0:1,149) = [  22.75,  0.1858]
  cd(0:1,150) = [  23.51,  0.1937]
  cd(0:1,151) = [  24.26,  0.2014]
  cd(0:1,152) = [  25.02,  0.2135]
  cd(0:1,153) = [  25.78,  0.2267]
  cd(0:1,154) = [  26.53,  0.2399]
  cd(0:1,155) = [  27.29,  0.2531]
  cd(0:1,156) = [  28.05,  0.2663]
  cd(0:1,157) = [  28.81,  0.2795]
  cd(0:1,158) = [  29.56,  0.2927]
  cd(0:1,159) = [  30.32,  0.3078]
  cd(0:1,160) = [  31.08,  0.3230]
  cd(0:1,161) = [  31.83,  0.3381]
  cd(0:1,162) = [  32.59,  0.3532]
  cd(0:1,163) = [  33.35,  0.3684]
  cd(0:1,164) = [  34.10,  0.3835]
  cd(0:1,165) = [  34.86,  0.3987]
  cd(0:1,166) = [  35.62,  0.4138]
  cd(0:1,167) = [  36.38,  0.4289]
  cd(0:1,168) = [  37.13,  0.4441]
  cd(0:1,169) = [  37.89,  0.4592]
  cd(0:1,170) = [  38.65,  0.4743]
  cd(0:1,171) = [  39.40,  0.4895]
  cd(0:1,172) = [  40.16,  0.5052]
  cd(0:1,173) = [  40.92,  0.5214]
  cd(0:1,174) = [  41.67,  0.5376]
  cd(0:1,175) = [  42.43,  0.5538]
  cd(0:1,176) = [  43.19,  0.5701]
  cd(0:1,177) = [  43.94,  0.5863]
  cd(0:1,178) = [  44.70,  0.6025]
  cd(0:1,179) = [  45.46,  0.6188]
  cd(0:1,180) = [  46.22,  0.6350]
  cd(0:1,181) = [  46.97,  0.6512]
  cd(0:1,182) = [  47.73,  0.6675]
  cd(0:1,183) = [  48.49,  0.6837]
  cd(0:1,184) = [  49.24,  0.6999]
  cd(0:1,185) = [  50.00,  0.7161]
  cd(0:1,186) = [  60.00,  0.9246]
  cd(0:1,187) = [  70.00,  1.0985]
  cd(0:1,188) = [  80.00,  1.2182]
  cd(0:1,189) = [  90.00,  1.2707]
  cd(0:1,190) = [ 100.00,  1.2182]
  cd(0:1,191) = [ 110.00,  1.0985]
  cd(0:1,192) = [ 120.00,  0.9246]
  cd(0:1,193) = [ 130.00,  0.7161]
  cd(0:1,194) = [ 140.00,  0.4970]
  cd(0:1,195) = [ 150.00,  0.2927]
  cd(0:1,196) = [ 160.00,  0.1268]
  cd(0:1,197) = [ 170.00,  0.0844]
  cd(0:1,198) = [ 180.00,  0.0844]
!_________________________________________________________________________

  IF ( y .EQ. cd(0,0) ) cd2 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd2 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN
     cd2 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd3_iea15.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd2 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd3_iea15.pro'
  ENDIF

  
END FUNCTION calc_cd2_iea15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd3_iea15( y ) RESULT( cd3 )
!_________________________________________________________________________
!
! Data from IEA 15 MW: FFA-W3-360
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 120

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd3
  INTEGER :: i

  cd3 = 0.0

!               alpha    CD   
  cd(0:1,  0) = [-180.00,  0.0371]
  cd(0:1,  1) = [-177.71,  0.0377]
  cd(0:1,  2) = [-175.43,  0.0395]
  cd(0:1,  3) = [-173.14,  0.0424]
  cd(0:1,  4) = [-170.86,  0.0465]
  cd(0:1,  5) = [-168.57,  0.0517]
  cd(0:1,  6) = [-166.29,  0.0607]
  cd(0:1,  7) = [-164.00,  0.0865]
  cd(0:1,  8) = [-161.71,  0.1159]
  cd(0:1,  9) = [-159.43,  0.1486]
  cd(0:1, 10) = [-157.14,  0.1844]
  cd(0:1, 11) = [-154.86,  0.2231]
  cd(0:1, 12) = [-152.57,  0.2645]
  cd(0:1, 13) = [-150.29,  0.3083]
  cd(0:1, 14) = [-148.00,  0.3542]
  cd(0:1, 15) = [-143.86,  0.4419]
  cd(0:1, 16) = [-139.71,  0.5338]
  cd(0:1, 17) = [-135.57,  0.6279]
  cd(0:1, 18) = [-131.43,  0.7224]
  cd(0:1, 19) = [-127.29,  0.8152]
  cd(0:1, 20) = [-123.14,  0.9044]
  cd(0:1, 21) = [-119.00,  0.9883]
  cd(0:1, 22) = [-114.86,  1.0649]
  cd(0:1, 23) = [-110.71,  1.1328]
  cd(0:1, 24) = [-106.57,  1.1906]
  cd(0:1, 25) = [-102.43,  1.2370]
  cd(0:1, 26) = [ -98.29,  1.2712]
  cd(0:1, 27) = [ -94.14,  1.2923]
  cd(0:1, 28) = [ -90.00,  1.3000]
  cd(0:1, 29) = [ -85.86,  1.2923]
  cd(0:1, 30) = [ -81.71,  1.2712]
  cd(0:1, 31) = [ -77.57,  1.2370]
  cd(0:1, 32) = [ -73.43,  1.1906]
  cd(0:1, 33) = [ -69.29,  1.1328]
  cd(0:1, 34) = [ -65.14,  1.0649]
  cd(0:1, 35) = [ -61.00,  0.9883]
  cd(0:1, 36) = [ -56.86,  0.9044]
  cd(0:1, 37) = [ -52.71,  0.8152]
  cd(0:1, 38) = [ -48.57,  0.7224]
  cd(0:1, 39) = [ -44.43,  0.6279]
  cd(0:1, 40) = [ -40.29,  0.5338]
  cd(0:1, 41) = [ -36.14,  0.4419]
  cd(0:1, 42) = [ -32.00,  0.3542]
  cd(0:1, 43) = [ -28.00,  0.2049]
  cd(0:1, 44) = [ -24.00,  0.1543]
  cd(0:1, 45) = [ -20.00,  0.1097]
  cd(0:1, 46) = [ -18.00,  0.0925]
  cd(0:1, 47) = [ -16.00,  0.0760]
  cd(0:1, 48) = [ -14.00,  0.0605]
  cd(0:1, 49) = [ -12.00,  0.0464]
  cd(0:1, 50) = [ -10.00,  0.0344]
  cd(0:1, 51) = [  -8.00,  0.0255]
  cd(0:1, 52) = [  -6.00,  0.0199]
  cd(0:1, 53) = [  -4.00,  0.0165]
  cd(0:1, 54) = [  -2.00,  0.0151]
  cd(0:1, 55) = [  -1.00,  0.0148]
  cd(0:1, 56) = [   0.00,  0.0146]
  cd(0:1, 57) = [   1.00,  0.0147]
  cd(0:1, 58) = [   2.00,  0.0148]
  cd(0:1, 59) = [   3.00,  0.0151]
  cd(0:1, 60) = [   4.00,  0.0154]
  cd(0:1, 61) = [   5.00,  0.0159]
  cd(0:1, 62) = [   6.00,  0.0165]
  cd(0:1, 63) = [   7.00,  0.0173]
  cd(0:1, 64) = [   8.00,  0.0183]
  cd(0:1, 65) = [   9.00,  0.0196]
  cd(0:1, 66) = [  10.00,  0.0215]
  cd(0:1, 67) = [  11.00,  0.0245]
  cd(0:1, 68) = [  12.00,  0.0297]
  cd(0:1, 69) = [  13.00,  0.0377]
  cd(0:1, 70) = [  14.00,  0.0482]
  cd(0:1, 71) = [  15.00,  0.0584]
  cd(0:1, 72) = [  16.00,  0.0699]
  cd(0:1, 73) = [  18.00,  0.1017]
  cd(0:1, 74) = [  20.00,  0.1392]
  cd(0:1, 75) = [  24.00,  0.2100]
  cd(0:1, 76) = [  28.00,  0.2820]
  cd(0:1, 77) = [  32.00,  0.3542]
  cd(0:1, 78) = [  36.14,  0.4419]
  cd(0:1, 79) = [  40.29,  0.5338]
  cd(0:1, 80) = [  44.43,  0.6279]
  cd(0:1, 81) = [  48.57,  0.7224]
  cd(0:1, 82) = [  52.71,  0.8152]
  cd(0:1, 83) = [  56.86,  0.9044]
  cd(0:1, 84) = [  61.00,  0.9883]
  cd(0:1, 85) = [  65.14,  1.0649]
  cd(0:1, 86) = [  69.29,  1.1328]
  cd(0:1, 87) = [  73.43,  1.1906]
  cd(0:1, 88) = [  77.57,  1.2370]
  cd(0:1, 89) = [  81.71,  1.2712]
  cd(0:1, 90) = [  85.86,  1.2923]
  cd(0:1, 91) = [  90.00,  1.3000]
  cd(0:1, 92) = [  94.14,  1.2923]
  cd(0:1, 93) = [  98.29,  1.2712]
  cd(0:1, 94) = [ 102.43,  1.2370]
  cd(0:1, 95) = [ 106.57,  1.1906]
  cd(0:1, 96) = [ 110.71,  1.1328]
  cd(0:1, 97) = [ 114.86,  1.0649]
  cd(0:1, 98) = [ 119.00,  0.9883]
  cd(0:1, 99) = [ 123.14,  0.9044]
  cd(0:1,100) = [ 127.29,  0.8152]
  cd(0:1,101) = [ 131.43,  0.7224]
  cd(0:1,102) = [ 135.57,  0.6279]
  cd(0:1,103) = [ 139.71,  0.5338]
  cd(0:1,104) = [ 143.86,  0.4419]
  cd(0:1,105) = [ 148.00,  0.3542]
  cd(0:1,106) = [ 150.29,  0.3083]
  cd(0:1,107) = [ 152.57,  0.2645]
  cd(0:1,108) = [ 154.86,  0.2231]
  cd(0:1,109) = [ 157.14,  0.1844]
  cd(0:1,110) = [ 159.43,  0.1486]
  cd(0:1,111) = [ 161.71,  0.1159]
  cd(0:1,112) = [ 164.00,  0.0865]
  cd(0:1,113) = [ 166.29,  0.0607]
  cd(0:1,114) = [ 168.57,  0.0517]
  cd(0:1,115) = [ 170.86,  0.0465]
  cd(0:1,116) = [ 173.14,  0.0424]
  cd(0:1,117) = [ 175.43,  0.0395]
  cd(0:1,118) = [ 177.71,  0.0377]
  cd(0:1,119) = [ 180.00,  0.0371]
  
  IF ( y .EQ. cd(0,0) ) cd3 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd3 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN 
     cd3 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd3_iea15.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd3 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd3_iea15.pro'
  ENDIF
  
!print,y,cd3
  
!_________________________________________________________________________
  
END FUNCTION calc_cd3_iea15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd4_iea15( y ) RESULT( cd4 )
!_________________________________________________________________________
!
! Data from IEA 15 MW FFA-W3-330blend
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 120

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd4
  INTEGER :: i

  cd4 = 0.0

!                 alpha    CD   
  cd(0:1,  0) = [-180.00,  0.0317]
  cd(0:1,  1) = [-177.71,  0.0323]
  cd(0:1,  2) = [-175.43,  0.0341]
  cd(0:1,  3) = [-173.14,  0.0370]
  cd(0:1,  4) = [-170.86,  0.0411]
  cd(0:1,  5) = [-168.57,  0.0464]
  cd(0:1,  6) = [-166.29,  0.0573]
  cd(0:1,  7) = [-164.00,  0.0832]
  cd(0:1,  8) = [-161.71,  0.1126]
  cd(0:1,  9) = [-159.43,  0.1453]
  cd(0:1, 10) = [-157.14,  0.1812]
  cd(0:1, 11) = [-154.86,  0.2200]
  cd(0:1, 12) = [-152.57,  0.2615]
  cd(0:1, 13) = [-150.29,  0.3053]
  cd(0:1, 14) = [-148.00,  0.3513]
  cd(0:1, 15) = [-143.86,  0.4391]
  cd(0:1, 16) = [-139.71,  0.5312]
  cd(0:1, 17) = [-135.57,  0.6255]
  cd(0:1, 18) = [-131.43,  0.7201]
  cd(0:1, 19) = [-127.29,  0.8131]
  cd(0:1, 20) = [-123.14,  0.9026]
  cd(0:1, 21) = [-119.00,  0.9866]
  cd(0:1, 22) = [-114.86,  1.0635]
  cd(0:1, 23) = [-110.71,  1.1316]
  cd(0:1, 24) = [-106.57,  1.1896]
  cd(0:1, 25) = [-102.43,  1.2363]
  cd(0:1, 26) = [ -98.29,  1.2707]
  cd(0:1, 27) = [ -94.14,  1.2920]
  cd(0:1, 28) = [ -90.00,  1.3000]
  cd(0:1, 29) = [ -85.86,  1.2920]
  cd(0:1, 30) = [ -81.71,  1.2707]
  cd(0:1, 31) = [ -77.57,  1.2363]
  cd(0:1, 32) = [ -73.43,  1.1896]
  cd(0:1, 33) = [ -69.29,  1.1316]
  cd(0:1, 34) = [ -65.14,  1.0635]
  cd(0:1, 35) = [ -61.00,  0.9866]
  cd(0:1, 36) = [ -56.86,  0.9026]
  cd(0:1, 37) = [ -52.71,  0.8131]
  cd(0:1, 38) = [ -48.57,  0.7201]
  cd(0:1, 39) = [ -44.43,  0.6255]
  cd(0:1, 40) = [ -40.29,  0.5312]
  cd(0:1, 41) = [ -36.14,  0.4391]
  cd(0:1, 42) = [ -32.00,  0.3513]
  cd(0:1, 43) = [ -28.00,  0.2065]
  cd(0:1, 44) = [ -24.00,  0.1500]
  cd(0:1, 45) = [ -20.00,  0.1060]
  cd(0:1, 46) = [ -18.00,  0.0873]
  cd(0:1, 47) = [ -16.00,  0.0705]
  cd(0:1, 48) = [ -14.00,  0.0547]
  cd(0:1, 49) = [ -12.00,  0.0405]
  cd(0:1, 50) = [ -10.00,  0.0293]
  cd(0:1, 51) = [  -8.00,  0.0221]
  cd(0:1, 52) = [  -6.00,  0.0174]
  cd(0:1, 53) = [  -4.00,  0.0147]
  cd(0:1, 54) = [  -2.00,  0.0136]
  cd(0:1, 55) = [  -1.00,  0.0134]
  cd(0:1, 56) = [   0.00,  0.0133]
  cd(0:1, 57) = [   1.00,  0.0133]
  cd(0:1, 58) = [   2.00,  0.0134]
  cd(0:1, 59) = [   3.00,  0.0137]
  cd(0:1, 60) = [   4.00,  0.0140]
  cd(0:1, 61) = [   5.00,  0.0144]
  cd(0:1, 62) = [   6.00,  0.0149]
  cd(0:1, 63) = [   7.00,  0.0155]
  cd(0:1, 64) = [   8.00,  0.0162]
  cd(0:1, 65) = [   9.00,  0.0172]
  cd(0:1, 66) = [  10.00,  0.0184]
  cd(0:1, 67) = [  11.00,  0.0201]
  cd(0:1, 68) = [  12.00,  0.0226]
  cd(0:1, 69) = [  13.00,  0.0267]
  cd(0:1, 70) = [  14.00,  0.0338]
  cd(0:1, 71) = [  15.00,  0.0433]
  cd(0:1, 72) = [  16.00,  0.0535]
  cd(0:1, 73) = [  18.00,  0.0771]
  cd(0:1, 74) = [  20.00,  0.1117]
  cd(0:1, 75) = [  24.00,  0.1910]
  cd(0:1, 76) = [  28.00,  0.2720]
  cd(0:1, 77) = [  32.00,  0.3513]
  cd(0:1, 78) = [  36.14,  0.4391]
  cd(0:1, 79) = [  40.29,  0.5312]
  cd(0:1, 80) = [  44.43,  0.6255]
  cd(0:1, 81) = [  48.57,  0.7201]
  cd(0:1, 82) = [  52.71,  0.8131]
  cd(0:1, 83) = [  56.86,  0.9026]
  cd(0:1, 84) = [  61.00,  0.9866]
  cd(0:1, 85) = [  65.14,  1.0635]
  cd(0:1, 86) = [  69.29,  1.1316]
  cd(0:1, 87) = [  73.43,  1.1896]
  cd(0:1, 88) = [  77.57,  1.2363]
  cd(0:1, 89) = [  81.71,  1.2707]
  cd(0:1, 90) = [  85.86,  1.2920]
  cd(0:1, 91) = [  90.00,  1.3000]
  cd(0:1, 92) = [  94.14,  1.2920]
  cd(0:1, 93) = [  98.29,  1.2707]
  cd(0:1, 94) = [ 102.43,  1.2363]
  cd(0:1, 95) = [ 106.57,  1.1896]
  cd(0:1, 96) = [ 110.71,  1.1316]
  cd(0:1, 97) = [ 114.86,  1.0635]
  cd(0:1, 98) = [ 119.00,  0.9866]
  cd(0:1, 99) = [ 123.14,  0.9026]
  cd(0:1,100) = [ 127.29,  0.8131]
  cd(0:1,101) = [ 131.43,  0.7201]
  cd(0:1,102) = [ 135.57,  0.6255]
  cd(0:1,103) = [ 139.71,  0.5312]
  cd(0:1,104) = [ 143.86,  0.4391]
  cd(0:1,105) = [ 148.00,  0.3513]
  cd(0:1,106) = [ 150.29,  0.3053]
  cd(0:1,107) = [ 152.57,  0.2615]
  cd(0:1,108) = [ 154.86,  0.2200]
  cd(0:1,109) = [ 157.14,  0.1812]
  cd(0:1,110) = [ 159.43,  0.1453]
  cd(0:1,111) = [ 161.71,  0.1126]
  cd(0:1,112) = [ 164.00,  0.0832]
  cd(0:1,113) = [ 166.29,  0.0573]
  cd(0:1,114) = [ 168.57,  0.0464]
  cd(0:1,115) = [ 170.86,  0.0411]
  cd(0:1,116) = [ 173.14,  0.0370]
  cd(0:1,117) = [ 175.43,  0.0341]
  cd(0:1,118) = [ 177.71,  0.0323]
  cd(0:1,119) = [ 180.00,  0.0317]
  
  IF ( y .EQ. cd(0,0) ) cd4 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd4 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN 
     cd4 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd4_iea15.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd4 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd4_iea15.pro'
  ENDIF
  
!print,y,cd4
  
!_________________________________________________________________________
  
END FUNCTION calc_cd4_iea15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd5_iea15( y ) RESULT( cd5 )
!_________________________________________________________________________
!
! Data from IEA 15 MW: FFA-W3-301
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 120

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd5
  INTEGER :: i

  cd5 = 0.0

!                alpha    CD  
  cd(0:1,  0) = [-180.00,  0.0245]
  cd(0:1,  1) = [-177.71,  0.0251]
  cd(0:1,  2) = [-175.43,  0.0269]
  cd(0:1,  3) = [-173.14,  0.0299]
  cd(0:1,  4) = [-170.86,  0.0341]
  cd(0:1,  5) = [-168.57,  0.0394]
  cd(0:1,  6) = [-166.29,  0.0591]
  cd(0:1,  7) = [-164.00,  0.0849]
  cd(0:1,  8) = [-161.71,  0.1143]
  cd(0:1,  9) = [-159.43,  0.1470]
  cd(0:1, 10) = [-157.14,  0.1829]
  cd(0:1, 11) = [-154.86,  0.2217]
  cd(0:1, 12) = [-152.57,  0.2631]
  cd(0:1, 13) = [-150.29,  0.3069]
  cd(0:1, 14) = [-148.00,  0.3529]
  cd(0:1, 15) = [-143.86,  0.4406]
  cd(0:1, 16) = [-139.71,  0.5326]
  cd(0:1, 17) = [-135.57,  0.6268]
  cd(0:1, 18) = [-131.43,  0.7213]
  cd(0:1, 19) = [-127.29,  0.8142]
  cd(0:1, 20) = [-123.14,  0.9036]
  cd(0:1, 21) = [-119.00,  0.9875]
  cd(0:1, 22) = [-114.86,  1.0642]
  cd(0:1, 23) = [-110.71,  1.1323]
  cd(0:1, 24) = [-106.57,  1.1902]
  cd(0:1, 25) = [-102.43,  1.2367]
  cd(0:1, 26) = [ -98.29,  1.2709]
  cd(0:1, 27) = [ -94.14,  1.2922]
  cd(0:1, 28) = [ -90.00,  1.3000]
  cd(0:1, 29) = [ -85.86,  1.2922]
  cd(0:1, 30) = [ -81.71,  1.2709]
  cd(0:1, 31) = [ -77.57,  1.2367]
  cd(0:1, 32) = [ -73.43,  1.1902]
  cd(0:1, 33) = [ -69.29,  1.1323]
  cd(0:1, 34) = [ -65.14,  1.0642]
  cd(0:1, 35) = [ -61.00,  0.9875]
  cd(0:1, 36) = [ -56.86,  0.9036]
  cd(0:1, 37) = [ -52.71,  0.8142]
  cd(0:1, 38) = [ -48.57,  0.7213]
  cd(0:1, 39) = [ -44.43,  0.6268]
  cd(0:1, 40) = [ -40.29,  0.5326]
  cd(0:1, 41) = [ -36.14,  0.4406]
  cd(0:1, 42) = [ -32.00,  0.3529]
  cd(0:1, 43) = [ -28.00,  0.2172]
  cd(0:1, 44) = [ -24.00,  0.1563]
  cd(0:1, 45) = [ -20.00,  0.1033]
  cd(0:1, 46) = [ -18.00,  0.0818]
  cd(0:1, 47) = [ -16.00,  0.0633]
  cd(0:1, 48) = [ -14.00,  0.0472]
  cd(0:1, 49) = [ -12.00,  0.0328]
  cd(0:1, 50) = [ -10.00,  0.0235]
  cd(0:1, 51) = [  -8.00,  0.0179]
  cd(0:1, 52) = [  -6.00,  0.0143]
  cd(0:1, 53) = [  -4.00,  0.0124]
  cd(0:1, 54) = [  -2.00,  0.0116]
  cd(0:1, 55) = [  -1.00,  0.0114]
  cd(0:1, 56) = [   0.00,  0.0114]
  cd(0:1, 57) = [   1.00,  0.0114]
  cd(0:1, 58) = [   2.00,  0.0116]
  cd(0:1, 59) = [   3.00,  0.0118]
  cd(0:1, 60) = [   4.00,  0.0120]
  cd(0:1, 61) = [   5.00,  0.0124]
  cd(0:1, 62) = [   6.00,  0.0128]
  cd(0:1, 63) = [   7.00,  0.0134]
  cd(0:1, 64) = [   8.00,  0.0141]
  cd(0:1, 65) = [   9.00,  0.0149]
  cd(0:1, 66) = [  10.00,  0.0159]
  cd(0:1, 67) = [  11.00,  0.0173]
  cd(0:1, 68) = [  12.00,  0.0191]
  cd(0:1, 69) = [  13.00,  0.0217]
  cd(0:1, 70) = [  14.00,  0.0257]
  cd(0:1, 71) = [  15.00,  0.0322]
  cd(0:1, 72) = [  16.00,  0.0416]
  cd(0:1, 73) = [  18.00,  0.0673]
  cd(0:1, 74) = [  20.00,  0.1053]
  cd(0:1, 75) = [  24.00,  0.1923]
  cd(0:1, 76) = [  28.00,  0.2745]
  cd(0:1, 77) = [  32.00,  0.3529]
  cd(0:1, 78) = [  36.14,  0.4406]
  cd(0:1, 79) = [  40.29,  0.5326]
  cd(0:1, 80) = [  44.43,  0.6268]
  cd(0:1, 81) = [  48.57,  0.7213]
  cd(0:1, 82) = [  52.71,  0.8142]
  cd(0:1, 83) = [  56.86,  0.9036]
  cd(0:1, 84) = [  61.00,  0.9875]
  cd(0:1, 85) = [  65.14,  1.0642]
  cd(0:1, 86) = [  69.29,  1.1323]
  cd(0:1, 87) = [  73.43,  1.1902]
  cd(0:1, 88) = [  77.57,  1.2367]
  cd(0:1, 89) = [  81.71,  1.2709]
  cd(0:1, 90) = [  85.86,  1.2922]
  cd(0:1, 91) = [  90.00,  1.3000]
  cd(0:1, 92) = [  94.14,  1.2922]
  cd(0:1, 93) = [  98.29,  1.2709]
  cd(0:1, 94) = [ 102.43,  1.2367]
  cd(0:1, 95) = [ 106.57,  1.1902]
  cd(0:1, 96) = [ 110.71,  1.1323]
  cd(0:1, 97) = [ 114.86,  1.0642]
  cd(0:1, 98) = [ 119.00,  0.9875]
  cd(0:1, 99) = [ 123.14,  0.9036]
  cd(0:1,100) = [ 127.29,  0.8142]
  cd(0:1,101) = [ 131.43,  0.7213]
  cd(0:1,102) = [ 135.57,  0.6268]
  cd(0:1,103) = [ 139.71,  0.5326]
  cd(0:1,104) = [ 143.86,  0.4406]
  cd(0:1,105) = [ 148.00,  0.3529]
  cd(0:1,106) = [ 150.29,  0.3069]
  cd(0:1,107) = [ 152.57,  0.2631]
  cd(0:1,108) = [ 154.86,  0.2217]
  cd(0:1,109) = [ 157.14,  0.1829]
  cd(0:1,110) = [ 159.43,  0.1470]
  cd(0:1,111) = [ 161.71,  0.1143]
  cd(0:1,112) = [ 164.00,  0.0849]
  cd(0:1,113) = [ 166.29,  0.0591]
  cd(0:1,114) = [ 168.57,  0.0394]
  cd(0:1,115) = [ 170.86,  0.0341]
  cd(0:1,116) = [ 173.14,  0.0299]
  cd(0:1,117) = [ 175.43,  0.0269]
  cd(0:1,118) = [ 177.71,  0.0251]
  cd(0:1,119) = [ 180.00,  0.0245]

  IF ( y .EQ. cd(0,0) ) cd5 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd5 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN 
     cd5 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd5_iea15.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd5 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd5_iea15.pro'
  ENDIF
  
!print,y,cd5
  
!_________________________________________________________________________
  
END FUNCTION calc_cd5_iea15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd6_iea15( y ) RESULT( cd6 )
!_________________________________________________________________________
!
! Data from IEA 15 MW: FFA-W3-270blend
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 120

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd6
  INTEGER :: i

  cd6 = 0.0

!                alpha    CD  
  cd(0:1,  0) = [-180.00,  0.0155]
  cd(0:1,  1) = [-177.71,  0.0161]
  cd(0:1,  2) = [-175.43,  0.0181]
  cd(0:1,  3) = [-173.14,  0.0213]
  cd(0:1,  4) = [-170.86,  0.0259]
  cd(0:1,  5) = [-168.57,  0.0329]
  cd(0:1,  6) = [-166.29,  0.0568]
  cd(0:1,  7) = [-164.00,  0.0847]
  cd(0:1,  8) = [-161.71,  0.1164]
  cd(0:1,  9) = [-159.43,  0.1518]
  cd(0:1, 10) = [-157.14,  0.1905]
  cd(0:1, 11) = [-154.86,  0.2323]
  cd(0:1, 12) = [-152.57,  0.2771]
  cd(0:1, 13) = [-150.29,  0.3244]
  cd(0:1, 14) = [-148.00,  0.3740]
  cd(0:1, 15) = [-143.86,  0.4688]
  cd(0:1, 16) = [-139.71,  0.5681]
  cd(0:1, 17) = [-135.57,  0.6700]
  cd(0:1, 18) = [-131.43,  0.7721]
  cd(0:1, 19) = [-127.29,  0.8726]
  cd(0:1, 20) = [-123.14,  0.9692]
  cd(0:1, 21) = [-119.00,  1.0600]
  cd(0:1, 22) = [-114.86,  1.1432]
  cd(0:1, 23) = [-110.71,  1.2169]
  cd(0:1, 24) = [-106.57,  1.2797]
  cd(0:1, 25) = [-102.43,  1.3303]
  cd(0:1, 26) = [ -98.29,  1.3677]
  cd(0:1, 27) = [ -94.14,  1.3911]
  cd(0:1, 28) = [ -90.00,  1.4000]
  cd(0:1, 29) = [ -85.86,  1.3911]
  cd(0:1, 30) = [ -81.71,  1.3677]
  cd(0:1, 31) = [ -77.57,  1.3303]
  cd(0:1, 32) = [ -73.43,  1.2797]
  cd(0:1, 33) = [ -69.29,  1.2169]
  cd(0:1, 34) = [ -65.14,  1.1432]
  cd(0:1, 35) = [ -61.00,  1.0600]
  cd(0:1, 36) = [ -56.86,  0.9692]
  cd(0:1, 37) = [ -52.71,  0.8726]
  cd(0:1, 38) = [ -48.57,  0.7721]
  cd(0:1, 39) = [ -44.43,  0.6700]
  cd(0:1, 40) = [ -40.29,  0.5681]
  cd(0:1, 41) = [ -36.14,  0.4688]
  cd(0:1, 42) = [ -32.00,  0.3740]
  cd(0:1, 43) = [ -28.00,  0.2188]
  cd(0:1, 44) = [ -24.00,  0.1598]
  cd(0:1, 45) = [ -20.00,  0.1074]
  cd(0:1, 46) = [ -18.00,  0.0869]
  cd(0:1, 47) = [ -16.00,  0.0684]
  cd(0:1, 48) = [ -14.00,  0.0473]
  cd(0:1, 49) = [ -12.00,  0.0308]
  cd(0:1, 50) = [ -10.00,  0.0198]
  cd(0:1, 51) = [  -8.00,  0.0144]
  cd(0:1, 52) = [  -6.00,  0.0115]
  cd(0:1, 53) = [  -4.00,  0.0103]
  cd(0:1, 54) = [  -2.00,  0.0098]
  cd(0:1, 55) = [  -1.00,  0.0097]
  cd(0:1, 56) = [   0.00,  0.0097]
  cd(0:1, 57) = [   1.00,  0.0098]
  cd(0:1, 58) = [   2.00,  0.0099]
  cd(0:1, 59) = [   3.00,  0.0102]
  cd(0:1, 60) = [   4.00,  0.0105]
  cd(0:1, 61) = [   5.00,  0.0108]
  cd(0:1, 62) = [   6.00,  0.0114]
  cd(0:1, 63) = [   7.00,  0.0120]
  cd(0:1, 64) = [   8.00,  0.0127]
  cd(0:1, 65) = [   9.00,  0.0135]
  cd(0:1, 66) = [  10.00,  0.0146]
  cd(0:1, 67) = [  11.00,  0.0160]
  cd(0:1, 68) = [  12.00,  0.0178]
  cd(0:1, 69) = [  13.00,  0.0204]
  cd(0:1, 70) = [  14.00,  0.0238]
  cd(0:1, 71) = [  15.00,  0.0324]
  cd(0:1, 72) = [  16.00,  0.0426]
  cd(0:1, 73) = [  18.00,  0.0767]
  cd(0:1, 74) = [  20.00,  0.1191]
  cd(0:1, 75) = [  24.00,  0.2019]
  cd(0:1, 76) = [  28.00,  0.2798]
  cd(0:1, 77) = [  32.00,  0.3740]
  cd(0:1, 78) = [  36.14,  0.4688]
  cd(0:1, 79) = [  40.29,  0.5681]
  cd(0:1, 80) = [  44.43,  0.6700]
  cd(0:1, 81) = [  48.57,  0.7721]
  cd(0:1, 82) = [  52.71,  0.8726]
  cd(0:1, 83) = [  56.86,  0.9692]
  cd(0:1, 84) = [  61.00,  1.0600]
  cd(0:1, 85) = [  65.14,  1.1432]
  cd(0:1, 86) = [  69.29,  1.2169]
  cd(0:1, 87) = [  73.43,  1.2797]
  cd(0:1, 88) = [  77.57,  1.3303]
  cd(0:1, 89) = [  81.71,  1.3677]
  cd(0:1, 90) = [  85.86,  1.3911]
  cd(0:1, 91) = [  90.00,  1.4000]
  cd(0:1, 92) = [  94.14,  1.3911]
  cd(0:1, 93) = [  98.29,  1.3677]
  cd(0:1, 94) = [ 102.43,  1.3303]
  cd(0:1, 95) = [ 106.57,  1.2797]
  cd(0:1, 96) = [ 110.71,  1.2169]
  cd(0:1, 97) = [ 114.86,  1.1432]
  cd(0:1, 98) = [ 119.00,  1.0600]
  cd(0:1, 99) = [ 123.14,  0.9692]
  cd(0:1,100) = [ 127.29,  0.8726]
  cd(0:1,101) = [ 131.43,  0.7721]
  cd(0:1,102) = [ 135.57,  0.6700]
  cd(0:1,103) = [ 139.71,  0.5681]
  cd(0:1,104) = [ 143.86,  0.4688]
  cd(0:1,105) = [ 148.00,  0.3740]
  cd(0:1,106) = [ 150.29,  0.3244]
  cd(0:1,107) = [ 152.57,  0.2771]
  cd(0:1,108) = [ 154.86,  0.2323]
  cd(0:1,109) = [ 157.14,  0.1905]
  cd(0:1,110) = [ 159.43,  0.1518]
  cd(0:1,111) = [ 161.71,  0.1164]
  cd(0:1,112) = [ 164.00,  0.0847]
  cd(0:1,113) = [ 166.29,  0.0568]
  cd(0:1,114) = [ 168.57,  0.0329]
  cd(0:1,115) = [ 170.86,  0.0259]
  cd(0:1,116) = [ 173.14,  0.0213]
  cd(0:1,117) = [ 175.43,  0.0181]
  cd(0:1,118) = [ 177.71,  0.0161]
  cd(0:1,119) = [ 180.00,  0.0155]

  IF ( y .EQ. cd(0,0) ) cd6 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd6 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN 
     cd6 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd6_iea15.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd6 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd6_iea15.pro'
  ENDIF
  
!print,y,cd6
  
!_________________________________________________________________________
  
END FUNCTION calc_cd6_iea15
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd7_iea15( y ) RESULT( cd7 )
!_________________________________________________________________________
!
! Data from  IEA 15 MW: FFA-W3-241
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 120

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd7
  INTEGER :: i

  cd7 = 0.0

!                alpha    CD  
  cd(0:1,  0) = [-180.00,  0.0118]
  cd(0:1,  1) = [-177.71,  0.0125]
  cd(0:1,  2) = [-175.43,  0.0146]
  cd(0:1,  3) = [-173.14,  0.0181]
  cd(0:1,  4) = [-170.86,  0.0230]
  cd(0:1,  5) = [-168.57,  0.0292]
  cd(0:1,  6) = [-166.29,  0.0538]
  cd(0:1,  7) = [-164.00,  0.0838]
  cd(0:1,  8) = [-161.71,  0.1179]
  cd(0:1,  9) = [-159.43,  0.1558]
  cd(0:1, 10) = [-157.14,  0.1974]
  cd(0:1, 11) = [-154.86,  0.2424]
  cd(0:1, 12) = [-152.57,  0.2904]
  cd(0:1, 13) = [-150.29,  0.3413]
  cd(0:1, 14) = [-148.00,  0.3946]
  cd(0:1, 15) = [-143.86,  0.4965]
  cd(0:1, 16) = [-139.71,  0.6032]
  cd(0:1, 17) = [-135.57,  0.7126]
  cd(0:1, 18) = [-131.43,  0.8225]
  cd(0:1, 19) = [-127.29,  0.9305]
  cd(0:1, 20) = [-123.14,  1.0345]
  cd(0:1, 21) = [-119.00,  1.1322]
  cd(0:1, 22) = [-114.86,  1.2218]
  cd(0:1, 23) = [-110.71,  1.3012]
  cd(0:1, 24) = [-106.57,  1.3690]
  cd(0:1, 25) = [-102.43,  1.4238]
  cd(0:1, 26) = [ -98.29,  1.4643]
  cd(0:1, 27) = [ -94.14,  1.4899]
  cd(0:1, 28) = [ -90.00,  1.5000]
  cd(0:1, 29) = [ -85.86,  1.4899]
  cd(0:1, 30) = [ -81.71,  1.4643]
  cd(0:1, 31) = [ -77.57,  1.4238]
  cd(0:1, 32) = [ -73.43,  1.3690]
  cd(0:1, 33) = [ -69.29,  1.3012]
  cd(0:1, 34) = [ -65.14,  1.2218]
  cd(0:1, 35) = [ -61.00,  1.1322]
  cd(0:1, 36) = [ -56.86,  1.0345]
  cd(0:1, 37) = [ -52.71,  0.9305]
  cd(0:1, 38) = [ -48.57,  0.8225]
  cd(0:1, 39) = [ -44.43,  0.7126]
  cd(0:1, 40) = [ -40.29,  0.6032]
  cd(0:1, 41) = [ -36.14,  0.4965]
  cd(0:1, 42) = [ -32.00,  0.3946]
  cd(0:1, 43) = [ -28.00,  0.2225]
  cd(0:1, 44) = [ -24.00,  0.1516]
  cd(0:1, 45) = [ -20.00,  0.0970]
  cd(0:1, 46) = [ -18.00,  0.0774]
  cd(0:1, 47) = [ -16.00,  0.0612]
  cd(0:1, 48) = [ -14.00,  0.0467]
  cd(0:1, 49) = [ -12.00,  0.0330]
  cd(0:1, 50) = [ -10.00,  0.0203]
  cd(0:1, 51) = [  -8.00,  0.0117]
  cd(0:1, 52) = [  -6.00,  0.0092]
  cd(0:1, 53) = [  -4.00,  0.0084]
  cd(0:1, 54) = [  -2.00,  0.0081]
  cd(0:1, 55) = [  -1.00,  0.0081]
  cd(0:1, 56) = [   0.00,  0.0081]
  cd(0:1, 57) = [   1.00,  0.0082]
  cd(0:1, 58) = [   2.00,  0.0084]
  cd(0:1, 59) = [   3.00,  0.0087]
  cd(0:1, 60) = [   4.00,  0.0090]
  cd(0:1, 61) = [   5.00,  0.0094]
  cd(0:1, 62) = [   6.00,  0.0100]
  cd(0:1, 63) = [   7.00,  0.0107]
  cd(0:1, 64) = [   8.00,  0.0115]
  cd(0:1, 65) = [   9.00,  0.0127]
  cd(0:1, 66) = [  10.00,  0.0140]
  cd(0:1, 67) = [  11.00,  0.0155]
  cd(0:1, 68) = [  12.00,  0.0172]
  cd(0:1, 69) = [  13.00,  0.0196]
  cd(0:1, 70) = [  14.00,  0.0229]
  cd(0:1, 71) = [  15.00,  0.0280]
  cd(0:1, 72) = [  16.00,  0.0361]
  cd(0:1, 73) = [  18.00,  0.0653]
  cd(0:1, 74) = [  20.00,  0.1046]
  cd(0:1, 75) = [  24.00,  0.1915]
  cd(0:1, 76) = [  28.00,  0.2863]
  cd(0:1, 77) = [  32.00,  0.3946]
  cd(0:1, 78) = [  36.14,  0.4965]
  cd(0:1, 79) = [  40.29,  0.6032]
  cd(0:1, 80) = [  44.43,  0.7126]
  cd(0:1, 81) = [  48.57,  0.8225]
  cd(0:1, 82) = [  52.71,  0.9305]
  cd(0:1, 83) = [  56.86,  1.0345]
  cd(0:1, 84) = [  61.00,  1.1322]
  cd(0:1, 85) = [  65.14,  1.2218]
  cd(0:1, 86) = [  69.29,  1.3012]
  cd(0:1, 87) = [  73.43,  1.3690]
  cd(0:1, 88) = [  77.57,  1.4238]
  cd(0:1, 89) = [  81.71,  1.4643]
  cd(0:1, 90) = [  85.86,  1.4899]
  cd(0:1, 91) = [  90.00,  1.5000]
  cd(0:1, 92) = [  94.14,  1.4899]
  cd(0:1, 93) = [  98.29,  1.4643]
  cd(0:1, 94) = [ 102.43,  1.4238]
  cd(0:1, 95) = [ 106.57,  1.3690]
  cd(0:1, 96) = [ 110.71,  1.3012]
  cd(0:1, 97) = [ 114.86,  1.2218]
  cd(0:1, 98) = [ 119.00,  1.1322]
  cd(0:1, 99) = [ 123.14,  1.0345]
  cd(0:1,100) = [ 127.29,  0.9305]
  cd(0:1,101) = [ 131.43,  0.8225]
  cd(0:1,102) = [ 135.57,  0.7126]
  cd(0:1,103) = [ 139.71,  0.6032]
  cd(0:1,104) = [ 143.86,  0.4965]
  cd(0:1,105) = [ 148.00,  0.3946]
  cd(0:1,106) = [ 150.29,  0.3413]
  cd(0:1,107) = [ 152.57,  0.2904]
  cd(0:1,108) = [ 154.86,  0.2424]
  cd(0:1,109) = [ 157.14,  0.1974]
  cd(0:1,110) = [ 159.43,  0.1558]
  cd(0:1,111) = [ 161.71,  0.1179]
  cd(0:1,112) = [ 164.00,  0.0838]
  cd(0:1,113) = [ 166.29,  0.0538]
  cd(0:1,114) = [ 168.57,  0.0292]
  cd(0:1,115) = [ 170.86,  0.0230]
  cd(0:1,116) = [ 173.14,  0.0181]
  cd(0:1,117) = [ 175.43,  0.0146]
  cd(0:1,118) = [ 177.71,  0.0125]
  cd(0:1,119) = [ 180.00,  0.0118]

  
  IF ( y .EQ. cd(0,0) ) cd7 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd7 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN 
     cd7 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd7_iea15.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd7 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd7_iea15.pro'
  ENDIF
  
!print,y,cd7
  
!_________________________________________________________________________
  
END FUNCTION calc_cd7_iea15
!_________________________________________________________________________

!=========================================================================

Function calc_cd8_iea15( y ) RESULT( cd8 )
!_________________________________________________________________________
!
! Data from IEA 15 MW: FFA-W3-211
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 120

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd8
  INTEGER :: i

  cd8 = 0.0

!              alpha    CD  
  cd(0:1,  0) = [-180.00,  0.0246]
  cd(0:1,  1) = [-177.71,  0.0253]
  cd(0:1,  2) = [-175.43,  0.0274]
  cd(0:1,  3) = [-173.14,  0.0309]
  cd(0:1,  4) = [-170.86,  0.0357]
  cd(0:1,  5) = [-168.57,  0.0560]
  cd(0:1,  6) = [-166.29,  0.0814]
  cd(0:1,  7) = [-164.00,  0.1111]
  cd(0:1,  8) = [-161.71,  0.1449]
  cd(0:1,  9) = [-159.43,  0.1824]
  cd(0:1, 10) = [-157.14,  0.2236]
  cd(0:1, 11) = [-154.86,  0.2681]
  cd(0:1, 12) = [-152.57,  0.3157]
  cd(0:1, 13) = [-150.29,  0.3660]
  cd(0:1, 14) = [-148.00,  0.4187]
  cd(0:1, 15) = [-143.86,  0.5194]
  cd(0:1, 16) = [-139.71,  0.6249]
  cd(0:1, 17) = [-135.57,  0.7329]
  cd(0:1, 18) = [-131.43,  0.8413]
  cd(0:1, 19) = [-127.29,  0.9477]
  cd(0:1, 20) = [-123.14,  1.0500]
  cd(0:1, 21) = [-119.00,  1.1460]
  cd(0:1, 22) = [-114.86,  1.2337]
  cd(0:1, 23) = [-110.71,  1.3113]
  cd(0:1, 24) = [-106.57,  1.3771]
  cd(0:1, 25) = [-102.43,  1.4299]
  cd(0:1, 26) = [ -98.29,  1.4684]
  cd(0:1, 27) = [ -94.14,  1.4920]
  cd(0:1, 28) = [ -90.00,  1.5000]
  cd(0:1, 29) = [ -85.86,  1.4920]
  cd(0:1, 30) = [ -81.71,  1.4684]
  cd(0:1, 31) = [ -77.57,  1.4299]
  cd(0:1, 32) = [ -73.43,  1.3771]
  cd(0:1, 33) = [ -69.29,  1.3113]
  cd(0:1, 34) = [ -65.14,  1.2337]
  cd(0:1, 35) = [ -61.00,  1.1460]
  cd(0:1, 36) = [ -56.86,  1.0500]
  cd(0:1, 37) = [ -52.71,  0.9477]
  cd(0:1, 38) = [ -48.57,  0.8413]
  cd(0:1, 39) = [ -44.43,  0.7329]
  cd(0:1, 40) = [ -40.29,  0.6249]
  cd(0:1, 41) = [ -36.14,  0.5194]
  cd(0:1, 42) = [ -32.00,  0.4187]
  cd(0:1, 43) = [ -28.00,  0.2869]
  cd(0:1, 44) = [ -24.00,  0.1396]
  cd(0:1, 45) = [ -20.00,  0.0834]
  cd(0:1, 46) = [ -18.00,  0.0651]
  cd(0:1, 47) = [ -16.00,  0.0489]
  cd(0:1, 48) = [ -14.00,  0.0342]
  cd(0:1, 49) = [ -12.00,  0.0213]
  cd(0:1, 50) = [ -10.00,  0.0139]
  cd(0:1, 51) = [  -8.00,  0.0108]
  cd(0:1, 52) = [  -6.00,  0.0088]
  cd(0:1, 53) = [  -4.00,  0.0070]
  cd(0:1, 54) = [  -2.00,  0.0066]
  cd(0:1, 55) = [  -1.00,  0.0066]
  cd(0:1, 56) = [   0.00,  0.0067]
  cd(0:1, 57) = [   1.00,  0.0068]
  cd(0:1, 58) = [   2.00,  0.0070]
  cd(0:1, 59) = [   3.00,  0.0072]
  cd(0:1, 60) = [   4.00,  0.0075]
  cd(0:1, 61) = [   5.00,  0.0080]
  cd(0:1, 62) = [   6.00,  0.0087]
  cd(0:1, 63) = [   7.00,  0.0097]
  cd(0:1, 64) = [   8.00,  0.0110]
  cd(0:1, 65) = [   9.00,  0.0123]
  cd(0:1, 66) = [  10.00,  0.0137]
  cd(0:1, 67) = [  11.00,  0.0153]
  cd(0:1, 68) = [  12.00,  0.0172]
  cd(0:1, 69) = [  13.00,  0.0197]
  cd(0:1, 70) = [  14.00,  0.0237]
  cd(0:1, 71) = [  15.00,  0.0309]
  cd(0:1, 72) = [  16.00,  0.0430]
  cd(0:1, 73) = [  18.00,  0.0773]
  cd(0:1, 74) = [  20.00,  0.1120]
  cd(0:1, 75) = [  24.00,  0.1841]
  cd(0:1, 76) = [  28.00,  0.2759]
  cd(0:1, 77) = [  32.00,  0.4187]
  cd(0:1, 78) = [  36.14,  0.5194]
  cd(0:1, 79) = [  40.29,  0.6249]
  cd(0:1, 80) = [  44.43,  0.7329]
  cd(0:1, 81) = [  48.57,  0.8413]
  cd(0:1, 82) = [  52.71,  0.9477]
  cd(0:1, 83) = [  56.86,  1.0500]
  cd(0:1, 84) = [  61.00,  1.1460]
  cd(0:1, 85) = [  65.14,  1.2337]
  cd(0:1, 86) = [  69.29,  1.3113]
  cd(0:1, 87) = [  73.43,  1.3771]
  cd(0:1, 88) = [  77.57,  1.4299]
  cd(0:1, 89) = [  81.71,  1.4684]
  cd(0:1, 90) = [  85.86,  1.4920]
  cd(0:1, 91) = [  90.00,  1.5000]
  cd(0:1, 92) = [  94.14,  1.4920]
  cd(0:1, 93) = [  98.29,  1.4684]
  cd(0:1, 94) = [ 102.43,  1.4299]
  cd(0:1, 95) = [ 106.57,  1.3771]
  cd(0:1, 96) = [ 110.71,  1.3113]
  cd(0:1, 97) = [ 114.86,  1.2337]
  cd(0:1, 98) = [ 119.00,  1.1460]
  cd(0:1, 99) = [ 123.14,  1.0500]
  cd(0:1,100) = [ 127.29,  0.9477]
  cd(0:1,101) = [ 131.43,  0.8413]
  cd(0:1,102) = [ 135.57,  0.7329]
  cd(0:1,103) = [ 139.71,  0.6249]
  cd(0:1,104) = [ 143.86,  0.5194]
  cd(0:1,105) = [ 148.00,  0.4187]
  cd(0:1,106) = [ 150.29,  0.3660]
  cd(0:1,107) = [ 152.57,  0.3157]
  cd(0:1,108) = [ 154.86,  0.2681]
  cd(0:1,109) = [ 157.14,  0.2236]
  cd(0:1,110) = [ 159.43,  0.1824]
  cd(0:1,111) = [ 161.71,  0.1449]
  cd(0:1,112) = [ 164.00,  0.1111]
  cd(0:1,113) = [ 166.29,  0.0814]
  cd(0:1,114) = [ 168.57,  0.0560]
  cd(0:1,115) = [ 170.86,  0.0357]
  cd(0:1,116) = [ 173.14,  0.0309]
  cd(0:1,117) = [ 175.43,  0.0274]
  cd(0:1,118) = [ 177.71,  0.0253]
  cd(0:1,119) = [ 180.00,  0.0246]

  IF ( y .EQ. cd(0,0) ) cd8 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd8 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE
  
  IF ( y .LT. cd(0,0) ) THEN 
     cd8 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd8_iea15.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd8 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd8_iea15.pro'
  ENDIF
  
!print,y,cd8
  
!_________________________________________________________________________
  
END FUNCTION calc_cd8_iea15
!_________________________________________________________________________

END MODULE module_gad_iea15

