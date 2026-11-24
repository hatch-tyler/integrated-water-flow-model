!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2026  
!  State of California, Department of Water Resources 
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!  (http://www.gnu.org/copyleft/gpl.html)
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!
!  For tecnical support, e-mail: IWFMtechsupport@water.ca.gov 
!***********************************************************************

! Marsaglia & Tsang generator for random normals & random exponentials.
! Translated from C by Alan Miller (amiller@bigpond.net.au)

! Marsaglia, G. & Tsang, W.W. (2000) `The ziggurat method for generating
! random variables', J. Statist. Software, v5(8).

! This is an electronic journal which can be downloaded from:
! http://www.jstatsoft.org/v05/i08

! N.B. It is assumed that all integer(int32)s are 32-bit.
! N.B. The value of M2 has been halved to compensate for the lack of
!      unsigned integer(int32)s in Fortran.

! Latest version - 1 January 2001

! Parallel version - October 2006

! This version has been customised for parallel processing use,
! specifically with OpenMP.  Each thread uses its own pseudo-random 
! sequence. (Gib Bogle)

! Changed to subroutines, cleaned tab characters, 
! unified to lower case, new interface (Vladimir Fuka) -  May 2014

! Changed the underlying integer generator from 32-bit SHR3 to
! 64-bit xoroshiro128+ (David Blackman and Sebastiano Vigna. 
!  Scrambled linear pseudorandom number generators, 2018) translated from C
! from http://xoshiro.di.unimi.it/ . The advantage is faster generating, 
! longer independent series and jump functions available for seeding 
! parallel sequences. Generation of uniform floating point numbers
! now using better and faster method from 
! https://experilous.com/1/blog/post/perfect-fast-random-floating-point-numbers
! Also unified all pre-fixes to rng_. 
! The code does not assume any default integer and can be compiled with
! larger default integer kinds.
! (Vladimir Fuka) - June 2018

!--------------------------------------------------------------------------

MODULE rng_int_to_real
 USE iso_fortran_env,              ONLY: int32, int64, real32, real64
  
 USE MessageLogger               , ONLY: LogMessage                      , &
                                            SetLastMessage                  , &
                                            EchoProgress                    , &
                                            IsLogFileDefined                , &
                                            MessageArray                    , &
                                            f_iFILE                         , &
                                            f_iMessage                      , &
                                            f_iFatal                        , &
                                            f_iWarn                         , &
                                            f_iInfo                         



CONTAINS

  ! Generate real numbers from interval [0, 1) from random integers
  ! Translated from C rand_float_co() and rand_float_co() from
  !! https:--experilous.com/1/blog/post/perfect-fast-random-floating-point-numbers#half-open-range
  
  FUNCTION rng_int32_to_real32(i) RESULT(res)
    REAL (real32) :: res
    INTEGER(int32), value :: i

    i  = IOR (INT(Z'3F800000',int32), SHIFTR(i, 9))
    res = TRANSFER(i, 1.0_real32) - 1;
  END FUNCTION
 
  FUNCTION rng_int64_to_real32(i) RESULT(res)
    REAL(real32) :: res
    INTEGER(int64), value :: i
    INTEGER(int32) :: tmp

    tmp = TRANSFER(SHIFTR(i,32), tmp)
    tmp  = ior(INT(Z'3F800000',int32), SHIFTR(tmp, 9))
    res = TRANSFER(tmp, 1.0_real32) - 1;
  END FUNCTION
 
  FUNCTION rng_int64_to_real64(i) RESULT(res)
    REAL(real64) :: res
    INTEGER(int64), value :: i
    !integer(int32) :: tmp
    
    i  = ior(INT(Z'3FF0000000000000',int64), SHIFTR(i, 12))
    res = TRANSFER(i, 1.0_real64) - 1;
  END FUNCTION
 
END MODULE

MODULE rng_par_zig

  USE iso_fortran_env, ONLY: int32, int64, real32, real64
  USE rng_int_to_real


  IMPLICIT NONE

  PRIVATE

  PUBLIC               :: rng_init, rng_uni, rng_norm, rng_exp, rng_jump
  
  INTEGER,  PARAMETER  :: sp = real32, dp = real64
  
  REAL(dp), PARAMETER  :: m1=2147483648.0_dp,   m2=2147483648.0_dp,       &
                          half=0.5_dp
  REAL(dp)             :: dn0=3.442619855899_dp, tn0=3.442619855899_dp,   &
                          vn=0.00991256303526217_dp,                      &
                          q,                    de0=7.697117470131487_dp, &
                          te0=7.697117470131487_dp,                       &
                          ve=0.003949659822581572_dp

  INTEGER, SAVE                     :: rng_n = 0, rng_step
  INTEGER(int32), ALLOCATABLE, save ::  rng_kn(:,:), rng_ke(:,:)
  INTEGER(int64), ALLOCATABLE, save ::  rng_jsr(:)
  REAL(dp), ALLOCATABLE, save       :: rng_wn(:,:), rng_fn(:,:), rng_we(:,:), rng_fe(:,:)
  


  INTERFACE rng_uni
    MODULE PROCEDURE rng_uni
    MODULE PROCEDURE rng_uni_32
    MODULE PROCEDURE rng_uni_ser
    MODULE PROCEDURE rng_uni_32_ser
  END INTERFACE

  INTERFACE rng_norm
    MODULE PROCEDURE rng_rnor
    MODULE PROCEDURE rng_rnor_32
    MODULE PROCEDURE rng_rnor_ser
    MODULE PROCEDURE rng_rnor_32_ser
  END INTERFACE

  INTERFACE rng_exp
    MODULE PROCEDURE rng_rexp
    MODULE PROCEDURE rng_rexp_32
    MODULE PROCEDURE rng_rexp_ser
    MODULE PROCEDURE rng_rexp_32_ser
  END INTERFACE
  
  INTERFACE rng_init
    MODULE PROCEDURE rng_zigset
  END INTERFACE
  
  INTERFACE rng_xoroshiro128plus
    MODULE PROCEDURE rng_xoroshiro128plus
    MODULE PROCEDURE rng_xoroshiro128plus_kpar
    MODULE PROCEDURE rng_xoroshiro128plus_kpar_32
  END INTERFACE
  
  INTERFACE rng_jump
    MODULE PROCEDURE rng_jump
    MODULE PROCEDURE rng_jump_n
  END INTERFACE

  ! Define if signed integer addition must not overflow.
  ! Unsigned integer addition can only be done in C.
!#ifdef STRICT_INTEGER_OVERFLOW    
!  interface 
!  function sum_and_overflow(a, b) result(res) bind(C, name="sum_and_overflow")
!    use, intrinsic :: iso_c_binding
!    integer(c_int32_t) :: res
!    integer(c_int32_t), value :: a, b
!  end function
!  end interface
!#endif    
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 26
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Random Number Generator::'



CONTAINS


  SUBROUTINE rng_zigset( npar, rng_jsrseed, grainsize)

    INTEGER, INTENT(IN)  :: npar
    INTEGER(int64), INTENT(IN)  :: rng_jsrseed(2)
    INTEGER, INTENT(IN), OPTIONAL  :: grainsize

    INTEGER(int64) :: s(2)
    INTEGER :: i, kpar
    REAL(dp) dn, tn, de, te

    rng_n = npar

    IF (PRESENT(grainsize)) THEN
      rng_step = grainsize
    ELSE
      rng_step = 32
    END IF

    ! First we need to allocate all the non-volatile arrays with the size npar
    ALLOCATE(rng_jsr(0:npar*rng_step))
    ALLOCATE(rng_kn(0:127,0:npar-1))
    ALLOCATE(rng_ke(0:255,0:npar-1))
    ALLOCATE(rng_wn(0:127,0:npar-1))
    ALLOCATE(rng_fn(0:127,0:npar-1))
    ALLOCATE(rng_we(0:255,0:npar-1))
    ALLOCATE(rng_fe(0:255,0:npar-1))
    
    s = rng_jsrseed

    ! Now treat each instance separately
    DO  kpar = 0,npar-1
      !  Set the seed
      rng_jsr(kpar*rng_step:kpar*rng_step+1) = s
      
      ! The next instance's seed is generated by the jump function for xoroshiro128+
      CALL  rng_jump(s)

      !  Tables for RNOR
      dn = dn0
      tn = tn0
      q = vn*EXP(half*dn*dn)
      rng_kn(0,kpar) = INT((dn/q)*m1, int32)
      rng_kn(1,kpar) = 0
      rng_wn(0,kpar) = q/m1
      rng_wn(127,kpar) = dn/m1
      rng_fn(0,kpar) = 1.0_dp
      rng_fn(127,kpar) = EXP( -half*dn*dn )
      DO  i = 126, 1, -1
          dn = SQRT( -2.0_dp * LOG( vn/dn + EXP( -half*dn*dn ) ) )       ! dn
          rng_kn(i+1,kpar) = INT((dn/tn)*m1, int32)
          tn = dn                                                        ! tn
          rng_fn(i,kpar) = EXP(-half*dn*dn)
          rng_wn(i,kpar) = dn/m1
      END DO

      !  Tables for Rexp
      de = de0
      te = te0
      q = ve*EXP( de )
      rng_ke(0,kpar) = INT((de/q)*m2, int32)
      rng_ke(1,kpar) = 0
      rng_we(0,kpar) = q/m2
      rng_we(255,kpar) = de/m2
      rng_fe(0,kpar) = 1.0_dp
      rng_fe(255,kpar) = EXP( -de )
      DO  i = 254, 1, -1
          de = -LOG( ve/de + EXP( -de ) )                                ! de
          rng_ke(i+1,kpar) = INT(m2 * (de/te), int32)
          te = de                                                        ! te
          rng_fe(i,kpar) = EXP( -de )
          rng_we(i,kpar) = de/m2
      END DO
    END DO
  END SUBROUTINE rng_zigset


  
  ! Generate random 64-bit integers
  SUBROUTINE rng_xoroshiro128plus_kpar(ival, kpar)
    INTEGER(int64), INTENT(OUT)   :: ival
    INTEGER, INTENT(IN) :: kpar
    
    CALL rng_xoroshiro128plus(ival, rng_jsr(kpar*rng_step:kpar*rng_step+1))
  END SUBROUTINE

  ! Generate random 32-bit integers by taking the *higher* 32-bits of the result
  SUBROUTINE rng_xoroshiro128plus_kpar_32(ival, kpar)
    INTEGER(int32), INTENT(OUT)   :: ival
    INTEGER, INTENT(IN) :: kpar
    INTEGER(int64) :: tmp
        
    CALL rng_xoroshiro128plus(tmp, rng_jsr(kpar*rng_step:kpar*rng_step+1))
    
    ival = TRANSFER(SHIFTR(tmp,32), ival)
  END SUBROUTINE

  ! Generate random 64-bit integers
  SUBROUTINE rng_xoroshiro128plus(ival, s) 
    INTEGER(int64), INTENT(OUT)   :: ival
    INTEGER(int64), INTENT(INOUT) :: s(2)
    INTEGER(int64) :: s1, s2
    
    s1 = s(1)
    s2 = s(2)
    
!#ifdef STRICT_INTEGER_OVERFLOW    
!    ival = sum_and_overflow(s1, s2)
!#else
    ival = s1 + s2
!#endif
    s2 = IEOR(s2, s1)
    s1 = IEOR( IEOR(rotl(s1, 24), s2), SHIFTL(s2, 16))
    s2 = rotl(s2, 37)
 
    s(1) = s1
    s(2) = s2

  CONTAINS
    FUNCTION rotl(x, k)
      INTEGER(int64) :: rotl
      INTEGER(int64) :: x
      INTEGER :: k
    
      rotl = IOR( SHIFTL(x, k), SHIFTR(x, 64-k))
    END FUNCTION
  END SUBROUTINE

  ! Jump by 2^64 xoroshiro128plus CALLs
  SUBROUTINE rng_jump(s)
    INTEGER(int64), INTENT(INOUT) :: s(2)
    ! The first constant is df900294d8f554a5 using not() to avoid overflow
    INTEGER(int64), PARAMETER :: jmp(2) = [ NOT(INT(Z'206ffd6b270aab5a', int64)), &
                                            INT(Z'170865df4b3201fc', int64)]
    INTEGER(int64) :: s1, s2, dummy
    
    INTEGER :: i, b
    
    s1 = 0
    s2 = 0
    
    DO i = 1, SIZE(jmp)
      DO b = 0, 63
        IF (IAND(jmp(i), SHIFTL(1_int64,b))/=0) THEN
          s1 = IEOR(s1, s(1))
          s2 = IEOR(s2, s(2))
        END if  
        CALL rng_xoroshiro128plus(dummy, s)
      END DO
    END DO
    s(1) = s1
    s(2) = s2
  END SUBROUTINE
  
  ! DO n jumps defined above
  SUBROUTINE rng_jump_n(s, n)
    INTEGER(int64), INTENT(INOUT) :: s(2)
    INTEGER, INTENT(IN) :: n
    INTEGER :: i
    
    DO i = 1, n
      CALL rng_jump(s)
    END DO
  END SUBROUTINE

  !  Generate uniformly distributed random numbers, sequence kpar
  SUBROUTINE rng_uni(fn_val, kpar, iStat)
    INTEGER :: kpar
    REAL(dp), INTENT(OUT) ::  fn_val
    INTEGER,  INTENT(OUT) :: iStat
    INTEGER(int64) :: x
    CHARACTER        :: text1, text2
    CHARACTER(LEN=ModNameLen+13) :: ThisProcedure = ModName // 'RandomNumberGeneratorZig'

    iStat=0
    
    IF (kpar >= rng_n) THEN
        WRITE (text1, '(I10)')  kpar
        WRITE (text2, '(I10)')  rng_n-1
        CALL SetLastMessage('RNG Error: thread number '//text1//' exceeds initialized max:'//text2//'!',f_iFatal,ThisProcedure)
        CALL SetLastMessage('or RNG not initialized.',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    CALL rng_xoroshiro128plus(x, kpar)
    
    fn_val = rng_int64_to_real64(x)
  END SUBROUTINE rng_uni



  !  Generate uniformly distributed random numbers, sequence kpar
  SUBROUTINE rng_uni_32(fn_val, kpar, iStat)
    INTEGER :: kpar
    INTEGER, INTENT(OUT) :: iStat
    REAL(sp), INTENT(OUT) ::  fn_val
    INTEGER(int64) :: x
    
    CHARACTER       :: text1, text2
    CHARACTER(LEN=ModNameLen+13) :: ThisProcedure = ModName // 'RandomNumberGeneratorZig'

    iStat=0
    IF (kpar >= rng_n) THEN
        WRITE (text1, '(I10)')  kpar
        WRITE (text2, '(I10)')  rng_n-1
        CALL SetLastMessage('RNG Error: thread number '//text1//' exceeds initialized max:'//text2//'!',f_iFatal,ThisProcedure)
        CALL SetLastMessage('or RNG not initialized.',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    CALL rng_xoroshiro128plus(x, kpar)
    
    fn_val = rng_int64_to_real32(x)
  END SUBROUTINE rng_uni_32



  !  Generate random normals, sequence kpar
  SUBROUTINE rng_rnor(fn_val, kpar,  iStat)
    REAL(dp), INTENT(OUT) ::  fn_val
    INTEGER, INTENT(OUT) :: iStat
    INTEGER :: kpar

    REAL(dp), PARAMETER  ::  r = 3.442620_dp
    REAL(dp)             ::  x, y, z
    INTEGER(int32) :: iz, hz
    CHARACTER        :: text1, text2
    CHARACTER(LEN=ModNameLen+13) :: ThisProcedure = ModName // 'RandomNumberGeneratorZig'


    iStat=0
    IF (kpar >= rng_n) THEN
        WRITE (text1, '(I10)')  kpar
        WRITE (text2, '(I10)')  rng_n-1
        CALL SetLastMessage('RNG Error: thread number '//text1//' exceeds initialized max:'//text2//'!',f_iFatal,ThisProcedure)
        CALL SetLastMessage('or RNG not initialized.',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
        
   END IF
    
    CALL rng_xoroshiro128plus(hz, kpar)
    iz = iand( hz, 127 )
    
    IF( ABS( hz ) < rng_kn(iz,kpar) ) THEN
        fn_val = hz * rng_wn(iz,kpar)
    else
        DO
          IF( iz == 0 ) THEN
              DO
                CALL rng_uni(z, kpar,  iStat)
                x = -0.2904764_dp * LOG( z )
                CALL rng_uni(z, kpar,  iStat)
                y = -LOG( z )
                IF( y+y >= x*x ) EXIT
              END DO
              fn_val = r+x
              IF( hz <= 0 ) fn_val = -fn_val
              RETURN
          END IF
          
          x = hz * rng_wn(iz,kpar)
          
          CALL rng_uni(z, kpar,  iStat)
          IF( rng_fn(iz,kpar) + z*(rng_fn(iz-1,kpar)-rng_fn(iz,kpar)) < EXP(-half*x*x) ) THEN
              fn_val = x
              RETURN
          END IF
          
          CALL rng_xoroshiro128plus(hz, kpar)
          iz = iand( hz, 127 )
          IF( ABS( hz ) < rng_kn(iz,kpar) ) THEN
              fn_val = hz * rng_wn(iz,kpar)
              RETURN
          END IF
        END do
    END IF
  END SUBROUTINE rng_rnor



  !  Generate random exponentials, sequence kpar
  SUBROUTINE rng_rexp(fn_val, kpar ,  iStat)
    REAL(dp), INTENT(OUT)  ::  fn_val
    INTEGER, INTENT (OUT)  ::  iStat
    INTEGER                                :: kpar

    REAL(dp)                               ::  x, y
    INTEGER(int32)                  :: iz, jz
    CHARACTER                         ::  text1, text2
    CHARACTER(LEN=ModNameLen+13) :: ThisProcedure = ModName // 'RandomNumberGeneratorZig'


    iStat=0
    
    IF (kpar >= rng_n) THEN
        WRITE (text1, '(I10)')  kpar
        WRITE (text2, '(I10)')  rng_n-1
        CALL SetLastMessage('RNG Error: thread number '//text1//' exceeds initialized max:'//text2//'!',f_iFatal,ThisProcedure)
        CALL SetLastMessage('or RNG not initialized.',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN

    END IF

    CALL rng_xoroshiro128plus(jz, kpar)
    iz = iand( jz, 255 )
    IF( ABS( jz ) < rng_ke(iz,kpar) ) THEN
        fn_val = ABS(jz) * rng_we(iz,kpar)
        RETURN
    END IF

    DO
        IF( iz == 0 ) THEN
          CALL rng_uni(y, kpar, iStat)
          fn_val = 7.69711_dp - LOG( y )
          RETURN
        END IF
        x = ABS( jz ) * rng_we(iz,kpar)
        
        CALL rng_uni(y, kpar, iStat)
        
        IF( rng_fe(iz,kpar) + y * (rng_fe(iz-1,kpar) - rng_fe(iz,kpar)) < EXP( -x ) ) THEN
          fn_val = x
          RETURN
        END IF
        
        CALL rng_xoroshiro128plus(jz, kpar)
        iz = iand( jz, 255 )
        IF( ABS( jz ) < rng_ke(iz,kpar) ) THEN
          fn_val = ABS( jz ) * rng_we(iz,kpar)
          RETURN
        END IF
    END DO
  END SUBROUTINE rng_rexp
  
  SUBROUTINE rng_rnor_32(x, kpar, iStat)
    REAL(sp), INTENT(OUT) :: x
    INTEGER :: kpar
    INTEGER, INTENT(OUT) :: iStat
    REAL(dp) :: x64
    
    iStat=0
    CALL rng_rnor(x64, kpar, iStat)
    x = REAL(x64, sp)
  END SUBROUTINE
  
  SUBROUTINE rng_rexp_32(x, kpar, iStat)
    REAL(sp), INTENT(OUT) :: x
    INTEGER :: kpar
    INTEGER, INTENT(OUT):: iStat
    REAL(dp) :: x64
    
    iStat=0
    CALL rng_rexp(x64, kpar, iStat)
    x = REAL(x64, sp)
  END SUBROUTINE
  
  SUBROUTINE rng_uni_ser(x,  iStat)
    REAL(dp), INTENT(OUT) :: x
    INTEGER, INTENT(OUT) :: iStat

    CALL rng_uni(x, 0, iStat)
  END SUBROUTINE
  
  SUBROUTINE rng_rnor_ser(x, iStat)
    REAL(dp), INTENT(OUT) :: x
    INTEGER, INTENT(OUT)  :: iStat
    
    iStat=0
    CALL rng_rnor(x, 0, iStat)
  END SUBROUTINE
  
  SUBROUTINE rng_rexp_ser(x, iStat)
    REAL(dp), INTENT(OUT) :: x
    INTEGER, INTENT(OUT):: iStat
    iStat=0
    call rng_rexp(x, 0, iStat)
  END SUBROUTINE
  
  SUBROUTINE rng_uni_32_ser(x,  iStat)
    REAL(sp), INTENT(OUT) :: x
    INTEGER, INTENT (OUT) :: iStat
    REAL(dp) :: x64
    iStat=0
    CALL rng_uni(x64, 0, iStat)
    x = REAL(x64, sp)
  END SUBROUTINE
  
  SUBROUTINE rng_rnor_32_ser(x, iStat)
    REAL(sp), INTENT(OUT) :: x
    REAL(dp) :: x64
    INTEGER, INTENT(OUT):: iStat
        
    iStat=0
    CALL rng_rnor(x64, 0, iStat)
    x = REAL(x64, sp)
  END SUBROUTINE
  
  SUBROUTINE rng_rexp_32_ser(x, iStat)
    REAL(sp), INTENT(OUT) :: x
    INTEGER, INTENT(OUT):: iStat
    REAL(dp) :: x64
    
    iStat=0
    CALL rng_rexp(x64, 0, iStat)
    x = REAL(x64, sp)
  END SUBROUTINE
  
END MODULE rng_par_zig