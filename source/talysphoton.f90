subroutine talysphoton
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose   : Read gamma decay scheme from TALYS
!
! Author    : Arjan Koning
!
! 2021-12-30: Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Use data from other modules
!
  use A0_tefal_mod
!
! All global variables
!   numlevels      ! maximum number of discrete levels
! Constants
!   parsym         ! symbol of particle
! Variables for photon production in ENDF format
!   branchlevel    ! level to which branching takes place
!   branchratio    ! branch ratio
!   Egamdis        ! energy of gamma ray
!   Nbranch        ! number of branches for level
!   Ngamdis        ! number of gamma ray lines per level
!   nlev           ! number of excited levels for nucleus
!   yieldg         ! total discrete gamma yield per level
!   yieldratio     ! yield ratio for level
!
! *** Declaration of local data
!
  implicit none
  logical           :: lexist          ! logical to determine existence
  character(len=7)  :: decayfile       ! decay data file
  character(len=10) :: discretefile    ! file with discrete level data
  integer           :: type            ! particle type
  integer           :: i               ! counter
  integer           :: j               ! counter
  integer           :: NNL             ! number of levels
!
! ********************* Read gamma decay schemes ***********************
!
  do type = 0, 6
    decayfile = 'decay.'//parsym(type)
    inquire (file = decayfile, exist = lexist)
    if (lexist) then
      open (unit = 1, file = decayfile, status = 'old')
      read(1, '(///14x, i4/)') NNL
      do i = 1, min(NNL, numlevels)
        read(1, '(4x, i4, f10.6)') Ngamdis(type, i), yieldg(type, i)
        do j = 1, Ngamdis(type, i)
          read(1, '(f11.6, e12.5)') Egamdis(type, i, j), yieldratio(type, i, j)
        enddo
      enddo
      close (unit = 1)
    endif
  enddo
!
! ************** Read gamma ray transition probabilities ***************
!
  do type = 0, 6
    discretefile = 'discrete.'//parsym(type)
    inquire (file = discretefile, exist = lexist)
    if (lexist) then
      open (unit = 1, file = discretefile, status = 'old')
      read(1, '(//19x, i4//)') nlev(type)
      do i = 0, min(nlev(type), numlevels)
        read(1, '(35x, i2)') Nbranch(type, i)
        do j = 1, Nbranch(type, i)
          read(1, '(37x, i3, f10.4)') branchlevel(type, i, j), branchratio(type, i, j)
        enddo
      enddo
      close (unit = 1)
    endif
  enddo
  return
end subroutine talysphoton
! Copyright A.J. Koning 2021
