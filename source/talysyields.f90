subroutine talysyields
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose   : Read yields from TALYS
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
! Constants
!   parsym    ! symbol of particle
! Variables for yields in ENDF format
!   xsprod    ! particle production cross section
!   yieldp    ! particle production yield
!
! *** Declaration of local data
!
  implicit none
  logical          :: lexist       ! logical to determine existence
  character(len=9) :: prodfile     ! file with total particle production cross sections
  integer          :: N            ! neutron number of residual nucleus
  integer          :: nin          ! counter for incident energy
  integer          :: type         ! particle type
!
! ****************** Read total production yields **********************
!
! 1. Production cross sections and particle yields
!
  do type = 0, 6
    prodfile = ' prod.tot'
    write(prodfile(1:1), '(a1)') parsym(type)
    inquire (file = prodfile, exist = lexist)
    if (lexist) then
      open (unit = 1, file = prodfile, status = 'old')
      read(1, '(///14x, i6/)') N
      do nin = 1, N
        read(1, '(12x, 2e12.5)') xsprod(type, nin), yieldp(type, nin)
      enddo
      close (unit = 1)
    endif
  enddo
  return
end subroutine talysyields
! Copyright A.J. Koning 2021
