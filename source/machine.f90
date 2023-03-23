subroutine machine
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose   : Machine dependent statements
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
! Variables for path names
!   month      ! month
!   path       ! directory containing files to be read
!   year       ! year
!
! *** Declaration of local data
!
  implicit none
  character(len=3)   :: monthC(12)    ! month
  character(len=132) :: basedir       ! base directory
  integer            :: values(8)     ! date and time values
!
! ********************* Set routine for date ***************************
!
!
  monthC = (/'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/)
  call date_and_time(VALUES = values)
  month = monthC(values(2))
  year = mod(values(1), 100)
!
! ************************ Set directories *****************************
!
  basedir = '/Users/koning/tefal/'
  path = basedir
  return
end subroutine machine
! Copyright A.J. Koning 2021
