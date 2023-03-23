subroutine mainout
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose   : Main output
!
! Author    : Arjan Koning
!
! 2023-03-10: Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
  write(*,'(/"    TEFAL-2.0 (Version: March 10, 2022)"/)')
  write(*, '(10x, " Creating ENDF-6 files with TALYS")')
  write(*, '(/" Copyright (C) 2023  A.J. Koning     ")')
!
! ***************** Write input file and default parameters ************
!
! inputout: subroutine to write the input parameters
!
  call inputout
  return
end subroutine mainout
! Copyright A.J. Koning 2023
