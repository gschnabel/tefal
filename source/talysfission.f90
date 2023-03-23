subroutine talysfission
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose   : Read fission data from TALYS
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
! Variables for input of ENDF structure
!   flagmulti      ! flag to include multi - chance fission
! Variables for partial cross sections in ENDF format
!   flagfission    ! flag for fission
!   idchannel      ! identifier for channel
!   reacstring     ! string with reaction information
!   xsexcl         ! exclusive cross section
!
! *** Declaration of local data
!
  implicit none
  logical           :: lexist      ! logical to determine existence
  character(len=11) :: fisfile     ! fission file
  character(len=12) :: fiscfile    ! fission file
  integer           :: idc         ! help variable
  integer           :: in          ! counter for neutrons
  integer           :: istat       ! logical for file existence
  integer           :: nen         ! energy counter
  integer           :: nin         ! counter for incident energy
!
! ******************** Read fission cross sections ********************
!
  idchannel(-1) = -99
  flagfission = .false.
  fisfile = 'fission.tot'
  inquire (file = fisfile, exist = lexist)
  if (lexist) then
    open (unit = 1, file = fisfile, status = 'old')
    reacstring(-1, 0)(1:12) = ' TALYS-2.0  '
    read(1, '(1x, a42)') reacstring(-1, 0)(13:54)
    read(1, '(//14x, i6/)') nen
    do nin = 1, nen
      read(1, '(12x, e12.5)') xsexcl(-1, nin)
    enddo
    close (unit = 1)
    flagfission = .true.
  endif
!
! ***************** Multichance fission cross sections *****************
!
  if (flagfission .and. flagmulti) then
    do in = 0, 3
      idc = -2 - in
      idchannel(idc) = -98 + in
      fiscfile = 'xs000000.fis'
      write(fiscfile(3:3), '(i1)') in
      inquire (file = fiscfile, exist = lexist)
      if (lexist) then
        open (unit = 1, file = fiscfile, status = 'old')
        read(1, '(///14x, i6/)') nen
        do nin = 1, nen
          read(1, '(12x, e12.5)', iostat=istat) xsexcl(idc, nin)
          if  (istat == -1) exit
        enddo
        close (unit = 1)
      endif
    enddo
  endif
  return
end subroutine talysfission
! Copyright A.J. Koning 2021
