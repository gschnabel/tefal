subroutine talysdiscrete
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose   : Read discrete level and continuum data from TALYS
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
! Definition of single and double precision variables
!   sgl          ! single precision kind
! All global variables
!   numl         ! number of l values
! Variables for input of ENDF structure
!   flagngn      ! flag to include (n, gamma n) data
! Variables for reaction initialization
!   Eangindex    ! enegy index for angular distributions
!   Nenang       ! number of incident energies for ang. dist.
!   nlevmax      ! number of included discrete levels
!   numcut       ! number of energies before high - energy format
! Variables for TALYS info
!   eninc        ! incident energy
!   flagblock    ! flag to block spectra, angle and gamma files
!   k0           ! index of incident particle
!   NLmax        ! maximum number of included discrete levels
! Constants
!   parsym       ! symbol of particle
! Variables for discrete state cross sections in ENDF format
!   cleg0        ! Legendre coefficients
!   Ethdisc      ! threshold energy
!   jdis         ! spin of level
!   ncleg        ! number of Legendre coefficients
!   ndisc        ! number of discrete levels
!   Qdisc        ! Q - value
!   xsang        ! differential cross section
!   xsngn        ! (projectile, gamma - ejectile) cross section
!   xscont       ! continuum cross section
!   xsdisc       ! discrete state cross section
!
! *** Declaration of local data
!
  implicit none
  logical           :: lexist      ! logical to determine existence
  character(len=6)  :: contfile    ! file for continuum
  character(len=6)  :: discfile    ! file with elastic scattering angular distribution
  character(len=17) :: angfile     ! name of file with angular distributions
  character(len=17) :: legfile     ! name of file with Legendre coefficients
  character(len=80) :: Estring     ! energy string
  integer           :: iang        ! running variable for angle
  integer           :: istat       ! logical for file access
  integer           :: L           ! counter for Legendre coefficients
  integer           :: N           ! neutron number of residual nucleus
  integer           :: Nfile       ! help variable
  integer           :: nang1       ! number of angles
  integer           :: nen         ! energy counter
  integer           :: nex         ! discrete level
  integer           :: nin         ! counter for incident energy
  integer           :: nleg1       ! number of Legendre coefficients
  integer           :: type        ! particle type
  real(sgl)         :: Ein         ! incident energy
  real(sgl)         :: Efile       ! incident energy
  real(sgl)         :: Qold        ! help variable
!
! ******************* Read discrete level cross sections ***************
!
  do type = 0, 6
    if (type > 0) then
      Qold = 0.
      do nex = 0, nlevmax
        discfile = '  .L  '
        write(discfile(1:2), '(2a1)') parsym(k0), parsym(type)
        write(discfile(5:6), '(i2.2)') nex
        inquire (file = discfile, exist = lexist)
        if (lexist) then
          open (unit = 1, file = discfile, status = 'old')
          read(1, '()')
          read(1, '(14x, e12.5, 6x, f5.1)') Qdisc(type, nex), jdis(type, nex)
          read(1, '(14x, e12.5)') Ethdisc(type, nex)
          read(1, '(14x, i6/)') N
          do nin = 1, N
            read(1, '(12x, e12.5)') xsdisc(type, nex, nin)
          enddo
          close (unit = 1)
          if (Qdisc(type, nex) == Qold .and. Qdisc(type, nex) /= 0.) Qdisc(type, nex) = Qdisc(type, nex) - 1.e-6
          Qold = Qdisc(type, nex)
          ndisc(type) = nex
        endif
      enddo
    endif
!
! ******************* Read continuum cross sections ********************
!
    contfile = '  .con'
    write(contfile(1:2), '(2a1)') parsym(k0), parsym(type)
    inquire (file = contfile, exist = lexist)
    if (lexist) then
      open (unit = 1, file = contfile, status = 'old')
      read(1, '(///14x, i6/)') N
      do nin = 1, N
        read(1, '(24x, 2e12.5)') xscont(type, nin), xsngn(type, nin)
!
! Possibility to store (n,gn) cross section into continuum inelastic.
!
        if ( .not. flagngn .and. xscont(type, nin) > 0.) then
          xscont(type, nin) = xscont(type, nin) + xsngn(type, nin)
          xsngn(type, nin) = 0.
        endif
      enddo
!
! Prevent zero capture gamma continuum cross sections for very low incident energies.
!
      if (type == 0) then
        do nin = numcut - 1, 1, -1
          if (xscont(type, nin) == 0.) xscont(type, nin) = xscont(type, nin + 1)
        enddo
      endif
      close (unit = 1)
    endif
  enddo
  Ethdisc(k0, 0) = 0.
!
! *************** Read elastic and discrete state Legendre coefficients and angular distributions ***************
!
! 1. Legendre coefficients
!
  do type = k0, k0
    do nex = 0, NLmax
      do nen = 1, Nenang
        Ein = eninc(Eangindex(nen))
        if (flagblock) then
          legfile = '  leg.L00'
          write(legfile(8:9), '(i2.2)') nex
        else
          legfile = '          leg.L00'
          write(legfile(3:10), '(f8.3)') Ein
          write(legfile(3:6), '(i4.4)') int(Ein)
          write(legfile(16:17), '(i2.2)') nex
        endif
        write(legfile(1:2), '(2a1)') parsym(k0), parsym(type)
        inquire (file=legfile, exist=lexist)
        if (lexist) then
          open (unit=1, file=legfile, status='old')
          do
            read(1,'(/,a,//14x,i4/)', iostat = istat) Estring,Nfile
            if (istat == -1) exit
            ncleg(type,nex,nen) = min(numl-1,Nfile-1)
            if (flagblock) then
              read(Estring(16:80), *) Efile
              if (abs(Ein-Efile) <= 1.e-4) then
                ncleg(type,nex,nen) = min(numl-1,Nfile-1)
                do L = 0, ncleg(type, nex, nen)
                  read(1, '(67x, e16.5)') cleg0(type, nex, nen, L)
                enddo
                exit
              else
                do L = 1, Nfile
                  read(1, '()')
                enddo
                cycle
              endif
            else
              ncleg(type,nex,nen) = min(numl-1,Nfile-1)
              do L = 0, ncleg(type,nex,nen)
                read(1, '(67x,e16.5)') cleg0(type, nex, nen, L)
              enddo
              exit
            endif
          enddo
          close (unit=1)
        endif
        legfile = '          leg.L00'
        write(legfile(1:2), '(2a1)') parsym(k0), parsym(type)
        write(legfile(3:10), '(f8.3)') Ein
        write(legfile(3:6), '(i4.4)') int(Ein)
        write(legfile(16:17), '(i2.2)') nex
        inquire (file = legfile, exist = lexist)
        if (lexist) then
          open (unit = 1, file = legfile, status = 'old')
          read(1, '(///14x, i4/)') nleg1
          ncleg(type, nex, nen) = min(numl - 1, nleg1 - 1)
          do L = 0, ncleg(type, nex, nen)
            read(1, '(67x, e16.5)') cleg0(type, nex, nen, L)
          enddo
          close (unit = 1)
        endif
      enddo
    enddo
  enddo
!
! 2. Angular distributions
!
  nang1 = 0
  do type = 1, 6
    do nex = 0, NLmax
      do nen = Nenang, 1, -1
        Ein = eninc(Eangindex(nen))
        if (flagblock) then
          angfile = '  ang.L00'
          write(angfile(8:9), '(i2.2)') nex
        else
          angfile = '          ang.L00'
          write(angfile(3:10), '(f8.3)') Ein
          write(angfile(3:6), '(i4.4)') int(Ein)
          write(angfile(16:17), '(i2.2)') nex
        endif
        write(angfile(1:2), '(2a1)') parsym(k0), parsym(type)
        inquire (file = angfile, exist = lexist)
        if (lexist) then
          open (unit=1, file = angfile, status = 'old')
          do
            read(1,'(/,a,//14x,i4/)', iostat = istat) Estring, nang1
            if (istat == -1) exit
            if (flagblock) then
              read(Estring(16:80), *) Efile
              if (abs(Ein-Efile) <= 1.e-4) then
                do iang = 0, nang1-1
                  read(1, '(5x, e16.5)') xsang(type,nex,nen,iang)
                enddo
                exit
              else
                do iang = 1, nang1
                  read(1, '()')
                enddo
                cycle
              endif
            else
              do iang = 0, nang1 - 1
                read(1, '(5x, e16.5)') xsang(type, nex, nen, iang)
              enddo
              exit
            endif
          enddo
          close (unit=1)
        endif
      enddo
!
! For very low energies, copy angular distribution of the lowest TALYS energy (seldom used anyway).
!
      do iang = 0, nang1-1
        do nen = Nenang, 1, -1
          if (xsang(type, nex, nen, iang) == 0.) xsang(type, nex, nen, iang) = xsang(type, nex, nen+1, iang)
        enddo
      enddo
    enddo
  enddo
  return
end subroutine talysdiscrete
