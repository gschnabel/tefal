subroutine talysresidual
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose   : Read residual production data from TALYS
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
!   sgl           ! single precision kind
! All global variables
!   numenin       ! number of incident energies
!   numenrec      ! number of incident energies for recoils
!   numN          ! maximal number of neutrons from initial compound nucleus
!   numZ          ! maximal number of protons from initial compound nucleus
! Variables for reaction initialization
!   Especindex    ! enegy index for spectra
!   Nenspec       ! number of incident energies for spectra
!   nlevmax       ! number of included discrete levels
! Variables for TALYS info
!   Ainit         ! mass number of initial compound nucleus
!   eninc         ! incident energy
!   flagblock     ! flaf to block spectra, angle and gamma files
!   Zinit         ! charge number of initial compound nucleus
! Variables for input of ENDF structure
!   flagrecoil    ! flag to include recoil information
! Variables for residual production cross sections in ENDF format
!   Erecrp        ! recoil energy of residual nucleus
!   Erpiso        ! energy of isomer
!   Ethrp         ! threshold energy for residual product
!   Ethrpiso      ! threshold energy for isomer of residual product
!   Nisorp        ! number of isomers
!   noutrecrp     ! number of recoil energies for residual nucleus
!   nucmass       ! mass of nucleus
!   Qrp           ! Q - value for residual product
!   Qrpiso        ! Q - value for isomer of residual product
!   recrp         ! recoils or residual nucleus
!   xsrp          ! residual production cross section
!   xsrpiso       ! residual production cross section for isomer
!
! *** Declaration of local data
!
  implicit none
  logical           :: lexist     ! logical to determine existence
  logical           :: lexist2    ! logical to determine existence
  character(len=12) :: isofile    ! file with isomeric cross section
  character(len=12) :: rpfile     ! file with residual production cross sections
  character(len=25) :: recfile    ! file with recoil spectra
  character(len=80) :: Estring    ! energy string
  integer           :: A          ! mass number of target nucleus
  integer           :: istat      ! logical for existence of file
  integer           :: nen        ! energy counter
  integer           :: nen2       ! energy counter
  integer           :: nex        ! discrete level
  integer           :: Nfile      ! help variable
  integer           :: nin        ! counter for incident energy
  integer           :: Nix        ! neutron number index for residual nucleus
  integer           :: Z          ! charge number of target nucleus
  integer           :: Zix        ! charge number index for residual nucleus
  real(sgl)         :: Ein        ! incident energy
  real(sgl)         :: Efile      ! incident energy
!
! **************** Read residual production cross sections *************
!
  do Zix = 0, numZ
    do Nix = 0, numN
      Z = Zinit - Zix
      A = Ainit - Zix - Nix
      rpfile = 'rp000000.tot'
      write(rpfile(3:5), '(i3.3)') Z
      write(rpfile(6:8), '(i3.3)') A
      inquire (file = rpfile, exist = lexist)
      if (lexist) then
        open (unit = 1, file = rpfile, status = 'old')
        read(1, '()')
        read(1, '(14x, e12.5, 6x, f11.6)') Qrp(Zix, Nix), nucmass(Zix, Nix)
        read(1, '(14x, e12.5)') Ethrp(Zix, Nix)
        read(1, '(14x, i6/)') nen
        nen = min(nen, numenin)
        do nin = 1, nen
          read(1, '(12x, e12.5)') xsrp(Zix, Nix, nin)
        enddo
        close (unit = 1)
      endif
!
! Isomers
!
      isofile(1:9) = rpfile(1:9)
      isofile(10:10) = 'L'
      Nisorp(Zix, Nix) = 0
      do nex = 0, nlevmax
        write(isofile(11:12), '(i2.2)') nex
        inquire (file = isofile, exist = lexist2)
        if (lexist2) then
          open (unit = 1, file = isofile, status = 'old')
          if (nex > 0) then
            read(1, '(43x, f12.5)') Erpiso(Zix, Nix, nex)
          else
            read(1, '()')
          endif
          read(1, '(14x, e12.5)') Qrpiso(Zix, Nix, nex)
          read(1, '(14x, e12.5)') Ethrpiso(Zix, Nix, nex)
          read(1, '(14x, i6/)') nen
          do nin = 1, nen
            read(1, '(12x, e12.5)') xsrpiso(Zix, Nix, nex, nin)
          enddo
          close (unit = 1)
          Nisorp(Zix, Nix) = Nisorp(Zix, Nix) + 1
        endif
      enddo
!
! Recoils of residual nuclides (general purpose files only)
!
      if (flagrecoil) then
        do nen = 1, Nenspec
          Ein = eninc(Especindex(nen))
          if (flagblock) then
            recfile = 'rec000000.tot'
          else
            recfile = 'rec000000spec0000.000.tot'
            write(recfile(14:21), '(f8.3)') Ein
            write(recfile(14:17), '(i4.4)') int(Ein)
          endif
          recfile(4:9) = rpfile(3:8)
          inquire (file = recfile, exist = lexist2)
          if (lexist2) then
            open (unit=1, file=recfile, status='old')
            do
              read(1,'(/,a,//14x,i6/)', iostat = istat) Estring, Nfile
              if (istat == -1) exit
              if (flagblock) then
                read(Estring(16:80), *) Efile
                if (abs(Ein-Efile) <= 1.e-4) then
                  noutrecrp(Zix, Nix, nen) = min(Nfile, numenrec)
                  do nen2 = 1, noutrecrp(Zix,Nix,nen)
                    read(1, '(f8.3, e12.5)') Erecrp(Zix,Nix,nen,nen2), recrp(Zix,Nix,nen,nen2)
                  enddo
                  exit
                else
                  do nen2 = 1, Nfile
                    read(1, '()')
                  enddo
                  cycle
                endif
              else
                noutrecrp(Zix, Nix, nen) = min(Nfile, numenrec)
                do nen2=1,noutrecrp(Zix,Nix,nen)
                  read(1,'(f8.3,e12.5)') Erecrp(Zix,Nix,nen,nen2), recrp(Zix,Nix,nen,nen2)
                enddo
                exit
              endif
            enddo
            close (unit=1)
          endif
        enddo
      endif
    enddo
  enddo
  return
end subroutine talysresidual
! Copyright A.J. Koning 2021
