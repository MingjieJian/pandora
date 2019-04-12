      subroutine HILDE
     $(NO,DISK,IU,IL,WVL,ICE,R1N,Z,N,FRR,MRR)
C
C     Rudolf Loeser, 1981 Sep 04
C---- Prints heading for Eclipse Calculation results.
C     (This is version 4 of HILDE.)
C     !DASH
      save
C     !DASH
      real*8 FRR, R1N, WVL, Z
      integer I, ICE, IL, IU, MRR, N, NO
      logical DISK
C     !COM
C---- MOSTAR      as of 2000 Sep 26
      real*8      COREWL,COREWN
      integer     ICORE
      character   WLAB1*10,WLAB2*2,WLAB3*12,WLAB4*10,WLAB5*2
      logical     WAVENO,SLINE,WHOLE,RED,BLUE
      common      /MOSTAR1/ COREWL,COREWN
      common      /MOSTAR2/ ICORE
      common      /MOSTAR3/ WLAB1,WLAB2,WLAB3,WLAB4,WLAB5
      common      /MOSTAR4/ WAVENO,SLINE,WHOLE,RED,BLUE
C     Wavelength/Wavenumber print/plot controls (subroutine WUMBLE).
C     .
C     !DASH
      external ABJECT, LINER, HI, BYE
C
C               Z(N), FRR(MRR)
      dimension Z(*), FRR(*)
C     !EJECT
C
      call HI ('HILDE')
C     !BEG
      if(NO.gt.0) then
        call ABJECT (NO)
        write (NO,100) IU,IL,WVL
  100   format(' ','"Eclipse" Intensity and Flux for Line (',I2,'/',
     $             I2,'), computed using spherical coordinates.',12X,
     $             1PE21.13,' Angstroms')
        call LINER (1,NO)
C
        if(ICE.ne.0) then
          write (NO,101)
  101     format(' ','Partial Redistribution effects included.')
          call LINER (1,NO)
        end if
C
        write (NO,102) R1N
  102   format(' ','Radius of curvature (km)',4X,1PE11.3)
C
        call LINER (1,NO)
        write (NO,103) (Z(I),I=1,N)
  103   format(' ','Depths (km)',17X,1P9E11.3/(' ',28X,9E11.3))
C
        if(DISK) then
          call LINER (1,NO)
          write (NO,104) (FRR(I),I=1,MRR)
  104     format(' ','Fractional radii',12X,1P9E11.3/(' ',28X,9E11.3))
        end if
C
        call LINER   (1,NO)
        if(WAVENO) then
          write (NO,105)
  105     format(' ','*** This run prints     WN: Wavenumber, in /cm.'/
     $           ' ','To use Delta-Lambda, turn option WAVENUMB = OFF.')
        else
          write (NO,106)
  106     format(' ','*** This run prints     DL: Delta-Lambda, the ',
     $               'displacement from line center, in Angstroms.'/
     $           ' ','To use Wavenumber, turn option WAVENUMB = ON.')
        end if
      end if
C     !END
      call BYE ('HILDE')
C
      return
      end
