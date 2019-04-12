      subroutine GREGORY
     $(NW,NZE,WTAB,XIS,IND,Z,ZECL,KODE,AC,DELTA,IJECT,NO)
C
C     Rudolf Loeser, 1993 Jun 15
C---- Prints, for GRIFFIN.
C     !DASH
      save
C     !DASH
      real*8 AC, DELTA, WTAB, XIS, Z, ZECL, ZS
      integer I, IJECT, IND, J, KODE, M, NO, NW, NZE
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
C     !EJECT
      external  DEJECT, LINER, SHIM, HI, BYE
      intrinsic min
C
C               WTAB(NW), IND(NZE), ZECL(NZE), XIS(NW,NZE), Z(N)
      dimension WTAB(*),  IND(*),   ZECL(*),   XIS(NW,*),   Z(*)
C
      dimension ZS(10)
C
      call HI ('GREGORY')
C     !BEG
      M = min(NZE,10)
C
      call DEJECT (NO, IJECT)
      if(KODE.eq.1) then
        write (NO,100) NZE
  100   format(' ','Continuum Intensities along rays tangent to ',I3,
     $             ' values of Z(IZ).')
      else
        write (NO,101) NZE,AC,DELTA
  101   format(' ','Continuum Intensities along beams tangent to ',I3,
     $             ' values of Z(IC).'/
     $         ' ','Beam width parameter BMWAC =',1PE12.5,5X,
     $             'Delta =',E12.5,' km.')
      end if
      call LINER  (1, NO)
C
      write (NO,102)
  102 format(' ','Intensity in units of ergs/cm**2/s/sr/Hz')
      write (NO,103) WLAB2,WLAB4
  103 format(' ',A2,' in ',A10)
      call LINER  (2, NO)
      write (NO,104) WLAB2,(IND(I),I=1,M)
  104 format(' ',8X,A2,7X,10(3X,'IZ=',I3,2X,:))
      call LINER  (1, NO)
C
      do 106 I = 1,NW
        write (NO,105) WTAB(I),(XIS(I,J),J=1,M)
  105   format(' ',1PE17.10,10E11.4)
        call SHIM (I, 5, NO)
  106 continue
C
      if(KODE.eq.1) then
        do 107 I = 1,M
          ZS(I) = Z(IND(I))
  107   continue
        call LINER  (2, NO)
        write (NO,108) 'Z(IZ)', (ZS(I),I=1,M)
  108   format(' ',11X,A5,'=',1P10E11.3)
        write (NO,108) ' ZECL', (ZECL(I),I=1,M)
      end if
C     !END
      call BYE ('GREGORY')
C
      return
      end
