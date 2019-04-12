      subroutine COMITO
     $(NO,IMAGE,NVY,PADI,NP)
C
C     Rudolf Loeser, 1982 Apr 12
C---- Prints a graph, for GELIMER.
C     !DASH
      save
C     !DASH
      real*8 PADI
      integer I, NO, NP, NVY
      character IMAGE*(*)
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C
C---- BASH        as of 1984 Apr 19
      integer            JXLOBA,JXHIBA,JYLOBA,JYHIBA,NTHRBA,NPOIBA
      common      /BASH/ JXLOBA,JXHIBA,JYLOBA,JYHIBA,NTHRBA,NPOIBA
C     Control data for "BEIGE".
C     .
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
      external ABJECT, LINER, KPRINT, HI, BYE
C
C               PADI(NP)
      dimension PADI(*)
C     !EJECT
C
      call HI ('COMITO')
C     !BEG
      if(NPOIBA.gt.NTHRBA) then
        call ABJECT (NO)
        write (NO,100) WLAB3
  100   format(' ','Log(Intensity) vs. ',A,', for selected values ',
     $             'of radius.')
C
        call LINER  (1,NO)
        call KPRINT (IMAGE,NO)
        call LINER  (1,NO)
        write (NO,101) (PADI(I),ALPHS(I),I=1,NP)
  101   format(' ','Radius',8(5X,F5.1,': ',A1)/
     $        (' ',6X,8(5X,F5.1,': ',A1)))
      end if
C     !END
      call BYE ('COMITO')
C
      return
      end
