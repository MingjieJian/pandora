      subroutine EMESA
     $(NO,IMAGE,NVY)
C
C     Rudolf Loeser, 1982 Apr 12
C---- Prints a graph, for RHODO.
C     !DASH
      save
C     !DASH
      integer NO, NVY
      character IMAGE*(*)
C     !COM
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
      call HI ('EMESA')
C     !BEG
      if(NPOIBA.gt.NTHRBA) then
C
        call ABJECT (NO)
C
        write (NO,100) WLAB3
  100   format(' ','Log(FLux) vs. ',A)
C
        call LINER  (1,NO)
        call KPRINT (IMAGE,NO)
        call LINER  (1,NO)
        write (NO,101)
  101   format(' ',10X,'S: Shell',5X,'D: Disk',5X,'T: Total')
C
      end if
C     !END
      call BYE ('EMESA')
C
      return
      end
