      subroutine BASSUS
     $(NO,IMAGE,NVY,PLAM,NP,PWAV)
C
C     Rudolf Loeser, 1982 Apr 12
C---- Prints a graph, for AMASIA.
C     !DASH
      save
C     !DASH
      real*8 PLAM, PWAV
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
      external ABJECT, LINER, MOVE1, DUNGUS, KPRINT, HI, BYE
C
C               PLAM(NP), PWAV(NP)
      dimension PLAM(*),  PWAV(*)
C     !EJECT
C
      call HI ('BASSUS')
C     !BEG
      if(NPOIBA.gt.NTHRBA) then
C
        call ABJECT   (NO)
C
        write (NO,100) WLAB3
  100   format(' ','Log(Intensity) vs. log(Radius), ',
     $             'for selected values of ',A)
C
        call LINER    (1,NO)
        call KPRINT   (IMAGE,NO)
        call LINER    (1,NO)
C
        if(WAVENO) then
          call DUNGUS (PLAM,COREWL,NP,PWAV)
        else
          call MOVE1  (PLAM,NP,PWAV)
        end if
C
        write (NO,101) WLAB3,(PWAV(I),ALPHS(I),I=1,NP)
  101   format(' ',A12,5(2X,1PE18.10,': ',A1)/
     $        (' ',12X,5(2X,1PE18.10,': ',A1)))
C
      end if
C     !END
      call BYE ('BASSUS')
C
      return
      end
