      subroutine FARAX
     $(NO,KIPE)
C
C     Rudolf Loeser, 1982 May 11
C---- Prints graph heading, for TONY.
C     !DASH
      save
C     !DASH
      integer KIPE, KNT, NO
      character LAB*9
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
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, ABJECT, LINER, HI, BYE
C
      dimension KNT(3), LAB(3)
C
      data KNT / 9,           8,          4/
      data LAB /'Absorbers', 'Emitters', 'Taus'/
C
      call HI ('FARAX')
C     !BEG
      if((KIPE.lt.1).or.(KIPE.gt.3)) then
        write (MSSLIN(1),100) KIPE
  100   format('KIPE =',I12,', which is not 1, 2 or 3.')
        call HALT ('FARAX', 1)
      end if
C
      call ABJECT (NO)
      write (NO,101) LAB(KIPE)(1:KNT(KIPE)),WLAB1(2:)
  101 format(' ','Plot of ',A,' vs. log(W',A,')')
      call LINER  (1,NO)
C     !END
      call BYE ('FARAX')
C
      return
      end
