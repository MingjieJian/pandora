      subroutine MUSU
     $(NO,TOT,REM,TITLE)
C
C     Rudolf Loeser, 1991 Jul 16
C---- Prints a heading for eclipse profile data.
C     !DASH
      save
C     !DASH
      integer NO
      character BLANK*1, REM*56, TITLE*5, TOT*24
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
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
      external LINER, HI, BYE
C     !EJECT
C
      call HI ('MUSU')
C     !BEG
      write (NO,100) TOT
  100 format(' ',8X,'----------------------------------------------',
     $           10X,A)
      write (NO,101) TITLE
  101 format(' ',A5,3X,'Integrated Ray Intensities, in ergs/cm**2/s/sr')
      call LINER (1,NO)
C
      if(WAVENO) then
        write (NO,102) TITLE(1:1),TITLE(1:1)
  102   format(' ',8X,'II',A1,'(I) is twice the integral of I',A1,
     $             '(I) from line center to the given WN')
        if(REM(1:1).ne.BLANK) then
          write (NO,103) REM
  103     format(' ',15X,A)
        end if
      else
        write (NO,104) TITLE(1:1),TITLE(1:1)
  104   format(' ',8X,'II'A1,'(I) is I',A1,'(I) integrated from ',
     $             '-DL to +DL')
        if(REM(1:1).ne.BLANK) then
          write (NO,103) REM
        end if
      end if
C     !END
      call BYE ('MUSU')
C
      return
      end
