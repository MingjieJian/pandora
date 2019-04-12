      subroutine DARAS
     $(NO,NVY,XMAX,XMIN,KS,KE,LINE)
C
C     Rudolf Loeser, 1982 Apr 12
C---- Prints header, for BUZES.
C     !DASH
      save
C     !DASH
      real*8 XMAX, XMIN
      integer I, KC, KE, KS, NO, NVY
      character BLANK*1, CHAR*3, DL*12, LINE*101, WN*10
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
      external ABJECT, LINER, ACHILES, HI, BYE
C
      data DL, WN /'Delta-Lambda', 'Wavenumber'/
C     !EJECT
C
      call HI ('DARAS')
C     !BEG
      call ABJECT    (NO)
      if(WAVENO) then
        write (NO,100) XMIN,XMAX,WN,'decreases',WN,WN
  100   format(' ','Scaled values of log( Intensity ).'//
     $         ' ','Printed values are 100 * log(Intensity/minimuum) ',
     $             '/ log(maximum/minimum), truncated.'/
     $         ' ','The value "100" is printed as "**".'/
     $         ' ','Minimum intensity =',1PE14.6,
     $             ', maximum intensity =',E14.6,'.'/
     $         ' ','Distance from center of disk increases downward, ',
     $             A,' ',A,' to the right.'/
     $         ' ','The indices of the ',A,' columns are printed ',
     $             'above them; the line core is marked with : :.'/
     $         ' ','The entire ',A,' table is printed at the end.')
      else
        write (NO,100) XMIN,XMAX,DL,'increases',DL,DL
      end if
      call LINER     (1,NO)
C
      LINE = BLANK
      KC   = 0
      do 101 I = KS,KE
        call ACHILES (I,CHAR)
        if(I.eq.ICORE) then
          LINE(KC+1:KC+5) = ' :'//CHAR(2:3)//':'
          KC = KC+5
        else
          LINE(KC+1:KC+3) = CHAR(1:3)
          KC = KC+3
        end if
  101 continue
      write (NO,102) LINE
  102 format(' ',5X,'D I S T A N C E',3X,A101)
C
      LINE = BLANK
      KC   = 0
      do 103 I = KS,KE
        if(I.eq.ICORE) then
          LINE(KC+1:KC+5) = ' :--:'
          KC = KC+5
        else
          LINE(KC+1:KC+3) = ' --'
          KC = KC+3
        end if
  103 continue
      write (NO,104) LINE
  104 format(' ',4X,'(km)',6X,'(radii)',2X,A101)
C     !END
      call BYE ('DARAS')
C
      return
      end
