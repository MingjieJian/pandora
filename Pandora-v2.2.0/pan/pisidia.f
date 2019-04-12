      subroutine PISIDIA
     $(FINT,NRAD,NLAM,I,KS,KE,LINE)
C
C     Rudolf Loeser, 1982 Apr 13
C---- Encodes a data line, for BUZES.
C     !DASH
      save
C     !DASH
      real*8 FINT
      integer I, J, JX, KC, KE, KS, NLAM, NRAD
      character BLANK*1, CHAR*3, LINE*101
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
      external ACHILES, HI, BYE
C
C               FINT(NRAD,KM)
      dimension FINT(NRAD,*)
C     !EJECT
C
      call HI ('PISIDIA')
C     !BEG
      LINE = BLANK
      KC   = 0
      do 100 J = KS,KE
        JX = FINT(I,J)
        if(JX.lt.0) then
          CHAR = BLANK
        else if(JX.ge.100) then
          CHAR = ' **'
        else
          call ACHILES (JX,CHAR)
        end if
C
        if(J.eq.ICORE) then
          LINE(KC+1:KC+5) = ' :'//CHAR(2:3)//':'
          KC = KC+5
        else
          LINE(KC+1:KC+3) = CHAR(1:3)
          KC = KC+3
        end if
  100 continue
C     !END
      call BYE ('PISIDIA')
C
      return
      end
