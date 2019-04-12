      subroutine FRONT
     $(NO,WVLA,XLTIT,HEAD,KTRU,LYM,TPOP)
C
C     Rudolf Loeser, 1980 Nov 06
C---- Makes a heading, for regular Continuum printouts.
C     (This is version 3 of FRONT.)
C     !DASH
      save
C     !DASH
      real*8 WVLA, WVLM, XLTIT
      integer KTRU, NO
      logical LYM
      character BLANK*1, HEAD*12, TPOP*3, TTIT*19, WTIT*40, qummy*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external MARABOU, DACAPO, LINER, MONGOL, HI, BYE
C
      call HI ('FRONT')
C     !BEG
C---- Get XLM in meters, and set up label with proper SI prefix
      call MARABOU (WVLA, WVLM, HEAD)
C
C---- Header still needed?
      call DACAPO  (WVLA)
C
C---- Print wavelength values, etc.
      TTIT = BLANK
      if(KTRU.eq.1) then
        TTIT = 'Line-free Continuum'
      else if(LYM) then
        TTIT = 'Lyman ('//TPOP//')'
      end if
      call LINER   (1,NO)
      write (NO,101) WVLM,WVLA,TTIT
  101 format(' ','Continuum Data at',1PE17.9,' meter',19X,E23.15,
     $           ' Angstroms',16X,A19)
C
C---- Print wavelength identifier
      call MONGOL  (XLTIT, WTIT, qummy)
      call LINER   (1, NO)
      write (NO,102) WTIT
  102 format(' ',A40)
C     !END
      call BYE ('FRONT')
C
      return
      end
