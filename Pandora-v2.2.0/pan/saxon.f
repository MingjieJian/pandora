      subroutine SAXON
     $(NO,LFB,LINK,IJECT)
C
C     Rudolf Loeser, 2004 Sep 10
C---- Writes a header for BOLO.
C     (This is version 2 of SAXON.)
C     !DASH
      save
C     !DASH
      integer IJECT, LFB, LINK, NO
      character BLANK*1, FACELAB*10, TYPE*12
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external DEJECT, DOUBLER, TUMBLE, LINER, HI, BYE
C
      call HI ('SAXON')
C     !BEG
      if(NO.gt.0) then
        call DEJECT  (NO, IJECT)
        call TUMBLE  (LFB, FACELAB)
C
        if(LINK.eq.3) then
          TYPE = '  Line-free '
        else
          TYPE = ' Background '
        end if
C
        if(FACELAB(10:10).eq.BLANK) then
          write (NO,100) TYPE
  100     format(' ',55X,A12,'Flux.')
        else
          write (NO,101) TYPE,FACELAB
  101     format(' ',49X,A12,'Flux.',2X,A10)
        end if
C
        call LINER   (1,NO)
        call DOUBLER (NO)
        call LINER   (1,NO)
      end if
C     !END
      call BYE ('SAXON')
C
      return
      end
