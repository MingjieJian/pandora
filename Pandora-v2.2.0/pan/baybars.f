      subroutine BAYBARS
     $(SR,S,MARK)
C
C     Rudolf Loeser, 1994 Oct 05
C---- Sets a "difference marker", for VERNAL.
C     !DASH
      save
C     !DASH
      real*8 S, SF12, SF3, SR
      integer IFLAG
      character BLANK*1, DOLLAR*1, MARK*1, MINUS*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(40),MINUS )
      equivalence (SYMBS(37),DOLLAR)
C     !DASH
      external COMPD, MARKI, HI, BYE
      data SF12,SF3 /1.D-12, 1.D-3/
C
      call HI ('BAYBARS')
C     !BEG
      call COMPD   (SR,S,SF12,IFLAG)
      if(IFLAG.eq.0) then
        MARK = BLANK
      else
        call COMPD (SR,S,SF3 ,IFLAG)
        call MARKI (IFLAG,0,MARK,MINUS,DOLLAR)
      end if
C     !END
      call BYE ('BAYBARS')
C
      return
      end
