      subroutine PLOP
     $(FUDGE,LTYP,LINE)
C
C     Rudolf Loeser, 1978 May 28
C---- Encodes opacity multiplier, for printing.
C     (This is version 2 of PLOP.)
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CRIT, D, FUDGE, ONE, TEN
      integer LTYP
      logical ISLINE, lummy
      character BLANK*1, LAB*1, LINE*6
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT(11),TEN   )
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  PADDLE, HI, BYE
      intrinsic abs
C
      data CRIT /1.D-6/
      data A,B,C,D /1.D-3, 1.D2, 1.D3, 1.D4/
C     !EJECT
C
      call HI ('PLOP')
C     !BEG
      if(abs(FUDGE-ONE).lt.CRIT) then
        LINE = BLANK
      else
        call PADDLE (LTYP,ISLINE,lummy)
        if(ISLINE) then
          LAB = ALPHS(12)
        else
          LAB = BLANK
        end if
        if((FUDGE.ge.A).and.(FUDGE.lt.TEN)) then
          write (LINE,100) FUDGE,LAB
  100     format(F5.3,A1)
        else if(FUDGE.lt.B) then
          write (LINE,101) FUDGE,LAB
  101     format(F5.2,A1)
        else if(FUDGE.lt.C) then
          write (LINE,102) FUDGE,LAB
  102     format(F5.1,A1)
        else if(FUDGE.lt.D) then
          write (LINE,103) FUDGE,LAB
  103     format(F5.0,A1)
        else
          LINE = ' ----'//LAB
        end if
      end if
C     !END
      call BYE ('PLOP')
C
      return
      end
