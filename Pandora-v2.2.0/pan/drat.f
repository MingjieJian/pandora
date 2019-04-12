      subroutine DRAT
     $(A,B,DIF,RAT)
C
C     Rudolf Loeser, 2006 Sep 20
C---- Sets up controlled values of (A-B) and (A/B).
C     (This is version 2 of DRAT.)
C     !DASH
      save
C     !DASH
      real*8 A, B, DELTA, DIF, ONE, RAT, ZERO
      integer IFLG
      logical SAME
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  COMPD, DIVIDE, HI, BYE
      intrinsic sign
C
      data DELTA /1.D-14/
C
      call HI ('DRAT')
C     !BEG
      call COMPD    (A, B, DELTA, IFLG)
C
      if(IFLG.eq.0) then
        SAME = sign(ONE,A).eq.sign(ONE,B)
        if(SAME) then
          DIF =  ZERO
          RAT = +ONE
        else
          DIF =  A-B
          RAT = -ONE
        end if
      else
C
        DIF = A-B
        call DIVIDE (A, B, RAT)
      end if
C     !END
      call BYE ('DRAT')
C
      return
      end
