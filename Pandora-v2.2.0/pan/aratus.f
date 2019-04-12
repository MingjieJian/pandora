      subroutine ARATUS
     $(M,RC1213,ABISO)
C
C     Rudolf Loeser, 1992 Dec 09
C---- Computes isotope abundance factor, for CO opacity calculation.
C     (This is version 2 of ARATUS.)
C     !DASH
      save
C     !DASH
      real*8 ABISO, ONE, RC1213
      integer M
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
      call HI ('ARATUS')
C     !BEG
      ABISO = ONE
      if(M.eq.1) then
        ABISO = ONE+ONE/RC1213
      else
        ABISO = ONE+RC1213
      end if
C     !END
      call BYE ('ARATUS')
C
      return
      end
