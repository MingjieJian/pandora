      subroutine KEEN
     $(X,YSML,YBIG)
C
C     Rudolf Loeser, 1982 Apr 22
C---- Gets limits, for KEEL.
C     !DASH
      save
C     !DASH
      real*8 X, YBIG, YSML, ZERO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  HI, BYE
      intrinsic min, max
C
      call HI ('KEEN')
C     !BEG
      if(X.ne.ZERO) then
        YBIG = max(YBIG,X)
        YSML = min(YSML,X)
      end if
C     !END
      call BYE ('KEEN')
C
      return
      end
