      subroutine MAKO
     $(DEE,DEEL,DMIN,DMAX)
C
C     Rudolf Loeser, 2001 Dec 27
C---- Finds extrema, for CONTA.
C     (This is version 2 of MAKO.)
C     !DASH
      save
C     !DASH
      real*8 DEE, DEEL, DMAX, DMIN, ZERO
      integer I
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
C               DEE(4,5), DEEL(4,5)
      dimension DEE(*),   DEEL(*)
C
      call HI ('MAKO')
C     !BEG
      do 100 I = 1,20
        if(DEE(I).ne.ZERO) then
          DMIN = min(DMIN,DEEL(I))
          DMAX = max(DMAX,DEEL(I))
        end if
  100 continue
C     !END
      call BYE ('MAKO')
C
      return
      end
