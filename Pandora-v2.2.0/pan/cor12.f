      subroutine COR12
     $(K,M,G)
C
C     Rudolf Loeser, 1992 Sep 29
C---- Computes the function G, for the Carbon-12 isotope, for COR.
C     !DASH
      save
C     !DASH
      real*8 A1, A2, A3, B1, B2, B3, EM2, G, ONE, ZERO
      integer K, M
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
      external  HI, BYE
      intrinsic max
C
      data A1,A2,A3 /1.206D-2, -2.11D-4, 1.05D-8/
      data B1,B2,B3 /7.054D-3, -2.79D-4, 1.86D-8/
C
      call HI ('COR12')
C     !BEG
      EM2 = M**2
C
      if(K.le.0) then
        G = A1*(ONE+EM2*(A2+A3*EM2))
      else
        G = B1*(ONE+EM2*(B2+B3*EM2))
      end if
C
      G = max(G,ZERO)
C     !END
      call BYE ('COR12')
C
      return
      end
