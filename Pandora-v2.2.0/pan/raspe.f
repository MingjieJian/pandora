      subroutine RASPE
     $(NFL,FNRML,NO)
C
C     Rudolf Loeser, 2007 Mar 26
C---- Weighting the ends of FNRML to force them to approach 1
C     !DASH
      save
C     !DASH
      real*8 FNRML, ONE, W
      integer K, NFL, NO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external VECOUT, HI, BYE
C
C               FNRML(NFL)
      dimension FNRML(*)
C
      dimension W(2)
C
      data W /1.D-1, 5.D-1/
C
      call HI ('RASPE')
C     !BEG
      K = 1
      FNRML(K  ) = ONE
      FNRML(K+1) = W(1)*FNRML(K+1)+(ONE-W(1))
      FNRML(K+2) = W(2)*FNRML(K+2)+(ONE-W(2))
C
      K = NFL
      FNRML(K  ) = ONE
      FNRML(K-1) = W(1)*FNRML(K-1)+(ONE-W(1))
      FNRML(K-2) = W(2)*FNRML(K-2)+(ONE-W(2))
C
      call VECOUT (NO, FNRML, NFL, 'weighted FNRML')
C     !END
      call BYE ('RASPE')
C
      return
      end
