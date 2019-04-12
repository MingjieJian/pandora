      subroutine RULFRY
     $(N,MRR,K,NRAD,SI,DI,XINT)
C
C     Rudolf Loeser, 2000 May 31
C---- Makes consolidated intensity arrays, for FLURRY.
C     !DASH
      save
C     !DASH
      real*8 DI, SI, XINT
      integer J, K, MRR, N, NRAD
C     !DASH
      external LIBYA, HI, BYE
C
C               DI(MRR,KM), SI(N,KM), XINT(NRAD,KM)
      dimension DI(MRR,*),  SI(N,*),  XINT(NRAD,*)
C
      call HI ('RULFRY')
C     !BEG
      do 100 J = 1,K
        call LIBYA (SI(1,J),N,DI(1,J),MRR,XINT(1,J),NRAD)
  100 continue
C     !END
      call BYE ('RULFRY')
C
      return
      end
