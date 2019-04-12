      subroutine BULB
     $(N,NLH,SA,BDI,HNI,SAB,SUM)
C
C     Rudolf Loeser, 1978 Aug 18
C---- Computes intermediates, for SHERBET.
C     !DASH
      save
C     !DASH
      real*8 BDI, HNI, SA, SAB, SUM
      integer N, NLH
C     !DASH
      external ROWSUM, ARRMUL, HI, BYE
C
C               BDI(N,NLH), SA(N), SAB(N), HNI(N,NLH), SUM(N)
      dimension BDI(N,*),   SA(*), SAB(*), HNI(*),     SUM(*)
C
      call HI ('BULB')
C     !BEG
      call ARRMUL (SA,BDI(1,1),SAB,N)
      call ROWSUM (HNI,N,N,2,NLH,SUM)
C     !END
      call BYE ('BULB')
C
      return
      end
