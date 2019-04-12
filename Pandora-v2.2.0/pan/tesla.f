      subroutine TESLA
     $(N,K,XRD,YRD,S,SLR,IMG,FO)
C
C     Rudolf Loeser, 2005 Jan 13
C---- Computes modified source function, SLR, a PRD term.
C     !DASH
      save
C     !DASH
      real*8 FO, S, SLR, XRD, YRD
      integer I, IMG, J, K, N
C     !DASH
      external BEZANT, HI, BYE
C
C               XRD(N,K), YRD(N,K), S(N), SLR(N,K), IMG(N), FO(N)
      dimension XRD(N,*), YRD(N,*), S(*), SLR(N,*), IMG(*), FO(*)
C
      call HI ('TESLA')
C     !BEG
      do 101 I = 1,N
        do 100 J = 1,K
          SLR(I,J) = XRD(I,J)*S(I)+YRD(I,J)
  100   continue
  101 continue
C
C---- Edit out negatives, if any
      call BEZANT (N, K, SLR, 'SLR', IMG, FO)
C     !END
      call BYE ('TESLA')
C
      return
      end
