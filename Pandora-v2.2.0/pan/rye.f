      subroutine RYE
     $(N,PJ,XNUJ,TE,GMJ)
C
C     Rudolf Loeser, 1978 May 03
C---- Computes GM of level J.
C     !DASH
      save
C     !DASH
      real*8 GMJ, HNUKT, PJ, SE, TE, XNUJ
      integer I, N
C     !DASH
      external PROD, HI, BYE
C
C               TE(N), GMJ(N)
      dimension TE(*), GMJ(*)
C
      call HI ('RYE')
C     !BEG
      do 100 I = 1,N
        call PROD (TE(I), XNUJ, 1, HNUKT, SE)
        GMJ(I) = PJ*SE
  100 continue
C     !END
      call BYE ('RYE')
C
      return
      end
