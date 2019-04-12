      subroutine OCTA
     $(X,N,NVX,FRS,HND,VX,RML,ABDEL,NMLR,VREQ,HREQ)
C
C     Rudolf Loeser, 1983 May 19
C---- Computes mass loss rates.
C     !DASH
      save
C     !DASH
      real*8 ABDEL, FRS, HND, HREQ, RML, VREQ, VX, X
      integer J, N, NMLR, NVX
C     !DASH
      external AUTUN, AGUE, HI, BYE
C
      dimension X(*)
C
C               VX(N,NVX), RML(N,NVX), VREQ(N,NVX), HREQ(N,NVX), FRS(N),
      dimension VX(N,*),   RML(N,*),   VREQ(N,*),   HREQ(N,*),   FRS(*),
C
C               HND(N), ABDEL(N)
     $          HND(*), ABDEL(*)
C
      call HI ('OCTA')
C     !BEG
      do 100 J = 1,NVX
        call AUTUN (N, X, FRS, HND, VX(1,J), ABDEL, RML(1,J))
        call AGUE  (N,    FRS, HND, NMLR, VX(1,J), VREQ(1,J), HREQ(1,J))
  100 continue
C     !END
      call BYE ('OCTA')
C
      return
      end
