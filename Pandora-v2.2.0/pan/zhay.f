      subroutine ZHAY
     $(N,K,EP,TF,TG,XK2,FRD,GRD,XRD,ZRD,YRD)
C
C     Rudolf Loeser, 2005 Mar 31
C---- Computes FRD, GRD, XRD, ZRD, and YRD, for PRD.
C     (This is version 2 of ZHAY.)
C     !DASH
      save
C     !DASH
      real*8 DIV, EP, FRD, GRD, OEP, ONE, TF, TG, XK2, XRD, YRD, ZRD
      integer I, J, K, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, HI, BYE
C
C               XK2(N,K), FRD(N,K), GRD(N,K), XRD(N,K), YRD(N,K),
      dimension XK2(N,*), FRD(N,*), GRD(N,*), XRD(N,*), YRD(N,*),
C
C               EP(N), TF(N), TG(N), ZRD(N,K)
     $          EP(*), TF(*), TG(*), ZRD(N,*)
C
      call HI ('ZHAY')
C     !BEG
      do 101 I = 1,N
        OEP = ONE+EP(I)
        do 100 J = 1,K
          FRD(I,J) =  XK2(I,J)*TF(I)
          GRD(I,J) = -XK2(I,J)*TG(I)
          DIV = OEP+GRD(I,J)
          call DIVIDE (OEP,      DIV, XRD(I,J))
          call DIVIDE (GRD(I,J), DIV, ZRD(I,J))
          call DIVIDE (FRD(I,J), DIV, YRD(I,J))
  100   continue
  101 continue
C     !END
      call BYE ('ZHAY')
C
      return
      end
