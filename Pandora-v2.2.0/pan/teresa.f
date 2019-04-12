      subroutine TERESA
     $(X,W,N,L,Z,ZZ,F,FF,R,RR,LABEL,IMG)
C
C     Rudolf Loeser, 2006 Dec 20
C---- Computes R = integral(F) over Z, for H.S.E., when L > 1.
C     !DASH
      save
C     !DASH
      real*8 F, FF, R, RR, W, X, Z, ZZ
      integer IMG, L, M, N
      character LABEL*100
C     !DASH
      external MOVE1, REVERSD, NEGATE, XAVIER, HI, BYE
C
      dimension X(*), W(*)
C
C               IMG(N), ZZ(N), FF(N), RR(N), Z(N), F(N), R(N)
      dimension IMG(*), ZZ(*), FF(*), RR(*), Z(*), F(*), R(*)
C
      call HI ('TERESA')
C     !BEG
      M = (N+1)-L
C
      call MOVE1   (Z, N, ZZ)
      call MOVE1   (F, N, FF)
      call REVERSD (ZZ, 1, N)
      call REVERSD (FF, 1, N)
      call NEGATE  (ZZ, N)
      call NEGATE  (FF, N)
      LABEL(88:) = 'negative part'
      call XAVIER  (X, W, L, ZZ(M), FF(M), RR(M), LABEL, IMG)
      call REVERSD (RR, 1, N)
      call MOVE1   (RR, L, R)
C
      LABEL(88:) = 'positive part'
      call XAVIER  (X, W, M, Z(L), F(L), R(L), LABEL, IMG)
C     !END
      call BYE ('TERESA')
C
      return
      end
