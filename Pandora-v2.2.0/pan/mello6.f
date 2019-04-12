      subroutine MELLO6
     $(Y,A,B,C,D,E,GAMMA,NT,DX,X,Z)
C
C     Rudolf Loeser, 2006 Aug 11
C---- Computes a value of GAMMA for MELILLA.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CQ, D, DX, E, GAMMA, ONE, X, X2, X3, X4, XL, Y,
     $       YM, Z
      integer I, NT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HART, HI, BYE
C
C               X(NT), Z(NT)
      dimension X(*),  Z(*)
C
      call HI ('MELLO6')
C     !BEG
      X(1) = ONE-DX
      do 100 I = 2,NT
        X(I) = X(I-1)+DX
        X2   = X(I)**2
        X3   = X(I)*X2
        XL   = X(I)
        YM   = exp(-Y*X(I))
        Z(I) = (A+B/X(I)+C/X2+D/X3+E*XL)*YM
  100 continue
C
      call HART ((NT-1), X(2), Z(2), CQ)
C
      YM    = exp(Y)
      GAMMA = (Y*YM)*CQ
C     !END
      call BYE ('MELLO6')
C
      return
      end
