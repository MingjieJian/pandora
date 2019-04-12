      subroutine GIG
     $(R,I,N,NL,IU,IL,CIJ,Z,GMI,PE,FE,BTA,EP,EF,BS)
C
C     Rudolf Loeser, 1979 Sep 07
C---- Sets up printline contents, for FIG.
C     !DASH
      save
C     !DASH
      real*8 BS, BTA, CIJ, EF, EP, FE, GLU, GMI, PE, R, Z
      integer I, IL, IU, IUL, N, NL
C     !DASH
      external INDXIJ, DIVIDE, HI, BYE
C
C               R(11), CIJ(N,NL**2), BTA(N), GMI(N,NSL), BS(N), EF(N),
      dimension R(*),  CIJ(N,*),     BTA(*), GMI(N,*),   BS(*), EF(*),
C
C               PE(N), FE(N), EP(N), Z(NL,NL)
     $          PE(*), FE(*), EP(*), Z(NL,*)
C
      call HI ('GIG')
C     !BEG
      call INDXIJ (IU, IL, IUL)
      call DIVIDE (GMI(I,IL), GMI(I,IU), GLU)
C
      R( 1) = CIJ(I,IUL)
      R( 2) = Z(IU,IL)
      R( 3) = PE(I)
      R( 4) = GLU*Z(IL,IU)
      R( 5) = FE(I)
      R( 6) = GLU
      R( 7) = Z(IL,IU)
      R( 8) = BTA(I)
      R( 9) = EP(I)
      R(10) = EF(I)
      R(11) = BS(I)
C     !END
      call BYE ('GIG')
C
      return
      end
