      subroutine GIGUE
     $(F,R,N,Z,KZERO,FREF)
C
C     Rudolf Loeser, 1980 Nov 03
C---- Finds reference ratio values, for FLIVVER.
C     !DASH
      save
C     !DASH
      real*8 F, FREF, ONE, R, RF1, Z
      integer KZERO, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, MOVE1, CONMUL, LOTUS, HI, BYE
C
C               F(N), R(N), Z(N)
      dimension F(*), R(*), Z(*)
C
      call HI ('GIGUE')
C     !BEG
      call MOVE1  (F,N,R)
      call DIVIDE (ONE,F(1),RF1)
      call CONMUL (RF1,R,N)
      call LOTUS  (N,Z,R,KZERO,FREF)
C     !END
      call BYE ('GIGUE')
C
      return
      end
