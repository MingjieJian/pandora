      subroutine GAFF
     $(Z,TE,COND,VEC)
C
C     Rudolf Loeser, 1980 Sep 26
C---- Computes approximate conductive flux gradient, for VILA.
C     !DASH
      save
C     !DASH
      real*8 COND, FAC, R, SEVEN, TE, TWO, VEC, Z
      integer I, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 8),SEVEN )
C     !DASH
      external DERIV2, CONMUL, HI, BYE
C
C               TE(N), COND(N), VEC(N), Z(N)
      dimension TE(*), COND(*), VEC(*), Z(*)
C
      data FAC /1.D-16/
C
      call HI ('GAFF')
C     !BEG
      R = SEVEN/TWO
      do 100 I = 1,N
        VEC(I) = TE(I)**R
  100 continue
      call DERIV2   (Z, VEC, COND, N)
C
      R = -FAC/R
      call CONMUL   (R, COND, N)
C     !END
      call BYE ('GAFF')
C
      return
      end
