      subroutine YAKUT
     $(I,N,WN,PAR,D,E,S,R,P)
C
C     Rudolf Loeser, 1981 Jun 21
C---- Computes the non-zero terms in row I for "GR" weight
C     matrix calculation.
C     (See also CHITA.)
C     !DASH
      save
C     !DASH
      real*8 D, E, P, R, S, WN
      integer I, N
      logical PAR
C     !DASH
      external ZEJA, ALTAI, BAIKAL, HI, BYE
C
C               WN(N,N), D(N), E(N), P(N), R(N), S(N)
      dimension WN(N,*), D(*), E(*), P(*), R(*), S(*)
C
      call HI ('YAKUT')
C     !BEG
C---- Do diagonal term
      call ZEJA   (I,N,PAR,WN,D,E,P)
C---- Go to the left
      call ALTAI  (I,N,PAR,WN,D,E,S,R)
C---- Go to the right
      call BAIKAL (I,N,PAR,WN,D,E,S,R)
C     !END
      call BYE ('YAKUT')
C
      return
      end
