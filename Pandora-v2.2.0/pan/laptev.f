      subroutine LAPTEV
     $(N,D,E,P)
C
C     Rudolf Loeser, 1989 Jun 30
C---- Computes auxiliaries for "GR" weight matrix calculation.
C     (This is version 2 of LAPTEV.)
C     !DASH
      save
C     !DASH
      real*8 D, E, P
      integer J, N
C     !DASH
      external QEXP4, HI, BYE
C
C               D(N), E(N), P(N)
      dimension D(*), E(*), P(*)
C
      call HI ('LAPTEV')
C     !BEG
      do 100 J = 1,N
        call QEXP4 (D(J),E(J),1,P(J))
  100 continue
C     !END
      call BYE ('LAPTEV')
C
      return
      end
