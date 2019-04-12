      subroutine CHITA
     $(I,N,WH,PAR,D,E,S,R)
C
C     Rudolf Loeser, 1989 Jun 28
C---- Computes the non-zero terms in row I, for PHI
C     operator calculation.
C     (See also YAKUT.)
C     !DASH
      save
C     !DASH
      real*8 D, E, R, S, WH
      integer I, N
      logical PAR
C     !DASH
      external ZIYA, GYDAN, TAYMYR, HI, BYE
C
C               WH(N,N), D(N), R(N), E(N), S(N)
      dimension WH(N,*), D(*), R(*), E(*), S(*)
C
      call HI ('CHITA')
C     !BEG
C---- Do diagonal term
      call ZIYA   (I,N,PAR,WH,D,E,R)
C---- Go to the left
      call GYDAN  (I,N,PAR,WH,D,E,S,R)
C---- Go to the right
      call TAYMYR (I,N,PAR,WH,D,E,S,R)
C     !END
      call BYE ('CHITA')
C
      return
      end
