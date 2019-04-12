      subroutine LEDGE
     $(N,NL,NSL,TE,SA,GM,RL,CK,HND,XNE,CQSI,DRCT)
C
C     Rudolf Loeser, 2001 May 31
C---- Adds dielectronic recombination to QS(1).
C     (This is version 2 of LEDGE.)
C     !DASH
      save
C     !DASH
      real*8 CK, CQSI, DRCT, GM, HND, RL, SA, TE, XNE, dummy
      integer I, N, NL, NSL
C     !DASH
      external RAGOUT, HI, BYE
C
C               CQSI(N), DRCT(N), XNE(N), HND(N), CK(N), RL(N), GM(N),
      dimension CQSI(*), DRCT(*), XNE(*), HND(*), CK(*), RL(*), GM(*),
C
C               SA(N), TE(N),
     $          SA(*), TE(*)
C
      call HI ('LEDGE')
C     !BEG
      do 100 I = 1,N
        call RAGOUT (I,N,1,NL,NSL,TE,SA,GM,RL,CK,HND,XNE,dummy,DRCT(I))
        CQSI(I) = CQSI(I)+DRCT(I)
  100 continue
C     !END
      call BYE ('LEDGE')
C
      return
      end
