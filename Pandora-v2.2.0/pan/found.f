      subroutine FOUND
     $(N,TE,M,BRIGHT,IMAGE,MUX)
C
C     Rudolf Loeser, 1980 Dec 11
C---- Initializes the spectrum summary graph.
C     (This is version 2 of FOUND.)
C     !DASH
      save
C     !DASH
      real*8 BRIGHT, TE, X
      integer I, LINC, M, MUX, N
      character IMAGE*(*), PERIOD*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(42),PERIOD)
C     !DASH
      external HALIBUT, LINK, HI, BYE
C
C               TE(N), BRIGHT(M), MUX(M)
      dimension TE(*), BRIGHT(*), MUX(*)
C
      call HI ('FOUND')
C     !BEG
C---- Set up plot image
      call HALIBUT (MUX,TE,BRIGHT,N,M,IMAGE)
C
C---- Enter TE curve
      LINC = 1
      do 100 I = 1,N
        X = I
        call LINK  (IMAGE,X,TE(I),PERIOD,LINC)
  100 continue
C     !END
      call BYE ('FOUND')
C
      return
      end
