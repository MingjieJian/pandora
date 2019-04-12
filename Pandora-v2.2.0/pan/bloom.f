      subroutine BLOOM
     $(NSL,NPQ,LRQ,NLPAIR)
C
C     Rudolf Loeser, 2006 Apr 25
C---- Sets up and checks principal quantum numbers.
C     !DASH
      save
C     !DASH
      integer J, LRQ, NLPAIR, NPQ, NSL
C     !DASH
      external GAUR, HI, BYE
C
C               NLPAIR(2,NSL), NPQ(NSL), LRQ(NSL)
      dimension NLPAIR(2,*),   NPQ(*),   LRQ(*)
C
      call HI ('BLOOM')
C     !BEG
      do 100 J = 1,NSL
        NPQ(J) = NLPAIR(1,J)
        LRQ(J) = NLPAIR(2,J)
  100 continue
C
      call GAUR (LRQ, NPQ, NSL)
C     !END
      call BYE ('BLOOM')
C
      return
      end
