      subroutine BLOND
     $(NL, NPQ, LRQ)
C
C     Rudolf Loeser, 2004 Dec 10
C---- Sets up principal and rotational quantum numbers for Hydrogen.
C     !DASH
      save
C     !DASH
      integer J, LRQ, NL, NPQ
C     !DASH
      external HI, BYE
C
C               NPQ(NL), LRQ(NL)
      dimension NPQ(*),  LRQ(*)
C
      call HI ('BLOND')
C     !BEG
      do 100 J = 1,NL
        NPQ(J) =  J
        LRQ(J) = -1
  100 continue
C     !END
      call BYE ('BLOND')
C
      return
      end
