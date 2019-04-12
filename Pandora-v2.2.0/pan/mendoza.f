      subroutine MENDOZA
     $(NL,NPQ,LRQ,XNUK,XNU,P)
C
C     Rudolf Loeser, 2004 Dec 14
C---- Gets Hydrogen data, for KALT.
C     !DASH
      save
C     !DASH
      real*8 P, XNU, XNUK, dummy1, dummy2
      integer J, LRQ, NL, NPQ
C     !DASH
      external BLOND, HYDATA, RUBAN, HI, BYE
C
C               NPQ(NL), LRQ(NL), XNU(NL), P(NL)
      dimension NPQ(*),  LRQ(*),  XNU(*),  P(*)
C
      call HI ('MENDOZA')
C     !BEG
      call BLOND    (NL, NPQ, LRQ)
C
      call HYDATA   (0,      XNUK,   dummy1, dummy2)
      do 100 J = 1,NL
        call HYDATA (NPQ(J), XNU(J), dummy1, dummy2)
        call RUBAN  (NPQ(J), P(J))
  100 continue
C     !END
      call BYE ('MENDOZA')
C
      return
      end
