      subroutine KNIFE
     $(NO,IB,IE,N,NL,BDIJ)
C
C     Rudolf Loeser, 1984 Feb 14
C---- Prints BDIJ, for INKY.
C     (This is version 3 of KNIFE.)
C     !DASH
      save
C     !DASH
      real*8 BDIJ
      integer IB, IE, N, NL, NO
C     !DASH
      external LINER, KHMER, HI, BYE
C
C               BDIJ(N,NL-1)
      dimension BDIJ(*)
C
      call HI ('KNIFE')
C     !BEG
      call LINER (1,NO)
      call KHMER (NO,2,NL,N,IB,IE,BDIJ,'/B1')
C     !END
      call BYE ('KNIFE')
C
      return
      end
