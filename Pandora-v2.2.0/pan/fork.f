      subroutine FORK
     $(NO,IB,IE,N,NL,BDI)
C
C     Rudolf Loeser, 1984 Feb 13
C---- Prints BDI, for INKY.
C     (This is version 2 of FORK.)
C     !DASH
      save
C     !DASH
      real*8 BDI
      integer IB, IE, N, NL, NO
C     !DASH
      external LINER, KHMER, HI, BYE
C
C               BDI(N,NL)
      dimension BDI(*)
C
      call HI ('FORK')
C     !BEG
      call LINER (1,NO)
      call KHMER (NO,1,NL,N,IB,IE,BDI,'   ')
C     !END
      call BYE ('FORK')
C
      return
      end
