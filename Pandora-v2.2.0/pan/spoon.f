      subroutine SPOON
     $(NO,IB,IE,N,NL,NSL,BDX)
C
C     Rudolf Loeser, 1984 Feb 14
C---- Prints BDX, for INKY.
C     (This is version 3 of SPOON.)
C     !DASH
      save
C     !DASH
      real*8 BDX
      integer IB, IE, N, NL, NO, NSL
C     !DASH
      external LINER, KHMER, HI, BYE
C
C               BDX(N,NSL)
      dimension BDX(*)
C
      call HI ('SPOON')
C     !BEG
      call LINER (1,NO)
      call KHMER (NO,(NL+1),NSL,N,IB,IE,BDX,'   ')
C     !END
      call BYE ('SPOON')
C
      return
      end
