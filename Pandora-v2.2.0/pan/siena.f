      subroutine SIENA
     $(NDW,N,IZ)
C
C     Rudolf Loeser, 1992 Mar 24
C---- Gets Z-indices, for YING.
C     (This is version 3 of SIENA.)
C     !DASH
      save
C     !DASH
      integer IZ, N, NDW
C     !DASH
C
C               IZ(5)
      dimension IZ(*)
C
      call HI ('SIENA')
C     !BEG
      IZ(3) = NDW
C
      IZ(1) = 1
      IZ(5) = N
      IZ(2) = (IZ(3)+IZ(1))/2
      IZ(4) = (IZ(3)+IZ(5))/2
C     !END
      call BYE ('SIENA')
C
      return
      end
