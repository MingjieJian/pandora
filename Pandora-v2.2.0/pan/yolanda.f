      subroutine YOLANDA
     $(N,II)
C
C     Rudolf Loeser, 1982 Dec 14
C---- Sets up indices of curves to be plotted, for HARP.
C     (This is version 4 of YOLANDA.)
C     !DASH
      save
C     !DASH
      integer II, N
C     !DASH
      external HI, BYE
C
C               II(5)
      dimension II(*)
C
      call HI ('YOLANDA')
C     !BEG
      II(1) = 1
      II(5) = N
      II(3) = (II(1)+II(5))/2
      II(2) = (II(1)+II(3))/2
      II(4) = (II(3)+II(5))/2
C     !END
      call BYE ('YOLANDA')
C
      return
      end
