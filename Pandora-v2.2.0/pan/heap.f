      subroutine HEAP
     $(N,Z,F,VEC,FRS)
C
C     Rudolf Loeser, 1980 Feb 26
C---- Integrates net rates.
C     !DASH
      save
C     !DASH
      real*8 F, FRS, VEC, Z
      integer N
C     !DASH
      external GUTI, BUSH, MOVE1, CATRIN, HI, BYE
C
C               F(N), VEC(N), FRS(N), Z(N)
      dimension F(*), VEC(*), FRS(*), Z(*)
C
      call HI ('HEAP')
C     !BEG
      call GUTI   (F,FRS,F,N)
      call BUSH   (Z,1,F,1,VEC,1,N)
      call MOVE1  (VEC,N,F)
      call CATRIN (F,N)
C     !END
      call BYE ('HEAP')
C
      return
      end
