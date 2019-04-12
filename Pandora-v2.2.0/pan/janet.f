      subroutine JANET
     $(LU,N,S,SD,VEC)
C
C     Rudolf Loeser, 1999 Dec 29
C---- Compares S with and without GNV.
C     (This is version 2 of JANET.)
C     !DASH
      save
C     !DASH
      real*8 S, SD, VEC
      integer LU, N
C     !DASH
      external ABJECT, LINER, TAKE, HI, BYE
C
C               S(N), SD(N), VEC(N)
      dimension S(*), SD(*), VEC(*)
C
      call HI ('JANET')
C     !BEG
      call ABJECT (LU)
      write (LU,100)
  100 format(' ','Comparison of Source Functions computed with and ',
     $           'without GNV')
      call LINER  (2,LU)
      call TAKE   (LU,N,SD,'S(GNV=0)',S,'S(with GNV)',VEC)
C     !END
      call BYE ('JANET')
C
      return
      end
