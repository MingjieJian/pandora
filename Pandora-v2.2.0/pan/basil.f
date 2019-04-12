      subroutine BASIL
     $(A,N,QNAME)
C
C     Rudolf Loeser, 1968 Apr 19
C---- Reads double precision vectors.
C     !DASH
      save
C     !DASH
      real*8 A
      integer N, jummy
      character QNAME*8
C     !DASH
      external MACE, ARRAN, HI, BYE
C
C               A(N)
      dimension A(*)
C
      call HI ('BASIL')
C     !BEG
      call MACE
      call ARRAN (1, A, jummy, N, QNAME)
C     !END
      call BYE ('BASIL')
C
      return
      end
