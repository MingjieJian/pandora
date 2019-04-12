      subroutine CILENTO
     $(INTG,N,QNAME)
C
C     Rudolf Loeser, 1979 Jun 04
C---- Reads integer vectors.
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer INTG, N
      character QNAME*8
C     !DASH
      external MACE, ARRAN, HI, BYE
C
C               INTG(N)
      dimension INTG(*)
C
      call HI ('CILENTO')
C     !BEG
      call MACE
      call ARRAN (2, dummy, INTG, N, QNAME)
C     !END
      call BYE ('CILENTO')
C
      return
      end
