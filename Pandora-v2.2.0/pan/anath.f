      subroutine ANATH
     $(NL,QNAME,AIJ,IU,IL)
C
C     Rudolf Loeser, 1999 Oct 28
C---- Reads Einstein A value.
C     !DASH
      save
C     !DASH
      real*8 AIJ
      integer IL, IU, NL, jummy
      character QNAME*8
C     !DASH
      external MACE, ARRAN, HI, BYE
C
C               AIJ(NL,NL)
      dimension AIJ(NL,*)
C
      call HI ('ANATH')
C     !BEG
      call MACE
      call ARRAN  (1, AIJ(IU,IL), jummy, 1, QNAME)
C     !END
      call BYE ('ANATH')
C
      return
      end
