      subroutine DAHLIA
     $(QNAME,LZA,ZAUX)
C
C     Rudolf Loeser, 1972 Feb 01
C---- Reads auxiliary Z tables.
C     !DASH
      save
C     !DASH
      real*8 ZAUX
      integer LZA, LZM, NAUX
      character QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(13),LZM)
C     !DASH
      external  MINT, BASIL, HI, BYE
      intrinsic abs
C
C               LZA(LZM), ZAUX(LZM,NZM)
      dimension LZA(*),   ZAUX(LZM,*)
C
      call HI ('DAHLIA')
C     !BEG
      call MINT  (QNAME,NAUX)
      call BASIL (ZAUX(1,NAUX),(abs(LZA(NAUX))),QNAME)
C     !END
      call BYE ('DAHLIA')
C
      return
      end
