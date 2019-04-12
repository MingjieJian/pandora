      subroutine DOUGLAS
     $(NO,NL,IB,IE,QAR,LINE)
C
C     Rudolf Loeser, 1982 Aug 06
C---- Writes a level heading, for TOAD.
C     !DASH
      save
C     !DASH
      integer IB, IE, NL, NO
      character LINE*120, QAR*10
C     !DASH
      external LINER, DUNE, DAZZLE, HI, BYE
C
C               QAR(16)
      dimension QAR(*)
C
      call HI ('DOUGLAS')
C     !BEG
      call LINER  (2,NO)
      call DUNE   (NL,IB,IE,QAR,LINE,NO)
      call DAZZLE (IB,IE,QAR,LINE,NO)
      call LINER  (1,NO)
C     !END
      call BYE ('DOUGLAS')
C
      return
      end
