      subroutine TURTLE
     $(JU,JL,IB,IE,KIJ,QAR,LINE,NO)
C
C     Rudolf Loeser, 1982 Aug 06
C---- Prints a transition header, for LIZARD.
C     (This is version 4 of TURTLE.)
C     !DASH
      save
C     !DASH
      integer IB, IE, JL, JU, KIJ, NO
      character LINE*120, QAR*10
C     !DASH
      external LINER, HANS, HOLME, HI, BYE
C
C               JU(MUL), JL(MUL), KIJ(MUL), QAR(16)
      dimension JU(*),   JL(*),   KIJ(*),   QAR(*)
C
      call HI ('TURTLE')
C     !BEG
      call LINER (2,NO)
      call HANS  (NO,IB,IE,JU,JL,    QAR,LINE)
      call HOLME (NO,IB,IE,JU,JL,KIJ,QAR,LINE)
      call LINER (1,NO)
C     !END
      call BYE ('TURTLE')
C
      return
      end
