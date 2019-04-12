      subroutine HOLME
     $(NO,IB,IE,JU,JL,KIJ,QAR,LINE)
C
C     Rudolf Loeser, 1982 AUG 82
C---- Prints transition descriptions.
C     !DASH
      save
C     !DASH
      integer IB, IE, JL, JU, KIJ, M, NO
      character BLANK*1, LINE*120, QAR*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external SETC, FUSSY, LINER, FRUG, HI, BYE
C
C               JU(MUL), JL(MUL), KIJ(MUL), QAR(16)
      dimension JU(*),   JL(*),   KIJ(*),   QAR(*)
C
      call HI ('HOLME')
C     !BEG
      call SETC    (QAR,1,8,BLANK)
C
      M = 0
      call FUSSY   (JU,JL,IB,IE,KIJ,QAR,M)
      if(M.gt.0) then
        call LINER (1,NO)
        call FRUG  (BLANK,QAR,M,LINE,NO)
      end if
C     !END
      call BYE ('HOLME')
C
      return
      end
