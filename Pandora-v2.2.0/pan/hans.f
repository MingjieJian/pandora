      subroutine HANS
     $(NO,IB,IE,JU,JL,QAR,LINE)
C
C     Rudolf Loeser, 1982 Aug 06
C---- Writes transition numbers.
C     !DASH
      save
C     !DASH
      integer I, IB, IE, JL, JU, K, NO
      character BLANKS*10, DOTS*10, LINE*120, QAR*16
C     !DASH
      external SETC, FRUG, HI, BYE
C
C               JU(MUL), JL(MUL), QAR(16)
      dimension JU(*),   JL(*),   QAR(*)
C
      data BLANKS, DOTS /'          ', '..........'/
C
      call HI ('HANS')
C     !BEG
      QAR(1) = DOTS
      call SETC (QAR(2),1,7,BLANKS)
C
      K = 1
      do 101 I = IB,IE
        K = K+1
        write (QAR(K),100) JU(I),JL(I)
  100   format(5X,I2,'/',I2)
  101 continue
C
      call FRUG ('Transitions',QAR,K,LINE,NO)
C     !END
      call BYE ('HANS')
C
      return
      end
