      subroutine CLACK
     $(ARR,QAR,KNT)
C
C     Rudolf Loeser, 1985 Jun 19
C---- Converts LDL into Line Components description, for LIZARD.
C     !DASH
      save
C     !DASH
      real*8 ARR
      integer I, KNT, LDL
      character BLANK*1, QAR*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external HI, BYE
C
C               ARR(8), QAR(16)
      dimension ARR(*), QAR(*)
C
      call HI ('CLACK')
C     !BEG
      do 102 I = 1,KNT
        LDL = ARR(I)
        QAR(I) = BLANK
C
        if(LDL.eq.1) then
          QAR(I) = '    Single'
        else if((LDL.gt.1).and.(LDL.lt.10)) then
          QAR(I) = '  Blend( )'
          write (QAR(I)(9:9),100) LDL
  100     format(I1)
        else if((LDL.gt.9).and.(LDL.lt.100)) then
          QAR(I) = ' Blend(  )'
          write (QAR(I)(8:9),101) LDL
  101     format(I2)
        end if
C
  102 continue
C     !END
      call BYE ('CLACK')
C
      return
      end
