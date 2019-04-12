      subroutine CLOOCK
     $(ARR,QAR,KNT)
C
C     Rudolf Loeser, 1996 Feb 21
C---- Converts RIJ into Single Rates switch description, for LIZARD.
C     !DASH
      save
C     !DASH
      real*8 ARR
      integer I, KNT, LRIJ
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
      call HI ('CLOOCK')
C     !BEG
      do 100 I = 1,KNT
        LRIJ = ARR(I)
        if(LRIJ.eq.1) then
          QAR(I) = '       net'
        else if(LRIJ.eq.2) then
          QAR(I) = '    single'
        end if
  100 continue
C     !END
      call BYE ('CLOOCK')
C
      return
      end
