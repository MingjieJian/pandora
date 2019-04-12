      subroutine TRACK
     $(ARR,QAR,KNT)
C
C     Rudolf Loeser, 1980 Dec 28
C---- Converts "DPC" into symbolic damping component selector message,
C     for LIZARD.
C     (This is version 3 of TRACK.)
C     !DASH
      save
C     !DASH
      real*8 ARR
      integer I, IDC, KNT
      character BLANK*1, QAR*10, TAB*10
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
      dimension TAB(32)
C
      data      TAB /
     $  '     OOOOO', '     OOOO1', '     OOO1O', '     OOO11',
     $  '     OO1OO', '     OO1O1', '     OO11O', '     OO111',
     $  '     O1OOO', '     O1OO1', '     O1O1O', '     O1O11',
     $  '     O11OO', '     O11O1', '     O111O', '     O1111',
     $  '     1OOOO', '     1OOO1', '     1OO1O', '     1OO11',
     $  '     1O1OO', '     1O1O1', '     1O11O', '     1O111',
     $  '     11OOO', '     11OO1', '     11O1O', '     11O11',
     $  '     111OO', '     111O1', '     1111O', '     11111'/
C
      call HI ('TRACK')
C     !BEG
      do 100 I = 1,KNT
        QAR(I) = BLANK
C
        IDC = ARR(I)
        if((IDC.ge.1).and.(IDC.le.32)) then
          QAR(I) = TAB(IDC)
        end if
C
  100 continue
C     !END
      call BYE ('TRACK')
C
      return
      end
