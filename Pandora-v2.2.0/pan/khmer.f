      subroutine KHMER
     $(NO,JB,JE,N,IB,IE,A,TIT)
C
C     Rudolf Loeser, 1984 Feb 13
C---- Printing utility, for INKY.
C     (This is version 2 of KHMER.)
C     !DASH
      save
C     !DASH
      real*8 A
      integer I, IB, IE, J, JB, JE, JINC, N, NO
      character BLANK*1, TIT*3
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  HI, BYE
C
      dimension A(N,*)
C
      call HI ('KHMER')
C     !BEG
      JINC = 0
      if(TIT(1:1).ne.BLANK) then
        JINC = -1
      end if
C
      do 101 J = JB,JE
        write (NO,100) J,TIT,(A(I,J+JINC),I=IB,IE)
  100   format(' ','B',I2,A3,' ',1P10E12.4)
  101 continue
C     !END
      call BYE ('KHMER')
C
      return
      end
