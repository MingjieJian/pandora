      subroutine PHOEBE
     $(INC,QLABEL)
C
C     Rudolf Loeser, 1986 Feb 26
C---- Makes an ion label.
C     !DASH
      save
C     !DASH
      integer INC, IONST, JONST
      character BLANK*1, MINUS*1, QELSM*8, QLABEL*8
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 56),IONST)
      equivalence (QZQ(  2),QELSM)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(40),MINUS )
C
C---- ROMAN       as of 1984 Apr 24
      character   ROMAN*5
      dimension   ROMAN(21)
      common      /ROMAN/ ROMAN
C     Roman numerals.
C     .
C     !DASH
      external HI, BYE
C     !EJECT
C
      call HI ('PHOEBE')
C     !BEG
      JONST = IONST+INC
C
      if(JONST.eq.1) then
        QLABEL = QELSM(1:2)//BLANK
C
      else if((JONST.gt.1).and.(JONST.lt.21)) then
        QLABEL = QELSM(1:2)//MINUS//ROMAN(JONST)
C
      else if((JONST.gt.-1000).and.(JONST.lt.10000)) then
        write (QLABEL,100) QELSM(1:2),JONST
  100   format(A2,'[',I4,']')
C
      else
        QLABEL = QELSM(1:2)//'[ ?? ]'
      end if
C
      if(QLABEL(2:2).eq.BLANK) then
        QLABEL = QLABEL(1:1)//QLABEL(3:8)//BLANK
      end if
C     !END
      call BYE ('PHOEBE')
C
      return
      end
