      subroutine YANEST
     $(GN,GO,RAT)
C
C     Rudolf Loeser, 2003 Dec 10
C---- Sets up RAT for AMNESTY.
C     !DASH
      save
C     !DASH
      real*8 GN, GO, R, XHI, XLO, ZERO
      character BLANK*1, RAT*16
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  HI, BYE
      intrinsic abs
C
      data XLO,XHI /1.D-5, 1.D5/
C
      call HI ('YANEST')
C     !BEG
      RAT = BLANK
      if(GO.ne.ZERO) then
        R = GN/GO
        if((abs(R).gt.XLO).and.(abs(R).lt.XHI)) then
          write (RAT,100) R
  100     format(F16.8)
        end if
      end if
C     !END
      call BYE ('YANEST')
C
      return
      end
