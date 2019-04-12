      subroutine MALDEN
     $(KRJ,ITUS,LEGEND)
C
C     Rudolf Loeser, 1996 Apr 16
C---- Makes a label for b-ratios calculations printouts.
C     !DASH
      save
C     !DASH
      integer ITUS, K, KRJ
      character BLANK*1, INPUT*8, LEGEND*33
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
      intrinsic min, max
C
      dimension INPUT(6)
C
      data INPUT /'?       ', 'Rho+Jbar', 'Jbar    ', 'Rho only',
     $            'Chi     ', '??      '/
C
      call HI ('MALDEN')
C     !BEG
      K = max(min(KRJ,5),0)+1
      LEGEND = BLANK
      write (LEGEND(:22),100) INPUT(K)
  100 format('b-ratios from ',A8)
C
      if(ITUS.gt.0) then
        write (LEGEND(23:33),101) ITUS
  101   format(', Iter =',I3)
      end if
C     !END
      call BYE ('MALDEN')
C
      return
      end
