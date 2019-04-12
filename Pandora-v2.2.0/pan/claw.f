      subroutine CLAW
     $(NO,N,NL,RKC,RLC)
C
C     Rudolf Loeser, 1979 Nov 26
C---- Prints, for RATES.
C     !DASH
      save
C     !DASH
      real*8 RKC, RLC
      integer N, NL, NO
      logical PRNTZ
C     !DASH
      external ABJECT, LINER, OMAR, HI, BYE
C
C               RKC(N,NL), RLC(N,NL)
      dimension RKC(*),    RLC(*)
C
      data PRNTZ /.false./
C
      call HI ('CLAW')
C     !BEG
      if(NO.gt.0) then
        call ABJECT (NO)
        write (NO,100)
  100   format(' ','Cooling Terms')
        call LINER  (3,NO)
        write (NO,101)
  101   format(' ','RKC')
        call OMAR   (NO,N,NL,RKC,'Level ',PRNTZ)
        call LINER  (3,NO)
        write (NO,102)
  102   format(' ','RLC')
        call OMAR   (NO,N,NL,RLC,'Level ',PRNTZ)
      end if
C     !END
      call BYE ('CLAW')
C
      return
      end
