      subroutine HILLAH
     $(N,NL,NO,CXX,CXXP,G)
C
C     Rudolf Loeser, 1990 Nov 30
C---- Prints, for upper-level charge exchange.
C     !DASH
      save
C     !DASH
      real*8 CXX, CXXP, G
      integer N, NL, NO
      logical PRNTZ
C     !DASH
      external PRIAM, LINER, OMAR, HI, BYE
C
C               CXX(N,NL), CXXP(N,NL), G(N,NL)
      dimension CXX(*),    CXXP(*),    G(*)
C
      data PRNTZ /.false./
C
      call HI ('HILLAH')
C     !BEG
      if(NO.gt.0) then
        call PRIAM (NO, 'CHARGE EXCH.', 12)
        call LINER (2, NO)
        write (NO,100)
  100   format(' ','Quantities needed for Upper-Level Charge Exchange ',
     $             'calculation.  (Printing controlled by option ',
     $             'CHXPRNT.)')
C
        call LINER (2, NO)
        write (NO,101)
  101   format(' ','Charge Transfer Coefficient, G')
        call OMAR  (NO, N, NL, G,    'Level ', PRNTZ)
C
        call LINER (2, NO)
        write (NO,102)
  102   format(' ','Ion-to-neutral Rate, XP')
        call OMAR  (NO, N, NL, CXXP, 'Level ', PRNTZ)
C
        call LINER (2, NO)
        write (NO,103)
  103   format(' ','Neutral-to-ion Rate, X')
        call OMAR  (NO, N, NL, CXX,  'Level ', PRNTZ)
      end if
C     !END
      call BYE ('HILLAH')
C
      return
      end
