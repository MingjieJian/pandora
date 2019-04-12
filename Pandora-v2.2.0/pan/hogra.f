      subroutine HOGRA
     $(XIS,KS,XIR,KR,XIB,KB,BXI,KBX,XK,KK)
C
C     Rudolf Loeser, 1988 Nov 23
C---- Checks monotonicity of frequency tables.
C     !DASH
      save
C     !DASH
      real*8 BXI, XIB, XIR, XIS, XK
      integer KB, KBX, KK, KR, KS
      logical STOP
C     !DASH
      external WARNINC, ABORT, HI, BYE
C
C               XIS(KS), XIR(KR), XIB(KB), BXI(KBX), XK(KK)
      dimension XIS(*),  XIR(*),  XIB(*),  BXI(*),   XK(*)
C
      call HI ('HOGRA')
C     !BEG
      STOP = .false.
C
      if(KS.gt.0) then
        call WARNINC (XIS, KS,  'XISYM', 'HOGRA', STOP)
      end if
      if(KR.gt.0) then
        call WARNINC (XIR, KR,  'XIRED', 'HOGRA', STOP)
      end if
      if(KB.gt.0) then
        call WARNINC (XIB, KB,  'XIBLU', 'HOGRA', STOP)
      end if
      if(KBX.gt.0) then
        call WARNINC (BXI, KBX, 'BXI',   'HOGRA', STOP)
      end if
      if(KK.gt.0) then
        call WARNINC (XK,  KK,  'XK   ', 'HOGRA', STOP)
      end if
C
      if(STOP) then
        call ABORT
      end if
C     !END
      call BYE ('HOGRA')
C
      return
      end
