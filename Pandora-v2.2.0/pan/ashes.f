      subroutine ASHES
     $(I,N,LU,LEV,ALFH,ALFHE,CXH,CXHE,RKAD,RLAD,RKI,RLI)
C
C     Rudolf Loeser, 2004 Sep 20
C---- Prints lower-level charge exchange results.
C     (This is version 2 of ASHES.)
C     !DASH
      save
C     !DASH
      real*8 ALFH, ALFHE, CXH, CXHE, RKAD, RKI, RLAD, RLI
      integer I, LEV, LU, N
C     !DASH
      external LINER, SHIM, HI, BYE
C
      call HI ('ASHES')
C     !BEG
      if(I.eq.1) then
        call LINER (3, LU)
        write (LU,100) LEV
  100   format(' ','For level',I3//
     $         ' ',96X,'------- Final  values -------'/
     $         ' ',4X,'i',9X,'ALPHAH',8X,'ALPHAHE',12X,'CXH',11X,
     $             'CXHE',10X,'RKadd',10X,'RLadd',13X,'RK',13X,'RL')
        call LINER (1, LU)
      end if
C
      write (LU,101) I,ALFH,ALFHE,CXH,CXHE,RKAD,RLAD,RKI,RLI
  101 format(' ',I5,1P8E15.7)
      call SHIM    (I, 5, LU)
C     !END
      call BYE ('ASHES')
C
      return
      end
