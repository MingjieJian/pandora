      subroutine COWVNUM
     $(J,JP,K,KP,ISO,WN,PRNT,METH,WNL)
C
C     Rudolf Loeser, 1992 Sep 11
C---- Computes the wavenumber of the CO transition from level (J,K) to
C     level (JP,KP).
C     !DASH
      save
C     !DASH
      real*8 WN, WNL, WNU
      integer ISO, J, JP, K, KP, METH
      logical PRNT
C     !DASH
      external  COWAVE, BLANKETY, HI, BYE
      intrinsic abs
C
      call HI ('COWVNUM')
C     !BEG
      call COWAVE     (JP,KP,ISO,METH,WNU)
      call COWAVE     (J ,K ,ISO,METH,WNL)
      WN = abs(WNU-WNL)
C
      if(PRNT) then
        call BLANKETY (2)
        write (*,100) ISO, J,K,WNL, JP,KP,WNU, WN
  100   format(' ','C',I2,'O: j=',I3,3X,  'v=',I2,3X,'WNL=',1PE22.15,5X,
     $                     'j''=',I3,3X,'v''=',I2,3X,'WNU=',  E22.15,5X,
     $                                                'WN=',  E22.15)
      end if
C     !END
      call BYE ('COWVNUM')
C
      return
      end
