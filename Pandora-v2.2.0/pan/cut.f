      subroutine CUT
     $(RKW,KOLEV,N,NL,XNUK,XNU,XK,YLYM,GK,KK,KXLYM,NO)
C
C     Rudolf Loeser, 1981 Oct 26
C---- Prints, for LINTEL.
C     (This is version 3 of CUT.)
C     !DASH
      save
C     !DASH
      real*8 GK, R, RKW, XK, XLM, XNU, XNUK, YLYM
      integer I, KK, KOLEV, KXLYM, N, NL, NO
      character YLAB*10
C     !DASH
      external LINER, PRIVET, HAKO, ANGIE, SHIM, HI, BYE
C
C               RKW(N), GK(KK), XNU(NSL), XK(KK), YLYM(KK)
      dimension RKW(*), GK(*),  XNU(*),   XK(*),  YLYM(*)
C
      call HI ('CUT')
C     !BEG
      call LINER   (2, NO)
      write (NO,100) KOLEV
  100 format(' ',6X,'Level',I3,'-to-Continuum'//
     $       ' ',14X,'XK',23X,'YLYM',10X,'GK'/
     $       ' ',8X,'(freq. ratio)  (Angstroms)')
      call LINER   (1, NO)
C
      R = XNUK-XNU(KOLEV)
      do 102 I = 1,KK
        call HAKO  (YLYM(I), YLAB)
        call ANGIE ((R*XK(I)), XLM)
        write (NO,101) I,XK(I),XLM,YLAB,GK(I)
  101   format(' ',I7,1P2E13.4,3X,A10,E13.4)
        call SHIM  (I, 5, NO)
  102 continue
C
      call LINER   (1, NO)
      write (NO,103) KXLYM
  103 format(' ','KXLYM =',I2,' (controls use of augmented ',
     $           'XK table)')
C
      call LINER   (2, NO)
      write (NO,104) KOLEV
  104 format(' ','Input values of RKWT',I3)
      call PRIVET  (NO, RKW, N)
C
C     !END
      call BYE ('CUT')
C
      return
      end
