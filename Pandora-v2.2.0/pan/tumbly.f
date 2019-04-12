      subroutine TUMBLY
     $(MN1,IVLSW,ZB,ZXH,P,SIGMA,HB,RB,SB,ZETA,Y,A)
C
C     Rudolf Loeser, 1997 Jul 29
C---- Prints N1-recalculation intermediates, for CARROT.
C     !DASH
      save
C     !DASH
      real*8 A, HB, P, RB, SB, SIGMA, Y, ZB, ZETA, ZXH
      integer I, IVLSW, LUEO, MN1
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external CARDIFF, LINER, LING, HI, BYE
C
C               ZB(N), ZXH(N), RB(N), SIGMA(N), HB(N), ZETA(N), SB(N),
      dimension ZB(*), ZXH(*), RB(*), SIGMA(*), HB(*), ZETA(*), SB(*),
C
C               A(N), P(N), Y(N),
     $          A(*), P(*), Y(*)
C
      call HI ('TUMBLY')
C     !BEG
      call CARDIFF (IVLSW)
      call LINER   (1, LUEO)
      write (LUEO,100)
  100 format(' ',14X,'ZXH',8X,'h-bar',8X,'r-bar',8X,'s-bar',
     $           8X,'Z-bar',12X,'p',12X,'zeta',11X,'sigma',12X,'y')
      call LINER   (1, LUEO)
C
      write (LUEO,101) (I,ZXH(I),HB(I),RB(I),SB(I),ZB(I),P(I),ZETA(I),
     $                  SIGMA(I),Y(I),I=1,MN1)
  101 format(5(' ',I4,1P6E13.5,2E16.8,E13.5/))
C
      call LING    (ZETA, A, MN1)
C     !END
      call BYE ('TUMBLY')
C
      return
      end
