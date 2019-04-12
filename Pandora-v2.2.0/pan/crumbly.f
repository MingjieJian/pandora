      subroutine CRUMBLY
     $(MN1,IVLSW,Z,ZXH,P,SIGMA,H,R,S,ZETA,Y,A)
C
C     Rudolf Loeser, 1997 Jul 29
C---- Prints N1-recalculation intermediates, for CARROT.
C     !DASH
      save
C     !DASH
      real*8 A, H, P, R, S, SIGMA, Y, Z, ZETA, ZXH
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
C               Z(N), ZXH(N), P(N), SIGMA(N), H(N), ZETA(N), Y(N),
      dimension Z(*), ZXH(*), P(*), SIGMA(*), H(*), ZETA(*), Y(*),
C
C               R(N), S(N), A(N)
     $          R(*), S(*), A(*)
C
      call HI ('CRUMBLY')
C     !BEG
      call CARDIFF (IVLSW)
      call LINER   (1, LUEO)
      write (LUEO,100)
  100 format(' ',14X,'ZXH',12X,'h',12X,'r',12X,'s',12X,'Z',12X,'p',
     $           12X,'zeta',11X,'sigma',12X,'y')
      call LINER   (1, LUEO)
C
      write (LUEO,101) (I,ZXH(I),H(I),R(I),S(I),Z(I),P(I),ZETA(I),
     $                  SIGMA(I),Y(I),I=1,MN1)
  101 format(5(' ',I4,1P6E13.5,2E16.8,E13.5/))
C
      call LING    (ZETA, A, MN1)
C     !END
      call BYE ('CRUMBLY')
C
      return
      end
