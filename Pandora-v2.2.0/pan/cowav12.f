      subroutine COWAV12
     $(J,K,EN)
C
C     Rudolf Loeser, 1992 Aug 04
C---- Computes the energy of the level with quantum numbers
C     j=J and v=K of the CO molecule with the Carbon-12 isotope.
C
C     From: Farrenq R., Guelachvili G., Sauval A.J., Grevesse N.,
C           Farmer C.B., 1991, J.Molec.Spectrosc. 149, 375.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, E, EN, F, G, HALF, ONE, T1, T2, T3, T4, T5, T6,
     $       XJ, XK
      integer J, K
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
      dimension A(9), B(8), C(5), D(3), E(3), F(2)
C
      data A/+2.169812668D+03, -1.328787537D+01, +1.041085340D-02,
     $       +6.939129677D-05, +1.662285161D-07, +2.129894878D-09,
     $       -8.505443348D-10, +1.242071848D-11, -8.246505047D-14/
C
      data B/+1.931280982D+00, -1.750439284D-02, +7.174192659D-07,
     $       -2.147899265D-08, +4.438412484D-09, -1.363094233D-10,
     $       +1.251301444D-12, -2.130389424D-14/
C
      data C/-6.121592067D-06, +1.035075821D-09, -1.849830874D-10,
     $       +2.433808936D-12, -1.043994903D-13/
C
      data D/+5.884885473D-12, -1.428672488D-13, -1.225509038D-15/
C
      data E/+3.615766233D-17, +7.405863866D-19, +5.166040400D-21/
C
      data F/+4.555456211D-23, +5.919841594D-24/
C
      data G/+1.724686033D-27/
C     !EJECT
C
      call HI ('COWAV12')
C     !BEG
      XK = K
      XK = XK+HALF
      XJ = J
      XJ = XJ*(XJ+ONE)
C
      T1 = ((A(1)+(A(2)+(A(3)+(A(4)+(A(5)+(A(6)+(A(7)+(A(8)+A(9)
     $     *XK)*XK)*XK)*XK)*XK)*XK)*XK)*XK)*XK)
C
      T2 = (B(1)+(B(2)+(B(3)+(B(4)+(B(5)+(B(6)+(B(7)+B(8)
     $     *XK)*XK)*XK)*XK)*XK)*XK)*XK)
C
      T3 = (C(1)+(C(2)+(C(3)+(C(4)+C(5)
     $     *XK)*XK)*XK)*XK)
C
      T4 = (D(1)+(D(2)+D(3)
     $     *XK)*XK)
C
      T5 = (E(1)+(E(2)+E(3)
     $     *XK)*XK)
C
      T6 = (F(1)+F(2)
     $     *XK)
C
      EN = T1+(T2+(T3+(T4-(T5-(T6-G*XJ)*XJ)*XJ)*XJ)*XJ)*XJ
C     !END
      call BYE ('COWAV12')
C
      return
      end
