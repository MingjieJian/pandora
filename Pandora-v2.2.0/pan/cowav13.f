      subroutine COWAV13
     $(J,K,EN)
C
C     Rudolf Loeser, 1992 Nov 02
C---- Computes the energy of the level with quantum numbers
C     j=J and v=K of the CO molecule with the Carbon-13 isotope.
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
      data A /+2.121439026D+03, -1.270204643D+01, +9.730658140D-03,
     $        +6.338503741D-05, +1.500336759D-07, +1.798728967D-09,
     $        -7.249007258D-10, +1.035182507D-11, -6.721736565D-14/
C
      data B /+1.846151725D+00, -1.635976671D-02, +6.555319987D-07,
     $        -1.917557543D-08, +3.874266547D-09, -1.162374288D-10,
     $        +1.040203815D-12, -1.734876798D-14/
C
      data C /-5.593901826D-06, +9.246022661D-10, -1.615746949D-10,
     $        +2.076206158D-12, -8.712901196D-14/
C
      data D /+5.140386572D-12, -1.220091918D-13, -1.023122487D-15/
C
      data E /+3.019101986D-17, +6.045838445D-19, +4.034237544D-21/
C
      data F /+3.635889807D-23, +4.621580733D-24/
C
      data G /+1.159454710D-27/
C     !EJECT
C
      call HI ('COWAV13')
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
      call BYE ('COWAV13')
C
      return
      end
