      subroutine JANUS
     $(QELSM,IONST,MESS,J,X,A,B,C,D)
C
C     Rudolf Loeser, 2004 Jul 28
C---- Delivers collisional ionization coefficient parameter tables,
C     of length J (1 .le. J .le. 3).
C     Returns J = 0 if no data are available.
C
C     Data from Arnaud & Rothenflug
C
C     (This is version 2 of JANUS.)
C     !DASH
      save
C     !DASH
      real*8 A, AT, B, BT, C, CT, D, DT, X, XT
      integer I, IONST, IST, J, JT, K, LL
      logical KILROY, MESS
      character EL*2, LABEL*19, QELSM*8
C     !DASH
      external LAPSE, HI, BYE
C
      dimension X(3), A(3), B(3), C(3), D(3)
C
      parameter (LL=44)
      dimension EL(LL), IST(LL), JT(LL), XT(3,LL), AT(3,LL), BT(3,LL),
     $          CT(3,LL), DT(3,LL)
C
      data EL( 1), IST( 1), JT( 1) /'H ', 1, 1/
      data (XT(I, 1),I=1,3) /13.6D0, 0.D0, 0.D0/
      data (AT(I, 1),I=1,3) /22.8D0, 0.D9, 0.D0/
      data (BT(I, 1),I=1,3) /-12.D0, 0.D0, 0.D0/
      data (CT(I, 1),I=1,3) /1.9D0, 0.D0, 0.D0/
      data (DT(I, 1),I=1,3) /-22.6D0, 0.D0, 0.D0/
C
      data EL( 2), IST( 2), JT( 2) /'HE', 2, 1/
      data (XT(I, 2),I=1,3) /54.4D0, 0.D0, 0.D0/
      data (AT(I, 2),I=1,3) /14.4D0, 0.D0, 0.D0/
      data (BT(I, 2),I=1,3) /-5.6D0, 0.D0, 0.D0/
      data (CT(I, 2),I=1,3) /1.9D0, 0.D0, 0.D0/
      data (DT(I, 2),I=1,3) /-13.3D0, 0.D0, 0.D0/
C
      data EL( 3), IST( 3), JT( 3) /'HE', 1, 1/
      data (XT(I, 3),I=1,3) /24.6D0, 0.D0, 0.D0/
      data (AT(I, 3),I=1,3) /17.8D0, 0.D0, 0.D0/
      data (BT(I, 3),I=1,3) /-11.D0, 0.D0, 0.D0/
      data (CT(I, 3),I=1,3) /7.D0, 0.D0, 0.D0/
      data (DT(I, 3),I=1,3) /-23.2D0, 0.D0, 0.D0/
C
      data EL( 4), IST( 4), JT( 4) /'C ', 4, 1/
      data (XT(I, 4),I=1,3) /64.5D0, 0.D0, 0.D0/
      data (AT(I, 4),I=1,3) /8.2D0, 0.D0, 0.D0/
      data (BT(I, 4),I=1,3) /-2.7D0, 0.D0, 0.D0/
      data (CT(I, 4),I=1,3) /1.4D0, 0.D0, 0.D0/
      data (DT(I, 4),I=1,3) /-6.6D0, 0.D0, 0.D0/
C
      data EL( 5), IST( 5), JT( 5) /'O ', 6, 2/
      data (XT(I, 5),I=1,3) /138.D0, 670.D0, 0.D0/
      data (AT(I, 5),I=1,3) /10.4D0, 20.8D0, 0.D0/
      data (BT(I, 5),I=1,3) /-3.3D0, -6.D0, 0.D0/
      data (CT(I, 5),I=1,3) /1.4D0, 4.1D0, 0.D0/
      data (DT(I, 5),I=1,3) /-7.4D0, -18.D0, 0.D0/
C
      data EL( 6), IST( 6), JT( 6) /'C ', 3, 2/
      data (XT(I, 6),I=1,3) /47.9D0, 325.D0, 0.D0/
      data (AT(I, 6),I=1,3) /23.2D0, 20.D0, 0.D0/
      data (BT(I, 6),I=1,3) /-7.4D0, -5.6D0, 0.D0/
      data (CT(I, 6),I=1,3) /2.5D0, 4.1D0, 0.D0/
      data (DT(I, 6),I=1,3) /-19.4D0, -18.D0, 0.D0/
C
      data EL( 7), IST( 7), JT( 7) /'O ', 5, 1/
      data (XT(I, 7),I=1,3) /114.D0, 644.D0, 0.D0/
      data (AT(I, 7),I=1,3) /16.4D0, 20.8D0, 0.D0/
      data (BT(I, 7),I=1,3) /-3.D0, 0.D0, 0.D0/
      data (CT(I, 7),I=1,3) /2.9D0, 4.1D0, 0.D0/
      data (DT(I, 7),I=1,3) /-12.D0, -18.D0, 0.D0/
C
      data EL( 8), IST( 8), JT( 8) /'C ', 2, 2/
      data (XT(I, 8),I=1,3) /24.2D0, 30.9D0, 0.D0/
      data (AT(I, 8),I=1,3) /16.D0, 23.7D0, 0.D0/
      data (BT(I, 8),I=1,3) /-9.D0, -7.6D0, 0.D0/
      data (CT(I, 8),I=1,3) /2.5D0, 2.5D0, 0.D0/
      data (DT(I, 8),I=1,3) /-10.5D0, 21.7D0, 0.D0/
C
      data EL( 9), IST( 9), JT( 9) /'O ', 4, 2/
      data (XT(I, 9),I=1,3) /77.4D0, 87.6D0, 0.D0/
      data (AT(I, 9),I=1,3) /15.D0, 16.8D0, 0.D0/
      data (BT(I, 9),I=1,3) /-5.D0, -3.3D0, 0.D0/
      data (CT(I, 9),I=1,3) /2.2D0, 2.8D0, 0.D0/
      data (DT(I, 9),I=1,3) /-10.5D0, -14.1D0, 0.D0/
C
      data EL(10), IST(10), JT(10) /'C ', 1, 2/
      data (XT(I,10),I=1,3) /11.3D0, 16.6D0, 0.D0/
      data (AT(I,10),I=1,3) /6.D0, 24.3D0, 0.D0/
      data (BT(I,10),I=1,3) /-16.D0, -7.8D0, 0.D0/
      data (CT(I,10),I=1,3) /12.D0, 2.5D0, 0.D0/
      data (DT(I,10),I=1,3) /-15.1D0, -24.D0, 0.D0/
C
      data EL(11), IST(11), JT(11) /'N ', 2, 2/
      data (XT(I,11),I=1,3) /29.6D0, 36.7D0, 0.D0/
      data (AT(I,11),I=1,3) /21.D0, 18.5D0, 0.D0/
      data (BT(I,11),I=1,3) /-9.D0, -4.3D0, 0.D0/
      data (CT(I,11),I=1,3) /5.3D0, 2.6D0, 0.D0/
      data (DT(I,11),I=1,3) /-22.5D0, -18.D0, 0.D0/
C
      data EL(12), IST(12), JT(12) /'O ', 3, 2/
      data (XT(I,12),I=1,3) /54.9D0, 63.8D0, 0.D0/
      data (AT(I,12),I=1,3) /25.D0, 17.3D0, 0.D0/
      data (BT(I,12),I=1,3) /-7.D0, -3.5D0, 0.D0/
      data (CT(I,12),I=1,3) /5.D0, 2.9D0, 0.D0/
      data (DT(I,12),I=1,3) /-18.D0, -16.1D0, 0.D0/
C
      data EL(13), IST(13), JT(13) /'N ', 1, 2/
      data (XT(I,13),I=1,3) /14.5D0, 20.3D0, 0.D0/
      data (AT(I,13),I=1,3) /19.5D0, 19.D0, 0.D0/
      data (BT(I,13),I=1,3) /-30.5D0, -4.5D0, 0.D0/
      data (CT(I,13),I=1,3) /15.D0, 2.8D0, 0.D0/
      data (DT(I,13),I=1,3) /-29.D0, -20.2D0, 0.D0/
C
      data EL(14), IST(14), JT(14) /'O ', 2, 2/
      data (XT(I,14),I=1,3) /35.1D0, 42.6D0, 0.D0/
      data (AT(I,14),I=1,3) /25.D0, 17.8D0, 0.D0/
      data (BT(I,14),I=1,3) /-8.D0, -3.8D0, 0.D0/
      data (CT(I,14),I=1,3) /8.4D0, 2.9D0, 0.D0/
      data (DT(I,14),I=1,3) /-29.5D0, -18.1D0, 0.D0/
C
      data EL(15), IST(15), JT(15) /'O ', 1, 2/
      data (XT(I,15),I=1,3) /13.6D0, 28.5D0, 0.D0/
      data (AT(I,15),I=1,3) /9.5D0, 18.2D0, 0.D0/
      data (BT(I,15),I=1,3) /-17.5D0, 4.D0, 0.D0/
      data (CT(I,15),I=1,3) /12.5D0, 2.8D0, 0.D0/
      data (DT(I,15),I=1,3) /-19.5D0, -20.2D0, 0.D0/
C
      data EL(16), IST(16), JT(16) /'NA', 1, 2/
      data (XT(I,16),I=1,3) /5.1D0, 34.D0, 0.D0/
      data (AT(I,16),I=1,3) /16.D0, 63.9D0, 0.D0/
      data (BT(I,16),I=1,3) /-1.D0, -27.D0, 0.D0/
      data (CT(I,16),I=1,3) /0.2D0, 33.D0, 0.D0/
      data (DT(I,16),I=1,3) /-13.5D0, -80.D0, 0.D0/
C
      data EL(17), IST(17), JT(17) /'MG', 2, 3/
      data (XT(I,17),I=1,3) /15.D0, 65.D0, 104.5D0/
      data (AT(I,17),I=1,3) /9.D0, 37.7D0, 17.6D0/
      data (BT(I,17),I=1,3) /-3.6D0, -30.D0, -5.2D0/
      data (CT(I,17),I=1,3) /0.3D0, 24.8D0, 3.3D0/
      data (DT(I,17),I=1,3) /-5.4D0, -62.D0, -19.D0/
C
      data EL(18), IST(18), JT(18) /'SI', 4, 3/
      data (XT(I,18),I=1,3) /45.1D0, 148.D0, 193.5D0/
      data (AT(I,18),I=1,3) /8.D0, 66.7D0, 22.D0/
      data (BT(I,18),I=1,3) /-3.D0, -24.8D0, -7.2D0/
      data (CT(I,18),I=1,3) /0.6D0, 18.7D0, 3.3D0/
      data (DT(I,18),I=1,3) /-5.8D0, -65.D0, -20.9D0/
C
      data EL(19), IST(19), JT(19) /'MG', 1, 3/
      data (XT(I,19),I=1,3) /7.6D0, 54.D0, 92.2D0/
      data (AT(I,19),I=1,3) /18.D0, 37.7D0, 17.6D0/
      data (BT(I,19),I=1,3) /-1.D0, -30.D0, -5.2D0/
      data (CT(I,19),I=1,3) /0.6D0, 24.8D0, 3.3D0/
      data (DT(I,19),I=1,3) /-4.D0, -62.D0, -19.D0/
C
      data EL(20), IST(20), JT(20) /'AL', 2, 3/
      data (XT(I,20),I=1,3) /18.8D0, 90.D0, 131.D0/
      data (AT(I,20),I=1,3) /17.D0, 31.3D0, 12.1D0/
      data (BT(I,20),I=1,3) /-6.D0, -22.7D0, -3.5D0/
      data (CT(I,20),I=1,3) /1.D0, 21.D0, 3.3D0/
      data (DT(I,20),I=1,3) /-8.D0, -44.1D0, -12.1D0/
C
      data EL(21), IST(21), JT(21) /'SI', 3, 3/
      data (XT(I,21),I=1,3) /33.5D0, 133.D0, 176.6D0/
      data (AT(I,21),I=1,3) /19.8D0, 66.7D0, 22.D0/
      data (BT(I,21),I=1,3) /-5.7D0, -24.8D0, -7.2D0/
      data (CT(I,21),I=1,3) /1.3D0, 18.7D0, 3.3D0/
      data (DT(I,21),I=1,3) /-11.9D0, -65.D0, -20.9D0/
C
      data EL(22), IST(22), JT(22) /'AL', 1, 2/
      data (XT(I,22),I=1,3) /6.D0, 10.6D0, 0.D0/
      data (AT(I,22),I=1,3) /47.D0, 55.1D0, 0.D0/
      data (BT(I,22),I=1,3) /-26.D0, -37.2D0, 0.D0/
      data (CT(I,22),I=1,3) /0.6D0, 1.4D0, 0.D0/
      data (DT(I,22),I=1,3) /-39.D0, -41.D0, 0.D0/
C
      data EL(23), IST(23), JT(23) /'SI', 2, 2/
      data (XT(I,23),I=1,3) /16.3D0, 22.9D0, 0.D0/
      data (AT(I,23),I=1,3) /50.4D0, 55.1D0, 0.D0/
      data (BT(I,23),I=1,3) /-33.4D0, -37.2D0, 0.D0/
      data (CT(I,23),I=1,3) /0.6D0, 1.4D0, 0.D0/
      data (DT(I,23),I=1,3) /-36.9D0, -41.D0, 0.D0/
C
      data EL(24), IST(24), JT(24) /'SI', 1, 2/
      data (XT(I,24),I=1,3) /8.1D0, 13.5D0, 0.D0/
      data (AT(I,24),I=1,3) /74.5D0, 53.8D0, 0.D0/
      data (BT(I,24),I=1,3) /-49.4D0, -35.8D0, 0.D0/
      data (CT(I,24),I=1,3) /1.3D0, 1.4D0, 0.D0/
      data (DT(I,24),I=1,3) /-54.6D0, -40.7D0, 0.D0/
C
      data EL(25), IST(25), JT(25) /'S ', 1, 2/
      data (XT(I,25),I=1,3) /10.4D0, 20.2D0, 0.D0/
      data (AT(I,25),I=1,3) /6.D0, 51.3D0, 0.D0/
      data (BT(I,25),I=1,3) /-22.D0, -33.2D0, 0.D0/
      data (CT(I,25),I=1,3) /20.D0, 1.4D0, 0.D0/
      data (DT(I,25),I=1,3) /-20.D0, -40.2D0, 0.D0/
C
      data EL(26), IST(26), JT(26) /'CA', 2, 3/
      data (XT(I,26),I=1,3) /11.9D0, 37.D0, 45.2D0/
      data (AT(I,26),I=1,3) /7.9D0, 74.3D0, 17.6D0/
      data (BT(I,26),I=1,3) /-2.D0, -24.2D0, -3.8D0/
      data (CT(I,26),I=1,3) /0.2D0, 7.D0, 1.9D0/
      data (DT(I,26),I=1,3) /-6.D0, -68.D0, -13.8D0/
C
      data EL(27), IST(27), JT(27) /'CA', 1, 3/
      data (XT(I,27),I=1,3) /6.1D0, 28.D0, 40.3D0/
      data (AT(I,27),I=1,3) /2.5D0, 74.3D0, 17.6D0/
      data (BT(I,27),I=1,3) /-2.5D0, -24.2D0, -3.8D0/
      data (CT(I,27),I=1,3) /8.D0, 7.D0, 1.9D0/
      data (DT(I,27),I=1,3) /-5.5D0, -63.D0, -13.8D0/
C
      data EL(28), IST(28), JT(28) /'N ', 3, 2/
      data (XT(I,28),I=1,3) /47.4D0, 55.8D0, 0.D0/
      data (AT(I,28),I=1,3) /16.D0, 18.1D0, 0.D0/
      data (BT(I,28),I=1,3) /-7.5D0, -4.D0, 0.D0/
      data (CT(I,28),I=1,3) /2.3D0, 2.8D0, 0.D0/
      data (DT(I,28),I=1,3) /-10.D0,-15.8D0, 0.D0/
C
      data EL(29), IST(29), JT(29) /'N ', 4, 2/
      data (XT(I,29),I=1,3) /77.5D0, 471.D0, 0.D0/
      data (AT(I,29),I=1,3) /17.6D0, 20.5D0, 0.D0/
      data (BT(I,29),I=1,3) /-3.8D0, -5.8D0, 0.D0/
      data (CT(I,29),I=1,3) /2.8D0, 4.1D0, 0.D0/
      data (DT(I,29),I=1,3) /-13.6D0, -18.D0, 0.D0/
C
      data EL(30), IST(30), JT(30) /'NE', 1, 2/
      data (XT(I,30),I=1,3) /21.6D0, 48.5D0, 0.D0/
      data (AT(I,30),I=1,3) /40.D0, 18.D0, 0.D0/
      data (BT(I,30),I=1,3) /-42.D0, -4.8D0, 0.D0/
      data (CT(I,30),I=1,3) /18.D0, 2.8D0, 0.D0/
      data (DT(I,30),I=1,3) /-56.D0, 22.D0, 0.D0/
C
      data EL(31), IST(31), JT(31) /'NE', 2, 2/
      data (XT(I,31),I=1,3) /41.1D0, 66.4D0, 0.D0/
      data (AT(I,31),I=1,3) /37.D0, 18.6D0, 0.D0/
      data (BT(I,31),I=1,3) /-33.D0, -4.6D0, 0.D0/
      data (CT(I,31),I=1,3) /15.5D0, 2.8D0, 0.D0/
      data (DT(I,31),I=1,3) /-46.D0, -20.2D0, 0.D0/
C
      data EL(32), IST(32), JT(32) /'S ', 2, 2/
      data (XT(I,32),I=1,3) /23.4D0, 30.7D0, 0.D0/
      data (AT(I,32),I=1,3) /98.7D0, 52.5D0, 0.D0/
      data (BT(I,32),I=1,3) /-65.4D0, -34.5D0, 0.D0/
      data (CT(I,32),I=1,3) /1.9D0, 1.4D0, 0.D0/
      data (DT(I,32),I=1,3) /-72.3D0, -40.5D0, 0.D0/
C
      data EL(33), IST(33), JT(33) /'S ', 3, 2/
      data (XT(I,33),I=1,3) /35.D0, 43.8D0, 0.D0/
      data (AT(I,33),I=1,3) /74.5D0, 53.8D0, 0.D0/
      data (BT(I,33),I=1,3) /-49.4D0, -35.8D0, 0.D0/
      data (CT(I,33),I=1,3) /1.3D0, 1.4D0, 0.D0/
      data (DT(I,33),I=1,3) /-54.6D0, -40.7D0, 0.D0/
C
      data EL(34), IST(34), JT(34) /'S ', 4, 2/
      data (XT(I,34),I=1,3) /47.3D0, 57.6D0, 0.D0/
      data (AT(I,34),I=1,3) /50.4D0, 55.1D0, 0.D0/
      data (BT(I,34),I=1,3) /-33.4D0, -37.2D0, 0.D0/
      data (CT(I,34),I=1,3) /0.6D0, 1.4D0, 0.D0/
      data (DT(I,34),I=1,3) /-36.9D0, -41.D0, 0.D0/
C
      data EL(35), IST(35), JT(35) /'NE', 8, 2/
      data (XT(I,35),I=1,3) /239.D0, 1107.D0, 0.D0/
      data (AT(I,35),I=1,3) /10.1D0, 21.5D0, 0.D0/
      data (BT(I,35),I=1,3) /-3.1D0, -6.4D0, 0.D0/
      data (CT(I,35),I=1,3) /1.4D0, 4.1D0, 0.D0/
      data (DT(I,35),I=1,3) /-7.1D0, -18.D0, 0.D0/
C
      data EL(36), IST(36), JT(36) /'O ', 5, 2/
      data (XT(I,36),I=1,3) /114.D0, 644.D0, 0.D0/
      data (AT(I,36),I=1,3) /16.4D0, 20.8D0, 0.D0/
      data (BT(I,36),I=1,3) /-3.D0, -6.D0, 0.D0/
      data (CT(I,36),I=1,3) /2.9D0, 4.1D0, 0.D0/
      data (DT(I,36),I=1,3) /-12.D0, -18.D0, 0.D0/
C
      data EL(37), IST(37), JT(37) /'NE', 7, 2/
      data (XT(I,37),I=1,3) /207.D0, 1073.D0, 0.D0/
      data (AT(I,37),I=1,3) /16.5D0, 21.5D0, 0.D0/
      data (BT(I,37),I=1,3) /-3.1D0, -6.4D0, 0.D0/
      data (CT(I,37),I=1,3) /2.8D0, 4.1D0, 0.D0/
      data (DT(I,37),I=1,3) /-11.4D0, -18.D0, 0.D0/
C
      data EL(38), IST(38), JT(38) /'NE', 6, 2/
      data (XT(I,38),I=1,3) /158.D0, 172.D0, 0.D0/
      data (AT(I,38),I=1,3) /14.5D0, 16.9D0, 0.D0/
      data (BT(I,38),I=1,3) /-4.6D0, -3.4D0, 0.D0/
      data (CT(I,38),I=1,3) /1.9D0, 2.8D0, 0.D0/
      data (DT(I,38),I=1,3) /-8.5D0, -13.2D0, 0.D0/
C
      data EL(39), IST(39), JT(39) /'NE', 5, 2/
      data (XT(I,39),I=1,3) /126.D0, 139.D0, 0.D0/
      data (AT(I,39),I=1,3) /25.5D0, 17.4D0, 0.D0/
      data (BT(I,39),I=1,3) /-8.5D0, -3.8D0, 0.D0/
      data (CT(I,39),I=1,3) /4.5D0, 2.8D0, 0.D0/
      data (DT(I,39),I=1,3) /-16.8D0, -14.9D0, 0.D0/
C
      data EL(40), IST(40), JT(40) /'NE', 4, 2/
      data (XT(I,40),I=1,3) /97.1D0, 108.D0, 0.D0/
      data (AT(I,40),I=1,3) /34.D0, 17.8D0, 0.D0/
      data (BT(I,40),I=1,3) /-10.D0, -4.D0, 0.D0/
      data (CT(I,40),I=1,3) /7.5D0, 2.8D0, 0.D0/
      data (DT(I,40),I=1,3) /-25.D0, -16.7D0, 0.D0/
C
      data EL(41), IST(41), JT(41) /'NE', 3, 2/
      data (XT(I,41),I=1,3) /63.5D0, 86.2D0, 0.D0/
      data (AT(I,41),I=1,3) /33.D0, 18.2D0, 0.D0/
      data (BT(I,41),I=1,3) /-17.5D0, -4.4D0, 0.D0/
      data (CT(I,41),I=1,3) /11.2D0, 2.8D0, 0.D0/
      data (DT(I,41),I=1,3) /-33.D0, -18.4D0, 0.D0/
C
      data EL(42), IST(42), JT(42) /'NA', 2, 2/
      data (XT(I,42),I=1,3) /47.3D0, 80.1D0, 0.D0/
      data (AT(I,42),I=1,3) /40.D0, 19.2D0, 0.D0/
      data (BT(I,42),I=1,3) /-28.D0, -5.3D0, 0.D0/
      data (CT(I,42),I=1,3) /19.4D0, 2.8D0, 0.D0/
      data (DT(I,42),I=1,3) /-60.D0, -21.2D0, 0.D0/
C
      data EL(43), IST(43), JT(43) /'FE', 2, 3/
      data (XT(I,43),I=1,3) /16.2D0, 17.5D0, 81.D0/
      data (AT(I,43),I=1,3) /90.D0, 18.6D0, 69.9D0/
      data (BT(I,43),I=1,3) /-60.D0, -5.9D0, -23.7D0/
      data (CT(I,43),I=1,3) /0.2D0, 0.6D0, 9.5D0/
      data (DT(I,43),I=1,3) /-86.D0, -9.D0, -51.7D0/
C
      data EL(44), IST(44), JT(44) /'FE', 1, 3/
      data (XT(I,44),I=1,3) /7.9D0, 9.D0, 59.D0/
      data (AT(I,44),I=1,3) /3.9D0, 9.6D0, 69.9D0/
      data (BT(I,44),I=1,3) /-1.3D0, -3.D0, -23.7D0/
      data (CT(I,44),I=1,3) /0.4D0, 0.3D0, 9.5D0/
      data (DT(I,44),I=1,3) /-1.9D0, -4.6D0, -51.7D0/
C
      data KILROY /.true./
      data LABEL  /'Arnaud & Rothenflug'/
C     !EJECT
C
      call HI ('JANUS')
C     !BEG
      do 101 I = 1,LL
        if((EL(I).eq.QELSM(:2)).and.(IST(I).eq.IONST)) then
          J = JT(I)
          do 100 K = 1,J
            X(K) = XT(K,I)
            A(K) = AT(K,I)
            B(K) = BT(K,I)
            C(K) = CT(K,I)
            D(K) = DT(K,I)
  100     continue
          goto 102
        end if
  101 continue
      J = 0
      if(KILROY.and.MESS) then
        KILROY = .false.
        call LAPSE (QELSM, IONST, LABEL, LL, EL, IST)
      end if
C
  102 continue
C     !END
      call BYE ('JANUS')
C
      return
      end
