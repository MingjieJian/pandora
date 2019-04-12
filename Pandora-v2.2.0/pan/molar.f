      subroutine MOLAR
     $(QELSM,IONST,MESS,DE,P,A,X,CK)
C
C     Rudolf Loeser, 2005 Aug 30
C---- Delivers collisional ionization coefficient parameters.
C     Returns DE = -1.0 if no data are available.
C
C     Data from Voronov
C
C     !DASH
      save
C     !DASH
      real*8 A, CK, DE, ERROR, P, T, X
      integer I, IONST, IST, N
      logical KILROY, MESS
      character EL*2, LABEL*7, QELSM*8
C     !DASH
      external LAPSE, HI, BYE
C
      parameter (N=45)
      dimension EL(N), IST(N), T(5,N)
C
      data EL( 1), IST( 1), (T(I, 1),I=1,5) /'HE', 1,
     $     24.6D0, 0.D0, 1.75D-8, 0.18D0, 0.35D0/
C
      data EL( 2), IST( 2), (T(I, 2),I=1,5) /'HE', 2,
     $     54.4D0, 1.D0, 2.05D-9, 0.265D0, 0.25D0/
C
      data EL( 3), IST( 3), (T(I, 3),I=1,5) /'LI', 1,
     $     5.4D0, 0.D0, 1.39D-7, 0.438D0, 0.41D0/
C
      data EL( 4), IST( 4), (T(I, 4),I=1,5) /'B ', 1,
     $     8.3D0, 0.D0, 6.49D-8, 0.2D0, 0.26D0/
C
      data EL( 5), IST( 5), (T(I, 5),I=1,5) /'C ', 1,
     $     11.3D0, 0.D0, 6.85D-8, 0.193D0, 0.25D0/
C
      data EL( 6), IST( 6), (T(I, 6),I=1,5) /'C ', 2,
     $     24.4D0, 1.D0, 1.86D-8, 0.286D0, 0.24D0/
C
      data EL( 7), IST( 7), (T(I, 7),I=1,5) /'C ', 3,
     $     47.9D0, 1.D0, 6.35D-9, 0.427D0, 0.21D0/
C
      data EL( 8), IST( 8), (T(I, 8),I=1,5) /'C ', 4,
     $     64.5D0, 1.D0, 1.5D-9, 0.416D0, 0.13D0/
C
      data EL( 9), IST( 9), (T(I, 9),I=1,5) /'N ', 1,
     $     14.5D0, 0.D0, 4.82D-8, 0.0652D0, 0.42D0/
C
      data EL(10), IST(10), (T(I,10),I=1,5) /'N ', 2,
     $     29.6D0, 0.D0, 2.98D-8, 0.31D0, 0.3D0/
C
      data EL(11), IST(11), (T(I,11),I=1,5) /'N ', 3,
     $     47.5D0, 1.D0, 8.1D-9, 0.35D0, 0.24D0/
C
      data EL(12), IST(12), (T(I,12),I=1,5) /'N ', 4,
     $     77.5D0, 1.D0, 3.71D-9, 0.549D0, 0.18D0/
C
      data EL(13), IST(13), (T(I,13),I=1,5) /'O ', 1,
     $     13.6D0, 0.D0, 3.59D-8, 0.073D0, 0.34D0/
C
      data EL(14), IST(14), (T(I,14),I=1,5) /'O ', 2,
     $     35.1D0, 1.D0, 1.39D-8, 0.212D0, 0.22D0/
C
      data EL(15), IST(15), (T(I,15),I=1,5) /'O ', 3,
     $     54.9D0, 1.D0, 9.31D-9, 0.27D0, 0.27D0/
C
      data EL(16), IST(16), (T(I,16),I=1,5) /'O ', 4,
     $     77.4D0, 0.D0, 1.02D-8, 0.614D0, 0.27D0/
C
      data EL(17), IST(17), (T(I,17),I=1,5) /'O ', 5,
     $     113.9D0, 1.D0, 2.19D-9, 0.63D0, 0.17D0/
C
      data EL(18), IST(18), (T(I,18),I=1,5) /'O ', 6,
     $     138.1D0, 0.D0, 1.95D-9, 0.36D0, 0.54D0/
C
      data EL(19), IST(19), (T(I,19),I=1,5) /'NE', 1,
     $     21.6D0, 1.D0, 1.5D-8, 0.0329D0, 0.43D0/
C
      data EL(20), IST(20), (T(I,20),I=1,5) /'NE', 2,
     $     41.D0, 0.D0, 1.98D-8, 0.295D0, 0.2D0/
C
      data EL(21), IST(21), (T(I,21),I=1,5) /'NA', 1,
     $     5.1D0, 1.D0, 1.01D-7, 0.275D0, 0.23D0/
C
      data EL(22), IST(22), (T(I,22),I=1,5) /'MG', 1,
     $     7.6D0, 0.D0, 6.21D-7, 0.592D0, 0.39D0/
C
      data EL(23), IST(23), (T(I,23),I=1,5) /'MG', 2,
     $     15.2D0, 0.D0, 1.92D-8, 0.0027D0, 0.85D0/
C
      data EL(24), IST(24), (T(I,24),I=1,5) /'AL', 1,
     $     6.D0, 1.D0, 2.28D-7, 0.387D0, 0.25D0/
C
      data EL(25), IST(25), (T(I,25),I=1,5) /'AL', 2,
     $     18.8D0, 0.D0, 1.18D-7, 2.21D0, 0.25D0/
C
      data EL(26), IST(26), (T(I,26),I=1,5) /'SI', 1,
     $     8.2D0, 1.D0, 1.88D-7, 0.376D0, 0.25D0/
C
      data EL(27), IST(27), (T(I,27),I=1,5) /'SI', 2,
     $     16.4D0, 1.D0, 6.43D-8, 0.632D0, 0.2D0/
C
      data EL(28), IST(28), (T(I,28),I=1,5) /'SI', 3,
     $     33.5D0, 1.D0, 2.01D-8, 0.473D0, 0.22D0/
C
      data EL(29), IST(29), (T(I,29),I=1,5) /'SI', 4,
     $     54.D0, 1.D0, 4.94D-9, 0.172D0, 0.23D0/
C
      data EL(30), IST(30), (T(I,30),I=1,5) /'S ', 1,
     $     10.4D0, 1.D0, 5.49D-8, 0.1D0, 0.25D0/
C
      data EL(31), IST(31), (T(I,31),I=1,5) /'S ', 2,
     $     23.3D0, 1.D0, 6.81D-8, 0.693D0, 0.21D0/
C
      data EL(32), IST(32), (T(I,32),I=1,5) /'S ', 3,
     $     34.8D0, 1.D0, 2.14D-8, 0.353D0, 0.24D0/
C
      data EL(33), IST(33), (T(I,33),I=1,5) /'S ', 4,
     $     47.3D0, 1.D0, 1.66D-8, 1.03D0, 0.14D0/
C
      data EL(34), IST(34), (T(I,34),I=1,5) /'K ', 1,
     $     4.3D0, 1.D0, 2.02D-7, 0.272D0, 0.31D0/
C
      data EL(35), IST(35), (T(I,35),I=1,5) /'CA', 1,
     $     6.1D0, 0.D0, 4.4D-7, 0.848D0, 0.33D0/
C
      data EL(36), IST(36), (T(I,36),I=1,5) /'CA', 2,
     $     11.9D0, 0.D0, 5.22D-8, 0.151D0, 0.34D0/
C
      data EL(37), IST(37), (T(I,37),I=1,5) /'FE', 1,
     $     7.9D0, 0.D0, 2.52D-7, 0.701D0, 0.25D0/
C
      data EL(38), IST(38), (T(I,38),I=1,5) /'NE', 3,
     $     63.5D0, 1.D0, 7.03D-9, 0.0677D0, 0.39D0/
C
      data EL(39), IST(39), (T(I,39),I=1,5) /'NE', 4,
     $     97.1D0, 1.D0, 4.24D-9, 0.0482D0, 0.58D0/
C
      data EL(40), IST(40), (T(I,40),I=1,5) /'NE', 5,
     $     126.1D0, 1.D0, 2.79D-9, 0.305D0, 0.25D0/
C
      data EL(41), IST(41), (T(I,41),I=1,5) /'NE', 6,
     $     157.9D0, 0.D0, 3.45D-9, 0.581D0, 0.28D0/
C
      data EL(42), IST(42), (T(I,42),I=1,5) /'NE', 7,
     $     207.3D0, 1.D0, 9.56D-10, 0.749D0, 0.14D0/
C
      data EL(43), IST(43), (T(I,43),I=1,5) /'NE', 8,
     $     239.1D0, 1.D0, 4.73D-10, 0.992D0, 0.04D0/
C
      data EL(44), IST(44), (T(I,44),I=1,5) /'NE', 9,
     $     1196.D0, 1.D0, 3.92D-11, 0.262D0, 0.20D0/
C
      data EL(45), IST(45), (T(I,45),I=1,5) /'NA', 2,
     $     47.3D0, 1.D0, 7.35D-9, 0.056D0, 0.35D0/
C
      data ERROR  /-1.D0/
      data KILROY /.true./
      data LABEL /'Voronov'/
C     !EJECT
C
      call HI ('MOLAR')
C     !BEG
      do 100 I = 1,N
        if((EL(I).eq.QELSM(:2)).and.(IST(I).eq.IONST)) then
          DE = T(1,I)
          P  = T(2,I)
          A  = T(3,I)
          X  = T(4,I)
          CK = T(5,I)
          goto 101
        end if
  100 continue
      DE = ERROR
      if(KILROY.and.MESS) then
        KILROY = .false.
        call LAPSE (QELSM, IONST, LABEL, N, EL, IST)
      end if
C
  101 continue
C     !END
      call BYE ('MOLAR')
C
      return
      end
