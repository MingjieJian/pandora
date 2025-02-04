      subroutine CEUTA
     $(N,IU,IL,IONST,KEQ,XNUU,XNUL,PL,A,B,C,D,E,F)
C
C     Rudolf Loeser, 2006 Aug 10
C---- Returns data for computing CE for Carbon I - V.
C     (If no data are available, returns KEQ = 0.)
C
C     S u n o ,  H . ,   &   K a t o ,  T .
C
C     Atomic Data and Nuclear Tables, 92 (2006), 407-455.
C     With corrections from Suno, 2006 Sep 06
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, E, F, PL, X, XNUL, XNUU
      integer IL, IONST, IU, J, K, KEQ, KK, MI, N
C     !DASH
      external HI, BYE
C
      dimension K(2,25,5), X(9,25,5), MI(5)
C
      data MI /25, 7, 21, 8, 8/
C
      data (K(I, 1,1),I=1,2),(X(I, 1,1),I=1,9) / 0 0201, 11,
     $      0.3056D0, 0.D0, 9.0D0,
     $      1.277D0, 1.304D1, -3.314D1, 6.357D1, -4.621D1, 4.0D-2/
C
      data (K(I, 2,1),I=1,2),(X(I, 2,1),I=1,9) / 0 0301, 10,
     $      0.649D0, 0.D0, 9.0D0,
     $      1.598D-1, 2.836D0, -9.813D0, 1.617D1, -1.012D1, 0.D0/
C
      data (K(I, 3,1),I=1,2),(X(I, 3,1),I=1,9) / 0 0401, 11,
     $      1.0014D0, 0.D0, 9.0D0,
     $      -2.302D0, 8.923D0, -2.873D1, 4.555D1, -2.431D1, 1.323D-1/
C
      data (K(I, 4,1),I=1,2),(X(I, 4,1),I=1,9) / 0 0501,  6,
     $      1.8093D0, 0.D0, 9.0D0,
     $      -6.117D0, 2.989D0, 4.148D0, 0.D0, 1.222D1, 0.D0/
C
      data (K(I, 5,1),I=1,2),(X(I, 5,1),I=1,9) / 0 0601, 11,
     $      1.8582D0, 0.D0, 9.D0,
     $      2.423D0, 7.590D-1, -6.486D0, 3.143D0, -2.074D1, 6.744D-1/
C
      data (K(I, 6,1),I=1,2),(X(I, 6,1),I=1,9) / 0 0701,  6,
     $      1.9214D0, 0.D0, 9.D0,
     $      1.307D1, -3.052D1, 1.835D1, 0.D0, 1.001D1, 0.D0/
C
      data (K(I, 7,1),I=1,2),(X(I, 7,1),I=1,9) / 1 0801,  7,
     $      2.1209D0, 0.D0, 9.D0,
     $      1.909D0, -6.101D-1, -2.336D0, -3.093D2, 7.450D2, 1.491D0/
C
      data (K(I, 8,1),I=1,2),(X(I, 8,1),I=1,9) / 2 0801, 10,
     $      2.1209D0, 0.D0, 9.D0,
     $      9.753D-1, 1.908D0, 2.161D0, -4.891D0, 0.D0, 0.D0/
C
      data (K(I, 9,1),I=1,2),(X(I, 9,1),I=1,9) / 3 0801, 10,
     $      2.1209D0, 0.D0, 9.D0,
     $      1.886D-3, 2.263D-1, 8.836D-1, -1.031D0, 0.D0, 0.D0/
C
      data (K(I,10,1),I=1,2),(X(I,10,1),I=1,9) / 4 0801,  6,
     $      2.1209D0, 0.D0, 9.D0,
     $      7.728D0, 8.661D-1, -1.640D1, 7.754D0, 0.D0, 0.D0/
C
      data (K(I,11,1),I=1,2),(X(I,11,1),I=1,9) / 5 0801,  7,
     $      2.1209D0, 0.D0, 9.D0,
     $      -1.389D0, 1.062D0, -3.742D-1, 4.786D0, -4.126D0, 2.606D-1/
C
      data (K(I,12,1),I=1,2),(X(I,12,1),I=1,9) / 1 0901,  7,
     $      2.2561D0, 0.D0, 9.D0,
     $      3.567D-2, 5.081D-1, -7.062D-1, 1.463D0, -2.569D0, 4.101D-1/
C
      data (K(I,13,1),I=1,2),(X(I,13,1),I=1,9) / 2 0901,  6,
     $      2.2561D0, 0.D0, 9.D0,
     $      1.146D1, -2.477D1, 1.381D1, 0.D0, 6.328D0, 0.D0/
C
      data (K(I,14,1),I=1,2),(X(I,14,1),I=1,9) / 3 0901,  6,
     $      2.2561D0, 0.D0, 9.D0,
     $      5.747D-2, -1.444D-1, 1.793D0, -1.719D0, 0.D0, 0.D0/
C
      data (K(I,15,1),I=1,2),(X(I,15,1),I=1,9) / 1 1001, 11,
     $      2.3419D0, 0.D0, 9.D0,
     $      -3.067D0, 2.352D0, -3.127D0, -9.123D0, 1.656D1, 1.476D-1/
C
      data (K(I,16,1),I=1,2),(X(I,16,1),I=1,9) / 2 1001,  6,
     $      2.3419D0, 0.D0, 9.D0,
     $      4.081D0, -9.274D0, 5.423D0, 0.D0, 4.970D0, 0.D0/
C
      data (K(I,17,1),I=1,2),(X(I,17,1),I=1,9) / 3 1001, 11,
     $      2.3419D0, 0.D0, 9.D0,
     $      1.183D0, -8.798D-1, 7.839D0, -1.718D1, -8.149D-1, 5.66D-1/
C
      data (K(I,18,1),I=1,2),(X(I,18,1),I=1,9) / 4 1001,  6,
     $      2.3419D0, 0.D0, 9.D0,
     $      1.247D0, -5.247D0, 3.983D0, 0.D0, 3.876D0, 0.D0/
C
      data (K(I,19,1),I=1,2),(X(I,19,1),I=1,9) / 0 1601,  7,
     $      2.9343D0, 0.D0, 9.D0,
     $      -3.820D0, 2.082D0, 2.801D0, -9.506D0, 1.552D1, 2.937D-1/
C
      data (K(I,20,1),I=1,2),(X(I,20,1),I=1,9) / 0 1801,  6,
     $      3.1718D0, 0.D0, 9.D0,
     $      8.801D-1, -9.630D0, 8.179D0, 0.D0, 1.199D1, 0.D0/
C
      data (K(I,21,1),I=1,2),(X(I,21,1),I=1,9) / 0 1901, 11,
     $      3.5939D0, 0.D0, 9.D0,
     $      2.975D0, -4.065D0, -2.943D-1, 7.361D1, -3.359D2, 1.40D0/
C
      data (K(I,22,1),I=1,2),(X(I,22,1),I=1,9) / 0 0302, 10,
     $      0.649D0, 0.3056D0, 5.D0,
     $      2.029D0, -1.093D1, 2.049D1, -1.221D1, 0.D0, 0.D0/
C
      data (K(I,23,1),I=1,2),(X(I,23,1),I=1,9) / 0 0702,  7,
     $      1.9214D0, 0.3056D0, 5.D0,
     $      1.003D2, -5.439D1, -1.260D2, 1.861D2, -9.357D2, 6.9D-1/
C
      data (K(I,24,1),I=1,2),(X(I,24,1),I=1,9) / 0 0502,  7,
     $      1.8093D0, 0.3056D0, 5.D0,
     $      9.576D0, -6.855D-1, 2.121D0, 3.983D0, -7.383D1, 4.750D-1/
C
      data (K(I,25,1),I=1,2),(X(I,25,1),I=1,9) / 0 0602,  6,
     $      1.8582D0, 0.3056D0, 5.D0,
     $      -1.795D0, -1.134D0, 2.995D0, 0.D0, 4.794D0, 0.D0/
C
C
      data (K(I, 1,2),I=1,2),(X(I, 1,2),I=1,9) / 0 0201,  7,
     $      1.2892D0, 0.D0, 6.D0,
     $      4.360D-1, 1.627D0, -2.923D0, 1.094D1, -6.580D0, 5.769D-2/
C
      data (K(I, 2,2),I=1,2),(X(I, 2,2),I=1,9) / 0 0301,  6,
     $      2.2463D0, 0.D0, 6.D0,
     $      -2.155D0, 1.374D1, -6.562D0, 0.D0, 1.018D0, 0.D0/
C
      data (K(I, 3,2),I=1,2),(X(I, 3,2),I=1,9) / 0 0401,  6,
     $      2.8928D0, 0.D0, 6.D0,
     $      8.572D-1, 2.079D-1, 7.058D-1, 0.D0, 4.136D0, 0.D0/
C
      data (K(I, 4,2),I=1,2),(X(I, 4,2),I=1,9) / 0 0501,  6,
     $      3.3164D0, 0.D0, 6.D0,
     $      -6.883D-1, 4.106D0, 3.435D-1, 0.D0, 1.631D1, 0.D0/
C
      data (K(I, 5,2),I=1,2),(X(I, 5,2),I=1,9) / 0 0601,  6,
     $      3.4937D0, 0.D0, 6.D0,
     $      -9.843D-1, 2.983D0, -9.975D-1, 0.D0, 8.910D-1, 0.D0/
C
      data (K(I, 6,2),I=1,2),(X(I, 6,2),I=1,9) / 0 0701,  6,
     $      3.9489D0, 0.D0, 6.D0,
     $      1.678D0, -2.238D0, 4.829D0, 0.D0, 0.D0, 0.D0/
C
      data (K(I, 7,2),I=1,2),(X(I, 7,2),I=1,9) / 0 0901,  6,
     $      4.3635D0, 0.D0, 6.D0,
     $      -1.703D0, 2.675D0, 1.165D0, 0.D0, 5.791D0, 0.D0/
C
C
      data (K(I, 1,3),I=1,2),(X(I, 1,3),I=1,9) / 0 0201,  6,
     $      1.5706D0, 0.D0, 1.D0,
     $      -4.024D-2, 2.914D0, -4.344D0, 2.121D0, 0.D0, 0.D0/
C
      data (K(I, 2,3),I=1,2),(X(I, 2,3),I=1,9) / 0 0301,  6,
     $      3.0684D0, 0.D0, 1.D0,
     $      6.261D-1, 3.044D0, 5.384D-1, 0.D0, 4.364D0, 0.D0/
C
      data (K(I, 3,3),I=1,2),(X(I, 3,3),I=1,9) / 0 0401,  6,
     $      4.1208D0, 0.D0, 1.D0,
     $      -5.732D-3, 8.467D-2, -1.676D-1, 1.307D-1, 0.D0, 0.D0/
C
      data (K(I, 4,3),I=1,2),(X(I, 4,3),I=1,9) / 0 0501,  6,
     $      4.3733D0, 0.D0, 1.D0,
     $      2.915D-1, 1.655D-1, -3.445D-1, 3.720D-1, 0.D0, 0.D0/
C
      data (K(I, 5,3),I=1,2),(X(I, 5,3),I=1,9) / 0 0601,  6,
     $      5.4718D0, 0.D0, 1.D0,
     $      2.092D-2, 2.289D-1, -4.268D-1, 2.137D-1, 0.D0, 0.D0/
C
      data (K(I, 6,3),I=1,2),(X(I, 6,3),I=1,9) / 0 0701,  6,
     $      7.1414D0, 0.D0, 1.D0,
     $      3.910D-3, 2.740D-1, 1.120D0, 1.110D0, 0.D0, 0.D0/
C
      data (K(I, 7,3),I=1,2),(X(I, 7,3),I=1,9) / 0 0801,  6,
     $      7.4100D0, 0.D0, 1.D0,
     $      4.260D-1, 4.350D-1, -2.280D0, 1.760D0, 0.D0, 0.D0/
C
      data (K(I, 8,3),I=1,2),(X(I, 8,3),I=1,9) / 0 0901,  6,
     $      7.7626D0, 0.D0, 1.D0,
     $      -1.140D-1, -5.230D-1, 8.110D-1, 0.D0, 3.730D-1, 0.D0/
C
      data (K(I, 9,3),I=1,2),(X(I, 9,3),I=1,9) / 0 1001,  6,
     $      8.0948D0, 0.D0, 1.D0,
     $      4.190D-2, 6.490D-2, 1.100D-2, 1.050D-1, 0.D0, 0.D0/
C
      data (K(I,10,3),I=1,2),(X(I,10,3),I=1,9) / 0 0402,  6,
     $      4.1208D0, 1.5706D0, 9.D0,
     $      9.842D0, -8.660D0, 2.184D1, 0.D0, 1.631D0, 0.D0/
C
      data (K(I,11,3),I=1,2),(X(I,11,3),I=1,9) / 0 0502,  6,
     $      4.3733D0, 1.5706D0, 9.D0,
     $      -1.822D-1, 4.490D0, -4.889D0, 2.056D0, 0.D0, 0.D0/
C
      data (K(I,12,3),I=1,2),(X(I,12,3),I=1,9) / 0 0602,  6,
     $      5.4718D0, 1.5706D0, 9.D0,
     $      3.280D-4, 1.676D-1, 2.778D-1, -4.524D-1, 0.D0, 0.D0/
C
      data (K(I,13,3),I=1,2),(X(I,13,3),I=1,9) / 0 0702,  6,
     $      7.1414D0, 1.5706D0, 9.D0,
     $      -1.200D-2, 1.310D0, -4.030D0, 3.590D0, 0.D0, 0.D0/
C
      data (K(I,14,3),I=1,2),(X(I,14,3),I=1,9) / 0 0902,  6,
     $      7.7626D0, 1.5706D0, 9.D0,
     $      -1.310D-2, 1.580D0, -4.800D0, 4.270D0, 0.D0, 0.D0/
C
      data (K(I,15,3),I=1,2),(X(I,15,3),I=1,9) / 0 1002,  6,
     $      8.0948D0, 1.5706D0, 9.D0,
     $      -2.900D-1, 2.090D0, 7.110D0, 0.D0, 1.010D1, 0.D0/
C
      data (K(I,16,3),I=1,2),(X(I,16,3),I=1,9) / 0 0403,  6,
     $      4.1208D0, 3.0684D0, 3.D0,
     $      -8.624D-3, 4.141D0, -7.089D0, 3.824D0, 0.D0, 0.D0/
C
      data (K(I,17,3),I=1,2),(X(I,17,3),I=1,9) / 0 0503,  6,
     $      4.3733D0, 3.0684D0, 3.D0,
     $      3.762D0, 9.351D0, -3.004D0, 0.D0, 7.320D0, 0.D0/
C
      data (K(I,18,3),I=1,2),(X(I,18,3),I=1,9) / 0 0603,  6,
     $      5.4718D0, 3.0684D0, 3.D0,
     $      3.032D0, -2.375D0, 2.675D0, 0.D0, 2.991D0, 0.D0/
C
      data (K(I,19,3),I=1,2),(X(I,19,3),I=1,9) / 0 0504,  6,
     $      4.3733D0, 4.1208D0, 9.D0,
     $      1.925D0, 1.440D1, -3.560D1, 2.727D1, 0.D0, 0.D0/
C
      data (K(I,20,3),I=1,2),(X(I,20,3),I=1,9) / 0 0604,  6,
     $      5.4718D0, 4.1208D0, 9.D0,
     $      4.319D-2, 1.004D0, -1.037D0, 3.630D-1, 0.D0, 0.D0/
C
      data (K(I,21,3),I=1,2),(X(I,21,3),I=1,9) / 0 0605,  6,
     $      5.4718D0, 4.3733D0, 5.D0,
     $      8.948D-1, -8.607D-1, 1.007D0, -4.136D-1, 0.D0, 0.D0/
C
C
      data (K(I, 1,4),I=1,2),(X(I, 1,4),I=1,9) / 0 0201,  6,
     $      1.9364D0, 0.D0, 2.D0,
     $      4.744D0, 4.440D0, 5.971D-1, 0.D0, 4.519D0, 0.D0/
C
      data (K(I, 2,4),I=1,2),(X(I, 2,4),I=1,9) / 0 0301,  6,
     $      9.0792D0, 0.D0, 2.D0,
     $      4.846D-1, -2.743D-1, 4.768D-1, -3.971D-1, 0.D0, 0.D0/
C
      data (K(I, 3,4),I=1,2),(X(I, 3,4),I=1,9) / 0 0401,  6,
     $      9.5958D0, 0.D0, 2.D0,
     $      -5.731D-1, 9.548D-1, -8.931D-2, 0.D0, 5.638D-1, 0.D0/
C
      data (K(I, 4,4),I=1,2),(X(I, 4,4),I=1,9) / 0 0501,  6,
     $      9.74D0, 0.D0, 2.D0,
     $      1.252D0, -1.243D0, 4.93D-1, 1.326D-1, 0.D0, 0.D0/
C
      data (K(I, 5,4),I=1,2),(X(I, 5,4),I=1,9) / 0 0701, 10,
     $      12.2413D0, 0.D0, 2.D0,
     $      2.775D-1, -6.663D-1, 4.959D-1, 0.D0, 3.530D-6, 0.D0/
C
      data (K(I, 6,4),I=1,2),(X(I, 6,4),I=1,9) / 0 0302, 10,
     $      9.0792D0, 1.9364D0, 6.D0,
     $      1.273D0, -5.038D0, 5.374D0, 0.D0, 2.27D-3, 0.D0/
C
      data (K(I, 7,4),I=1,2),(X(I, 7,4),I=1,9) / 0 0502, 10,
     $      9.74D0, 1.9364D0, 6.D0,
     $      1.899D1, -4.193D1, 2.857D1, 0.D0, 4.027D-3, 0.D0/
C
      data (K(I, 8,4),I=1,2),(X(I, 8,4),I=1,9) / 0 0602, 10,
     $      12.0321D0, 1.9364D0, 6.D0,
     $      1.964D-1, -6.743D-1, 7.166D-1, 0.D0, 2.411D-4, 0.D0/
C
C
      data (K(I, 1,5),I=1,2),(X(I, 1,5),I=1,9) / 0 0201,  6,
     $      72.2881D0, 0.D0, 1.D0,
     $      -1.820D-5, 1.093D-3, 1.675D-2, -8.788D-3, 0.D0, 0.D0/
C
      data (K(I, 2,5),I=1,2),(X(I, 2,5),I=1,9) / 0 0301,  6,
     $      73.6061D0, 0.D0, 1.D0,
     $      8.926D-5, -2.824D-3, 7.379D-2, -2.670D-2, 0.D0, 0.D0/
C
      data (K(I, 3,5),I=1,2),(X(I, 3,5),I=1,9) / 0 0401,  6,
     $      74.4496D0, 0.D0, 1.D0,
     $      1.642D-2, -7.345D-2, 9.320D-2, 0.D0, 1.247D-1, 0.D0/
C
      data (K(I, 4,5),I=1,2),(X(I, 4,5),I=1,9) / 0 0501,  6,
     $      85.4837D0, 0.D0, 1.D0,
     $      5.061D-5, -5.619D-4, 2.116D-3, 7.710D-4, 0.D0, 0.D0/
C
      data (K(I, 5,5),I=1,2),(X(I, 5,5),I=1,9) / 0 0601,  6,
     $      85.7217D0, 0.D0, 1.D0,
     $      1.286D-2, -2.133D-2, 1.656D-2, 0.D0, 2.107D-2, 0.D0/
C
      data (K(I, 6,5),I=1,2),(X(I, 6,5),I=1,9) / 0 0302,  6,
     $      73.6061D0, 72.2881D0, 3.D0,
     $      9.507D0, 1.195D1, -7.559D0, 0.D0, 4.417D0, 0.D0/
C
      data (K(I, 7,5),I=1,2),(X(I, 7,5),I=1,9) / 0 0402,  6,
     $      74.4496D0, 72.2881D0, 3.D0,
     $      -5.935D-3, 6.629D-1, -7.935D-1, 2.695D-1, 0.D0, 0.D0/
C
      data (K(I, 8,5),I=1,2),(X(I, 8,5),I=1,9) / 0 0502,  6,
     $      85.4837D0, 72.2881D0, 3.D0,
     $      -7.449D-1, 1.335D0, -3.423D-1, 0.D0, 1.011D0, 0.D0/
C     !EJECT
C
      call HI ('CEUTA')
C     !BEG
      KEQ = 0
C
      if((IONST.ge.1).and.(IONST.le.5)) then
        KK = 10000*N+100*IU+IL
        do 100 J = 1,MI(IONST)
          if(K(1,J,IONST).eq.KK) then
            KEQ  = K(2,J,IONST)
            XNUU = X(1,J,IONST)
            XNUL = X(2,J,IONST)
            PL   = X(3,J,IONST)
            A    = X(4,J,IONST)
            B    = X(5,J,IONST)
            C    = X(6,J,IONST)
            D    = X(7,J,IONST)
            E    = X(8,J,IONST)
            F    = X(9,J,IONST)
          end if
  100   continue
      end if
C
  101 continue
C     !END
      call BYE ('CEUTA')
C
      return
      end
