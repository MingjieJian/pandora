      subroutine DIEGO
     $(K,KD,ISO,M,EF)
C
C     Rudolf Loeser, 1993 Mar 19
C---- Computes F(K,K+dK,m) for FRESH.
C     ISO may equal 12 or 13.
C     !DASH
      save
C     !DASH
      real*8 EF, EM, ONE, PM, TAB
      integer I, ISO, K, KD, M
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
      dimension TAB(7,21,2,2)
C
      data (TAB(I, 1,1,1),I=1,7) /
     $  1.0000D+00,  9.6530D-05,  3.2232D-06, -1.0856D-08, -1.5228D-11,
     $ -9.1649D-14,  0.0000D+00/
      data (TAB(I, 2,1,1),I=1,7) /
     $  1.0000D+00,  7.1456D-05,  3.0903D-06, -1.1108D-08, -1.3030D-11,
     $ -9.5811D-14,  0.0000D+00/
      data (TAB(I, 3,1,1),I=1,7) /
     $  1.0000D+00,  4.5883D-05,  2.9720D-06, -1.1345D-08, -1.1069D-11,
     $ -1.0101D-13,  0.0000D+00/
      data (TAB(I, 4,1,1),I=1,7) /
     $  1.0000D+00,  1.9928D-05,  2.8664D-06, -1.1585D-08, -9.3645D-12,
     $ -1.0669D-13,  0.0000D+00/
      data (TAB(I, 5,1,1),I=1,7) /
     $  1.0000D+00, -6.4001D-06,  2.7727D-06, -1.1838D-08, -7.9661D-12,
     $ -1.1251D-13,  0.0000D+00/
      data (TAB(I, 6,1,1),I=1,7) /
     $  1.0000D+00, -3.3188D-05,  2.6893D-06, -1.2091D-08, -6.8980D-12,
     $ -1.1898D-13,  0.0000D+00/
      data (TAB(I, 7,1,1),I=1,7) /
     $  1.0000D+00, -6.0358D-05,  2.6149D-06, -1.2359D-08, -6.1924D-12,
     $ -1.2553D-13,  0.0000D+00/
      data (TAB(I, 8,1,1),I=1,7) /
     $  1.0000D+00, -8.7980D-05,  2.5459D-06, -1.2635D-08, -5.7816D-12,
     $ -1.3240D-13,  0.0000D+00/
      data (TAB(I, 9,1,1),I=1,7) /
     $  1.0000D+00, -1.1610D-04,  2.4843D-06, -1.2913D-08, -5.8562D-12,
     $ -1.3991D-13,  0.0000D+00/
      data (TAB(I,10,1,1),I=1,7) /
     $  1.0000D+00, -1.4468D-04,  2.4272D-06, -1.3203D-08, -6.3505D-12,
     $ -1.4764D-13,  0.0000D+00/
      data (TAB(I,11,1,1),I=1,7) /
     $  9.9999D-01, -1.7381D-04,  2.3733D-06, -1.3491D-08, -7.3186D-12,
     $ -1.5619D-13,  0.0000D+00/
      data (TAB(I,12,1,1),I=1,7) /
     $  9.9999D-01, -2.0331D-04,  2.3217D-06, -1.3815D-08, -8.8143D-12,
     $ -1.6410D-13,  0.0000D+00/
      data (TAB(I,13,1,1),I=1,7) /
     $  9.9998D-01, -2.3349D-04,  2.2702D-06, -1.4125D-08, -1.0838D-11,
     $ -1.7328D-13,  0.0000D+00/
      data (TAB(I,14,1,1),I=1,7) /
     $  9.9997D-01, -2.6413D-04,  2.2198D-06, -1.4461D-08, -1.3527D-11,
     $ -1.8228D-13,  0.0000D+00/
      data (TAB(I,15,1,1),I=1,7) /
     $  1.0000D+00, -2.9534D-04,  2.0635D-06, -1.4821D-08, -2.3341D-12,
     $ -1.9079D-13, -4.6426D-16/
      data (TAB(I,16,1,1),I=1,7) /
     $  9.9995D-01, -3.2736D-04,  2.1110D-06, -1.5137D-08, -2.0834D-11,
     $ -2.0236D-13,  0.0000D+00/
      data (TAB(I,17,1,1),I=1,7) /
     $  9.9993D-01, -3.5985D-04,  2.0503D-06, -1.5504D-08, -2.5590D-11,
     $ -2.1254D-13,  0.0000D+00/
      data (TAB(I,18,1,1),I=1,7) /
     $  1.0000D+00, -3.9285D-04,  1.7898D-06, -1.5932D-08, -3.9080D-12,
     $ -2.2067D-13, -8.7278D-16/
      data (TAB(I,19,1,1),I=1,7) /
     $  9.9990D-01, -4.2692D-04,  1.9104D-06, -1.6269D-08, -3.7602D-11,
     $ -2.3425D-13,  0.0000D+00/
      data (TAB(I,20,1,1),I=1,7) /
     $  1.0000D+00, -4.6128D-04,  1.3317D-06, -1.6750D-08, -6.2705D-12,
     $ -2.4187D-13, -1.2394D-15/
      data (TAB(I,21,1,1),I=1,7) /
     $  1.0000D+00, -4.9673D-04,  1.4146D-06, -1.7170D-08, -8.4661D-12,
     $ -2.5329D-13, -1.4382D-15/
      data (TAB(I, 1,2,1),I=1,7) /
     $  9.9930D-01,  2.5301D-03,  1.4706D-05,  0.0000D+00,  0.0000D+00,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I, 2,2,1),I=1,7) /
     $  9.9947D-01,  2.4734D-03,  1.4070D-05,  0.0000D+00,  0.0000D+00,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I, 3,2,1),I=1,7) /
     $  9.9961D-01,  2.3927D-03,  1.3481D-05,  1.8864D-09,  0.0000D+00,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I, 4,2,1),I=1,7) /
     $  9.9968D-01,  2.3490D-03,  1.2949D-05,  1.3950D-09,  0.0000D+00,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I, 5,2,1),I=1,7) /
     $  9.9970D-01,  2.3205D-03,  1.2467D-05,  0.0000D+00,  0.0000D+00,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I, 6,2,1),I=1,7) /
     $  9.9970D-01,  2.2740D-03,  1.2036D-05,  0.0000D+00,  0.0000D+00,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I, 7,2,1),I=1,7) /
     $  9.9969D-01,  2.2292D-03,  1.1654D-05,  0.0000D+00,  0.0000D+00,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I, 8,2,1),I=1,7) /
     $  9.9965D-01,  2.1856D-03,  1.1317D-05,  0.0000D+00,  0.0000D+00,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I, 9,2,1),I=1,7) /
     $  9.9958D-01,  2.1431D-03,  1.1028D-05,  0.0000D+00,  0.0000D+00,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I,10,2,1),I=1,7) /
     $  9.9950D-01,  2.1012D-03,  1.0780D-05,  0.0000D+00,  0.0000D+00,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I,11,2,1),I=1,7) /
     $  9.9940D-01,  2.0599D-03,  1.0576D-05,  0.0000D+00,  0.0000D+00,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I,12,2,1),I=1,7) /
     $  9.9928D-01,  2.0187D-03,  1.0412D-05,  0.0000D+00,  0.0000D+00,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I,13,2,1),I=1,7) /
     $  1.0001D+00,  2.0255D-03,  9.4628D-06, -3.3578D-09,  4.4353D-11,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I,14,2,1),I=1,7) /
     $  1.0001D+00,  1.9936D-03,  9.2362D-06, -4.0285D-09,  5.1913D-11,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I,15,2,1),I=1,7) /
     $  1.0001D+00,  1.9619D-03,  9.0417D-06, -4.7522D-09,  5.9754D-11,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I,16,2,1),I=1,7) /
     $  1.002D+00,  1.9135D-03,  8.8677D-06, -2.2324D-09,  6.8404D-11,
     $ -1.2440D-13,  0.0000D+00/
      data (TAB(I,17,2,1),I=1,7) /
     $  1.0002D+00,  1.8790D-03,  8.7297D-06, -2.5062D-09,  7.6962D-11,
     $ -1.4610D-13,  0.0000D+00/
      data (TAB(I,18,2,1),I=1,7) /
     $  1.0002D+00,  1.8439D-03,  8.6168D-06, -2.7800D-09,  8.5798D-11,
     $ -1.7057D-13,  0.0000D+00/
      data (TAB(I,19,2,1),I=1,7) /
     $  1.0002D+00,  1.8350D-03,  8.5435D-06, -8.3143D-09,  9.4091D-11,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I,20,2,1),I=1,7) /
     $  1.0002D+00,  1.8027D-03,  8.4902D-06, -9.4133D-09,  1.0279D-10,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I,21,2,1),I=1,7) /
     $  1.0002D+00,  1.7700D-03,  8.4636D-06, -1.0612D-08,  1.1132D-10,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I, 1,1,2),I=1,7) /
     $  1.0000D+00,  9.5178D-05,  3.0845D-06, -1.0186D-08, -1.4014D-11,
     $ -7.9877D-14,  0.0000D+00/
      data (TAB(I, 2,1,2),I=1,7) /
     $  1.0000D+00,  7.1138D-05,  2.9610D-06, -1.0404D-08, -1.2101D-11,
     $ -8.3853D-14,  0.0000D+00/
      data (TAB(I, 3,1,2),I=1,7) /
     $  1.0000D+00,  5.8634D-05,  2.8543D-06, -1.2967D-08, -1.0601D-11,
     $  0.0000D+00,  0.0000D+00/
      data (TAB(I, 4,1,2),I=1,7) /
     $  1.0000D+00,  2.2005D-05,  2.7518D-06, -1.0855D-08, -8.8516D-12,
     $ -9.2780D-14,  0.0000D+00/
      data (TAB(I, 5,1,2),I=1,7) /
     $  1.0000D+00, -3.1573D-06,  2.6631D-06, -1.1080D-08, -7.5548D-12,
     $ -9.8008D-14,  0.0000D+00/
      data (TAB(I, 6,1,2),I=1,7) /
     $  1.0000D+00, -2.8595D-05,  2.5844D-06, -1.1332D-08, -6.5676D-12,
     $ -1.0283D-13,  0.0000D+00/
      data (TAB(I, 7,1,2),I=1,7) /
     $  1.0000D+00, -5.4582D-05,  2.5128D-06, -1.1567D-08, -5.8322D-12,
     $ -1.0879D-13,  0.0000D+00/
      data (TAB(I, 8,1,2),I=1,7) /
     $  1.0000D+00, -8.0905D-05,  2.4480D-06, -1.1821D-08, -5.4179D-12,
     $ -1.1461D-13,  0.0000D+00/
      data (TAB(I, 9,1,2),I=1,7) /
     $  1.0000D+00, -1.0767D-04,  2.3888D-06, -1.2081D-08, -5.3473D-12,
     $ -1.2083D-13,  0.0000D+00/
      data (TAB(I,10,1,2),I=1,7) /
     $  1.0000D+00, -1.3484D-04,  2.3337D-06, -1.2356D-08, -5.6342D-12,
     $ -1.2708D-13,  0.0000D+00/
      data (TAB(I,11,1,2),I=1,7) /
     $  1.0000D+00, -1.6256D-04,  2.2820D-06, -1.2623D-08, -6.3332D-12,
     $ -1.3420D-13,  0.0000D+00/
      data (TAB(I,12,1,2),I=1,7) /
     $  9.9999D-01, -1.9074D-04,  2.2326D-06, -1.2904D-08, -7.4718D-12,
     $ -1.4145D-13,  0.0000D+00/
      data (TAB(I,13,1,2),I=1,7) /
     $  9.9998D-01, -2.1935D-04,  2.1835D-06, -1.3203D-08, -9.0496D-12,
     $ -1.4869D-13,  0.0000D+00/
      data (TAB(I,14,1,2),I=1,7) /
     $  9.9998D-01, -2.4851D-04,  2.1345D-06, -1.3505D-08, -1.1136D-11,
     $ -1.5646D-13,  0.0000D+00/
      data (TAB(I,15,1,2),I=1,7) /
     $  9.9997D-01, -2.7825D-04,  2.0838D-06, -1.3808D-08, -1.3757D-11,
     $ -1.6491D-13,  0.0000D+00/
      data (TAB(I,16,1,2),I=1,7) /
     $  9.9996D-01, -3.0851D-04,  2.0308D-06, -1.4130D-08, -1.6966D-11,
     $ -1.7331D-13,  0.0000D+00/
      data (TAB(I,17,1,2),I=1,7) /
     $  9.9995D-01, -3.3936D-04,  1.9738D-06, -1.4462D-08, -2.0798D-11,
     $ -1.8209D-13,  0.0000D+00/
      data (TAB(I,18,1,2),I=1,7) /
     $  9.9994D-01, -3.7080D-04,  1.9112D-06, -1.4811D-08, -2.5280D-11,
     $ -1.9096D-13,  0.0000D+00/
      data (TAB(I,19,1,2),I=1,7) /
     $  9.9993D-01, -4.0291D-04,  1.8427D-06, -1.5164D-08, -3.0510D-11,
     $ -2.0044D-13,  0.0000D+00/
      data (TAB(I,20,1,2),I=1,7) /
     $  9.9991D-01, -4.3574D-04,  1.7659D-06, -1.5527D-08, -3.6494D-11,
     $ -2.1031D-13,  0.0000D+00/
      data (TAB(I,21,1,2),I=1,7) /
     $  1.0000D+00, -4.6900D-04,  1.4305D-06, -1.5980D-08, -8.2778D-12,
     $ -2.1688D-13, -1.1228D-15/
      data (TAB(I, 1,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I, 2,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I, 3,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I, 4,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I, 5,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I, 6,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I, 7,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I, 8,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I, 9,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I,10,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I,11,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I,12,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I,13,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I,14,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I,15,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I,16,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I,17,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I,18,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I,19,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I,20,2,2),I=1,7) / 7*0.D0 /
      data (TAB(I,21,2,2),I=1,7) / 7*0.D0 /
C
      call HI ('DIEGO')
C     !BEG
      EF = TAB(1,(K+1),KD,(ISO-11))
      EM = M
      PM = ONE
      do 100 I = 2,7
        PM = PM*EM
        EF = EF+TAB(I,(K+1),KD,(ISO-11))*PM
  100 continue
      EF = EF**2
C     !END
      call BYE ('DIEGO')
C
      return
      end
