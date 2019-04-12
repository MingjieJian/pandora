      subroutine NURGAN
     $(LEV,XLM,GBF)
C
C     Rudolf Loeser, 2003 Aug 05
C---- Computes Hydrogen bound-free Gaunt factor
C     for level LEV at wavelength XLM (Angstroms).
C
C     Based on the tables of
C
C     K a r z a s   &   L a t t e r ,    1962, ApJS, 6, 167;
C
C     which list the Gaunt factors for each level as functions
C     of electron energy in Rydberg units measured from the
C     ionization limit.
C
C     These tables have been modified as follows:
C     Gaunt factor values have been changed by 1 in the 4th place
C       for level 9 at E = 0.1111+0
C       for levels 10,14,15 at E = 0.1000+1;
C     and additional values have been inserted in the tables for
C       levels 9 and 11-15.
C     !DASH
      save
C     !DASH
      real*8 CRIT11, CRIT9, E, EE, EEL, EL, GBF, GF, GFL, GL, ONE, P, R,
     $       RYDBRG, XLM, ZERO
      integer I, IRET, J, KODEL, KODEP, LEV, M, MEV, MODEL, N
      logical KILROY
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 4),RYDBRG)
C     !DASH
      external PARINT, HI, BYE
C
      dimension  EE(31,15),  GF(31,15), N(15), R(4),
     $          EEL(31,15), GFL(31,15)
C     !EJECT
      data N(     1) /31/
      data (EE(I, 1),I=1,31) /
     $  0.1000D-05, 0.1111D-04, 0.1000D-03, 0.1111D-02, 0.2500D-02,
     $  0.4444D-02, 0.1000D-01, 0.2041D-01, 0.4000D-01, 0.1111D+00,
     $  0.2500D+00, 0.4444D+00, 0.6944D+00, 0.1000D+01, 0.1235D+01,
     $  0.1562D+01, 0.2041D+01, 0.2778D+01, 0.4000D+01, 0.6250D+01,
     $  0.1111D+02, 0.1600D+02, 0.2500D+02, 0.4444D+02, 0.1000D+03,
     $  0.2041D+03, 0.4000D+03, 0.1111D+04, 0.2500D+04, 0.4444D+04,
     $  0.1000D+05  /
      data (GF(I, 1),I=1,31) /
     $  0.7973D+00, 0.7973D+00, 0.7973D+00, 0.7976D+00, 0.7980D+00,
     $  0.7985D+00, 0.7999D+00, 0.8026D+00, 0.8076D+00, 0.8246D+00,
     $  0.8531D+00, 0.8850D+00, 0.9157D+00, 0.9423D+00, 0.9571D+00,
     $  0.9721D+00, 0.9856D+00, 0.9948D+00, 0.9939D+00, 0.9729D+00,
     $  0.9129D+00, 0.8585D+00, 0.7800D+00, 0.6687D+00, 0.5129D+00,
     $  0.3918D+00, 0.2971D+00, 0.1894D+00, 0.1302D+00, 0.9917D-01,
     $  0.6715D-01  /
C
      data N(     2) /31/
      data (EE(I, 2),I=1,31) /
     $  0.1000D-05, 0.1111D-04, 0.1000D-03, 0.1111D-02, 0.2500D-02,
     $  0.4444D-02, 0.1000D-01, 0.2041D-01, 0.4000D-01, 0.1111D+00,
     $  0.2500D+00, 0.4444D+00, 0.6944D+00, 0.1000D+01, 0.1235D+01,
     $  0.1562D+01, 0.2041D+01, 0.2778D+01, 0.4000D+01, 0.6250D+01,
     $  0.1111D+02, 0.1600D+02, 0.2500D+02, 0.4444D+02, 0.1000D+03,
     $  0.2041D+03, 0.4000D+03, 0.1111D+04, 0.2500D+04, 0.4444D+04,
     $  0.1000D+05  /
      data (GF(I, 2),I=1,31) /
     $  0.8761D+00, 0.8761D+00, 0.8763D+00, 0.8770D+00, 0.8779D+00,
     $  0.8793D+00, 0.8830D+00, 0.8896D+00, 0.9011D+00, 0.9342D+00,
     $  0.9755D+00, 0.1009D+01, 0.1033D+01, 0.1049D+01, 0.1056D+01,
     $  0.1061D+01, 0.1063D+01, 0.1058D+01, 0.1043D+01, 0.1006D+01,
     $  0.9316D+00, 0.8711D+00, 0.7875D+00, 0.6724D+00, 0.5142D+00,
     $  0.3923D+00, 0.2972D+00, 0.1894D+00, 0.1302D+00, 0.9918D-01,
     $  0.6715D-01  /
C
      data N(     3) /31/
      data (EE(I, 3),I=1,31) /
     $  0.1000D-05, 0.1111D-04, 0.1000D-03, 0.1111D-02, 0.2500D-02,
     $  0.4444D-02, 0.1000D-01, 0.2041D-01, 0.4000D-01, 0.1111D+00,
     $  0.2500D+00, 0.4444D+00, 0.6944D+00, 0.1000D+01, 0.1235D+01,
     $  0.1562D+01, 0.2041D+01, 0.2778D+01, 0.4000D+01, 0.6250D+01,
     $  0.1111D+02, 0.1600D+02, 0.2500D+02, 0.4444D+02, 0.1000D+03,
     $  0.2041D+03, 0.4000D+03, 0.1111D+04, 0.2500D+04, 0.4444D+04,
     $  0.1000D+05  /
      data (GF(I, 3),I=1,31) /
     $  0.9075D+00, 0.9075D+00, 0.9076D+00, 0.9089D+00, 0.9105D+00,
     $  0.9128D+00, 0.9189D+00, 0.9293D+00, 0.9458D+00, 0.9854D+00,
     $  0.1024D+01, 0.1051D+01, 0.1068D+01, 0.1078D+01, 0.1081D+01,
     $  0.1083D+01, 0.1081D+01, 0.1072D+01, 0.1053D+01, 0.1013D+01,
     $  0.9352D+00, 0.8735D+00, 0.7889D+00, 0.6731D+00, 0.5144D+00,
     $  0.3924D+00, 0.2973D+00, 0.1894D+00, 0.1302D+00, 0.9918D-01,
     $  0.6715D-01  /
C
      data N(     4) /31/
      data (EE(I, 4),I=1,31) /
     $  0.1000D-05, 0.1111D-04, 0.1000D-03, 0.1111D-02, 0.2500D-02,
     $  0.4444D-02, 0.1000D-01, 0.2041D-01, 0.4000D-01, 0.1111D+00,
     $  0.2500D+00, 0.4444D+00, 0.6944D+00, 0.1000D+01, 0.1235D+01,
     $  0.1562D+01, 0.2041D+01, 0.2778D+01, 0.4000D+01, 0.6250D+01,
     $  0.1111D+02, 0.1600D+02, 0.2500D+02, 0.4444D+02, 0.1000D+03,
     $  0.2041D+03, 0.4000D+03, 0.1111D+04, 0.2500D+04, 0.4444D+04,
     $  0.1000D+05  /
      data (GF(I, 4),I=1,31) /
     $  0.9247D+00, 0.9248D+00, 0.9249D+00, 0.9267D+00, 0.9291D+00,
     $  0.9323D+00, 0.9408D+00, 0.9542D+00, 0.9736D+00, 0.1014D+01,
     $  0.1047D+01, 0.1069D+01, 0.1082D+01, 0.1089D+01, 0.1091D+01,
     $  0.1091D+01, 0.1087D+01, 0.1077D+01, 0.1056D+01, 0.1015D+01,
     $  0.9365D+00, 0.8745D+00, 0.7894D+00, 0.6733D+00, 0.5145D+00,
     $  0.3924D+00, 0.2973D+00, 0.1894D+00, 0.1302D+00, 0.9918D-01,
     $  0.6715D-01  /
C
      data N(     5) /31/
      data (EE(I, 5),I=1,31) /
     $  0.1000D-05, 0.1111D-04, 0.1000D-03, 0.1111D-02, 0.2500D-02,
     $  0.4444D-02, 0.1000D-01, 0.2041D-01, 0.4000D-01, 0.1111D+00,
     $  0.2500D+00, 0.4444D+00, 0.6944D+00, 0.1000D+01, 0.1235D+01,
     $  0.1562D+01, 0.2041D+01, 0.2778D+01, 0.4000D+01, 0.6250D+01,
     $  0.1111D+02, 0.1600D+02, 0.2500D+02, 0.4444D+02, 0.1000D+03,
     $  0.2041D+03, 0.4000D+03, 0.1111D+04, 0.2500D+04, 0.4444D+04,
     $  0.1000D+05  /
      data (GF(I, 5),I=1,31) /
     $  0.9358D+00, 0.9358D+00, 0.9361D+00, 0.9385D+00, 0.9416D+00,
     $  0.9458D+00, 0.9563D+00, 0.9719D+00, 0.9925D+00, 0.1030D+01,
     $  0.1060D+01, 0.1078D+01, 0.1089D+01, 0.1094D+01, 0.1095D+01,
     $  0.1095D+01, 0.1090D+01, 0.1080D+01, 0.1058D+01, 0.1016D+01,
     $  0.9371D+00, 0.8747D+00, 0.7897D+00, 0.6734D+00, 0.5146D+00,
     $  0.3924D+00, 0.2973D+00, 0.1894D+00, 0.1302D+00, 0.9918D-01,
     $  0.6715D-01  /
C
      data N(     6) /31/
      data (EE(I, 6),I=1,31) /
     $  0.1000D-05, 0.1111D-04, 0.1000D-03, 0.1111D-02, 0.2500D-02,
     $  0.4444D-02, 0.1000D-01, 0.2041D-01, 0.4000D-01, 0.1111D+00,
     $  0.2500D+00, 0.4444D+00, 0.6944D+00, 0.1000D+01, 0.1235D+01,
     $  0.1562D+01, 0.2041D+01, 0.2778D+01, 0.4000D+01, 0.6250D+01,
     $  0.1111D+02, 0.1600D+02, 0.2500D+02, 0.4444D+02, 0.1000D+03,
     $  0.2041D+03, 0.4000D+03, 0.1111D+04, 0.2500D+04, 0.4444D+04,
     $  0.1000D+05  /
      data (GF(I, 6),I=1,31) /
     $  0.9436D+00, 0.9436D+00, 0.9439D+00, 0.9470D+00, 0.9509D+00,
     $  0.9560D+00, 0.9682D+00, 0.9851D+00, 0.1006D+01, 0.1041D+01,
     $  0.1067D+01, 0.1083D+01, 0.1092D+01, 0.1097D+01, 0.1098D+01,
     $  0.1097D+01, 0.1092D+01, 0.1081D+01, 0.1059D+01, 0.1017D+01,
     $  0.9374D+00, 0.8750D+00, 0.7898D+00, 0.6735D+00, 0.5146D+00,
     $  0.3924D+00, 0.2973D+00, 0.1894D+00, 0.1302D+00, 0.9918D-01,
     $  0.6715D-01  /
C
      data N(     7) /31/
      data (EE(I, 7),I=1,31) /
     $  0.1000D-05, 0.1111D-04, 0.1000D-03, 0.1111D-02, 0.2500D-02,
     $  0.4444D-02, 0.1000D-01, 0.2041D-01, 0.4000D-01, 0.1111D+00,
     $  0.2500D+00, 0.4444D+00, 0.6944D+00, 0.1000D+01, 0.1235D+01,
     $  0.1562D+01, 0.2041D+01, 0.2778D+01, 0.4000D+01, 0.6250D+01,
     $  0.1111D+02, 0.1600D+02, 0.2500D+02, 0.4444D+02, 0.1000D+03,
     $  0.2041D+03, 0.4000D+03, 0.1111D+04, 0.2500D+04, 0.4444D+04,
     $  0.1000D+05  /
      data (GF(I, 7),I=1,31) /
     $  0.9494D+00, 0.9495D+00, 0.9498D+00, 0.9535D+00, 0.9582D+00,
     $  0.9641D+00, 0.9776D+00, 0.9952D+00, 0.1015D+01, 0.1047D+01,
     $  0.1071D+01, 0.1086D+01, 0.1095D+01, 0.1099D+01, 0.1099D+01,
     $  0.1098D+01, 0.1093D+01, 0.1082D+01, 0.1060D+01, 0.1017D+01,
     $  0.9376D+00, 0.8751D+00, 0.7899D+00, 0.6735D+00, 0.5146D+00,
     $  0.3924D+00, 0.2973D+00, 0.1894D+00, 0.1302D+00, 0.9918D-01,
     $  0.6715D-01  /
C
      data N(     8) /31/
      data (EE(I, 8),I=1,31) /
     $  0.1000D-05, 0.1111D-04, 0.1000D-03, 0.1111D-02, 0.2500D-02,
     $  0.4444D-02, 0.1000D-01, 0.2041D-01, 0.4000D-01, 0.1111D+00,
     $  0.2500D+00, 0.4444D+00, 0.6944D+00, 0.1000D+01, 0.1235D+01,
     $  0.1562D+01, 0.2041D+01, 0.2778D+01, 0.4000D+01, 0.6250D+01,
     $  0.1111D+02, 0.1600D+02, 0.2500D+02, 0.4444D+02, 0.1000D+03,
     $  0.2041D+03, 0.4000D+03, 0.1111D+04, 0.2500D+04, 0.4444D+04,
     $  0.1000D+05  /
      data (GF(I, 8),I=1,31) /
     $  0.9540D+00, 0.9540D+00, 0.9544D+00, 0.9588D+00, 0.9642D+00,
     $  0.9708D+00, 0.9853D+00, 0.1003D+01, 0.1022D+01, 0.1052D+01,
     $  0.1074D+01, 0.1088D+01, 0.1096D+01, 0.1100D+01, 0.1100D+01,
     $  0.1099D+01, 0.1094D+01, 0.1082D+01, 0.1060D+01, 0.1017D+01,
     $  0.9377D+00, 0.8752D+00, 0.7899D+00, 0.6735D+00, 0.5146D+00,
     $  0.3925D+00, 0.2973D+00, 0.1894D+00, 0.1302D+00, 0.9918D-01,
     $  0.6715D-01  /
C
      data N(     9) /26/
      data (EE(I, 9),I=1,26) /
     $  0.1000D-05, 0.1111D-04, 0.1000D-03, 0.1111D-02, 0.1000D-01,
     $  0.1111D+00, 0.2500D+00, 0.4444D+00, 0.6944D+00, 0.1000D+01,
     $  0.1235D+01, 0.1562D+01, 0.2041D+01, 0.2778D+01, 0.4000D+01,
     $  0.6250D+01, 0.1111D+02, 0.2500D+02, 0.4444D+02, 0.1000D+03,
     $  0.2041D+03, 0.4000D+03, 0.1111D+04, 0.2500D+04, 0.4444D+04,
     $  0.1000D+05  /
      data (GF(I, 9),I=1,26) /
     $  0.9576D+00, 0.9577D+00, 0.9582D+00, 0.9632D+00, 0.9917D+00,
     $  0.1055D+01, 0.1077D+01, 0.1090D+01, 0.1097D+01, 0.1101D+01,
     $  0.1101D+01, 0.1099D+01, 0.1094D+01, 0.1082D+01, 0.1060D+01,
     $  0.1017D+01, 0.9378D+00, 0.7899D+00, 0.6736D+00, 0.5146D+00,
     $  0.3925D+00, 0.2973D+00, 0.1894D+00, 0.1302D+00, 0.9918D-01,
     $  0.6715D-01  /
C
      data N(    10) /31/
      data (EE(I,10),I=1,31) /
     $  0.1000D-05, 0.1111D-04, 0.1000D-03, 0.1111D-02, 0.2500D-02,
     $  0.4444D-02, 0.1000D-01, 0.2041D-01, 0.4000D-01, 0.1111D+00,
     $  0.2500D+00, 0.4444D+00, 0.6944D+00, 0.1000D+01, 0.1235D+01,
     $  0.1562D+01, 0.2041D+01, 0.2778D+01, 0.4000D+01, 0.6250D+01,
     $  0.1111D+02, 0.1600D+02, 0.2500D+02, 0.4444D+02, 0.1000D+03,
     $  0.2041D+03, 0.4000D+03, 0.1111D+04, 0.2500D+04, 0.4444D+04,
     $  0.1000D+05  /
      data (GF(I,10),I=1,31) /
     $  0.9606D+00, 0.9607D+00, 0.9613D+00, 0.9666D+00, 0.9737D+00,
     $  0.9814D+00, 0.9970D+00, 0.1014D+01, 0.1032D+01, 0.1058D+01,
     $  0.1078D+01, 0.1091D+01, 0.1098D+01, 0.1102D+01, 0.1102D+01,
     $  0.1100D+01, 0.1095D+01, 0.1083D+01, 0.1060D+01, 0.1018D+01,
     $  0.9379D+00, 0.8753D+00, 0.7900D+00, 0.6736D+00, 0.5146D+00,
     $  0.3925D+00, 0.2973D+00, 0.1894D+00, 0.1302D+00, 0.9918D-01,
     $  0.6715D-01  /
C
      data N(    11) /14/
      data (EE(I,11),I=1,14) /
     $  0.1000D-05, 0.1111D-04, 0.1000D-03, 0.1111D-02, 0.1000D-01,
     $  0.1111D+00, 0.2500D+00, 0.4444D+00, 0.6944D+00, 0.1000D+01,
     $  0.1111D+02, 0.1000D+03, 0.1111D+04, 0.1000D+05  /
      data (GF(I,11),I=1,14) /
     $  0.9632D+00, 0.9633D+00, 0.9639D+00, 0.9703D+00, 0.1002D+01,
     $  0.1060D+01, 0.1080D+01, 0.1092D+01, 0.1099D+01, 0.1102D+01,
     $  0.9379D+00, 0.5146D+00, 0.1894D+00, 0.6715D-01  /
C
      data N(    12) /14/
      data (EE(I,12),I=1,14) /
     $  0.1000D-05, 0.1111D-04, 0.1000D-03, 0.1111D-02, 0.1000D-01,
     $  0.1111D+00, 0.2500D+00, 0.4444D+00, 0.6944D+00, 0.1000D+01,
     $  0.1111D+02, 0.1000D+03, 0.1111D+04, 0.1000D+05  /
      data (GF(I,12),I=1,14) /
     $  0.9654D+00, 0.9654D+00, 0.9661D+00, 0.9732D+00, 0.1005D+01,
     $  0.1062D+01, 0.1081D+01, 0.1092D+01, 0.1099D+01, 0.1102D+01,
     $  0.9379D+00, 0.5146D+00, 0.1894D+00, 0.6715D-01  /
C
      data N(    13) /14/
      data (EE(I,13),I=1,14) /
     $  0.1000D-05, 0.1111D-04, 0.1000D-03, 0.1111D-02, 0.1000D-01,
     $  0.1111D+00, 0.2500D+00, 0.4444D+00, 0.6944D+00, 0.1000D+01,
     $  0.1111D+02, 0.1000D+03, 0.1111D+04, 0.1000D+05  /
      data (GF(I,13),I=1,14) /
     $  0.9672D+00, 0.9673D+00, 0.9681D+00, 0.9758D+00, 0.1008D+01,
     $  0.1063D+01, 0.1082D+01, 0.1093D+01, 0.1099D+01, 0.1102D+01,
     $  0.9380D+00, 0.5146D+00, 0.1894D+00, 0.6715D-01  /
C
      data N(    14) /14/
      data (EE(I,14),I=1,14) /
     $  0.1000D-05, 0.1111D-04, 0.1000D-03, 0.1111D-02, 0.1000D-01,
     $  0.1111D+00, 0.2500D+00, 0.4444D+00, 0.6944D+00, 0.1000D+01,
     $  0.1111D+02, 0.1000D+03, 0.1111D+04, 0.1000D+05  /
      data (GF(I,14),I=1,14) /
     $  0.9689D+00, 0.9690D+00, 0.9698D+00, 0.9780D+00, 0.1011D+01,
     $  0.1064D+01, 0.1082D+01, 0.1093D+01, 0.1099D+01, 0.1102D+01,
     $  0.9380D+00, 0.5146D+00, 0.1894D+00, 0.6715D-01  /
C
      data N(    15) /14/
      data (EE(I,15),I=1,14) /
     $  0.1000D-05, 0.1111D-04, 0.1000D-03, 0.1111D-02, 0.1000D-01,
     $  0.1111D+00, 0.2500D+00, 0.4444D+00, 0.6944D+00, 0.1000D+01,
     $  0.1111D+02, 0.1000D+03, 0.1111D+04, 0.1000D+05  /
      data (GF(I,15),I=1,14) /
     $  0.9703D+00, 0.9704D+00, 0.9714D+00, 0.9803D+00, 0.1014D+01,
     $  0.1064D+01, 0.1082D+01, 0.1093D+01, 0.1099D+01, 0.1102D+01,
     $  0.9380D+00, 0.5146D+00, 0.1894D+00, 0.6715D-01  /
C     !EJECT
      data KODEP /1/
      data KODEL,MODEL /1, 3/
      data ZERO,ONE /0.D0, 1.D0/
      data CRIT9,CRIT11 /1.111D1, 1.0D0/
      data KILROY /.true./
C
      call HI ('NURGAN')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        do 101 J = 1,15
          do 100 I = 1,N(J)
            EEL(I,J) = log(EE(I,J))
            GFL(I,J) = log(GF(I,J))
  100     continue
  101   continue
      end if
C
      if((LEV.ge.1).and.(LEV.le.15).and.(XLM.gt.ZERO)) then
        P = LEV**2
        E = RYDBRG/XLM-ONE/P
C
        if(E.le.EE(1,LEV)) then
          GBF = GF(1,LEV)
        else
C
          MEV = LEV
          if((E.ge.CRIT9).and.(LEV.eq.9)) then
            MEV = 8
          end if
          if((E.ge.CRIT11).and.(LEV.gt.10)) then
            MEV = 10
          end if
C
          M  = 0
          EL = log(E)
          call PARINT   (EEL(1,MEV), 1, GFL(1,MEV), 1, N(MEV), EL, GL,
     $                   KODEP, IRET, M, R)
          GBF = exp(GL)
        end if
C
      else
C
        GBF = ONE
      end if
C     !END
      call BYE ('NURGAN')
C
      return
      end
