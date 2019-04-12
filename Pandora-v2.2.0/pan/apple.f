      subroutine APPLE
     $(TS,M,YPRE,KF,XIF,AF,KB,XIB,KR,XIR,KS,XIS,AS,NDW,MSFQM,NO)
C
C     Rudolf Loeser, 1983 Mar 07.
C---- Prints intermediates for Line Source Function calculations.
C     (This is version 4 of APPLE.)
C     !DASH
      save
C     !DASH
      real*8 AF, AS, TS, XIB, XIF, XIR, XIS, YAFUL, YASYM, YPRE
      integer I, IQSFS, KB, KF, KR, KS, M, MSFQM, NDW, NO
      character YMSS*24
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (REST( 6),YASYM)
      equivalence (REST( 7),YAFUL)
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ( 31),IQSFS)
C     !DASH
C     !EJECT
      external PRIAM, LINER, CAMP, HI, BYE
C
C               XIF(KF), AF(KF), XIS(KS), AS(KS), XIB(KB), XIR(KR),
      dimension XIF(*),  AF(*),  XIS(*),  AS(*),  XIB(*),  XIR(*),
C
C               TS(M)
     $          TS(*)
C
      call HI ('APPLE')
C     !BEG
      call PRIAM     (NO, 'TABLES', 6)
      call LINER     (1, NO)
      write (NO,100)
  100 format(' ','Precalculated Numerical Tables')
C
      if((IQSFS.le.0).and.(MSFQM.gt.0)) then
        call LINER   (1, NO)
        write (NO,101) YPRE,M
  101   format(' ','For the pre-tabulated QR Matrix',5X,
     $             9('**********'),'*'//
     $         ' ','TS is a representative set of '
     $             'optical depth values.'/
     $         ' ','(The "pre-tabulated QR Matrix" is used only ',
     $             'if any source function calculation is done with ',
     $             'the "QR-MAPPED" method.)'//
     $         ' ','Damping Parameter',2X,1PE12.4,I12)
        call LINER   (1, NO)
        write (NO,102) 'TS   ',(TS(I),I=1,M)
  102   format(' ',A6,1X,1P10E12.4/(' ',7X,10E12.4))
      end if
C     !EJECT
      call LINER     (1, NO)
      write (NO,103) NDW
  103 format(' ','For the Line Source Function frequency ',
     $           'integrations',5X,7('**********'),'*'//
     $       ' ','The XI values represent wavelength displacements ',
     $           'DELTA LAMBDA, or frequency displacements DELTA NU,'/
     $       ' ','from line center, in units of the Doppler Width ',
     $           '(DW) at depth point # NDW =',I3,'.'/
     $       ' ','The A values are the frequency integration weights ',
     $           'calculated from XI.'/
     $       ' ','XIsym and Asym pertain to half of a symmetric ',
     $           'profile;'/
     $       ' ','XIblue pertains to the half-profile on the ',
     $           'blue side of line center;'/
     $       ' ','XIred pertains to the half-profile on the ',
     $           'red side of line center;'/
     $       ' ','XIfull and Afull pertain to a full profile, and ',
     $           'have been constructed from XIblue and XIred;'/
     $       ' ','(if XIblue and/or XIred are not specifically ',
     $           'specified in the input, then they are set equal ',
     $           'to XISYM by default).')
C
      call CAMP    (YASYM, YMSS)
      call LINER   (1,NO)
      write (NO,104) 'Asym:',YMSS
  104 format(' ','For ',A,2X,A)
      call CAMP    (YAFUL, YMSS)
      call LINER   (1,NO)
      write (NO,104) 'Afull:',YMSS
C
      call LINER   (1, NO)
      write (NO,102) 'XIsym ',(XIS(I),I=1,KS)
      call LINER   (1, NO)
      write (NO,102) 'Asym  ',(AS(I) ,I=1,KS)
      call LINER   (1, NO)
      write (NO,102) 'XIblue',(XIB(I),I=1,KB)
      call LINER   (1, NO)
      write (NO,102) 'XIred ',(XIR(I),I=1,KR)
      call LINER   (1, NO)
      write (NO,102) 'XIfull',(XIF(I),I=1,KF)
      call LINER   (1, NO)
      write (NO,102) 'Afull ',(AF(I) ,I=1,KF)
C     !END
      call BYE ('APPLE')
C
      return
      end
