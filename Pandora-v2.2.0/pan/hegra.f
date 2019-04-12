      subroutine HEGRA
     $(XNK,XND,NL,NT,N,MN1,MNG1,KAMB,KVLG,KTKIN,KMASN,KDFGS,KDFGA,KDFGB,
     $ PNF,N1MET,MFMV,KBNDS,WEIGHT)
C
C     Rudolf Loeser, 1989 Sep 11
C---- Makes sure things are all set for diffusion calculations.
C     (This is version 2 of HEGRA.)
C     !DASH
      save
C     !DASH
      real*8 PNF, WEIGHT, XND, XNK
      integer ION, IQAMD, IQAN1, IQEXA, IQSFS, IQVLG, IQVLS, J, KAMB,
     $        KBNDS, KDFGA, KDFGB, KDFGS, KDZIN, KMASN, KTKIN, KVLG, KW,
     $        LUEO, MFMV, MN1, MNG1, N, N1MET, NL, NT
      logical DOIT, KILROY, ZD, ZN, ZP, ZW
C     !COM
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
      equivalence (IQQ(169),IQEXA)
      equivalence (IQQ(219),IQAMD)
      equivalence (IQQ(221),IQVLG)
      equivalence (IQQ(265),IQVLS)
      equivalence (IQQ(272),IQAN1)
      equivalence (IQQ( 31),IQSFS)
C     !EJECT
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(76),KDZIN)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external  PENDA, TERROR, MUSHED, MASHED, ONE1, NAUGHTD, HALT,
     $          HI, BYE
      intrinsic max, min
C
C               XNK(N), XND(N,NL), PNF(N), WEIGHT(mul,NT)
      dimension XNK(*), XND(N,*),  PNF(*), WEIGHT(*)
C
      call HI ('HEGRA')
C     !BEG
      KAMB = 0
      KVLG = 0
      DOIT = (IQAMD.gt.0).or.(IQVLG.gt.0)
C
      if(.not.DOIT) goto 999
C
      call PENDA    (ION)
      if(ION.gt.0) then
        if(IQAMD.gt.0) then
          KAMB = ION
        end if
        if(IQVLG.gt.0) then
          KVLG = ION
        end if
      end if
C
      call TERROR   (N, MN1, MNG1)
C
      KDFGS = max(min(KDFGS,2),0)
      if(KDFGS.gt.0) then
        if(KDFGA.lt.0) then
          KDFGA = N
        end if
        if(KDFGB.lt.0) then
          KDFGB = N
        end if
        if(KDFGA.gt.KDFGB) then
          write (MSSLIN(1),100) KDFGS,KDFGA,KDFGB
  100     format('KDIFGS =',I2,10X,'KDIFGA =',I10,10X,'KDIFGB =',I10)
          call HALT ('HEGRA', 1)
        end if
      end if
C
      call NAUGHTD  (PNF, 1, N, ZP)
      if(ZP) then
        call ONE1   (PNF, N)
      end if
C
      KILROY = .true.
C
      if(IQAN1.le.0) then
        call MUSHED ('HEGRA', 3, KILROY)
        write (LUEO,101)
  101   format(' ','Turning option AMDN1 off is NOT recommended!'/
     $         ' ','(This option is mainly for testing the program.)')
      end if
C     !EJECT
      if(IQAMD.gt.0) then
        if((IQVLS.gt.0).and.(IQEXA.le.0)) then
          call MUSHED ('HEGRA', 3, KILROY)
          write (LUEO,102)
  102     format(' ','Option VELS = on when option EXPAND = off ',
     $               'does not make sense.'/
     $           ' ','Note: moving Line Source Functions are computed ',
     $               'only when option EXPAND = on.')
        end if
C
        call NAUGHTD   (XNK,      1, N, ZD)
        do 103 J = 1,NL
          call NAUGHTD (XND(1,J), 1, N, ZN)
          if(ZN) then
            ZD = .true.
          end if
  103   continue
C
        if(ZD) then
          KDZIN = 1
          call MUSHED ('HEGRA', 3, KILROY)
          write (LUEO, 104)
  104     format(' ','A run with option AMDIFFF = on should have ',
     $               'input values of NK and all ND.'/
     $           ' ','The diffusion calculation will not be done in ',
     $               'the first iteration.')
        end if
C
        if((KTKIN.gt.0).or.(KMASN.gt.0)) then
          call MUSHED ('HEGRA', 3, KILROY)
          write (LUEO,105)
  105     format(' ','Giving ZMASS or TAUKIN values in the input ',
     $               'implies subsequent updating of the Z values,'/
     $           ' ','while AMDIFF = ON assumes that the input Z ',
     $               'values remain unchanged during the run.'/
     $           ' ','ZMASS or TAUKIN values should not ordinarily ',
     $               'be included when AMDIFF = ON.')
        end if
C
        if((N1MET.eq.3).and.(KAMB.eq.1)) then
          write (MSSLIN(1),106)
  106     format(' ','Diffusion: N1MET = 3 (simultaneous solution) ',
     $               'does not make sense for hydrogen.')
          call HALT    ('HEGRA', 1)
        end if
      end if
C     !EJECT
      if(IQVLG.gt.0) then
        if(IQSFS.gt.0) then
          write (MSSLIN(1),107)
  107     format(' ','Mass motion: SPHERE = ON is not allowed when ',
     $               'VELGRAD = ON (cf. special calculation of GX-1).')
          call HALT ('HEGRA', 1)
        end if
        if(MFMV.ne.0) then
          write (MSSLIN(1),108)
  108     format(' ','Mass motion: FMV = 0 is required when VELGRAD ',
     $               '= ON (cf. special calculation of GX-1).')
          call HALT ('HEGRA', 1)
        end if
      else
        KBNDS = 0
      end if
C
      KW = NT*((NL*(NL-1))/2)
      call NAUGHTD  (WEIGHT,1,KW,ZW)
      if(.not.ZW) then
        write (MSSLIN(1),109)
  109   format(' ','When AMDIFF = ON and/or VELGRAD = ON, all input ',
     $             'values of WEIGHT must = 0.')
        call HALT   ('HEGRA', 1)
      end if
C
      if(.not.KILROY) then
        call MASHED ('HEGRA')
      end if
C
  999 continue
C     !END
      call BYE ('HEGRA')
C
      return
      end
