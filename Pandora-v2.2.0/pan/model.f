      subroutine MODEL
     $(N,Z,TE,XNE,HND,V,VT,DGM,RZM,AEL,BDHM,HE304,J304I,KMASN,GMASIN,
     $ GMASRF,RFMAS,TEX,KTKIN,TAUKIN,VXS,FRS,H2N,NH2CS,RML,VR,REFLM,
     $ VSB,NVSB,VM,KVLG,KAMB,RHEAB,MFMV,FMV,ZME,XNC,CGR,YH,BHORIZ,R1N,
     $ KPRSW,CPRSS,CLOGG,LGGIN,PZERO,IZZ,ARR,LINE,NO)
C
C     Rudolf Loeser, 1981 Jan 02
C     Revised RL/SGK Apr 15 2014 
C---- Prints the atmosphere tables, for DABBLE.
C     (This is version 4 of MODEL.)
C     !DASH
      save
C     !DASH
      real*8 AEL, ARR, BDHM, BHORIZ, CGR, CLOGG, CPRSS, DGM, FMV, FRS,
     $       GMASIN, GMASRF, H2N, HE304, HND, PZERO, R1N, REFLM, RFMAS,
     $       RHEAB, RML, RZM, TAUKIN, TE, TEX, V, VM, VR, VSB, VT, VXS,
     $       XNC, XNE, YH, Z, ZERO, ZME
      integer IB, IE, INC, IQHSE, IQPZT, IQSFS, IQVSW, IZZ, J304I, KAMB,
     $        KKBHM, KKBHZ, KKDGM, KKFMV, KKNC, KKRHE, KKRZM, KKV, KKVM,
     $        KKVR, KKVSB, KKVT, KKVXS, KKZME, KMASN, KODE, KPRSW,
     $        KTKIN, KVLG, LGGIN, MFMV, N, NH2CS, NO, NVSB
      logical HEJB, IMSS, KVRV, SPHR, TAUK, TEXP, TFMV, TVM, TVSB, VMZ
      character LINE*120, TIT*40
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
      equivalence (IQQ( 31),IQSFS)
      equivalence (IQQ(173),IQVSW)
      equivalence (IQQ(292),IQPZT)
      equivalence (IQQ( 16),IQHSE)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external  PRIAM, FROGGY, COTTON, NAUGHTD, FROGZ, ARISOD, PHYLLIS,
     $          PIPE, LINER, FROGG, HI, BYE
      intrinsic min
C
C               RZM(N), AEL(N), HE304(N), VXS(N), GMASIN(N), GMASRF(N),
      dimension RZM(*), AEL(*), HE304(*), VXS(*), GMASIN(*), GMASRF(*),
C
C               XNE(N), HND(N), TAUKIN(N), BDHM(N), VR(N), VT(N), V(N),
     $          XNE(*), HND(*), TAUKIN(*), BDHM(*), VR(*), VT(*), V(*),
C
C               TEX(N), RHEAB(N), FRS(N), H2N(N), VSB(N), VM(N), TE(N),
     $          TEX(*), RHEAB(*), FRS(*), H2N(*), VSB(*), VM(*), TE(*),
C
C               RML(N), ARR(8), ZME(N), Z(N), FMV(N), XNC(N), DGM(N),
     $          RML(*), ARR(*), ZME(*), Z(*), FMV(*), XNC(*), DGM(*),
C
C               BHORIZ(N)
     $          BHORIZ(*)
C
      call HI ('MODEL')
C     !BEG
      if(IQPZT.gt.0) then
        call PRIAM     (NO, 'ATMOSPHERE-1', 12)
      else
        call PRIAM     (NO, 'ATMOSPHERE',   10)
      end if
C
      call ARISOD      (TE, N, TEX, N, ZERO, KODE)
      call NAUGHTD     (VM, 1, N, VMZ)
C
      KVRV = IQVSW.gt.0
      TEXP = KODE.eq.0
      IMSS = KMASN.gt.0
      TAUK = KTKIN.gt.0
      HEJB = J304I.gt.0
      SPHR = IQSFS.gt.0
      TVSB = NVSB.gt.0
      TVM  = ((KVLG.gt.0).or.(KAMB.gt.0)).and.(.not.VMZ)
      TFMV = MFMV.gt.0
C     !EJECT
      IE = 0
  100 continue
        IB  = IE+1
        IE  = min(IE+8,N)
        INC = (IE-IB)+1
C
        call COTTON    (IB, IE, NO)
        call LINER     (1, NO)
        call FROGZ     ('Depth (km)',
     $                  Z(IB), INC, LINE, IZZ, NO)
        if(SPHR) then
          call FROGZ   ('Distance (radii)',
     $                  FRS(IB), INC, LINE, IZZ, NO)
        end if
        if(IMSS) then
          if(RFMAS.gt.ZERO) then
            call FROGG ('Reference mass (g/cm**2)',
     $                  GMASRF(IB), INC, LINE, IZZ, NO)
          end if
          call FROGG   ('Mass (g/cm**2)',
     $                  GMASIN(IB), INC, LINE, IZZ, NO)
        end if
        if(TAUK) then
          write (TIT,101) REFLM
  101     format(22X,'TAUK(',1PE12.5,')')
          call FROGZ   (TIT,
     $                  TAUKIN(IB), INC, LINE, IZZ, NO)
        end if
        call LINER     (1,NO)
        call FROGG     ('Kinetic Temperature (K)',
     $                  TE(IB), INC, LINE, IZZ, NO)
        if(TEXP) then
          call FROGG   ('Excitation Temperature (K)',
     $                  TEX(IB), INC, LINE, IZZ, NO)
        end if
        call LINER     (1, NO)
        call FROGG     ('Electron density ( /cm**3)',
     $                  XNE(IB), INC, LINE, IZZ, NO)
        call FROGGY    ('Metals ion density',
     $                  ZME(IB), INC, LINE, IZZ, NO, IB, KKZME, N)
        call FROGG     ('Total Hydrogen density ( /cm**3)',
     $                  HND(IB), INC, LINE, IZZ, NO)
        call FROGGY    ('[Charged particle density ( /cm**3)]',
     $                  XNC(IB), INC, LINE, IZZ, NO, IB, KKNC, N)
        if(NH2CS.gt.0) then
          call FROGG   ('Molecular Hydrogen density ( /cm**3)',
     $                  H2N(IB), INC, LINE, IZZ, NO)
        end if
        call LINER     (1, NO)
C     !EJECT
        if(KVRV) then
          call FROGGY ('Radial broadening velocity (km/s)',
     $                 VR(IB), INC, LINE, IZZ, NO, IB, KKVR, N)
          call FROGGY ('Tangential broadening velocity (km/s)',
     $                 V(IB), INC, LINE, IZZ, NO, IB, KKV, N)
        else
          call FROGGY ('Broadening velocity (km/s)',
     $                 V(IB), INC, LINE, IZZ, NO, IB, KKV, N)
        end if
        if(IQHSE.gt.0) then
          call FROGGY ('Turbulent pressure velocity (km/s)',
     $                 VT(IB), INC, LINE, IZZ, NO, IB, KKVT, N)
        end if
        call FROGGY   ('G-multiplier [DGM(z)]',
     $                 DGM(IB), INC, LINE, IZZ, NO, IB, KKDGM, N)
        call FROGGY   ('B-horizontal (dyn/cm**2)',
     $                 BHORIZ(IB), INC, LINE, IZZ, NO, IB, KKBHZ, N)
        call FROGGY   ('Expansion velocity (km/s)',
     $                 VXS(IB), INC, LINE, IZZ, NO, IB, KKVXS, N)
        if(TVM) then
          call FROGGY ('Mass-motion velocity (km/s)',
     $                 VM(IB), INC, LINE, IZZ, NO, IB, KKVM, N)
        end if
        if(TVSB) then
          call FROGGY ('"Sobolev" expansion velocity (km/s)',
     $                 VSB(IB), INC, LINE, IZZ, NO, IB, KKVSB, N)
        end if
        if(TFMV) then
          call FROGGY ('(Fluid velocity multiplier)',
     $                 FMV(IB), INC, LINE, IZZ, NO, IB, KKFMV, N)
        end if
        if(SPHR) then
          call FROGG  ('Mass loss rate (g/s)',
     $                 RML(IB), INC, LINE, IZZ, NO)
        else
          call FROGG  ('Mass loss rate (g/s/cm**2)',
     $                 RML(IB), INC, LINE, IZZ, NO)
        end if
C     !EJECT
        call LINER   (1, NO)
        call FROGGY  ('Helium abundance variation',
     $                RHEAB(IB), INC, LINE, IZZ, NO, IB, KKRHE, N)
        call FROGGY  ('Electron multiplier',
     $                RZM(IB), INC, LINE, IZZ, NO, IB, KKRZM, N)
        call FROGG   ('Added electrons',
     $                AEL(IB), INC, LINE, IZZ, NO)
        call FROGGY  ('H- departure coefficient',
     $                BDHM(IB), INC, LINE, IZZ, NO, IB, KKBHM, N)
        if(HEJB) then
          call FROGG ('He-II 304 Line Jbar',
     $                HE304(IB), INC, LINE, IZZ, NO)
        end if
      if(IE.lt.N) goto 100
C
      call PIPE      (CGR, YH, RFMAS, R1N, KPRSW, CPRSS, CLOGG, LGGIN,
     $                PZERO, KTKIN, LINE, NO)
      call PHYLLIS   (NO, TEXP, VXS, N, TVM, TVSB, IQPZT, IZZ)
C     !END
      call BYE ('MODEL')
C
      return
      end
