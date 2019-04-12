      subroutine MAGENTA
     $(Z,TE,NW,WTAB,DDL,LDL,INDL,YHZ,WS,SNU,LEGEND,VEC,IW,LU)
C
C     Rudolf Loeser, 1986 Dec 11
C---- Drives Depths-of-Formation analysis printing for profiles.
C     (This is version 2 of MAGENTA.)
C     !DASH
      save
C     !DASH
      real*8 DDL, ONE, SNU, TE, VEC, WS, WTAB, YHZ, Z
      integer INDL, IORIC, IQORI, IW, KODE, LDL, LU, NW
      logical LEGEND
C     !COM
C---- LOOPER      as of 2006 May 05
      integer     NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      real*8      EMOO,WVLTRN
      logical     EXPAND,SPHERE,VXZERO,FLOBRD
      common      /LOOPER1/ NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      common      /LOOPER2/ EMOO,WVLTRN
      common      /LOOPER3/ EXPAND,SPHERE,VXZERO,FLOBRD
C
C     Emergent Profiles calculations control data (simplified version).
C
C     NVEL   : number of velocity tables
C     NVY    : current value of velocity-loop index, 1.le.NVY.le.NVEL
C              (i.e. index of current velocity set)
C     JVEL   : code describing current velocity set (i.e. KVEL(NVY) )
C              =     1 : VSB
C              =     2 : VXS
C              =     3 : VADD     (from AMDIFF and/or VELGRAD)
C              = 100+j : VXN(j)
C              = 200+j : VXN(j) + VADD
C              = 300+j : VAX(j)
C              = 400+j : VFB(j)
C              = 500+j : VFB(j) + VADD
C
C     LFBV   : number of viewing positions (front only, or back also)
C     LFB    : current value of views-loop index, 1 .le. LFB .le. LFBV
C              = 1 - front-face
C              = 2 - back-face
C
C     MF     : current value of look-angles-loop index, 1.le.MF.le.LF
C     MUK    : is .gt. 0 if line intensity profile must be printed
C              [when MUK > 0, then EMOO = EMU(MUK) ]
C     EMOO   : current value of look-angle
C     WVLTRN : wavelength (Angstroms) (i.e. at Delta-Lambda = 0).
C
C     VXZERO : tells whether the current velocity =0, or not
C     EXPAND : tells whether the procedures for expanding atmospheres
C              should be used (set up in SWEET)
C     SPHERE : tells whether this is a spherical, as opposed to
C              plane-parallel, atmosphere
C     FLOBRD : tells whether to use the flow-broadening procedure
C     .
C     !EJECT
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(210),IORIC)
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
      equivalence (IQQ(101),IQORI)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external ABJECT, LINER, TINAMOU, WEATHER, HI, BYE
C
      dimension IW(*)
C
C               SNU(N,KM), YHZ(KM), WS(N,KM), TE(N), Z(N), INDL(LDLMX),
      dimension SNU(*),    YHZ(*),  WS(*),    TE(*), Z(*), INDL(*),
C
C               WTAB(KM), DDL(LDLMX), VEC(N)
     $          WTAB(*),  DDL(*),     VEC(*)
C
      data KODE /2/
C
      call HI ('MAGENTA')
C     !BEG
      if((NW.gt.2).and.(LU.gt.0)) then
C
        if((IORIC.gt.0).and.(EMOO.eq.ONE)) then
          call LINER   (2, LU)
          if(LDL.gt.1) then
            write (LU,100) 's.'
          else
            write (LU,100) '.'
          end if
  100     format(' ','DEPTHS-OF-FORMATION analysis for line core',A,
     $           10X,'(To omit, set input parameter IORIC = 0)')
          call TINAMOU (Z, TE, VEC, YHZ, WS, SNU, DDL, INDL, LDL, LU)
        end if
C
        if(IQORI.gt.0) then
          call ABJECT  (LU)
          write (LU,101)
  101     format(' ','DEPTHS-OF-FORMATION analysis for the preceding ',
     $               'line intensity profile.')
          call WEATHER (Z, NW, WTAB, YHZ, WS, SNU, LEGEND, KODE, LU, IW)
        end if
C
      end if
C     !END
      call BYE ('MAGENTA')
C
      return
      end
