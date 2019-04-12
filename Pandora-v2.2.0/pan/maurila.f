      subroutine MAURILA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1981 Sep 02
C---- Allocates scratch storage for GREY.
C     (Completely revised, 2000 Jul 24)
C     (This is version 2 of MAURILA.)
C     !DASH
      save
C     !DASH
      integer IN, IPEX, IQLTE, IS, J, KM, KMZ, LF, LKM, LKMB, LKZ, LUEO,
     $        MUX, N, NKM, NKZ
      character CALLER*(*)
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
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(19),LF )
      equivalence (JZQ(49),KM )
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
      equivalence (IQQ( 33),IQLTE)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external WGET, WLCK, MESHED, MASHED, HI, BYE
C
      dimension IN(*)
C
      call HI ('MAURILA')
C     !BEG
      call WGET (IS, CALLER)
C
      NKM = KM*N
      LKM = KM*LF
      if(IQLTE.le.0) then
        NKZ = 0
        LKZ = 0
        KMZ = 0
      else
        NKZ = NKM
        LKZ = LKM
        KMZ = KM
      end if
      if(FLOBRD) then
        LKMB = LKM
      else
        LKMB = 0
      end if
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+KM
      IN( 3) = IN( 2)+NKM
      IN( 4) = IN( 3)+NKZ
      IN( 5) = IN( 4)+NKM
      IN( 6) = IN( 5)+NKZ
      IN( 7) = IN( 6)+LKM
      IN( 8) = IN( 7)+LKZ
      IN( 9) = IN( 8)+KM
      IN(10) = IN( 9)+KM
      IN(11) = IN(10)+KMZ
C
      IN(12) = IN(11)+N
      IN(13) = IN(12)+KM
      IN(14) = IN(13)+KMZ
      IN(15) = IN(14)+KM
      MUX    = IN(15)+LKMB
C
      call WLCK     (MUX, CALLER)
C
      if((IPEX.lt.0).or.(IPEX.eq.4)) then
        call MESHED ('MAURILA', 2)
        write (LUEO,100) IS,KM,NKM,LKM,IQLTE,FLOBRD,MUX
  100   format(' ',5I12,L5,I12)
        write (LUEO,100) (IN(J),J=1,15)
        call MASHED ('MAURILA')
      end if
C     !END
      call BYE ('MAURILA')
C
      return
      end
