      subroutine ERATO
     $(X,W,IW,N,Z,HK,H1,HEK,HE1,HE2K,HE21,BETA,TE,ZT,XNE,HND,H2N,HEND,
     $ RHEAB,VM,VAMB,VBMB,VCMB,VDMB,VEMB,XION,ZION,ZI,Z1,Z2,Z3,ZXG,
     $ DEE,KVLG,DUMP)
C     Rudolf Loeser, 1991 Jan 03
C---- Computes velocities VAMB, VBMB, VCMB, VDMB, VEMB,
C     and intermediates Z1, Z2, Z3, ZXG, and DEE,
C     for the diffusion calculation.
C     May also recompute RHEAB and VM.
C     (This is version 2 of ERATO.)
C     !DASH
      save
C     !DASH
      real*8 BETA, DEE, H1, H2N, HE1, HE21, HE2K, HEK, HEND, HK, HND,
     $       RHEAB, TE, VAMB, VBMB, VCMB, VDMB, VEMB, VM, W, X, XION,
     $       XNE, Z, Z1, Z2, Z3, ZI, ZION, ZT, ZXG
      integer IDEEL, IDEN, IHEDF, IN, IPGS, IQAMD, IS, IVEC, IW, KVLG,
     $        MOX, N
      logical AMDIFF, DUMP
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 47),IHEDF)
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
      equivalence (IQQ(219),IQAMD)
C     !DASH
C     !EJECT
      external    EUTERPE, POLYP, GASSER, VULPES, THALIA, SIGMA, ZERO1,
     $            DANA, AQUILA, WGIVE, HI, BYE
C
      dimension   X(*), W(*), IW(*)
C
C                 Z(N), HK(N), H1(N), HEK(N), HE1(N), HE2K(N), HE21(N),
      dimension   Z(*), HK(*), H1(*), HEK(*), HE1(*), HE2K(*), HE21(*),
C
C                 TE(N), ZT(N), XNE(N), HND(N), Z2(N), RHEAB(N), ZI(N),
     $            TE(*), ZT(*), XNE(*), HND(*), Z2(*), RHEAB(*), ZI(*),
C
C                 VAMB(N), VBMB(N), VCMB(N), VDMB(N), VEMB(N), ZION(N),
     $            VAMB(*), VBMB(*), VCMB(*), VDMB(*), VEMB(*), ZION(*),
C
C                 XION(N), Z3(N), BETA(N), H2N(N), DEE(4,5,N), HEND(N),
     $            XION(*), Z3(*), BETA(*), H2N(*), DEE(4,5,*), HEND(*),
C
C                 VM(M), Z1(N), ZXG(N)
     $            VM(*), Z1(*), ZXG(*)
C
      dimension   IN(4)
      equivalence
     $(IN( 1),IDEN  ),(IN( 2),IPGS  ),(IN( 3),IVEC  ),(IN( 4),IDEEL )
C     !EJECT
C
      call HI ('ERATO')
C     !BEG
C     (Get, and allocate, W allotment)
      call EUTERPE  (IN, IS, MOX, 'ERATO')
C
      AMDIFF = IQAMD.gt.0
C
C---- Compute d-matrix
      if(AMDIFF) then
        call GASSER (X, N, W(IDEN), W(IPGS), W)
        call DANA   (N, Z, TE, XNE, W(IPGS), XION, RHEAB, HEK, HE1,
     $               HE2K, HE21, ZION, DEE, W(IDEEL), W(IVEC), W, IW,
     $               DUMP)
      else
        call ZERO1  (DEE, (20*N))
      end if
C
C---- Compute Z1
      call SIGMA    (N, Z, RHEAB, Z1, 'Z1', W, IW)
C
      if(IHEDF.eq.0) then
C----   Compute Helium ionization terms: Z2, Z3, ZXG
        call VULPES (N, Z, HE1, BETA, HE2K, HEND, Z2, Z3, ZXG,
     $               W(IVEC), W, IW)
C
C----   Recompute RHEAB, or edit d-matrix         ( ? [HEABD] )
        call THALIA (X, W, N, RHEAB, Z, HND, ZI, Z2, Z3, ZT, TE, HK,
     $               H1, HE1, BETA, HE2K, HEND, DEE, ZION, KVLG, VBMB,
     $               VM, DUMP)
C
      else
C       (Suppress Helium)
        call ZERO1  (Z2,  N)
        call ZERO1  (Z3,  N)
        call ZERO1  (ZXG, N)
      end if
C
      if(AMDIFF) then
C----   Compute velocities
        call AQUILA (N, Z, ZI, Z1, Z2, Z3, ZT, DEE, VAMB, VBMB, VCMB,
     $               VDMB, VEMB, W, IW)
      else
        call ZERO1  (VAMB, N)
        call ZERO1  (VBMB, N)
        call ZERO1  (VCMB, N)
        call ZERO1  (VDMB, N)
        call ZERO1  (VEMB, N)
      end if
C
C     (Give back W allotment)
      call WGIVE    (W, 'ERATO')
C     !END
      call BYE ('ERATO')
C
      return
      end
