      subroutine REX
     $(K,LF,WTP,YHZ,YHZFB)
C
C     Rudolf Loeser, 2005 Jan 06
C---- Accumulates the flow-broadened profiles /Hz.
C     !DASH
      save
C     !DASH
      real*8 W, WTP, WTPZ, YHZ, YHZFB, ZERO
      integer J, K, KL, LF
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
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (REST( 1),WTPZ )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  ZERO1, ARRINC, HI, BYE
      intrinsic mod
C
C               YHZ(KM,LF), YHZFB(KM,LF), WTP(NVX)
      dimension YHZ(K ,*),  YHZFB(K ,*),  WTP(*)
C
      call HI ('REX')
C     !BEG
      KL = K*LF
      if(NVY.eq.1) then
        call ZERO1 (YHZFB, KL)
      end if
C
      W = ZERO
      if(JVEL.eq.2) then
        W = WTPZ
      else if((JVEL.gt.400).and.(JVEL.lt.600)) then
        J = mod(JVEL,100)
        W = WTP(J)
      end if
C
      if(W.gt.ZERO) then
        call ARRINC (YHZ, W, YHZFB, KL)
      end if
C     !END
      call BYE ('REX')
C
      return
      end
