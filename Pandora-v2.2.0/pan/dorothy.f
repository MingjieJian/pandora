      subroutine DOROTHY
     $(EMUF,EMU,L)
C
C     Rudolf Loeser, 1980 Oct 24
C---- Sets up EMOO, the current value of look angle as needed for the
C     Flux integral, and determines whether EMOO equals on of the mu
C     values for which line intensity profiles must be computed.
C     Also,
C     prints a dump of LOOPER (if required).
C     (This is version 2 of DOROTHY.)
C     !DASH
      save
C     !DASH
      real*8 DELTA, EMU, EMUF
      integer L, jummy
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
C     !DASH
C     !EJECT
      external LOOKUD, MUKTAR, HI, BYE
C
C               EMUF(LF), EMU(L)
      dimension EMUF(*),  EMU(*)
C
      data DELTA /1.D-6/
C
      call HI ('DOROTHY')
C     !BEG
      MUK  = 0
      EMOO = EMUF(MF)
      call LOOKUD (EMU,L,DELTA,EMOO,MUK,jummy)
C
      call MUKTAR ('GREY/DOROTHY')
C     !END
      call BYE ('DOROTHY')
C
      return
      end
