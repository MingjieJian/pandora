      subroutine PERCY
     $(FLX,LTE,K,YNT,YNTL,WMUF,WINT,WINTL)
C
C     Rudolf Loeser, 2000 Jul 24
C---- Accumulates weighted intensity sums, for flux calculation.
C     !DASH
      save
C     !DASH
      real*8 WINT, WINTL, WMUF, YNT, YNTL
      integer K
      logical FLX, LTE
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
      external MARZI, HI, BYE
C
C               WMUF(LF), WINTL(KM), WINT(KM), YNTL(KM), YNT(KM)
      dimension WMUF(*),  WINTL(*),  WINT(*),  YNTL(*),  YNT(*)
C
      call HI ('PERCY')
C     !BEG
      if(FLX) then
        call MARZI   (MF,K,YNT ,WMUF,WINT )
        if(LTE) then
          call MARZI (MF,K,YNTL,WMUF,WINTL)
        end if
      end if
C     !END
      call BYE ('PERCY')
C
      return
      end
