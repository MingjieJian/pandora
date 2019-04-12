      subroutine ANASTAS
     $(JSAV,IU,IL,WVL,YNT,MUX,Y,MYX,BT,KODE,NLTE,ISTAR)
C
C     Rudolf Loeser, 1980 Dec 10
C---- Saves data for spectrum summary.
C     (This is version 2 of ANASTAS.)
C     !DASH
      save
C     !DASH
      real*8 BT, ONE, WVL, XLTIT, Y, YNT
      integer IL, ISTAR, IU, JSAV, JSTAR, JV, KAK1, KAK2, KAK3, KODE,
     $        KTYPE, MUX, MYX, NLTE
      logical FLAG
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- DWARF       as of 1997 Nov 19
      integer     KAKOD,KAKODS
      parameter   (KAKOD=4)
      dimension   KAKODS(KAKOD)
      common      /DWARF/ KAKODS
C     Continuum wavelength value type specification parameters.
C     (These parameters are packed and unpacked by "BET".)
C     .
      equivalence
     $(KAKODS( 1),KAK1 ),(KAKODS( 2),KAK2 ),(KAKODS( 3),KAK3 ),
     $(KAKODS( 4),KTYPE)
C     .
C---- SALKA       as of 1989 Nov 17
      character   GENLAB*60
      dimension   GENLAB(4)
      common      /SALKA/ GENLAB
C     Texts for error messages from GENISTA/AJUGA.
C     .
C     !DASH
      external BET, PANSA, HI, BYE
C
      call HI ('ANASTAS')
C     !BEG
      FLAG = ((JSAV.gt.0).and.(EMOO.eq.ONE).and.(NLTE.gt.0))
      if(FLAG.and.(KODE.eq.0)) then
        if(EXPAND) then
          JV = JVEL
        else
          JV = 0
        end if
        KAK1  = IU
        KAK2  = IL
        KAK3  = JV
        KTYPE = 0
        GENLAB(1) = 'Ion-of-the-run level index'
        GENLAB(2) = 'Ion-of-the-run level index'
        GENLAB(3) = 'Velocity index'
        call BET   (1,XLTIT)
        JSTAR = 100*LFB+ISTAR
        call PANSA (WVL,YNT,MUX,Y,MYX,BT,XLTIT,JSTAR,1,EXPAND)
      end if
C     !END
      call BYE ('ANASTAS')
C
      return
      end
