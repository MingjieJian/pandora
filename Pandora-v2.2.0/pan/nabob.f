      subroutine NABOB
     $(XLTIT,JSTAR,MODE,LABEL)
C
C     Rudolf Loeser, 1980 Nov 14
C---- Makes a label, for ETHEL.
C     !DASH
      save
C     !DASH
      real*8 XLTIT
      integer ISTAR, JSTAR, KAK1, KAK2, KAK3, MODE
      character BLANK*1, LABEL*48, TIT*24, qummy*10
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
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- DWARF       as of 1997 Nov 19
      integer     KAKOD,KAKODS
      parameter   (KAKOD=4)
      dimension   KAKODS(KAKOD)
      common      /DWARF/ KAKODS
C     Continuum wavelength value type specification parameters.
C     (These parameters are packed and unpacked by "BET".)
      equivalence (KAKODS( 1),KAK1 )
      equivalence (KAKODS( 2),KAK2 )
      equivalence (KAKODS( 3),KAK3 )
C     !DASH
      external MONGOL, BET, LOOT, HI, BYE
C
      call HI ('NABOB')
C     !BEG
      LABEL = BLANK
C
      if(MODE.ne.1) then
        call MONGOL (XLTIT, LABEL, qummy)
      else
        call BET    (2, XLTIT)
        JVEL = KAK1
        EXPAND = JVEL.gt.0
        call LOOT   (TIT)
        write (LABEL,100) KAK2,KAK3,TIT
  100   format('Core: ',I2,'/',I2,', ',A)
      end if
C
      LFB = JSTAR/100
      ISTAR = JSTAR-100*LFB
      if(LFB.eq.1) then
        LABEL(38:43) = ' frnt '
      else if(LFB.eq.2) then
        LABEL(38:43) = ' back '
      end if
      if(ISTAR.eq.1) then
        LABEL(44:48) = '+star'
      end if
C     !END
      call BYE ('NABOB')
C
      return
      end
