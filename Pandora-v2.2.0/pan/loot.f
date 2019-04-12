      subroutine LOOT
     $(TIT)
C
C     Rudolf Loeser, 1981 Apr 24
C---- Sets up an expansion velocity descriptor.
C     (This is version 4 of LOOT.)
C     !DASH
      save
C     !DASH
      character TIT*24
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
      external HI, BYE
C
      call HI ('LOOT')
C     !BEG
      if(EXPAND) then
        if(JVEL.eq.1) then
          TIT = 'Sobolev velocity'
C
        else if(JVEL.eq.2) then
          TIT = 'Source Function velocity'
C
        else if(JVEL.eq.3) then
          TIT = 'V-add {AMDIFF, VELGRAD}'
C
        else if(JVEL.gt.100) then
          if(JVEL.gt.500) then
            write (TIT,100) (JVEL-500)
  100       format('Flow-broad',I3,' + V-add')
C
          else if(JVEL.gt.400) then
            write (TIT,101) (JVEL-400)
  101       format('Flow-broad table',I3)
C
          else if(JVEL.gt.300) then
            write (TIT,102) (JVEL-300)
  102       format('Shock table',I3)
C
          else if(JVEL.gt.200) then
            write (TIT,103) (JVEL-200)
  103       format('VX table',I3,' + V-add')
C
          else
            write (TIT,104) (JVEL-100)
  104       format('VX table',I3)
C
          end if
        end if
C
      else
        if(NVY.eq.0) then
          TIT = 'Flow-broadened'
        else
          TIT = 'Stationary'
        end if
      end if
C     !END
      call BYE ('LOOT')
C
      return
      end
