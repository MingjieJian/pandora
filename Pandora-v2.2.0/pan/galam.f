      subroutine GALAM
     $(N,MVEL,KVEL,VEL,WTP,NVX,NVSBP,VZERO,CALVAX,NO)
C
C     Rudolf Loeser, 1990 May 22
C---- Prints velocities used for emergent profile calculations.
C     !DASH
      save
C     !DASH
      real*8 VEL, WTP
      integer J, JS, KVEL, MVEL, N, NO, NVSBP, NVX
      logical CALVAX, EXPSAV, FLOBRD, PRNTZ, VZERO
      character BLANK*1, LABEL*24
C     !COM
C---- LOOPER      as of 2006 May 05
      integer     NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      real*8      EMOO,WVLTRN
      logical     EXPAND,SPHERE,VXZERO
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
C     !DASH
      external  PRIAM, LINER, LOOT, OMAR, HI, BYE
      intrinsic mod
C
C               KVEL(MVEL), VEL(N,MVEL), WTP(NVX)
      dimension KVEL(*),    VEL(N,*),    WTP(*)
C
      data PRNTZ /.false./
C
      call HI ('GALAM')
C     !BEG
      if(NO.gt.0) then
C       (JVEL and EXPAND in LOOPER must be fiddled with here in GALAM
C       so that subr. LOOT will function properly; see also, below.)
        EXPSAV = EXPAND
        EXPAND = .true.
C
        call PRIAM (NO, 'VELOCITIES', 10)
        call LINER (2, NO)
        write (NO,100)
  100   format(' ','Descriptions of velocity sets used for ',
     $             'emergent profile calculations.'//
     $         ' ','(To omit this printout set LPVEL = 0; to omit ',
     $             'mass loss rates only, set LPMRL = 0)')
        call LINER (1, NO)
C     !EJECT
        do 102 J = 1,MVEL
          JVEL = KVEL(J)
          call LOOT  (LABEL)
          JS   = 0
          if((JVEL.gt.400).and.(JVEL.lt.600)) then
            JS = mod(JVEL,100)
            if((JS.le.0).or.(JS.gt.NVX)) then
              JS = 0
            end if
          end if
          if(JS.eq.0) then
            write (NO,101) J,LABEL
  101       format(' ','Set #',I3,': ',A,:,5X,'profile weight =',
     $                 1PE12.4)
          else
            write (NO,101) J,LABEL,WTP(JS)
          end if
  102   continue
        if(NVSBP.gt.0) then
          call LINER (1, NO)
          write (NO,103)
  103     format(' ','The Sobolev velocity will be used in place of ',
     $               'the Source Function velocity as needed.')
        end if
C
        call LINER   (2, NO)
        if(CALVAX) then
          write (NO,104) 'Shock velocities (km/s)'
  104     format(' ',A)
        else if(FLOBRD) then
          write (NO,104) 'Velocity sets (km/s)'
        else
          write (NO,104) 'Expansion velocities (km/s)'
        end if
        call OMAR    (NO, N, MVEL, VEL , '  Set ', PRNTZ)
C
        EXPAND = EXPSAV
      end if
C     !END
      call BYE ('GALAM')
C
      return
      end
