      subroutine MUGADI
     $(X,N,VEL,KVEL,VXS,VSB,VXN,WTP,ISSV,NVX,VADD,RVEL,VREQ,HREQ,
     $ VPD,VEC,NO)
C
C     Rudolf Loeser, 1990 May 10
C---- Sets up the velocity set for the profile calculations.
C     !DASH
      save
C     !DASH
      real*8 HREQ, RVEL, VADD, VEC, VEL, VPD, VREQ, VSB, VXN, VXS, WTP,
     $       X
      integer ISSV, J, JJFRS, JJHND, JJTE, JJZ, KINC, KVEL, LPMLR,
     $        LPVEL, MVEL, N, NMLR, NO, NVSBP, NVX
      logical CALVAX, PRNT, PWTD, USE, VXST, VZERO
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
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(138),JJFRS)
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 37),JJZ  )
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
      equivalence (KZQ(217),LPVEL)
      equivalence (KZQ(218),LPMLR)
      equivalence (KZQ(191),NMLR )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(50),NVSBP)
C     !DASH
C     !EJECT
      external MUNGA, NAUGHTD, ARRADD, WHITE, GALAM, UMIGDA, MALADY,
     $         OCTA, MOVE1, HI, BYE
C
      dimension X(*)
C
C               VXN(N,NVX), KVEL(NVEL), VXS(N), VSB(N), VEC(N), VPD(N),
      dimension VXN(N,*),   KVEL(*),    VXS(*), VSB(*), VEC(*), VPD(*),
C
C               VREQ(N,NVEL), HREQ(N,NVEL), RVEL(N,NVEL), ISSV(NVX),
     $          VREQ(N,*),    HREQ(N,*),    RVEL(N,*),    ISSV(*),
C
C               VADD(N), VEL(N,NVEL), WTP(NVX)
     $          VADD(*), VEL(N,*),    WTP(*)
C
      call HI ('MUGADI')
C     !BEG
C---- First, enter Source Function Expansion velocity
      NVEL = 1
      call MOVE1     (VXS, N, VEL(1,NVEL))
      KVEL(NVEL) = 2
C---- Next, enter VADD (AMDIFF and/or VELGRAD)
      call NAUGHTD   (VADD, 1, N, VZERO)
      if(.not.VZERO) then
        call MUNGA   (N, NVEL, VEL, VADD, USE)
        if(USE) then
          NVEL = NVEL+1
          call MOVE1 (VADD, N, VEL(1,NVEL))
          KVEL(NVEL) = 3
        end if
      end if
C
      CALVAX = .false.
      if(.not.FLOBRD) then
        CALVAX = ISSV(1).gt.0
      end if
C
      VZERO = .true.
      if(.not.CALVAX) then
        call NAUGHTD (VADD, 1, N, VZERO)
      end if
C     !EJECT
C---- Next, enter additional velocities
      if(NVX.gt.0) then
        call UMIGDA     (FLOBRD, CALVAX, VZERO, KINC)
        do 100 J = 1,NVX
          if(VZERO) then
            call MOVE1  (VXN(1,J), N, VPD)
          else
            call ARRADD (VXN(1,J), VADD, VPD, N)
          end if
          call MUNGA    (N, NVEL, VEL, VPD, USE)
          if(USE) then
            NVEL = NVEL+1
            call MOVE1  (VPD, N, VEL(1,NVEL))
            KVEL(NVEL) = KINC+J
          end if
  100   continue
      end if
C
      MVEL = NVEL
      if(NVSBP.gt.0) then
C----   Finally, squirrel away the Sobolev velocity
        MVEL = MVEL+1
        call MOVE1      (VSB, N, VEL(1,MVEL))
        KVEL(MVEL) = 1
      end if
C
C---- Check velocity set # 1
      call NAUGHTD      (VEL(1,1), 1, N, VZERO)
      VXST = (MVEL.gt.1).or.(.not.VZERO)
      PWTD = (NO.gt.0).and.(LPVEL.gt.0)
      PRNT = VXST.and.PWTD
C
      if(PRNT) then
C----   Print velocities
        call GALAM      (N, MVEL, KVEL, VEL, WTP, NVX, NVSBP,
     $                   VZERO, CALVAX, NO)
C
        if(LPMLR.gt.0) then
C----     Compute and print mass loss rates, etc
          call OCTA     (X, N, MVEL, X(JJFRS), X(JJHND), VEL, RVEL,
     $                   VEC, NMLR, VREQ, HREQ)
          call MALADY   (NMLR, N, MVEL, VEL, RVEL, VREQ, HREQ, X(JJZ),
     $                   X(JJTE), X(JJFRS), X(JJHND), NO)
        end if
C
      end if
C---- Write to special Spectrum Save File
      call WHITE        (MVEL, VEL, NVX, WTP)
C     !END
      call BYE ('MUGADI')
C
      return
      end
