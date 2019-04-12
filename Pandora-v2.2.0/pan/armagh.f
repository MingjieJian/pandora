      subroutine ARMAGH
     $(J,TE,DENS,XNU,XNUC,NPQ,LRQ,NLE,DUMP,CI)
C
C     Rudolf Loeser, 2006 Mar 28
C---- Computes a value of CI.
C
C     An input value of DENS is required only for Hydrogen.
C
C     (This is version 2 of ARMAGH.)
C     !DASH
      save
C     !DASH
      real*8 CI, DENS, TE, XNU, XNUC
      integer J, JDCIA, JDCIC, JDCIJ, JDCIS, JDCIV, JDCIW, LRQ, NLE,
     $        NPQ
      logical DUMP, MESS, OK
C     !COM
C---- TANGELO     as of 2007 Mar 26
      integer     LCISW,NCISW,KCISW,KCISWD,LCESW,NCESW,KCESW,KCESWD
      parameter   (LCISW=6, LCESW=7)
      dimension   KCISW(LCISW), KCISWD(LCISW)
      dimension   KCESW(LCESW), KCESWD(LCESW)
      common      /TANGELO1/ NCISW,KCISW,KCISWD
      common      /TANGELO2/ NCESW,KCESW,KCESWD
C     Control switches for default CI & CE calculations.
C CI: 1 SHAH, 2 AR, 3 VORONOV, 4 VS, 5 JOHNSON, 6 CLARK
C CE: 1 SCHOLZ, 2 PB, 3 VS, 4 JOHNSON, 5 SEATON, 6 VREGE, 7 AGGRWL
C     (The default configurations are set up in subroutine SULTANA.)
C     .
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
      equivalence (MEST(20),JDCIC)
      equivalence (MEST(22),JDCIA)
      equivalence (MEST(26),JDCIW)
      equivalence (MEST(25),JDCIS)
      equivalence (MEST( 7),JDCIJ)
      equivalence (MEST( 6),JDCIV)
C     !DASH
C     !EJECT
      external ROMATIN, MURRE, POLAR, DOLBI, DALBO, DULBA, HI, BYE
C
C               XNUC(NSL), XNU(NSL), NPQ(NSL), LRQ(NSL), NLE(NSL)
      dimension XNUC(*),   XNU(*),   NPQ(*),   LRQ(*),   NLE(*)
C
      data MESS /.true./
C
      call HI ('ARMAGH')
C     !BEG
      OK = .false.
C
      if(J.eq.1) then
        if(KCISW(1).gt.0) then
C         Shah et al., Level 1 only (Hydrogen only)
          call ROMATIN (TE, CI)
          OK    = .true.
          JDCIS = JDCIS+1
        else if(KCISW(2).gt.0) then
C         Arnaud & Rothenflug, Level 1 only
          call MURRE   (TE, DUMP, MESS, CI, OK)
          if(OK) then
            JDCIA = JDCIA+1
          end if
        else if(KCISW(3).gt.0) then
C         Voronov, Level 1 only (not Hydrogen)
          call POLAR   (TE, DUMP, MESS, CI, OK)
          if(OK) then
            JDCIW = JDCIW+1
          end if
        end if
      else
        if(KCISW(4).gt.0) then
C         Vriens & Smeets (Hydrogen only)
          call DOLBI   (J, TE, DENS, DUMP, CI)
          OK    = .true.
          JDCIV = JDCIV+1
        else if(KCISW(5).gt.0) then
C         Johnson (Hydrogen only)
          call DALBO   (J, TE, DENS, DUMP, CI)
          OK    = .true.
          JDCIJ = JDCIJ+1
        end if
      end if
C
      if(.not.OK) then
C       Clark et al. (Coulomb-Born)
        call DULBA     (J, TE, XNUC, XNU, NPQ, LRQ, NLE, CI)
        JDCIC = JDCIC+1
      end if
C     !END
      call BYE ('ARMAGH')
C
      return
      end
