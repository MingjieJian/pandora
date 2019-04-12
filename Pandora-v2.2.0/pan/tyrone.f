      subroutine TYRONE
     $(IU,IL,AIJ,AATIJ,TE,DENS,XNU,XNUC,P,NPQ,LRQ,DUMP,CE)
C
C     Rudolf Loeser, 2006 Mar 30
C---- Computes a value of CE.
C
C     An input value of DENS is required only for Hydrogen.
C     !DASH
      save
C     !DASH
      real*8 AATIJ, AIJ, CE, DENS, P, TE, XNU, XNUC
      integer IL, IONST, IU, JDCEG, JDCEI, JDCEJ, JDCER, JDCES, JDCEV,
     $        JDCEW, LRQ, NPQ
      logical DUMP, HYDR, MESS, OK
      character QELSM*8
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
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 56),IONST)
      equivalence (QZQ(  2),QELSM)
C
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
      equivalence (MEST( 1),JDCES)
      equivalence (MEST( 2),JDCEG)
      equivalence (MEST( 4),JDCEV)
      equivalence (MEST( 5),JDCEJ)
      equivalence (MEST( 3),JDCEI)
      equivalence (MEST(15),JDCER)
      equivalence (MEST(28),JDCEW)
C     !DASH
C     !EJECT
      external DONAR, BONOR, AMOS, DONNER, DONWAI, BEKLA, AMPHION,
     $         HI, BYE
C
C               AIJ(NL,NL), XNU(NSL), AATIJ(NL,NL), NPQ(NSL), LRQ(NSL),
      dimension AIJ(*),     XNU(*),   AATIJ(*),     NPQ(*),   LRQ(*),
C
C               P(NSL), XNUC(NSL)
     $          P(*),   XNUC(*)
C
      data MESS /.true./
C
      call HI ('TYRONE')
C     !BEG
      HYDR = QELSM(:3).eq.'H  '
      OK   = .false.
C
      if((IU.eq.2).and.(KCESW(1).gt.0)) then
C       Scholz et al.
        call DONAR    (IU, IL, TE, CE)
        OK    = .true.
        JDCES = JDCES+1
      else if((IU.le.7).and.(KCESW(2).gt.0)) then
C       Przybilla & Butler
        call BONOR    (IU, IL, P, TE, CE)
        OK    = .true.
        JDCEG = JDCEG+1
      else if((IU.le.5).and.(KCESW(7).gt.0)) then
C       Aggarwal et al.
        call AMOS     (IU, IL, TE, CE)
        OK = .true.
        JDCEW = JDCEW+1
      end if
C
      if(.not.OK) then
        if(KCESW(3).gt.0) then
C         Vriens & Smeets
          call DONNER (IU, IL, TE, DENS, DUMP, CE)
          OK    = .true.
          JDCEV = JDCEV+1
        else if(KCESW(4).gt.0) then
C         Johnson
          call DONWAI (IU, IL, TE, DENS, DUMP, CE)
          OK    = .true.
          JDCEJ = JDCEJ+1
        end if
      end if
C     !EJECT
      if(.not.OK) then
        if(KCESW(5).gt.0) then
C         Seaton (impact parameter)
          call BEKLA (HYDR, MESS, IU, IL, TE, AIJ, P, XNU, XNUC,
     $                NPQ, LRQ, CE, OK)
          if(OK) then
            JDCEI = JDCEI+1
          end if
        end if
      end if
C
      if(.not.OK) then
C       van Regemorter
        call AMPHION (IU, IL, TE, AATIJ, P, XNU, IONST, CE)
        JDCER = JDCER+1
      end if
C     !END
      call BYE ('TYRONE')
C
      return
      end
