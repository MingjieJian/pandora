      subroutine GALIUM
     $(X,W,IW,XLB1,XISU,ASU,XIFU,AFU,YALSF,GOODA,GOODT,KODE,K,KXMAX,LU)
C
C     Rudolf Loeser, 1989 Jan 31
C---- Sets up : XI, A and DL , and
C     computes XI and A for blended lines (if any), in the
C     Line Intensity Data blocks for radiative and passive transitions.
C     !DASH
      save
C     !DASH
      real*8 AFU, ASU, DNU, W, X, XIFU, XISU, XLB1, YALSF
      integer IL, IU, IUL, IW, JJAF, JJAS, JJXIF, JJXIS, JJXNU, K, KBT,
     $        KF, KFU, KODE, KRT, KS, KST, KSU, KXMAX, LDL, LU, MMA,
     $        MMCDW, MMDDL, MMDL, MMIXB, MMIXR, MMIXS, MMLAM, MMPGD,
     $        MMXI, MODE
      logical GOODA, GOODT, GOODX, GOODY, YES
      character LAB*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 4),KF )
      equivalence (JZQ(36),KS )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(121),JJXIF)
      equivalence (IZOQ(122),JJAF )
      equivalence (IZOQ(146),JJXIS)
      equivalence (IZOQ(147),JJAS )
      equivalence (IZOQ( 26),JJXNU)
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML( 4),MMCDW)
      equivalence (MML( 3),MMXI )
      equivalence (MML(59),MMDL )
      equivalence (MML(14),MMA  )
      equivalence (MML(58),MMIXS)
      equivalence (MML(56),MMIXB)
      equivalence (MML(57),MMIXR)
      equivalence (MML(34),MMDDL)
      equivalence (MML( 2),MMLAM)
      equivalence (MML(54),MMPGD)
C     !EJECT
C---- LINUS       as of 2004 May 12
      integer     LINKDS
      dimension   LINKDS(22)
      common      /LINUS/ LINKDS
C     Line source function calculation control parameters for the
C     current transition as set up by "PET" (and printed by "LINSEED").
C     IU    - index of upper level
C     IL    - index of lower level
C     KLIN  - line "type" code (1: radiative, 2: passive, etc)
C     ICE   - PRD calculation control
C     IPRO  - emergent profiles calculation control
C     METSE - statistical equilibrium calculation method selector
C     METSF - LSF calculation method selector (QR, RT, GR)
C     IBRSW - damping components selector
C     INKSW - input opacity signal
C     LSFT  - LSF solution code (0: full, 1:direct, etc)
C     ILFLX - line flux calculation control
C     LDL   - number of line components
C     LINT  - frequency integration range (half vs. full profile)
C     LSFP  - LSF printout control
C     IFDB  - LSF background control (constant vs. varying)
C     ISBG  - blended line profile plot mode switch
C     KBT   - length of input table XIBLUT
C     KRT   - length of input table XIREDT
C     KST   - length of input table XISYMT
C     KTRN  - length of actual tables XI and DL
C     LOML  - "line-background-continuum-opacity" control
C     ....  - (available)
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
      equivalence (LINKDS(12),LDL  )
      equivalence (LINKDS(19),KST  )
      equivalence (LINKDS(17),KBT  )
      equivalence (LINKDS(18),KRT  )
C     !DASH
C     !EJECT
      external BERGAMO, LANDAS, DEBRIS, URTICA, DOVIZI, DUNKER, BUSTLE,
     $         DISTOMA, INTRANS, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XLB1(Li1len), YALSF(NT), XIFU(KM), XISU(KM), ASU(KM),
      dimension XLB1(*),      YALSF(*),  XIFU(*),  XISU(*),  ASU(*),
C
C               AFU(KM)
     $          AFU(*)
C
      call HI ('GALIUM')
C     !BEG
      write (LAB,100) IU,IL
  100 format('A(',I2,'/',I2,')')
      call INTRANS (IU, IL, 'GALIUM', IUL)
C
      GOODX = .true.
      GOODY = .true.
C
C---- Note: CRUMB initialized GOODA & GOODT; GALIUM just updates them:
C     a single GOODX = .false. will turn GOODA false forever,
C     and analogously with GOODY for GOODT.
C
C---- Get DNU for this transition
      call DUNKER  (X(JJXNU), IU, IL, DNU)
C---- Set up "candidate" tables (suffix "U") for this transition
C     (Upon return, MODE=1 if transition-specific tables have been
C     used, MODE=2 if standard tables have been used.)
      call DISTOMA (X(JJXIS), X(JJAS), KS, X(JJXIF), X(JJAF), KF,
     $              XLB1(MMIXS), KST, XLB1(MMIXB), KBT, XLB1(MMIXR),
     $              KRT, XISU, ASU, KSU, XIFU, AFU, KFU, MODE,
     $              W, IW, LAB, YALSF(IUL), GOODX)
      if(.not.GOODX) then
        GOODA = .false.
      end if
C     !EJECT
C---- Now, select XI, A, DL, and K for this transition
      if(LDL.le.1) then
C----   Set up XI, A, and K for a single line
C       (upon return, KODE=1 for symmetric, KODE=2 for full)
        call LANDAS   (XIFU, AFU, KFU, XISU, ASU, KSU, XLB1(MMXI),
     $                 XLB1(MMA), K, KODE)
        if(KODE.eq.2) then
C----     Augment a full table if necessary to capture possible
C         coincident background lines
          call BUSTLE (X, W, IW, XLB1(MMLAM), XLB1(MMCDW), DNU,
     $                 XLB1(MMXI), XLB1(MMA), K, GOODX, GOODY, LAB,
     $                 YALSF(IUL), YES)
          if(.not.GOODX) then
            GOODA = .false.
          end if
          if(.not.GOODY) then
            GOODT = .false.
          end if
          if(YES) then
            KODE = 4
          end if
        end if
C----   Set up Delta-Lambda table
        call DEBRIS   (XLB1(MMXI), K, XLB1(MMCDW), XLB1(MMDL))
      else
C----   Make special XI, A and Delta-Lambda tables
C       for a blended line (and set KODE=3 for this case)
        call BERGAMO  (X, XLB1(MMDDL), LDL, XLB1(MMLAM), XLB1(MMCDW),
     $                 DNU, GOODX, GOODY, LAB, YALSF(IUL), W, IW,
     $                 XIFU, KFU, XLB1(MMXI), XLB1(MMA), XLB1(MMDL), K)
        if(.not.GOODX) then
          GOODA = .false.
        end if
        if(.not.GOODY) then
          GOODT = .false.
        end if
        KODE = 3
      end if
C
C---- Set up data to be used for profile graphs
      call DOVIZI     (IU, IL, XLB1(MMLAM), LDL, XLB1(MMDDL), K,
     $                 XLB1(MMXI), KODE, XLB1(MMCDW), XLB1(MMPGD))
C---- Print
      call URTICA     (LU, IU, IL, KODE, MODE, XLB1(MMXI),
     $                 XLB1(MMA), K, YALSF(IUL))
C     !END
      call BYE ('GALIUM')
C
      return
      end
