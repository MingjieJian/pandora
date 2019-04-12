      subroutine NUT
     $(X,W,IW,XLB1,MPROM,LU)
C
C     Rudolf Loeser, 1980 Jun 27
C---- Drives Profile Analysis.
C     (This is version 4 of NUT.)
C     !DASH
      save
C     !DASH
      real*8 W, X, XLB1
      integer IFDDL, IHSDD, IHSDP, IHSSP, IL, IN, IS, IU, IW, JJVXS,
     $        JJXNE, JJZ, KTRN, LDL, LU, MMCDL, MMDDL, MMDL, MMDP, MMDW,
     $        MMLAM, MMSTE, MOX, MPROM, MS, N, NS
      logical DOIT, KOK, NOK, TOK
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ(  9),JJXNE)
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
      equivalence (KZQ(  2),MS   )
      equivalence (KZQ(  3),NS   )
      equivalence (KZQ(124),IHSDP)
      equivalence (KZQ(125),IHSDD)
      equivalence (KZQ(136),IHSSP)
C     !EJECT
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(59),MMDL )
      equivalence (MML(13),MMDW )
      equivalence (MML(12),MMDP )
      equivalence (MML( 2),MMLAM)
      equivalence (MML(34),MMDDL)
      equivalence (MML(31),MMCDL)
      equivalence (MML(61),MMSTE)
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
      equivalence (LINKDS(20),KTRN )
C     !DASH
C     !EJECT
      external NIALL, NIMBLE, LAPWING, PEEK, VELVET, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XLB1(Li1len)
      dimension XLB1(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IFDDL )
C
      call HI ('NUT')
C     !BEG
      if(LU.gt.0) then
C
        if(MPROM.le.0) then
          DOIT = .true.
        else
          NOK  = (IHSDP.ge.1).and.(IHSDP.le.N)
          KOK  = (IHSDD.ge.1).and.(IHSDD.le.KTRN)
          TOK  = (IU.eq.MS).and.(IL.eq.NS)
          DOIT = NOK.and.KOK.and.TOK
        end if
C
        if(DOIT) then
C         (Get W allotment)
          call NIALL    (IN, IS, MOX, 'NUT')
C
          call NIMBLE   (XLB1(MMSTE), X(JJXNE), W(IFDDL), N)
          call LAPWING  (LU, IU, IL, LDL, 0)
          call PEEK     (KTRN, XLB1(MMDL), N, X(JJZ), XLB1(MMDW),
     $                   XLB1(MMDP), X(JJVXS) ,MPROM, X(JJXNE),
     $                   XLB1(MMLAM), XLB1(MMDDL), W(IFDDL), IHSSP,
     $                   XLB1(MMCDL), LDL, IU, IL, LU, W, IW)
          if(MPROM.gt.0) then
            call VELVET (XLB1(MMLAM), XLB1(MMDDL), W(IFDDL),
     $                   XLB1(MMCDL), LDL, XLB1(MMDL), KTRN,XLB1(MMDP),
     $                   XLB1(MMDW), X(JJVXS), X(JJXNE), N, LU, W, IW)
          end if
C
C         (Give back W allotment)
          call WGIVE    (W, 'NUT')
        end if
C
      end if
C     !END
      call BYE ('NUT')
C
      return
      end
