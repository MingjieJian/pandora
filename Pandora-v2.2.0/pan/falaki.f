      subroutine FALAKI
     $(JAYTI,IU,IL,NMAX,KMAX,N,Z,XLB1,XLB2,W,LU,FOUND)
C
C     Rudolf Loeser, 1982 Jan 08.
C---- Gets input PRD Jnu values for transition iu/il,
C     from the ".JNU" input file.
C     !DASH
      save
C     !DASH
      real*8 W, XLB1, XLB2, Z
      integer IDOLD, IJINT, IJOCP, IJOLD, IL, IN, INTZ, IS, IU, IZOLD,
     $        JAYTI, KMAX, KOLD, KTRN, LU, MMDL, MMJNU, MOX, N, NMAX,
     $        NOLD
      logical FOUND
C     !COM
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(59),MMDL )
      equivalence (MMP( 5),MMJNU)
C
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
      equivalence (LINKDS(20),KTRN )
C     !DASH
C     !EJECT
      external MORIANA, ZAP, TAFFY, WGIVE, HI, BYE
C
      dimension W(*)
C
C               Z(N), XLB1(Li1len), XLB2(Li2len)
      dimension Z(*), XLB1(*),      XLB2(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),IZOLD ),(IN( 2),IDOLD ),(IN( 3),IJOLD ),(IN( 4),IJINT ),
     $(IN( 5),IJOCP )
C
      call HI ('FALAKI')
C     !BEG
C     (Get, and allocate, W allotment)
      call MORIANA (IN, IS, MOX, 'FALAKI', NMAX, KMAX)
C
C---- Attempt to get XJNUs
      call ZAP     (JAYTI, IU, IL, N, Z, KTRN, XLB1(MMDL), XLB2(MMJNU),
     $              NMAX, KMAX, NOLD, W(IZOLD), KOLD, W(IDOLD),
     $              W(IJOLD), W(IJOCP), W(IJINT), INTZ, FOUND)
C---- Print (if desired)
      call TAFFY   (IU, IL, N, Z, KTRN, XLB1(MMDL), XLB2(MMJNU), NOLD,
     $              W(IZOLD), KOLD, W(IDOLD), W(IJOLD), LU, INTZ)
C
C     (Give back W allotment)
      call WGIVE   (W, 'FALAKI')
C     !END
      call BYE ('FALAKI')
C
      return
      end
