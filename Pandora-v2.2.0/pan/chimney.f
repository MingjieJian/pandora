      subroutine CHIMNEY
     $(Z,TE,XNE,MPROM,XND,XNU,HN1,IFBRSW,NO,XLB1,POPK,W)
C
C     Rudolf Loeser, 1980 Aug 14
C---- Computes and prints Damping Parameters for the
C     current transition.
C     !DASH
      save
C     !DASH
      real*8 CIB, DPM, HN1, POPK, TE, W, XLB1, XND, XNE, XNU, Z
      integer ICC, IFBRSW, IFF, IL, IN, IRM, IS, ITIB, ITRD, ITRS, ITSK,
     $        ITVW, IU, IVION, IWW, LDL, MMCRD, MMCRS, MMCSK, MMCVW,
     $        MMDP, MMDPM, MMDW, MOX, MPROM, N, NO
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
      equivalence (LINKDS(12),LDL  )
C     !EJECT
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML( 7),MMCRD)
      equivalence (MML( 8),MMCVW)
      equivalence (MML( 9),MMCSK)
      equivalence (MML(10),MMCRS)
      equivalence (MML(12),MMDP )
      equivalence (MML(13),MMDW )
      equivalence (MML( 6),MMDPM)
C     !DASH
      external MILL, QUAIL, MUSKEG, ZIENNA, ENZIAN, WGIVE, HI, BYE
C
      dimension W(*)
C
C               XLB1(Li1len), HN1(N), XND(N,NL), XNE(N), POPK(N,NPOPS),
      dimension XLB1(*),      HN1(*), XND(N,*),  XNE(*), POPK(*),
C
C               Z(N), XNU(NSL), TE(N)
     $          Z(*), XNU(*),   TE(*)
C
      dimension IN(10)
      equivalence
     $(IN( 1),ITRD  ),(IN( 2),ITVW  ),(IN( 3),ITSK  ),(IN( 4),ITRS  ),
     $(IN( 5),IFF   ),(IN( 6),ICC   ),(IN( 7),ITIB  ),(IN( 8),IWW   ),
     $(IN( 9),IRM   ),(IN(10),IVION )
C
      call HI ('CHIMNEY')
C     !BEG
C     (Get, and allocate, W allotment)
      call MILL   (IN, IS, MOX, 'CHIMNEY')
C
C---- Initialize
      call ZIENNA (IFBRSW, XLB1(MMDPM), DPM)
      call QUAIL  (TE, XNE, XND(1,1), HN1, W(ITRD), W(ITVW), W(ITSK),
     $             W(ITRS))
      call MUSKEG (IU, IL, XNU, TE, XNE, POPK, NO, W(IWW), W(IRM),
     $             W(IVION), W(ITIB), CIB)
C---- Compute
      call ENZIAN (N, LDL, NO, IFBRSW, MPROM, Z, TE, XNE, XND(1,1),
     $             HN1, XLB1(MMDP), DPM, XLB1(MMDW), XLB1(MMCRD),
     $             XLB1(MMCVW), XLB1(MMCSK), XLB1(MMCRS), CIB, W(IFF),
     $             W(ITRD), W(ITVW), W(ITSK), W(ITRS), W(ITIB), W(ICC))
C
C     (Give back W allotment)
      call WGIVE  (W, 'CHIMNEY')
C     !END
      call BYE ('CHIMNEY')
C
      return
      end
