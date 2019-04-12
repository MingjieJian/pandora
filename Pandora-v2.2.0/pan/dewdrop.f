      subroutine DEWDROP
     $(XLB1,K,KODE,LU,W,IW)
C
C     Rudolf Loeser, 1992 Oct 15
C---- Checks absorption edges for CRUMB.
C     (This is version 2 of DEWDROP.)
C     !DASH
      save
C     !DASH
      real*8 W, XLB1
      integer IIPNT, IIWRK, IL, ILEVT, INUMT, IU, IW, IWS, JN, K, KODE,
     $        LU, MMDL, MMLAM, MUX
C     !COM
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML( 2),MMLAM)
      equivalence (MML(59),MMDL )
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
C     !DASH
C     !EJECT
      external AMBROSE, NECTAR, IGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               XLB1(Li1len)
      dimension XLB1(*)
C
      dimension JN(4)
      equivalence
     $(JN( 1),INUMT ),(JN( 2),ILEVT ),(JN( 3),IIPNT ),(JN( 4),IIWRK )
C
      call HI ('DEWDROP')
C     !BEG
C     (Get, and allocate, IW allotment)
      call NECTAR  (JN, IWS, MUX, 'DEWDROP')
C
      call AMBROSE (XLB1(MMLAM), XLB1(MMDL), K, KODE, IU, IL, LU,
     $              W, IW(INUMT), IW(ILEVT), IW(IIPNT), IW(IIWRK))
C
C     (Give back IW allotment)
      call IGIVE   (IW, 'DEWDROP')
C     !END
      call BYE ('DEWDROP')
C
      return
      end
