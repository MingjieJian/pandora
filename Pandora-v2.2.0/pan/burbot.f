      subroutine BURBOT
     $(LUMR,NT,NL,NTE,AIJ,FA,CEIJ,FCEIJ,XLB1,FCRD,FCVW,FCSK,FCRS)
C
C     Rudolf Loeser, 1992 Jan 14
C---- Saves atomic transition data for ANATINI.
C     (This is version 2 of BURBOT.)
C     !DASH
      save
C     !DASH
      real*8 AIJ, CEIJ, XLB1, dummy
      integer I, IL, IU, JL, JU, JUL, LUMR, MMCRD, MMCRS, MMCSK, MMCVW,
     $        MODE, NL, NT, NTE
      logical FA, FBLK, FCEIJ, FCRD, FCRS, FCSK, FCVW
      character LABEL*17
C     !COM
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
C
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
C     !DASH
C     !EJECT
      external INDXUL, PET, PUNT, LIDGET, ANOMURA, HI, BYE
C
C               AIJ(NL,NL), CEIJ(NTE,MUL), XLB1(Li1len)
      dimension AIJ(NL,*),  CEIJ(NTE,*),   XLB1(*)
C
      data MODE /1/
C
      call HI ('BURBOT')
C     !BEG
      if(NT.gt.0) then
        FBLK = FCRD.or.FCVW.or.FCSK.or.FCRS
        do 101 I = 1,NT
          call PET       (I)
C
          if(FA) then
            write (LABEL,100) 'A ',IU,IL
  100       format(A2,9X,2I3)
            call PUNT    (LUMR,AIJ(IU,IL),1,MODE,LABEL)
          end if
C
          if(FBLK) then
            call LIDGET  (XLB1,1,dummy,0,dummy,0,I)
            call ANOMURA (LUMR,IU,IL,XLB1(MMCRD),FCRD,XLB1(MMCVW),
     $                    FCVW,XLB1(MMCSK),FCSK,XLB1(MMCRS),FCRS)
          end if
C
  101   continue
      end if
C
      if(FCEIJ) then
        do 103 JU = 2,NL
          do 102 JL = 1,(JU-1)
            call INDXUL  (JU,JL,JUL)
            write (LABEL,100) 'CE',JU,JL
            call PUNT    (LUMR,CEIJ(1,JUL),NTE,MODE,LABEL)
  102     continue
  103   continue
      end if
C     !END
      call BYE ('BURBOT')
C
      return
      end
