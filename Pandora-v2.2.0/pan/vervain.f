      subroutine VERVAIN
     $(N,NT,XLB1,S,AW,YBAR,RHOIJ,CHI,RHWL,ORHO)
C
C     Rudolf Loeser, 1980 May 04
C---- Updates stuff in Line Intensity Data Blocks, for LILY.
C     !DASH
      save
C     !DASH
      real*8 AW, CHI, ORHO, RHOIJ, RHWL, S, XLB1, YBAR, dummy
      integer IL, IU, IUL, J, KLIN, MMAW, MMJBR, MMORH, MMQHI, MMRHO,
     $        MMRHW, MMS, N, NT
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
      equivalence (LINKDS( 3),KLIN )
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(27),MMJBR)
      equivalence (MML(28),MMRHO)
      equivalence (MML(29),MMRHW)
      equivalence (MML(30),MMORH)
      equivalence (MML(26),MMS  )
      equivalence (MML(65),MMAW )
      equivalence (MML(66),MMQHI)
C     !DASH
C     !EJECT
      external INTRANS, LIDGET, LIDPUT, MOVE1, PET, HI, BYE
C
C               YBAR(N,NT), RHOIJ(N,NT), RHWL(N,NT), AW(N,NT), S(N,NT),
      dimension YBAR(N,*),  RHOIJ(N,*),  RHWL(N,*),  AW(N,*),  S(N,*),
C
C               ORHO(N,NT), CHI(N,NT), XLB1(Li1len)
     $          ORHO(N,*),  CHI(N,*),  XLB1(*)
C
      call HI ('VERVAIN')
C     !BEG
      do 100 J = 1,NT
        call PET       (J)
        if(KLIN.eq.1) then
          call INTRANS (IU, IL, 'VERVAIN', IUL)
C
          call LIDGET  (XLB1, 1, dummy, 0, dummy, 0, J)
C
          call MOVE1   (YBAR (1,IUL), N, XLB1(MMJBR))
          call MOVE1   (RHOIJ(1,IUL), N, XLB1(MMRHO))
          call MOVE1   (CHI  (1,IUL), N, XLB1(MMQHI))
          call MOVE1   (RHWL (1,IUL), N, XLB1(MMRHW))
          call MOVE1   (ORHO (1,IUL), N, XLB1(MMORH))
          call MOVE1   (S    (1,IUL), N, XLB1(MMS  ))
          call MOVE1   (AW   (1,IUL), N, XLB1(MMAW ))
C
          call LIDPUT  (XLB1, 1, dummy, 0, dummy, 0, J)
        end if
  100 continue
C     !END
      call BYE ('VERVAIN')
C
      return
      end
