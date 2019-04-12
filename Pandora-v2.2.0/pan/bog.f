      subroutine BOG
     $(XLB1,N,NL,NT,XNU,XND,A,CRT)
C
C     Rudolf Loeser, 1979 Oct 24
C---- Controls computation of cooling rates for bound-bound transitions.
C     !DASH
      save
C     !DASH
      real*8 A, CRT, XLB1, XND, XNU, dummy
      integer I, IL, IU, KLIN, MMRHO, N, NL, NT
C     !COM
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(28),MMRHO)
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
      equivalence (LINKDS( 3),KLIN )
C     !DASH
C     !EJECT
      external LIDGET, BIBINGA, PET, HI, BYE
C
C               CRT(N,NT), A(NL,NL), XND(N,NL), XNU(NSL), XLB1(Li1len)
      dimension CRT(*),    A(*),     XND(*),    XNU(*),   XLB1(*)
C
      call HI ('BOG')
C     !BEG
      do 100 I = 1,NT
        call PET       (I)
        if(KLIN.eq.1) then
          call LIDGET  (XLB1, 1, dummy, 0, dummy, 0, I)
          call BIBINGA (N, NL, XNU, XND, A, XLB1(MMRHO), IU, IL, CRT)
        end if
  100 continue
C     !END
      call BYE ('BOG')
C
      return
      end
