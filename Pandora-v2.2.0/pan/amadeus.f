      subroutine AMADEUS
     $(XLB1,MSFT)
C
C     Rudolf Loeser, 1994 Aug 25
C---- Sets up Line Source Function Method switch.
C     !DASH
      save
C     !DASH
      real*8 XLB1
      integer ICE, LSFT, LUEO, MMTAU, MSFT
C     !COM
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(16),MMTAU)
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
      equivalence (LINKDS(10),LSFT )
      equivalence (LINKDS( 4),ICE  )
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external FEDELM, MESHED, MASHED, HI, BYE
C
C               XLB1(Li1len)
      dimension XLB1(*)
C
      call HI ('AMADEUS')
C     !BEG
      MSFT = LSFT
C
      if(ICE.eq.0) then
C----   Force Sobolev solution when TAU is too large (only for CRD
C       transitions)
        call FEDELM (XLB1(MMTAU), MSFT)
      end if
C
C---- Now make sure that the value of MSFT is valid
      if((MSFT.lt.0).or.(MSFT.gt.2)) then
        call MESHED ('AMADEUS', 3)
        write (LUEO,100) MSFT
  100   format(' ','LSFTYP =',I12,'! Meaningless value; will be set ',
     $             '= 0 (full solution).')
        call MASHED ('AMADEUS')
C
        MSFT = 0
      end if
C     !END
      call BYE ('AMADEUS')
C
      return
      end
