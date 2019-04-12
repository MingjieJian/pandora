      subroutine OTTER
     $(JU,JL,X,NT,YCONT,OMLT,OLLT,PCET,YCO,OML,OLL,PCE)
C
C     Rudolf Loeser, 1991 Sep 09
C---- Sets up YCO, OML, and OLL, for GECKO.
C     (This is version 3 of OTTER.)
C     !DASH
      save
C     !DASH
      real*8 OLL, OLLT, OML, OMLT, PCE, PCET, X, YCO, YCONT
      integer I, IL, IU, IUL, JL, JU, JUL, LUEO, NT
      logical OK
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
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
      external INDXUL, INTRANS, PET, MESHED, ABORT, HI, BYE
C
      dimension X(*)
C
C               YCONT(NT), YCO(MUL), PCE(MUL), OML(MUL), OLLT(NT),
      dimension YCONT(*),  YCO(*),   PCE(*),   OML(*),   OLLT(*),
C
C               OLL(MUL), OMLT(NT), JU(MUL), JL(MUL), PCET(NT)
     $          OLL(*),   OMLT(*),  JU(*),   JL(*),   PCET(*)
C
      call HI ('OTTER')
C     !BEG
C---- Loop over all INPAIR transitions
      do 101 I = 1,NT
        call PET     (I)
C----   Get index of corresponding data array slot
        call INDXUL  (IU, IL, JUL)
C----   Make sure transition index pairs agree
        OK = (IU.eq.JU(JUL)).and.(IL.eq.JL(JUL))
        if(.not.OK) then
          call MESHED ('OTTER', 1)
          write (LUEO,100) I,JUL,IU,IL,JU(JUL),JL(JUL)
  100     format('Index mixup: I =',I12,', JUL =',I12/
     $           'IU =',I12,', IL =',I12,'; JU =',I12,', JL =',I12)
          call ABORT
        end if
C----   Get index of item in General (in-memory) data block
        call INTRANS (IU, IL, 'Otter', IUL)
C----   Now, set things up
        YCO(JUL) = YCONT(IUL)
        OLL(JUL) = OLLT(IUL)
        OML(JUL) = OMLT(IUL)
        PCE(JUL) = PCET(IUL)
  101 continue
C     !END
      call BYE ('OTTER')
C
      return
      end
