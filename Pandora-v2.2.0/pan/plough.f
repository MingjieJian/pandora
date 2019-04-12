      subroutine PLOUGH
     $(XCBL,XLM,CSF,YBRC,COP)
C
C     Rudolf Loeser, 1980 Aug 22
C---- Retrieves continuum data for core of the current transition.
C     !DASH
      save
C     !DASH
      real*8 COP, CSF, XCBL, XLM, YBRC
      integer ICE, INKSW, IOVER, KKCAPP, KKJNU, KKMULT, KKOPAC, KKSCON,
     $        KKSIGS, KTAB, N
      logical SKIP
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
      equivalence (LINKDS( 9),INKSW)
      equivalence (LINKDS( 4),ICE  )
C     !EJECT
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(14),KKSCON)
      equivalence (KKK(13),KKJNU )
      equivalence (KKK( 6),KKOPAC)
      equivalence (KKK(29),KKCAPP)
      equivalence (KKK( 2),KKMULT)
      equivalence (KKK(45),KKSIGS)
C     !DASH
      external PLUG, MOVE1, ARRADD, CONMUL, HI, BYE
C
C               XCBL(Miklen), CSF(N), YBRC(N), COP(N)
      dimension XCBL(*),      CSF(*), YBRC(*), COP(*)
C
      dimension KTAB(6)
C
      data KTAB /1, 24, 18, 19, 25, 26/
C
      call HI ('PLOUGH')
C     !BEG
C---- Read most appropriate Continuum Data Block into XCBL
      call PLUG      (XLM, KTAB, 6, XCBL)
C
C---- Move some data as needed
      call MOVE1     (XCBL(KKSCON), N, CSF )
      call MOVE1     (XCBL(KKJNU ), N, YBRC)
C
C---- Move appropriate "opacity"
      if(ICE.eq.0) then
        SKIP = (INKSW.gt.0).and.(IOVER.eq.1)
        if(.not.SKIP) then
          call MOVE1 (XCBL(KKOPAC), N, COP)
        end if
      else
        call ARRADD  (XCBL(KKCAPP), XCBL(KKSIGS), COP, N)
        call CONMUL  (XCBL(KKMULT), COP, N)
      end if
C     !END
      call BYE ('PLOUGH')
C
      return
      end
