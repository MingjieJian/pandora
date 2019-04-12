      subroutine BERRY
     $(NO,KFUNC,KNLTE,KSTAR,WVL,IJECT,ISB1)
C
C     Rudolf Loeser, 1986 Feb 20
C---- Prints page headings, for FABLE.
C     (This is version 2 of BERRY.)
C     !DASH
      save
C     !DASH
      real*8 WVL
      integer ICE, IJECT, IL, ISB1, IU, KFUNC, KNLTE, KSTAR, NO
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
      equivalence (LINKDS( 4),ICE  )
C     !DASH
      external DEJECT, FLACK, BADEFOL, HI, BYE
C     !EJECT
C
      call HI ('BERRY')
C     !BEG
      if(NO.gt.0) then
C----   Print detailed header
        call DEJECT   (NO,IJECT)
        call FLACK    (NO,KNLTE,WVL,IU,IL,ICE,ISB1)
C----   Print general header
        call BADEFOL  (NO,KFUNC,KSTAR)
      end if
C     !END
      call BYE ('BERRY')
C
      return
      end
