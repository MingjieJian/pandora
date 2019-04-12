      subroutine BOJCA
     $(KIC,LTYPE,BACKGR,LINCON,USE)
C
C     Rudolf Loeser, 2003 Mar 26
C---- Selects wavelengths, for BERMUDA.
C     !DASH
      save
C     !DASH
      integer IL, IU, KIC, LTYPE
      logical BACKGR, LINCON, LINE, REGCOR, SPECOR, SPEWNG, USE
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
C     !DASH
C     !EJECT
      external FULEX, HI, BYE
C
      call HI ('BOJCA')
C     !BEG
      USE = .false.
C
      if(BACKGR) then
        call FULEX (LTYPE, REGCOR, SPECOR, SPEWNG)
        LINE = REGCOR.or.SPECOR.or.SPEWNG
        USE  = .not.LINE
        goto 101
      end if
C
      if(LINCON) then
        if(KIC.gt.0) then
          USE = KIC.eq.(100*IU+IL)
        end if
      end if
C
  101 continue
C     !END
      call BYE ('BOJCA')
C
      return
      end
