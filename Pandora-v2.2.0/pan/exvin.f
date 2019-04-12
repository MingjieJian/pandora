      subroutine EXVIN
     $(LINTPS,STOP)
C
C     Rudolf Loeser, 2003 Apr 03
C---- Checks whether escape probability (Sobolev) method allowed
C     if selected.
C     !DASH
      save
C     !DASH
      integer ICE, IL, IU, JBD, LINTPS, LUEO
      logical STOP
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
      equivalence (LINKDS( 4),ICE  )
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 10),JBD  )
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, HI, BYE
C
      call HI ('EXVIN')
C     !BEG
      if(LINTPS.eq.2) then
C
        if(JBD.eq.2) then
          STOP = .true.
          call MESHED ('EXVIN', 1)
          write (LUEO,100) IU,IL
  100     format(' ','Transition (',I2,'/',I2,')')
          write (LUEO,101)
  101     format(' ','BDOPT = BDQ: b-ratios from CHI;'/
     $           ' ','this is not allowed in a run using an escape ',
     $               'probability method for any transition.')
        end if
C
        if(ICE.ne.0) then
          STOP = .true.
          call MESHED ('EXVIN', 1)
          write (LUEO,100) IU,IL
          write (LUEO,102)
  102     format(' ','The escape probability (Sobolev) method may ',
     $               'not be selected for a PRD transition.')
        end if
C
      end if
C     !END
      call BYE ('EXVIN')
C
      return
      end
