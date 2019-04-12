      subroutine GUENON
     $(KILROY,CALLER)
C
C     Rudolf Loeser, 1985 Feb 14
C---- Summarizes Line Source Function weight matrix methods.
C     !DASH
      save
C     !DASH
      integer I, IL, IU, KLIN, METSF, MSFGR, MSFQM, MSFQR, MSFRT, NT,
     $        NY
      logical KILROY
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 5),NT )
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
      equivalence (LINKDS( 7),METSF)
C     !EJECT
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(16),MSFQR)
      equivalence (LEST(18),MSFQM)
      equivalence (LEST(20),MSFRT)
      equivalence (LEST(21),MSFGR)
C     !DASH
      external HALT, AIMERIC, PET, HI, BYE
C
      call HI ('GUENON')
C     !BEG
      NY = 0
      do 101 I = 1,NT
        call PET      (I)
        if((KLIN.eq.1).or.(KLIN.eq.2)) then
C
          if((METSF.lt.0).or.(METSF.gt.3)) then
            write (MSSLIN(1),100) METSF
  100       format('METSF =',I12,', which is not 0, 1, 2, or 3.')
            call HALT ('GUENON', 1)
          end if
C
          NY = NY+1
          if(METSF.eq.0) then
            MSFRT = MSFRT+1
          else if(METSF.eq.1) then
            MSFQR = MSFQR+1
          else if(METSF.eq.2) then
            MSFQM = MSFQM+1
          else
            MSFGR = MSFGR+1
          end if
        end if
  101 continue
C
      call AIMERIC    (KILROY, CALLER, 'YLINE', NY)
C     !END
      call BYE ('GUENON')
C
      return
      end
