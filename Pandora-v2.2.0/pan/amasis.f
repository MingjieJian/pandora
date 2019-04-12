      subroutine AMASIS
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 2006 Mar 15
C---- Drives background (continuum) calculations for computed profiles.
C     (This is version 4 of AMASIS.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IL, ITRN, IU, IW, IX, KODE, KTRU, MODE, NO, NPROG, NT
      logical DOPROF, KILROY
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
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external PET, ISIDORE, KONTMU, SHEEP, LOGIN, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data KODE,MODE,NPROG /3, 3, 7/
C
      call HI ('AMASIS')
C     !BEG
      KILROY = .true.
C
      do 102 ITRN = 1,NT
        call PET       (ITRN)
        call ISIDORE   (DOPROF)
        if(DOPROF) then
C
          if(KILROY) then
            KILROY = .false.
            call LOGIN (NPROG)
          end if
C
          call KONTMU  (NO)
          write (NO,101) IU,IL
  101     format(' ','Transition (',I2,'/',I2,')')
C
C----     Regular background (continuum)
          KTRU = 0
          call SHEEP   (X, IX, W, IW, 1, 1, 1, 1, KODE, MODE, KTRU)
C
C----     Line-free background (continuum)
          KTRU = 1
          call SHEEP   (X, IX, W, IW, 1, 1, 1, 1, KODE, MODE, KTRU)
        end if
  102 continue
C
      if(.not.KILROY) then
        call LOGOUT    (NPROG)
      end if
C     !END
      call BYE ('AMASIS')
C
      return
      end
