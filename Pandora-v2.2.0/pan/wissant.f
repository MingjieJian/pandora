      subroutine WISSANT
     $(X,W)
C
C     Rudolf Loeser, 1982 Jul 23
C---- Controls PRD Jnu restart values reading.
C     !DASH
      save
C     !DASH
      real*8 W, X, dummy
      integer I, ICE, IL, IN, IS, IU, IXLB1, IXLB2, JAYTI, JJZ, KMAX,
     $        LU, LUEO, MOX, N, NMAX, NT
      logical FOUND
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 5),NT )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(22),JAYTI)
      equivalence (LUNITS( 6),LUEO )
C     !EJECT
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
C     !EJECT
      external ABJECT, PIGWEED, LIDGET, LIDPUT, NIDAU, FALAKI, ALONZO,
     $         LACK, PET, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IXLB1 ),(IN( 2),IXLB2 )
C
      call HI ('WISSANT')
C     !BEG
C     (Get W allotmwnt)
      call NIDAU        (IN, IS, MOX, 'WISSANT')
C
C     Get NMAX and KMAX
      call PIGWEED      (JAYTI, NMAX, KMAX)
C---- Set up printing
      call ALONZO       (LU)
      call ABJECT       (LU)
C---- Loop over all transitions
      do 100 I = 1,NT
        call PET        (I)
        if(ICE.ne.0) then
C----     This is a PRD transition: get Jnu's if possible.
C         First, read data block to get DLTRN and KTRN,
C         and default XJNU
          call LIDGET   (W(IXLB1), 1, W(IXLB2), 1, dummy, 0, I)
C----     (rewind file for every search for a desired set, so
C         that entire file will always be searched)
          rewind JAYTI
C----     Attempt to get data, and interpolate if necessary
          call FALAKI   (JAYTI, IU, IL, NMAX, KMAX, N, X(JJZ),
     $                   W(IXLB1), W(IXLB2), W, LU, FOUND)
          if(FOUND) then
C----       Save updated data block
            call LIDPUT (dummy, 0, W(IXLB2), 1, dummy, 0, I)
          end if
        end if
  100 continue
C---- "Close" input file
      call LACK         (JAYTI, LUEO)
C
C     (Give back W allotment)
      call WGIVE        (W, 'WISSANT')
C     !END
      call BYE ('WISSANT')
C
      return
      end
