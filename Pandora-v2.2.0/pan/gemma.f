      subroutine GEMMA
     $(KALTG)
C
C     Rudolf Loeser, 2005 Feb 23
C---- Sets up KALTG,
C     the Hydrogen Lyman alpha & beta PRD GMMA switch.
C     !DASH
      save
C     !DASH
      integer ICE, IGMSW, IL, IU, JUL, KALTG
      character QELSM*8
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
C     !EJECT
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ(183),IGMSW)
C     !DASH
      external HI, BYE
C
      call HI ('GEMMA')
C     !BEG
      KALTG = 0
C
      if(IGMSW.gt.0) then
        if(QELSM(1:3).eq.'H  ') then
          JUL = 100*IU+IL
          if((JUL.eq.201).or.(JUL.eq.301)) then
            if(ICE.ne.0) then
              KALTG = IU
            end if
          end if
        end if
      end if
C     !END
      call BYE ('GEMMA')
C
      return
      end
