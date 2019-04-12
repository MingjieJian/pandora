      subroutine GROUSE
     $(IFBRSW,CRD,CVW,CSK,CRS,CIB,F)
C
C     Rudolf Loeser, 1980 Aug 14
C---- Computes Damping Parameter components coefficients.
C     !DASH
      save
C     !DASH
      real*8 CIB, CRD, CRS, CSK, CVW, F, ZERO
      integer I, IBRSW, IFBRSW, JBRSW
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
      equivalence (LINKDS( 8),IBRSW)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  HI, BYE
      intrinsic mod
C
C               F(5)
      dimension F(*)
C     !EJECT
C
      call HI ('GROUSE')
C     !BEG
      F(1) = CRD
      F(2) = CVW
      F(3) = CSK
      F(4) = CRS
      F(5) = CIB
C
      if(IFBRSW.le.0) then
        JBRSW = IBRSW
        do 100 I = 1,5
          if(mod(JBRSW,2).eq.0) then
            F(I) = ZERO
          end if
          JBRSW = JBRSW/2
  100   continue
      end if
C     !END
      call BYE ('GROUSE')
C
      return
      end
