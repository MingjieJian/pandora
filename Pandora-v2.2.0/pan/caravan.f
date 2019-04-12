      subroutine CARAVAN
     $(X,W,LL,N,XI,DL,PHI,COP,GTN,TNU,IMG,KODE,DMP1)
C
C     Rudolf Loeser, 1977 Jun 21
C---- Computes TNUR for Diana Data Blocks.
C     !DASH
      save
C     !DASH
      real*8 COP, DL, GTN, PHI, TNU, W, X, XI
      integer IL, IMG, IU, KODE, KRP, LL, N
      logical DMP1
      character LABEL*100
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
      external  MIKE, NOLA, HI, BYE
C
      dimension X(*), W(*)
C
C               PHI(N), COP(N), GTN(N), TNU(N), IMG(N)
      dimension PHI(*), COP(*), GTN(*), TNU(*), IMG(*)
C     !EJECT
C
      call HI ('CARAVAN')
C     !BEG
      write (LABEL,100) IU,IL,LL,XI,DL
  100 format('Line',I3,'/',I2,' Monochromatic TAU at',I3,'. XI =',
     $        F15.7,', DL =',F15.7)
C
      call MIKE   (X, W, N, 1, N, PHI, COP, GTN, LABEL, TNU, KODE, IMG)
C
      if(LL.eq.1) then
        KRP = 1
      end if
      if(DMP1) then
        call NOLA (LL, KRP, N, COP, GTN, TNU)
      end if
C     !END
      call BYE ('CARAVAN')
C
      return
      end
