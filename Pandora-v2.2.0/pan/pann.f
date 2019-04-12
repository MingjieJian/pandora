      subroutine PANN
     $(NO,COPUL,GTNUL,PHI,DPUL,DWUL,BTEUL,BCUL,S,SLUL,TAUUL)
C
C     Rudolf Loeser, 1975 Dec 05
C---- Prints data for COOK.
C     !DASH
      save
C     !DASH
      real*8 BCUL, BTEUL, COPUL, DPUL, DWUL, FR, GTNUL, PHI, S, SLUL,
     $       TAUUL
      integer I, IL, IU, N, NO
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
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
C     !DASH
C     !EJECT
      external LINER, DIVIDE, SHIM, HI, BYE
C
C               COPUL(N), GTNUL(N), PHI(N), DPUL(N), DWUL(N), BTEUL(N),
      dimension COPUL(*), GTNUL(*), PHI(*), DPUL(*), DWUL(*), BTEUL(*),
C
C               BCUL(N), SLUL(N), TAUUL(N), S(N)
     $          BCUL(*), SLUL(*), TAUUL(*), S(*)
C
      call HI ('PANN')
C     !BEG
      if(NO.gt.0) then
        call LINER    (3,NO)
        write (NO,100) IU,IL
  100   format(' ','Results of Source Function calculation ',
     $             'for passive transition',I3,'/',I2//
     $         ' ',82X,'Continuum',2X,'Monochrom.',4X,'Line'/
     $         ' ',5X,'Continuum',23X,'Absorption',3X,'Damping',4X,
     $            'Doppler',5X,'Planck',3(5X,'Source'),4X,'Optical'/
     $         ' ',6X,'Opacity',26X,'Profile',3X,'Parameter',4X,
     $            'Width',2X,4(3X,'Function'),4X,'Depth'//
     $         ' ',8X,'XKC',9X,'FR',8X,'GTN',8X,'PHI',9X,'DP',9X,'DW',
     $            8X,'BTE',9X,'BC',10X,'S',9X,'SL',8X,'TAU')
        call LINER    (1,NO)
C
        do 102 I = 1,N
          call DIVIDE (COPUL(I),GTNUL(I),FR)
          write (NO,101) I,COPUL(I),FR,GTNUL(I),PHI(I),DPUL(I),DWUL(I),
     $                   BTEUL(I),BCUL(I),S(I),SLUL(I),TAUUL(I)
  101     format(' ',I3,1P11E11.3)
          call SHIM   (I,5,NO)
  102   continue
      end if
C     !END
      call BYE ('PANN')
C
      return
      end
