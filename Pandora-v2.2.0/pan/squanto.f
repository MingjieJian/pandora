      subroutine SQUANTO
     $(JSAV,WVL,DL,K,YHZ,MUX,YY,MYX,BT,KODE,NLTE)
C
C     Rudolf Loeser, 2000 Jul 19
C---- Saves Spectrum-Summary data, for ZAMIDAR.
C     !DASH
      save
C     !DASH
      real*8 BT, DL, WVL, YHZ, YY
      integer IL, IU, IX, JSAV, K, KODE, MUX, MYX, NLTE
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
      external QUEBEC, ANASTAS, HI, BYE
C
C               DL(KM), KODE(KM), YHZ(KM), MUX(KM), MYX(KM), BT(KM),
      dimension DL(*),  KODE(*),  YHZ(*),  MUX(*),  MYX(*),  BT(*),
C
C               YY(KM)
     $          YY(*)
C     !EJECT
C
      call HI ('SQUANTO')
C     !BEG
C---- Get line-center index
      call QUEBEC  (DL,K,'DL','SQUANTO/ZAMIDAR',IX)
C---- Save data
      call ANASTAS (JSAV,IU,IL,WVL,YHZ(IX),MUX(IX),YY(IX),MYX(IX),
     $              BT(IX),KODE(IX),NLTE,1)
C     !END
      call BYE ('SQUANTO')
C
      return
      end
