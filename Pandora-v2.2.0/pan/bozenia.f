      subroutine BOZENIA
     $(NO,K,DL,LF,EMUF,WVL,WVNUM,WTAB,LTE,FLX,FLXL,TF,TFL,SF,SFL,FHZ,
     $ TCF,KTF,KILROY,ISB1,LDL,DDL,IJECT,W)
C
C     Rudolf Loeser, 2000 Jun 29
C---- Computes and prints flux, FHZ, using spherical coordinates.
C     !DASH
      save
C     !DASH
      real*8 DDL, DL, EMUF, FHZ, FLX, FLXL, SF, SFL, TCF, TF, TFL, W,
     $       WTAB, WVL, WVNUM
      integer IFQ, IJECT, IN, IS, ISB1, K, KLIN, KTF, LDL, LF, MOX, NO
      logical KILROY, LTE
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
      equivalence (LINKDS( 3),KLIN )
C     !DASH
C     !EJECT
      external NOBAZ, ZENOBIA, WGIVE, HI, BYE
C
      dimension W(*)
C
C               EMUF(LF), FLXL(KM), FLX(KM), FHZ(KM), TFL(KM), SFL(KM),
      dimension EMUF(*),  FLXL(*),  FLX(*),  FHZ(*),  TFL(*),  SFL(*),
C
C               DL(KM), TF(KM), SF(KM), TCF(KM), WVNUM(KM), DDL(LDLMX),
     $          DL(*),  TF(*),  SF(*),  TCF(*),  WVNUM(*),  DDL(*),
C
C               WTAB(KM)
     $          WTAB(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IFQ   )
C
      call HI ('BOZENIA')
C     !BEG
      if(NO.gt.0) then
C       (Get, and allocate, W allotment)
        call NOBAZ     (IN, IS, MOX, 'BOZENIA')
C
C----   Compute and print non-LTE flux
        call ZENOBIA   (NO, K, LF, EMUF, DL, WVL, WVNUM, WTAB, FLX,
     $                  TF, SF, FHZ, 1, KLIN, TCF, KTF, KILROY,
     $                  ISB1, LDL, DDL, IJECT, W)
        if(LTE) then
C----     Compute and print LTE flux
          call ZENOBIA (NO, K, LF, EMUF, DL, WVL, WVNUM, WTAB, FLXL,
     $                  TFL, SFL, W(IFQ), 0, KLIN, TCF, KTF, KILROY,
     $                  ISB1, LDL, DDL, IJECT, W)
        end if
C
C       (Give back W allotment)
        call WGIVE     (W, 'BOZENIA')
      end if
C     !END
      call BYE ('BOZENIA')
C
      return
      end
