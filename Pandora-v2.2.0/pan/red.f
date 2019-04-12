      subroutine RED
     $(W,FLX,LF,K,LTE,EMUF,WVL,DL,WVNUM,WTAB,WINT,WINTL,TCFLX,PROGLI,
     $ LDL,DDL,PGD,ISB1,KILROY,NO,IJECT,LUG)
C
C     Rudolf Loeser, 2000 Jul 20
C---- Computes, prints and plots Flux profiles.
C     (This is version 4 of RED.)
C     !DASH
      save
C     !DASH
      real*8 DDL, DL, EMUF, PGD, PROGLI, TCFLX, W, WINT, WINTL, WTAB,
     $       WVL, WVNUM, dummy
      integer IFHZ, IFHZL, IJECT, IN, IS, ISB1, ITCF, K, KLIN, KTF, LDL,
     $        LF, LUG, MOX, NO
      logical FLX, KILROY, LTE
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
      external ZENOBIA, APRICOT, INEZ, OOZE, DESMAN, WGIVE, HI, BYE
C
      dimension W(*)
C
C               PGD(3,LDLMX), DDL(LDLMX), WVNUM(KM), WINTL(KM), DL(KM),
      dimension PGD(*),       DDL(*),     WVNUM(*),  WINTL(*),  DL(*),
C
C               TCFLX(KM,LFBV), WINT(KM), EMUF(LF), WTAB(KM)
     $          TCFLX(*),       WINT(*),  EMUF(*),  WTAB(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),ITCF  ),(IN( 2),IFHZ  ),(IN( 3),IFHZL )
C
      call HI ('RED')
C     !BEG
      if(FLX) then
C
C       (Get, and allocate, W allotment)
        call INEZ      (IN, IS, MOX, 'RED')
C
C----   Get background data (for residuals calculation)
        call OOZE      (K, TCFLX, W(ITCF), KTF)
C----   Compute, print and plot non-LTE profile
        call ZENOBIA   (NO, K, LF, EMUF, DL, WVL, WVNUM, WTAB, WINT ,
     $                  dummy, dummy, W(IFHZ ), 1, KLIN, W(ITCF), KTF,
     $                  KILROY, ISB1, LDL, DDL, IJECT, W)
        call APRICOT   (LUG, K, WVL, DL, WVNUM, WTAB, W(IFHZ), PROGLI,
     $                  LDL, DDL, PGD, W)
C----   Save checksum for Flux/Hz
        call DESMAN    (W(IFHZ), K)
C
        if(LTE) then
C----     Compute and print LTE profile
          call ZENOBIA (NO, K, LF, EMUF, DL, WVL, WVNUM, WTAB, WINTL,
     $                  dummy, dummy, W(IFHZL), 0, KLIN, W(ITCF), KTF,
     $                  KILROY, ISB1, LDL, DDL, IJECT, W)
        end if
C
C       (Give back W allotment)
        call WGIVE     (W, 'RED')
C
      end if
C     !END
      call BYE ('RED')
C
      return
      end
