      subroutine APRICOT
     $(NO,K,CORE,DL,WVNUM,WTAB,FHZ,PROGLI,LDL,DDL,PGD,W)
C
C     Rudolf Loeser, 1992 Aug 19
C---- Drives OSTRICH to make a graph of an absolute flux profile.
C     (This is version 2 of APRICOT.)
C     !DASH
      save
C     !DASH
      real*8 CORE, DDL, DL, FHZ, PGD, PROGLI, SCORE, W, WTAB, WVNUM
      integer IL, IN, IS, ISBG, ISDL, ISFZ, IU, IWTB, IWVNM, J, K,
     $        KOUNT, LBLU, LDL, LRED, MOX, NO
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
      equivalence (LINKDS(16),ISBG )
C     !DASH
C     !EJECT
      external OSTRICH, NEPEAN, MOVE1, CONSUB, IPOTA, SNUGUD, WUMBLE,
     $         WGIVE, HI, BYE
C
      dimension W(*)
C
C               PGD(3,LDLMX), DDL(LDLMX), WTAB(KM), FHZ(KM), WVNUM(KM),
      dimension PGD(3,*),     DDL(*),     WTAB(*),  FHZ(*),  WVNUM(*),
C
C               DL(KM)
     $          DL(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IWTB  ),(IN( 2),ISDL  ),(IN( 3),ISFZ  ),(IN( 4),IWVNM )
C
      call HI ('APRICOT')
C     !BEG
      if(NO.gt.0) then
C----   Plot for entire range of DL values
        call OSTRICH     (NO, IU, IL, WTAB, K, PROGLI, FHZ, 0, LDL)
C
        if((LDL.gt.1).and.(ISBG.gt.0)) then
C         (Get, and allocate, W allotment)
          call IPOTA     (IN, IS, MOX, 'APRICOT')
C
C----     Need separate plots of blended line components
          do 100 J = 1,LDL
            call NEPEAN  (DL, K, PGD(2,J), PGD(3,J), LBLU, LRED)
            KOUNT = LRED-LBLU+1
            SCORE = PGD(1,J)
            call MOVE1   (DL(LBLU), KOUNT, W(ISDL))
            call CONSUB  (DDL(J), W(ISDL), KOUNT)
            call MOVE1   (FHZ(LBLU), KOUNT, W(ISFZ))
            call SNUGUD  (KOUNT, W(ISDL), SCORE, W(IWVNM), W(IWTB), 1)
            call OSTRICH (NO, IU, IL, W(IWTB), KOUNT, PROGLI, W(ISFZ),
     $                    J, LDL)
  100     continue
C
C         (Give back W allotment)
          call WGIVE     (W, 'APRICOT')
C
C----     Restore the contents of common block MOSTAR (which were
C         destroyed by those special calls to SNUGUD, above).
          call WUMBLE    (K, DL, WVNUM, WTAB, CORE, 1)
        end if
C
      end if
C     !END
      call BYE ('APRICOT')
C
      return
      end
