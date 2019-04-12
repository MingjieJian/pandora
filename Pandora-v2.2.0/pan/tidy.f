      subroutine TIDY
     $(X,IX,W,T,CIJ,GMI,PE,FE,EP,EF,BS,BATA,RHOIJ,YBRIJ,PN,FN,EPN,EFN,
     $ BSN,LUT,LUG,LUC,ALL,KILROY,MM,METNAM,KNT,KRJ,LAST)
C
C     Rudolf Loeser, 1980 Apr 15
C---- Produces printout for SWALLOW.
C     (This is version 2 of TIDY.)
C     !DASH
      save
C     !DASH
      real*8 BATA, BS, BSN, CIJ, EF, EFN, EP, EPN, FE, FN, GMI, PE, PN,
     $       RHOIJ, T, W, X, YBRIJ
      integer IL, IU, IUL, IX, KBDSE, KNT, KRJ, LUC, LUG, LUT, METSE,
     $        MM, MO, N, NL
      logical ALL, KILROY, LAST
      character METNAM*(*), TIT*40
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
      equivalence (LINKDS( 6),METSE)
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external GIGGLE, DAIT, FIG, INDXUL, GRAPE, HI, BYE
C
      dimension X(*), IX(*), W(*)
C
C               BATA(N,MUL), CIJ(N,NL**2), RHOIJ(N,NT), BS(N), MM(KNT),
      dimension BATA(N,*),   CIJ(*),       RHOIJ(*),    BS(*), MM(*),
C
C               GMI(N,NSL), PE(N), FE(N), EP(N), PN(N,KNT), BSN(N,KNT),
     $          GMI(*),     PE(*), FE(*), EP(*), PN(*),     BSN(*),
C
C               YBRIJ(N,NT), EPN(N,KNT), EFN(N,KNT), FN(N,KNT), T(KNT),
     $          YBRIJ(*),    EPN(*),     EFN(*),     FN(*),     T(*),
C
C               METNAM(KNT), EF(N)
     $          METNAM(*),   EF(*)
C
      data KBDSE /2/
C     !EJECT
C
      call HI ('TIDY')
C     !BEG
      if(MO.gt.0) then
C
        if(KILROY) then
          KILROY = .false.
          if(METSE.ne.4) then
C----       Detail printout
            write (TIT,100) IU,IL,METSE
  100       format('Statistical Equilibrium',I4,'/',I2,', METSE',I2)
            call GIGGLE (X, IX, W, RHOIJ, YBRIJ, KRJ, KBDSE, IU, IL,
     $                   TIT, MO)
          end if
        end if
C
C----   Timing information
        call DAIT       (LUT, T, METNAM, KNT, IU, IL)
C----   General results
        call INDXUL     (IU, IL, IUL)
        call FIG        (X, IX, W, LUG, N, CIJ, GMI, PE, FE,
     $                   BATA(1,IUL), EP, EF, BS, IU, IL, KRJ, NL,
     $                   METSE, METNAM, KNT, LAST)
C----   Methods comparison
        if(ALL.and.(LUC.gt.0)) then
          call GRAPE    (LUC, 'PE', N, IU, IL, METSE, MM, METNAM, KNT,
     $                   PN )
          call GRAPE    (LUC, 'FE', N, IU, IL, METSE, MM, METNAM, KNT,
     $                   FN )
          call GRAPE    (LUC, 'EP', N, IU, IL, METSE, MM, METNAM, KNT,
     $                   EPN)
          call GRAPE    (LUC, 'EF', N, IU, IL, METSE, MM, METNAM, KNT,
     $                   EFN)
          call GRAPE    (LUC, 'BS', N, IU, IL, METSE, MM, METNAM, KNT,
     $                   BSN)
        end if
      end if
C     !END
      call BYE ('TIDY')
C
      return
      end
