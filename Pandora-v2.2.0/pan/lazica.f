      subroutine LAZICA
     $(N,NT,TAU,RHO,RHOWT,S,QHI,A,KIBI,NO,PRAT)
C
C     Rudolf Loeser, 1984 Jan 25
C---- Supervises some iterative summaries.
C     (This is version 3 of LAZICA.)
C     !DASH
      save
C     !DASH
      real*8 A, PRAT, QHI, RHO, RHOWT, S, TAU
      integer I, IL, IU, KIBI, KLIN, N, NO, NQHITR, NRHITR, NRWITR,
     $        NSSITR, NT, NTAITR
      logical TR
      character TITLE*15
C     !COM
C---- IBIS        as of 2003 May 30
      integer     MXIBIS,NTMX,IBADR,IBKOD,IBNIT,IBLEN,IBIN1,IBIN2,
     $            IBITE,IBLIT,IBITH,IBIOV,NITS,NIBIS
      character   IBSRC*10, IBNAM*10
      parameter   (MXIBIS=1000)
      parameter   (NTMX=14)
C     (Remember to recompile all users when changing MXIBIS or NTMX)
      dimension   IBADR(MXIBIS), IBKOD(MXIBIS), IBNIT(MXIBIS),
     $            IBLEN(MXIBIS), IBIN1(MXIBIS), IBIN2(MXIBIS),
     $            IBITE(MXIBIS), IBLIT(MXIBIS), IBITH(MXIBIS),
     $            IBIOV(MXIBIS), IBSRC(MXIBIS), IBNAM(MXIBIS),
     $            NITS(NTMX)
      common      /IBIS1/ NIBIS, IBADR, IBKOD, IBNIT, IBLEN, IBIN1,
     $                           IBIN2, IBITE, IBLIT, IBITH, IBIOV
      common      /IBIS2/        IBSRC, IBNAM
      common      /IBIS3/ NITS
C     Control information for iterative summary data.
C
C         NITS = counts of iteration summary data records for:
C
C      1: TAU(IU,IL),    2: CHECK(L)       3: RHO(IU,IL)
C      4: RK(KOLEV)      5: ND(L)          6: RHOWT(IU,IL)
C      7: BD(KOLEV)      8: NE             9: CHI(IU,IL)
C     10: Z             11: S(IU,IL)      12: NH
C     13: TDST          14: NK
      equivalence (NITS(11),NSSITR)
      equivalence (NITS( 1),NTAITR)
      equivalence (NITS( 3),NRHITR)
      equivalence (NITS( 6),NRWITR)
      equivalence (NITS( 9),NQHITR)
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
      equivalence (LINKDS( 3),KLIN )
C     !DASH
      external PET, RIB, CROW, HI, BYE
C
C               TAU(N,NT,NTAITR), RHO(N,NT,NRHITR), A(N, max. # iters),
      dimension TAU(*),           RHO(*),           A(*),
C
C               RHOWT(N,NT,NRWITR), S(N,NT,NSSITR), QHI(N,NT,NQHITR),
     $          RHOWT(*),           S(*),           QHI(*),
C
C               PRAT(N)
     $          PRAT(*)
C
      data TR /.true./
C     !EJECT
C
      call HI ('LAZICA')
C     !BEG
      if(NT.gt.0) then
        do 105 I = 1,NT
          call PET      (I)
          if(KLIN.eq.1) then
C
            if(NSSITR.gt.1) then
              call RIB  (N,NT,NSSITR,IU,IL,S,A)
              write (TITLE,100) IU,IL
  100         format('S(',I2,'/',I2,')',7X)
              call CROW (N,NSSITR,0,A,TITLE,NO,KIBI,1,TR,PRAT)
            end if
            if(NTAITR.gt.1) then
              call RIB  (N,NT,NTAITR,IU,IL,TAU,A)
              write (TITLE,101) IU,IL
  101         format('TAU(',I2,'/',I2,')',5X)
              call CROW (N,NTAITR,0,A,TITLE,NO,KIBI,1,TR,PRAT)
            end if
            if(NRHITR.gt.1) then
              call RIB  (N,NT,NRHITR,IU,IL,RHO,A)
              write (TITLE,102) IU,IL
  102         format('RHO(',I2,'/',I2,')',5X)
              call CROW (N,NRHITR,0,A,TITLE,NO,KIBI,1,TR,PRAT)
            end if
            if(NRWITR.gt.1) then
              call RIB  (N,NT,NRWITR,IU,IL,RHOWT,A)
              write (TITLE,103) IU,IL
  103         format('RHOWT(',I2,'/',I2,')',3X)
              call CROW (N,NRWITR,0,A,TITLE,NO,KIBI,1,TR,PRAT)
            end if
            if(NQHITR.gt.1) then
              call RIB  (N,NT,NQHITR,IU,IL,QHI,A)
              write (TITLE,104) IU,IL
  104         format('CHI(',I2,'/',I2,')',5X)
              call CROW (N,NQHITR,0,A,TITLE,NO,KIBI,1,TR,PRAT)
            end if
C
          end if
  105   continue
      end if
C     !END
      call BYE ('LAZICA')
C
      return
      end
