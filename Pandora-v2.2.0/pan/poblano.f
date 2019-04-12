      subroutine POBLANO
     $(N,XR,TE,XNC,DRLIMI)
C
C     Rudolf Loeser, 2003 Feb 25
C     (reconstructed 2005 Apr 25)
C---- Computes depth-dependent DR-limit for H Ly lines,
C     for regular PRD calculations.
C     (Note: subroutine DRECK takes care that XR = -1 occurs only
C     for such lines.)
C     (This is version 2 of POBLANO.)
C     !DASH
      save
C     !DASH
      real*8 A, CNC, CTE, DRLIMI, ONE, SA, TE, XLMD2, XLMD3, XNC, XR, Z
      integer I, IU, JXNCS, N
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
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
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
      equivalence (RZQ(164),XLMD2)
      equivalence (RZQ(154),XLMD3)
      equivalence (KZQ(189),JXNCS)
C     !DASH
      external KERBAU, BUREKA, DIVIDE, SET1, HI, BYE
C
C               TE(N), XNC(N), DRLIMI(N)
      dimension TE(*), XNC(*), DRLIMI(*)
C
      data CTE,CNC /0.D0, 0.D0/
C
      call HI ('POBLANO')
C     !BEG
      if(XR.eq.(-ONE)) then
        if(IU.eq.2) then
          call SET1       (DRLIMI, N, XLMD2)
        else if(IU.eq.3) then
          call SET1       (DRLIMI, N, XLMD3)
        else
C
          if(JXNCS.le.0) then
            call KERBAU   (IU, 1, CTE, CNC, A )
            call BUREKA   (IU,    CTE, CNC, SA)
            call DIVIDE   (SA, (A+SA), Z)
            call SET1     (DRLIMI, N, Z)
          else
C
            do 100 I = 1,N
              call KERBAU (IU, 1, TE(I), XNC(I), A )
              call BUREKA (IU,    TE(I), XNC(I), SA)
              call DIVIDE (SA, (A+SA), DRLIMI(I))
  100       continue
          end if
C
        end if
      end if
C     !END
      call BYE ('POBLANO')
C
      return
      end
