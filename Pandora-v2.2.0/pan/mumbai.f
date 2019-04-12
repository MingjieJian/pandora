      subroutine MUMBAI
     $(K,L,BXI,KBX,TE,V,AMASS,CORES,LDL,LDLMX,DDL,N,NMAX,TAB)
C
C     Rudolf Loeser, 2007 Feb 01
C---- Sets up a table of wavelengths (Angstroms) to capture
C     lines K - L of the background-contributing lines of a
C     given ion; in the special case of the presence of
C     blended lines.
C
C     (See also WOMBAT.)
C     !DASH
      save
C     !DASH
      real*8 AMASS, BXI, CDW, CORES, DDL, ONE, TAB, TE, V, XI, XIF
      integer I, J, K, KBX, KF, KK, L, LDL, LDLMX, N, NDWM, NMAX
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- NOTHER      as of 2004 Jun 25
      integer     NIAUGM
      parameter   (NIAUGM=3000)
C     (Be sure to recompile all users when changing NIAUGM ! )
C     Upper limit for total profile data points for coincident
C     background lines.
C     .
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(128),NDWM )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external HAPAX, VORTEX, MOLE, HALT, HI, BYE
C
C               LDL(LDLMX), BXI(KBX), TE(N), V(N), TAB(NMAX), CORES(*),
      dimension LDL(LDLMX), BXI(*),   TE(*), V(*), TAB(*),    CORES(*),
C
C               DDL(LDLMX,*)
     $          DDL(LDLMX,*)
C
      dimension XI(NIAUGM), XIF(NIAUGM)
C
      call HI ('MUMBAI')
C     !BEG
      do 102 J = K,L
C
C----   Set up symmetric full XI-table to be used: XI of length KK
        if(LDL(J).le.1) then
          KK = 2*KBX-1
          call HAPAX  (BXI, KBX, BXI, KBX, XI, KK, 1)
        else
          KF = 2*KBX-1
          call HAPAX  (BXI, KBX, BXI, KBX, XIF, KF, 1)
          call VORTEX (TE, V, AMASS, CORES(J), LDL(J), DDL(1,J),
     $                 XIF, KF, XI, KK)
        end if
C
C----   Make sure TAB is big enough
        if((N+KK).gt.NMAX) then
          write (MSSLIN(1), 100) J, KK, N, NMAX
  100     format('J =',I3,', KK =',I10,', N =',I10,'; and NMAX =',
     $           I12,', which is too small.')
          call HALT   ('MUMBAI', 1)
        end if
C
C----   Add points for J'th line into TAB
        call MOLE     (TE, V, NDWM, AMASS, CORES(J), CDW)
        do 101 I = 1,KK
          N = N+1
          TAB(N) = CORES(J)+CDW*XI(I)
  101   continue
C
  102 continue
C     !END
      call BYE ('MUMBAI')
C
      return
      end
