      subroutine WOMBAT
     $(K,L,BXI,KBX,TE,V,AMASS,CORES,NMAX,TAB,N)
C
C     Rudolf Loeser, 2004 Aug 06
C---- Sets up a table of wavelengths (Angstroms) to capture
C     lines K - L of the background-contributing lines of a
C     given ion.
C     (This is version 5 of WOMBAT.)
C
C     (See also MUMBAI.)
C     !DASH
      save
C     !DASH
      real*8 AMASS, BXI, CDW, CORES, ONE, TAB, TE, V
      integer I, INC, J, K, KBX, L, N, NDWM, NMAX
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
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
      external MOLE, HALT, HI, BYE
C
C               BXI(KBX), TE(N), V(N), TAB(NMAX), CORES(*)
      dimension BXI(*),   TE(*), V(*), TAB(*),    CORES(*)
C
      call HI ('WOMBAT')
C     !BEG
      INC = 2*KBX-1
C
      do 102 J = K,L
C
        if((N+INC).gt.NMAX) then
          write (MSSLIN(1), 100) J, INC, N, NMAX
  100     format('J =',I3,', INC =',I10,', N =',I10,'; and NMAX =',
     $           I12,', which is too small.')
          call HALT ('WOMBAT', 1)
        end if
C
        call MOLE   (TE, V, NDWM, AMASS, CORES(J), CDW)
        N = N+1
        TAB(N) =   CORES(J)
        do 101 I = 2,KBX
          N = N+1
          TAB(N) = CORES(J)+CDW*BXI(I)
          N = N+1
          TAB(N) = CORES(J)-CDW*BXI(I)
  101   continue
C
  102 continue
C     !END
      call BYE ('WOMBAT')
C
      return
      end
