      subroutine FASAN
     $(IU,IL,WVL,DDL,FDDL,DL,K,DP,DW,V,XNE,N,DV,A,U,MUSE,PHI,LU,PD,
     $ SAVE,VEC,IPNT)
C
C     Rudolf Loeser, 1991 Dec 16
C---- Computes values of the absorption profile for Hydrogen, by
C     convolving a Voigt profile and a Stark profile,
C     for the current component line.
C
C     The results return in PHI.
C
C     Voigt profile intermediates return in DV, A and UU.
C     PD and SAVE are scratch storage for the Stark calculation.
C     If LD > 0, the detailed output from the Stark calculation is
C     written to unit LD.
C     (See also KARAKUL.)
C     (This is version 3 of FASAN.)
C     !DASH
      save
C     !DASH
      real*8 A, DDL, DDLU, DL, DP, DV, DW, FDDL, OFF, ONE, PD, PHI, R,
     $       SAVE, U, V, VEC, WVL, XNE, ZERO
      integer I, IHSKM, IHSSM, IL, IPNT, IU, J, K, LU, MUSE, N
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(126),IHSKM)
      equivalence (KZQ(127),IHSSM)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external  EPSOM, DIVIDE, FESCUE, FISON, GRIMACE, HI, BYE
      intrinsic abs
C
C               DL(K), DP(N), DW(N), V(N), XNE(N), DV(N), A(N), U(N,K),
      dimension DL(*), DP(*), DW(*), V(*), XNE(*), DV(*), A(*), U(*),
C
C               PHI(N,K), PD(9,KMAX), SAVE(NMAX,4), VEC(NMAX), FDDL(N),
     $          PHI(N,*), PD(*),      SAVE(*),      VEC(*),    FDDL(*),
C
C               IPNT(NMAX)
     $          IPNT(*)
C
      call HI ('FASAN')
C     !BEG
      call EPSOM       (WVL, V, N, DV)
C
      do 101 I = 1,N
        call DIVIDE    (ONE, abs(DW(I)), R)
        DDLU = DDL*FDDL(I)
        OFF  = DV(I)-DDLU
        A(I) = R*DP(I)
C
        do 100 J = 1,K
          call FESCUE  (LU, IU, IL, DL(J), MUSE, DP(I), DW(I), V(I),
     $                  XNE(I), DV(I), A(I), WVL, DDLU, R)
          call FISON   (DL(J), A(I), XNE(I), IU, IL, R, OFF, LU, MUSE,
     $                  PHI(I,J), PD, IHSKM, SAVE, VEC, IPNT, IHSSM)
          call GRIMACE (I, N, J, K, PHI(I,J), DL(J), A(I), XNE(I),
     $                  IU, IL, R, OFF)
  100   continue
  101 continue
C     !END
      call BYE ('FASAN')
C
      return
      end
