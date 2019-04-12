      subroutine IGNIS
     $(M,INOUT,I4DEQ,Z,F,G,R,S,A,B,C,D,DEL,FB,GB,RB,SB,DELB,ZETA,ETA,
     $ SIGMA,RHO,Y,CHK,W,IW,DUMP)
C
C     Rudolf Loeser, 1997 Oct 22
C---- Solves the alternate four-diagonal form of the diffusion equations
C     for y.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CHK, D, DEL, DELB, ETA, F, FB, G, GB, ONE, R, RB,
     $       RHO, S, SB, SIGMA, W, Y, Z, ZETA
      integer I4DEQ, INOUT, IW, M
      logical DUMP
      character LABEL*38
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external OMEGA, ARRDIV, MOVE1, REVERSD, NUMBLY, RUMBLY, ARRMUL,
     $         PAW, SET1, HI, BYE
C
      dimension W(*), IW(*)
C
C               Z(N), F(N), G(N), R(N), S(N), A(N), B(N), C(N), DEL(N),
      dimension Z(*), F(*), G(*), R(*), S(*), A(*), B(*), C(*), DEL(*),
C
C               SIGMA(N), FB(N), GB(N), RB(N), SB(N), DELB(N), ZETA(N),
     $          SIGMA(*), FB(*), GB(*), RB(*), SB(*), DELB(*), ZETA(*),
C
C               D(N), Y(N), ETA(N), RHO(N), CHK(N)
     $          D(*), Y(*), ETA(*), RHO(*), CHK(*)
C
      data LABEL /'Diffusion: zeta = integral of r over Z'/
C     !EJECT
C
      call HI ('IGNIS')
C     !BEG
C---- Compute ZETA and DEL (= zeta differences)
      call OMEGA      (M, Z, R, ZETA, DEL, LABEL, DUMP, W)
C---- Compute ETA and SIGMA, and set up RHO
      call ARRMUL     (R, F, ETA, M)
      call ARRDIV     (S, R, SIGMA, M)
      call SET1       (RHO, M, ONE)
C
      if(INOUT.eq.1) then
C----   Calculate inwards
C
        call PAW      (M, I4DEQ, DEL, ETA, G, RHO, SIGMA,
     $                 A, B, C, D, Y, CHK, W, IW)
C
        if(DUMP) then
          call NUMBLY (M, ETA, G, RHO, SIGMA, DEL, A, B, C, D, ZETA,
     $                 CHK)
        end if
C
      else if(INOUT.eq.2) then
C----   Calculate outwards (using reversed tables)
        call MOVE1    (DEL, M,DELB)
        call REVERSD  (DELB(2), 1, (M-1))
C
        call MOVE1    (ETA, M, FB)
        call REVERSD  (FB, 1, M)
C
        call MOVE1    (G, M, GB)
        call REVERSD  (GB, 1, M)
C
        call MOVE1    (RHO, M, RB)
        call REVERSD  (RB, 1, M)
C
        call MOVE1    (SIGMA, M, SB)
        call REVERSD  (SB, 1, M)
C
C                                      eta       rho   sigma
        call PAW      (M, I4DEQ, DELB, FB  , GB, RB  , SB    ,
     $                 A, B, C, D, Y, CHK, W, IW)
C
C       (Need to reverse the result)
        call REVERSD  (Y, 1, M)
C
        if(DUMP) then
          call RUMBLY (M, FB, GB, RB, SB, DELB, A, B, C, D, ZETA, CHK)
        end if
      end if
C     !END
      call BYE ('IGNIS')
C
      return
      end
