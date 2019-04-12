      subroutine AGNES
     $(M,INOUT,I4DEQ,Z,F,G,R,S,A,B,C,D,DEL,FB,GB,RB,SB,DELB,Y,CHK,
     $ W,IW,DUMP)
C
C     Rudolf Loeser, 1997 Aug 27
C---- Solves the original four-diagonal form of the
C     diffusion equations for y.
C     (This is version 2 of AGNES.)
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CHK, CRIT, D, DEL, DELB, F, FB, G, GB, R, RB, S,
     $       SB, W, Y, Z
      integer I4DEQ, INOUT, IW, M
      logical DUMP
C     !DASH
      external HESSON, PAW, MOVE1, REVERSD, HUMBLY, DUMBLY, HI, BYE
C
      dimension W(*), IW(*)
C
C               Z(N), F(N), G(N), R(N), S(N), A(N), B(N), C(N), DEL(N),
      dimension Z(*), F(*), G(*), R(*), S(*), A(*), B(*), C(*), DEL(*),
C
C               CHK(N), FB(N), GB(N), RB(N), SB(N), DELB(N), Y(N),
     $          CHK(*), FB(*), GB(*), RB(*), SB(*), DELB(*), Y(*),
C
C               D(N)
     $          D(*)
C
      data CRIT /1.D-7/
C     !EJECT
C
      call HI ('AGNES')
C     !BEG
C---- Compute DEL
      call HESSON     (M, Z, DEL, CRIT, 'AGNES')
C
      if(INOUT.eq.1) then
C----   Calculate inwards
C
        call PAW      (M, I4DEQ, DEL, F, G, R, S, A, B, C, D, Y,
     $                 CHK, W, IW)
C
        if(DUMP) then
          call HUMBLY (M, F, G, R, S, DEL, A, B, C, D, CHK)
        end if
C
      else if(INOUT.eq.2) then
C----   Calculate outwards (using reversed tables)
        call MOVE1    (DEL, M, DELB)
        call REVERSD  (DELB(2), 1, M-1)
C
        call MOVE1    (F, M, FB)
        call REVERSD  (FB, 1, M)
C
        call MOVE1    (G, M, GB)
        call REVERSD  (GB, 1, M)
C
        call MOVE1    (R, M, RB)
        call REVERSD  (RB, 1, M)
C
        call MOVE1    (S, M, SB)
        call REVERSD  (SB, 1, M)
C
        call PAW      (M, I4DEQ, DELB, FB, GB, RB, SB, A, B, C, D, Y,
     $                 CHK, W, IW)
C
C       (Need to reverse the result)
        call REVERSD  (Y, 1, M)
C
        if(DUMP) then
          call DUMBLY (M, FB, GB, RB, SB, DELB, A, B, C, D, CHK)
        end if
      end if
C     !END
      call BYE ('AGNES')
C
      return
      end
