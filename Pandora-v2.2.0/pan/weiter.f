      subroutine WEITER
     $(RW,R1,R2,WVAR,WCON,N,KODE,MODE,KMSS,TITLE,WEIGHT)
C
C     Rudolf Loeser, 2002 Jan 29
C---- Depth-varying weighting, with optional dump printout.
C
C     "Weight" multiplies R1;      (1-"Weight") multiplies R2;
C
C                 the final result is RW.
C
C     KODE = 0 means: do linear weighting;
C     KODE = 1 means: try logarithmic weighting (else linear).
C
C     MODE = 0 means: WVAR does not exist;
C     MODE = 1 means: WVAR does exist, and should be used.
C
C     (WEIGHT is working storage.)
C     !DASH
      save
C     !DASH
      real*8 R1, R2, RW, WCON, WEIGHT, WVAR
      integer KMSS, KODE, MODE, N
      character TITLE*(*)
C     !DASH
      external SET1, MOVE1, CONMUL, WEITING, HI, BYE
C
C               RW(N), R1(N), R2(N), WVAR(N), WEIGHT(N)
      dimension RW(*), R1(*), R2(*), WVAR(*), WEIGHT(*)
C
      call HI ('WEITER')
C     !BEG
      if(MODE.le.0) then
        call SET1   (WEIGHT,N,WCON)
      else
        call MOVE1  (WVAR,N,WEIGHT)
        call CONMUL (WCON,WEIGHT,N)
      end if
      call WEITING  (R2,R1,RW,N,WEIGHT,KODE,KMSS,TITLE)
C     !END
      call BYE ('WEITER')
C
      return
      end
