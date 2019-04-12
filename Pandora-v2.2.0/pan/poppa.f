      subroutine POPPA
     $(KAPPA,XLM,TE,POPN,BD,IPOP,H,LIMD,KILROY,N,ORES,OREM,KISLV)
C
C     Rudolf Loeser, 1973 Jun 25
C---- Computes bound-free opacity contributions, for population ions.
C
C     Hydrogen (IPOP=1) is treated as a special case.
C
C     Contributions are returned as two parts:
C        ORES: reserved contribution (i.e. level KISLV),
C        OREM: sum of remaining ones.
C
C     (This is version 2 of POPPA.)
C     !DASH
      save
C     !DASH
      real*8 BD, H, OREM, ORES, POPN, TE, XLM
      integer IPOP, KAPPA, KISLV, LEVEL, LIMD, N
      logical KILROY
C     !DASH
      external  HONEY, GINSENG, EVAN, HI, BYE
      intrinsic max
C
C               ORES(N), OREM(N), H(LIMD), POPN(N,LIMD), BD(N,LIMD),
      dimension ORES(*), OREM(*), H(*),    POPN(*),      BD(*),
C
C               TE(N)
     $          TE(*)
C
      call HI ('POPPA')
C     !BEG
      LEVEL = max(KISLV,1)
C
      if(IPOP.eq.1) then
        call GINSENG (XLM, TE, POPN, BD, LEVEL, N, ORES, OREM)
      else
C
        if(KILROY) then
          KILROY = .false.
          call HONEY (IPOP, H, LIMD, XLM)
        end if
        call EVAN    (IPOP, H, LIMD, XLM, TE, POPN, BD, LEVEL, N,
     $                ORES, OREM)
      end if
C     !END
      call BYE ('POPPA')
C
      return
      end
