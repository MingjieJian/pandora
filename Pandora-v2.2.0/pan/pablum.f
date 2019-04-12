      subroutine PABLUM
     $(KAPPA,XLM,B,TE,BD,IPOP,H,LIMD,KILROY,N,SRES,SREM,KISLV)
C
C     Rudolf Loeser, 1974 May 28
C---- Computes bound-free emission contributions, for population ions.
C
C     Hydrogen (IPOP=1) is treated as a special case.
C
C     Contributions are returned as two parts:
C     SRES: reserved contribution (i.e. level KISLV),
C     SREM: sum of remaining ones.
C
C     Note: to complete the calculation of these terms,
C     SRES must be multiplied by ORES, and SREM by OREM.
C
C     (This is version 3 of PABLUM.)
C     !DASH
      save
C     !DASH
      real*8 B, BD, H, SREM, SRES, TE, XLM
      integer IPOP, KAPPA, KISLV, LEVEL, LIMD, N
      logical KILROY
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external  HONEY, NAOMI, ROSE, HALT, HI, BYE
      intrinsic max
C
C               SRES(N), SREM(N), B(N), TE(N), BD(N,LIMD), H(LIMD)
      dimension SRES(*), SREM(*), B(*), TE(*), BD(*),      H(*)
C
      call HI ('PABLUM')
C     !BEG
      LEVEL = max(KISLV,1)
C
      if(IPOP.eq.1) then
C----   Hydrogen
        if(LEVEL.ne.1) then
          write (MSSLIN(1),100) LEVEL
  100     format('LEVEL =',I12,', but must = 1 for NAOMI.')
          call HALT ('PABLUM', 1)
        end if
C
        call NAOMI   (XLM, TE, B, BD, LEVEL, N, SRES, SREM)
      else
C----   Other ions
        if(KILROY) then
          KILROY = .false.
          call HONEY (IPOP, H, LIMD, XLM)
        end if
        call ROSE    (IPOP, H, LIMD, XLM, TE, B, BD, LEVEL, N,
     $                SRES, SREM)
      end if
C     !END
      call BYE ('PABLUM')
C
      return
      end
