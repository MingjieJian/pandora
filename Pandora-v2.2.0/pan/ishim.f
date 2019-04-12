      subroutine ISHIM
     $(N,K,SNU,XOBL,H,PHI,DP,DW,XNE,XNEST,MPROM,DDL,FDDL,CDL,LDL,DL,VX,
     $ W,IW)
C
C     Rudolf Loeser, 1985 Jan 23
C---- Supervises calculation of H, and of PHI, for TOBOL.
C     (This is version 2 of ISHIM.)
C     !DASH
      save
C     !DASH
      real*8 CDL, DDL, DL, DP, DW, FDDL, H, PHI, SNU, VX, W, XNE, XNEST,
     $       XOBL, ZERO
      integer IW, K, LDL, MPROM, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external AMGUN, NIMBLE, YMUIR, HI, BYE
C
      dimension W(*), IW(*)
C
C               XOBL(Lodlen), DL(K), PHI(N,K), SNU(N,K), H(N,K), VX(N),
      dimension XOBL(*),      DL(*), PHI(*),   SNU(*),   H(*),   VX(*),
C
C               DP(N,LDL), DDL(LDL), FDDL(N), CDL(LDL), XNE(N), DW(N)
     $          DP(*),     DDL(*),   FDDL(*), CDL(*),   XNE(*), DW(*)
C
      call HI ('ISHIM')
C     !BEG
      call AMGUN  (N,K,XOBL,SNU,H)
      call NIMBLE (XNEST,XNE,FDDL,N)
      call YMUIR  (W,IW,ZERO,DL,K,DP,DW,XNE,VX,N,DDL,FDDL,CDL,LDL,
     $             MPROM,PHI)
C     !END
      call BYE ('ISHIM')
C
      return
      end
