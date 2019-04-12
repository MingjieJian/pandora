      subroutine YMIR
     $(W,IW,DP,DW,XNE,N,MPROM,PHI)
C
C     Rudolf Loeser, 1981 Sep 02
C---- Drives PROFILE to compute absorption profile, mith MUSE = 0:
C
C     this means, use U = 0 in the Voigt function; therefore
C     no values of CORE,V,DL and FDDL are needed, and this should
C     be treated as a single, unblended transition (LDL = 0).
C
C     (This is a special version of YMUIR.)
C     !DASH
      save
C     !DASH
      real*8 CDL, DDL, DP, DW, FDDL, PHI, W, XNE, dummy
      integer IAA, IDV, IN, IPC, IS, IUU, IW, K, LDL, MOX, MPROM, MUSE,
     $        N
C     !DASH
      external ISAURA, PROFILE, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               DP(N,LDL), DW(N), XNE(N), PHI(N,K)
      dimension DP(N,*),   DW(*), XNE(*), PHI(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IDV   ),(IN( 2),IAA   ),(IN( 3),IUU   ),(IN( 4),IPC   )
C
      data MUSE /0/
C
      data K,LDL /1, 1/
      data DDL,CDL /0.D0, 1.D0/
C
      call HI ('YMIR')
C     !BEG
C     (Get, and allocate, W allotment)
      call ISAURA  (IN, IS, MOX, 'YMIR', N)
C
      call PROFILE (dummy, DDL, dummy, CDL, LDL, dummy, K, MPROM,
     $              MUSE, XNE, DP(1,1), DW, dummy, N, PHI, W(IDV),
     $              W(IAA), W(IUU), W(IPC), W, IW)
C
C     (Give back W allotment)
      call WGIVE   (W, 'YMIR')
C     !END
      call BYE ('YMIR')
C
      return
      end
