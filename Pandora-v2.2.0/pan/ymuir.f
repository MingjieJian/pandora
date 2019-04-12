      subroutine YMUIR
     $(W,IW, CORE,DL,K, DP,DW,XNE,VXP,N, DDL,FDDL,CDL,LDL, MPROM, PHI)
C
C     Rudolf Loeser, 1981 Sep 02
C---- Drives PROFILE to compute absorption profile, mith MUSE = 1
C     (so that U will be computed normally).
C     (See also YMIR.)
C     !DASH
      save
C     !DASH
      real*8 CDL, CORE, DDL, DL, DP, DW, FDDL, PHI, VXP, W, XNE
      integer IAA, IDV, IN, IPC, IS, IUU, IW, K, LDL, MOX, MPROM, MUSE,
     $        N
C     !DASH
      external ISAURA, PROFILE, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               DL(K), DP(N,LDL), VXP(N), DDL(LDL), CDL(LDL), PHI(N,K),
      dimension DL(*), DP(*),     VXP(*), DDL(*),   CDL(*),   PHI(*),
C
C               DW(N), XNE(N), FDDL(N)
     $          DW(*), XNE(*), FDDL(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IDV   ),(IN( 2),IAA   ),(IN( 3),IUU   ),(IN( 4),IPC   )
C
      data MUSE /1/
C
      call HI ('YMUIR')
C     !BEG
C     (Get, and allocate, W allotment)
      call ISAURA  (IN, IS, MOX, 'YMUIR',N)
C
      call PROFILE (CORE, DDL, FDDL, CDL, LDL, DL, K, MPROM, MUSE,
     $              XNE, DP, DW, VXP, N, PHI, W(IDV), W(IAA), W(IUU),
     $              W(IPC), W, IW)
C
C     (Give back W allotment)
      call WGIVE   (W, 'YMUIR')
C     !END
      call BYE ('YMUIR')
C
      return
      end
