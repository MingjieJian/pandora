      subroutine PEEK
     $(K,DL,N,Z,DWIJ,DPIJ,VX,MPROM,XNE,WVL,DDL,FDDL,IHSSP,CDL,LDL,
     $ IU,IL,NO,W,IW)
C
C     Rudolf Loeser, 1978 Jun 14
C---- Does absorption profile analysis.
C     (This is version 2 of PEEK.)
C     !DASH
      save
C     !DASH
      real*8 CDL, DDL, DL, DPIJ, DWIJ, FDDL, VX, W, WVL, XNE, Z, dummy
      integer IAR, IDPR, IDVR, IDWR, IFDLR, IHSSP, IL, IN, INER, IPHC,
     $        IPHI, IPHK, IPHV, IS, ISTK, IU, IUU, IVXR, IW, IZR, K,
     $        LDL, MOX, MPROM, MUSE, N, NO, NR
C     !DASH
      external MANGO, MOUSE, JERBOA, PROFILE, BANANA, CANARY, EGORI,
     $         JANE, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               DL(K), XNE(N), DWIJ(N), DPIJ(N,LDL), FDDL(N), DDL(LDL),
      dimension DL(*), XNE(*), DWIJ(*), DPIJ(*),     FDDL(*), DDL(*),
C
C               CDL(LDL), Z(N), VX(N)
     $          CDL(*),   Z(*), VX(*)
C
      dimension IN(14)
      equivalence
     $(IN( 1),IZR   ),(IN( 2),IDWR  ),(IN( 3),IDPR  ),(IN( 4),IVXR  ),
     $(IN( 5),IDVR  ),(IN( 6),IAR   ),(IN( 7),IUU   ),(IN( 8),IPHI  ),
     $(IN( 9),IPHC  ),(IN(10),INER  ),(IN(11),IFDLR ),(IN(12),IPHV  ),
     $(IN(13),IPHK  ),(IN(14),ISTK  )
C
      data MUSE /1/
C
      call HI ('PEEK')
C     !BEG
      if(NO.gt.0) then
C       (Get, and allocate, W allotment)
        call MANGO (IN, IS, MOX, 'PEEK')
C
        call MOUSE (N, NR)
C     !EJECT
        if(MPROM.le.1) then
C----     Set up reduced depth tables
          call JANE      (N, Z, DWIJ, DPIJ, VX, XNE, FDDL, LDL, NR,
     $                    W(IZR), W(IDWR), W(IDPR), W(IVXR), W(INER),
     $                    W(IFDLR))
C----     Set up final PHI, and U, tables (Note: Mu=1)
          call PROFILE   (WVL, DDL, W(IFDLR), CDL, LDL, DL, K, MPROM,
     $                    MUSE, W(INER), W(IDPR), W(IDWR), W(IVXR),
     $                    NR, W(IPHI), W(IDVR), W(IAR), W(IUU),
     $                    W(IPHC), W, IW)
          if(MPROM.eq.1) then
C----       Set up Voigt function table
            call PROFILE (WVL, DDL, W(IFDLR), CDL, LDL, DL, K, 0,
     $                    MUSE, dummy, W(IDPR), W(IDWR), W(IVXR),
     $                    NR, W(IPHV), W(IDVR), W(IAR), W(IUU),
     $                    W(IPHK), W, IW)
C----       Set up Stark profile
            call EGORI   (DL, K, W(INER), NR, IU, IL, W(ISTK))
          end if
C----     Print stuff
          call BANANA    (NO, NR, W(IZR), W(IDWR), W(IDPR), W(IVXR),
     $                    W(IDVR), W(IAR), K, DL, W(IPHI), W(IUU), DDL,
     $                    W(IFDLR), IHSSP, CDL, LDL, W(IPHC), W(INER),
     $                    MPROM, W(IPHV), W(IPHK), W(ISTK))
C----     Plot PHI
          call CANARY    (NO, DL, K, W(IPHI), W(IZR), NR)
        else
C----     Error
          call JERBOA    (IU, IL, K, N, NR, MPROM, 'PEEK')
        end if
C
C       (Give back W allotment)
        call WGIVE       (W, 'PEEK')
      end if
C     !END
      call BYE ('PEEK')
C
      return
      end
