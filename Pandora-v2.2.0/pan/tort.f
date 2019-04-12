      subroutine TORT
     $(XM,XR,SM,K,L,N,NL,NLM,GMI,CIJ,PIJ,YBRIJ,SAIJ,YIJ,Z,ZM11,
     $ X,IX,DMPI)
C
C     Rudolf Loeser, 2003 Nov 19
C---- Produces the necessary modifications of GLASS's M and R,
C     and computes ZM11 (as a by-product), at a given depth,
C     for the VAMOS method.
C
C     See also TROT.
C     !DASH
      save
C     !DASH
      real*8 CIJ, GMI, PIJ, SAIJ, SM, SR, X, XM, XR, YBRIJ, YIJ, Z,
     $       ZM11
      integer IX, J, K, KM, L, LM, N, NL, NLM
      logical DMPI
C     !DASH
      external GLASS, REWHIPS, MOVED, NEGATE, SWAPD, GOSSIP, HI, BYE
C
      dimension X(*), IX(*)
C
C               XM(NLM,NLM), XR(NLM), SM(NLM), SAIJ(NL,NL), PIJ(NL,NL),
      dimension XM(NLM,*),   XR(*),   SM(*),   SAIJ(*),     PIJ(*),
C
C               CIJ(NL,NL), GMI(NL), YIJ(NL,NL), YBRIJ(NL,NL), Z(NL,NL)
     $          CIJ(*),     GMI(*),  YIJ(*),     YBRIJ(*),     Z(*)
C     !EJECT
C
      call HI ('TORT')
C     !BEG
      LM = L-1
      KM = K-1
C---- Get Z, XM, XR, SM, SR, and ZM11
      call GLASS     (K, L, NL, PIJ, CIJ, GMI, SAIJ, YIJ, YBRIJ, X, IX,
     $                Z, XM, XR, SM, SR, ZM11)
C
      if(DMPI) then
        call REWHIPS (NL, PIJ, CIJ, GMI, YBRIJ, SAIJ, YIJ, Z,
     $                NLM, XM, XR, SM, SR, ZM11)
      end if
C
      if(LM.gt.0) then
C----   Use SM and SR
        XR(LM) = SR
        call MOVED   (SM, 1, NLM, XM(LM,1), NLM, NLM)
C----   Do column interchanges involving R
        call SWAPD   (XM(1,LM), NLM, XR, NLM)
        call NEGATE  (XM(1,LM), NLM)
        call NEGATE  (XR, NLM)
      end if
C
      if(KM.gt.0) then
C----   Interchange row 1 and row K-1
        do 100 J = 1,NLM
          call SWAPD (XM(1,J), 1, XM(KM,J), 1)
  100   continue
C----   Interchange column 1 and column K-1
        call SWAPD   (XM(1,1), NLM, XM(1,KM), NLM)
C----   Also perform an interchange in R
        call SWAPD   (XR(1), 1, XR(KM), 1)
      end if
C
      if(DMPI) then
        call GOSSIP  (NLM, XM, XR)
      end if
C     !END
      call BYE ('TORT')
C
      return
      end
