      subroutine TROT
     $(XM,XR,SM,I,K,L,N,NL,NLM,NSL,MSL,KRJ,GM,XND,NOK,CIJ,PIJ,RHO,
     $ YBR,KIJ,WEIGHT,BDI,Z,ZM11,X,IX,DMPI)
C
C     Rudolf Loeser, 1968 Jan 30
C---- Produces the necessary modifications of GRASS's M and R,
C     and computes ZM11 (as a by-product), for the NOVA method.
C
C     See also TORT.
C     !DASH
      save
C     !DASH
      real*8 BDI, CIJ, GM, PIJ, RHO, SM, SR, WEIGHT, X, XM, XND, XR,
     $       YBR, Z, ZM11
      integer I, IX, J, K, KIJ, KM, KRJ, L, LM, MSL, N, NL, NLM, NSL
      logical DMPI, NOK
C     !DASH
      external GRASS, WHISPER, MOVED, NEGATE, SWAPD, GOSSIP, HI, BYE
C
      dimension X(*), IX(*)
C
C               XM(NLM,NLM), XR(NLM), SM(NLM), YBR(N,NT), PIJ(N,NL**2),
      dimension XM(NLM,*),   XR(*),   SM(*),   YBR(*),    PIJ(*),
C
C               CIJ(N,NL**2), WEIGHT(MUL,NT), GM(N,NSL), KIJ(NL,NL),
     $          CIJ(*),       WEIGHT(*),      GM(*),     KIJ(*),
C
C               RHO(N,NT), XND(N,NL), Z(NL,NL), BDI(N,NL)
     $          RHO(*),    XND(*),    Z(*),     BDI(*)
C     !EJECT
C
      call HI ('TROT')
C     !BEG
      LM = L-1
      KM = K-1
C---- Get Z, XM, XR, SM, SR, and ZM11
      call GRASS     (I, K, L, N, NL, NSL, MSL, KRJ ,PIJ, CIJ, GM,
     $                XND, NOK, BDI, RHO, KIJ, YBR, WEIGHT, X, IX,
     $                Z, XM, XR, SM, SR, ZM11)
C
      if(DMPI) then
        call WHISPER (NL, Z, NLM, XM, XR, SM, SR, ZM11)
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
      call BYE ('TROT')
C
      return
      end
