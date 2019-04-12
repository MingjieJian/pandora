      subroutine MILAN
     $(X,IX,W,IW,N,NL,NT,MRHO,CRIT,ITMX,ALF,BATA,RBDR,S,SSTAR,RHOJ,
     $ YBAR,RHOS,CWJ,RHOX,ITUS,CWU,KNW,IMG)
C
C     Rudolf Loeser, 1996 Apr 05
C---- Updates RHOJ iteratively, computes S* (and may update MRHO).
C     !DASH
      save
C     !DASH
      real*8 ALF, BATA, CRIT, CWJ, CWS, CWU, EDIT, RBDR, RHOJ, RHOS,
     $       RHOX, S, SSTAR, W, X, YBAR, dummy
      integer IFUDGE, ILMIT, IMG, ITMX, ITS, ITUS, IW, IX, KNW, LIMIT,
     $        MRHO, N, NL, NNT, NT, jummy
      logical CONV, SKIP
C     !DASH
      external MOVE1, CONVERD, PADUA, VETCH, VIOLET, VITALE, SERLO,
     $         HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               YBAR(N,NT), ALF(MUL), BATA(N,MUL), RBDR(N,NL), S(N,NT),
      dimension YBAR(*),    ALF(*),   BATA(*),     RBDR(*),    S(*),
C
C               RHOS(N,NT), RHOJ(N,NT), IMG(N), RHOX(N,NT), SSTAR(N,NT)
     $          RHOS(*),    RHOJ(*),    IMG(*), RHOX(*),    SSTAR(*)
C
      parameter (ILMIT=8)
      dimension CWS(ILMIT), ITS(ILMIT)
C
      data LIMIT       /ILMIT/
      data IFUDGE,EDIT /0, 0.D0/
C     !EJECT
C
      call HI ('MILAN')
C     !BEG
      NNT = N*NT
      CWU = CWJ
      KNW = 1
      CWS(KNW) = CWU
      ITS(KNW) = 0
      call MOVE1      (RHOS, NNT, RHOX)
C
      do 100 ITUS = 1,ITMX
        call VETCH    (X, IX, W, IW, RHOX, dummy, YBAR, RBDR, IMG,
     $                 IFUDGE, 0, EDIT, ITUS)
C
        call PADUA    (EDIT, ITUS, CWU, KNW, SKIP, LIMIT, CWS, ITS)
        if(.not.SKIP) then
          call VITALE (ALF, BATA, RBDR, S, SSTAR, N, NL, NT, MRHO)
        end if
C
        call VIOLET   (SSTAR, S, RHOS, CWU, N, NT, RHOJ)
        call CONVERD  (RHOX, 1, NNT, RHOJ, 1, NNT, CRIT, dummy, jummy,
     $                 CONV)
        if(CONV) then
          goto 101
        end if
        call MOVE1    (RHOJ, NNT, RHOX)
  100 continue
C
      ITUS = ITMX
      if(KNW.gt.1) then
        call SERLO    (CWS, ITS, KNW)
      end if
C
  101 continue
C     !END
      call BYE ('MILAN')
C
      return
      end
