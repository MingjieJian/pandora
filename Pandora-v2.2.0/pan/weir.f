      subroutine WEIR
     $(W,J,K,M,NB,XCOL,NCL,NDW,TE,V,SCOL,WAVES,JIND,KIND,NIND,MIND,N,
     $ KODE)
C
C     Rudolf Loeser, 1987 Nov 17
C---- Makes an expanded table of CO wavelengths.
C     !DASH
      save
C     !DASH
      real*8 HALF, ONE, SCOL, TE, V, W, WAVES, WC, WLO, WUP, XCOL
      integer I, ISO, J, JIND, K, KIND, KODE, L, M, MIND, N, NB, NBM,
     $        NCL, NDW, NIND
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external MULKA, HI, BYE
C
C               NMX = 2*NCL+2
C
C               WAVES(NMX), K(NB), XCOL(NCL), SCOL(NCL,2), TE(N), V(N),
      dimension WAVES(*),   K(*),  XCOL(*),   SCOL(NCL,*), TE(*), V(*),
C
C               JIND(NMX), KIND(NMX), J(NB), W(NB), M(NB), NIND(NMX),
     $          JIND(*),   KIND(*),   J(*),  W(*),  M(*),  NIND(*),
C
C               MIND(NMX)
     $          MIND(*)
C
      call HI ('WEIR')
C     !BEG
      NBM = NB-1
C---- Compute wavelengths offsets
      call MULKA (XCOL, NCL, SCOL, NDW, TE, V)
C
C---- Use first point
      N = 1
      WAVES(N) = W(1)
      if(KODE.eq.1) then
        JIND(N) = J(1)
        KIND(N) = K(1)
        MIND(N) = M(1)
        NIND(N) = 0
      end if
C     !EJECT
C---- Set up expanded middle of table
      do 102 I = 2,NBM
C
        if(K(I).ge.500) then
          ISO = 2
        else
          ISO = 1
        end if
C
        WLO = HALF*(W(I-1)+W(I))
        WUP = HALF*(W(I)+W(I+1))
        if(I.eq.2) then
          WLO = W(I-1)
        end if
        if(I.eq.NBM) then
          WUP = W(I+1)
        end if
C
        do 100 L = NCL,2,-1
          WC = W(I)*(ONE-SCOL(L,ISO))
          if(WC.ge.WLO) then
            N = N+1
            WAVES(N) = WC
            if(KODE.eq.1) then
              JIND(N) = J(I)
              KIND(N) = K(I)
              MIND(N) = M(I)
              NIND(N) = 100-L
            end if
          end if
  100   continue
C
        N = N+1
        WAVES(N) = W(I)
        if(KODE.eq.1) then
          JIND(N) = J(I)
          KIND(N) = K(I)
          MIND(N) = M(I)
          NIND(N) = 0
        end if
C
        do 101 L = 2,NCL,+1
          WC = W(I)*(ONE+SCOL(L,ISO))
          if(WC.le.WUP) then
            N = N+1
            WAVES(N) = WC
            if(KODE.eq.1) then
              JIND(N) = J(I)
              KIND(N) = K(I)
              MIND(N) = M(I)
              NIND(N) = 100+L
            end if
          end if
  101   continue
  102 continue
C     !EJECT
C---- Use last point
      N = N+1
      WAVES(N) = W(NB)
      if(KODE.eq.1) then
        JIND(N) = J(NB)
        KIND(N) = K(NB)
        MIND(N) = M(NB)
        NIND(N) = 0
      end if
C     !END
      call BYE ('WEIR')
C
      return
      end
