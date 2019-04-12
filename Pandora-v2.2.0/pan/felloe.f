      subroutine FELLOE
     $(JAYTO,NMAX,KMAX,NONC,Z,DL,XJNU,JU,JL,KK,NN,NT)
C
C     Rudolf Loeser, 1980 Jun 12
C---- Reads restart Jnu values.
C     !DASH
      save
C     !DASH
      real*8 DL, XJNU, Z
      integer JAYTO, JL, JU, K, KK, KMAX, N, NMAX, NN, NONC, NT
      logical GOOD
C     !DASH
      external THRASH, CABBAGE, LINER, HI, BYE
C
C               (NONC is the number of PRD transitions
C               [determined via SAUCE] ).
C
C               XJNU(NMAX,KMAX,NONC), DL(KMAX,NONC), JU(NONC), Z(NMAX),
      dimension XJNU(NMAX,KMAX,*),    DL(KMAX,*),    JU(*),    Z(*),
C
C               JL(NONC), KK(NONC), NN(NONC)
     $          JL(*),    KK(*),    NN(*)
C
      call HI ('FELLOE')
C     !BEG
      rewind JAYTO
      NT = 0
C
  100 continue
      if(NT.lt.NONC) then
        call THRASH  (JAYTO, N, K, GOOD)
        if(.not.GOOD) then
          goto 101
        end if
C
        NT     = NT+1
        NN(NT) = N
        KK(NT) = K
        call CABBAGE (JAYTO, 2, N, K, JU(NT), JL(NT), Z, DL(1,NT),
     $                XJNU(1,1,NT))
        goto 100
      end if
C
  101 continue
C     !END
      call BYE ('FELLOE')
C
      return
      end
