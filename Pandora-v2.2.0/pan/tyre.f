      subroutine TYRE
     $(N,NMAX,KMAX,Z,DL,XJNU,JU,JL,KK,NN,NT)
C
C     Rudolf Loeser, 1980 Jun 12
C---- Rewrites restart Jnu values.
C     !DASH
      save
C     !DASH
      real*8 DL, XJNU, Z
      integer I, JL, JU, KK, KMAX, N, NMAX, NN, NT
      logical KILROY
C     !DASH
      external BASALT, HI, BYE
C
C               XJNU(NMAX,KMAX,NONC), DL(KMAX,NONC), JU(NONC), Z(NMAX),
      dimension XJNU(NMAX,KMAX,*),    DL(KMAX,*),    JU(*),    Z(*),
C
C               JL(NONC), KK(NONC), NN(NONC)
     $          JL(*),    KK(*),    NN(*)
C
      call HI ('TYRE')
C     !BEG
      KILROY = .true.
C
      do 100 I = 1,NT
        call BASALT (KILROY,JU(I),JL(I),N,Z,KK(I),DL(1,I),XJNU(1,1,I))
        KILROY = .false.
  100 continue
C     !END
      call BYE ('TYRE')
C
      return
      end
