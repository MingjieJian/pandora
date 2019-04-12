      subroutine SHIVER
     $(KK,XK,GK,XNUK,NCR,XLCR,XICR,XL,GL,VL,U,EMUL,BDI,N,NL,KOLEV,TNUL,
     $ F1,RNDT,DNRT,KOOL,DNRTC,XNU,CPK,EXT)
C     Rudolf Loeser, 1975 Jun 11
C---- Computes RNDT, DNRT, and DNRTC, for FERN.
C     !DASH
      save
C     !DASH
      real*8 BDI, CA, CON20, CON7, CPK, DNRT, DNRTC, EMUL, EXT, F1,
     $       FRQUNT, GK, GL, ONE, PI, RNDT, T, TC, TD, TNUL, TR, TWO, U,
     $       VL, XICR, XK, XL, XLCR, XNU, XNUK, dummy
      integer I, KK, KOLEV, L, N, NCR, NL
      logical KOOL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 1),PI    )
      equivalence (TUNI( 3),FRQUNT)
C     !DASH
C     !EJECT
      external EXCEL, DIVIDE, SQUEEK, ZERO1, RIGEL, RAGE, PUREE, HI, BYE
C
C               XL(NCR), GL(NCR), XICR(NCR), F1(N), RNDT(N), VL(N,NCR),
      dimension XL(*),   GL(*),   XICR(*),   F1(*), RNDT(*), VL(N,*),
C
C               DNRT(N), XNU(NSL), XK(KKX), TNUL(N,NCR), U(N), GK(KKX),
     $          DNRT(*), XNU(*),   XK(*),   TNUL(N,*),   U(*), GK(*),
C
C               XLCR(NCR), BDI(N,NL), DNRTC(N), EMUL(N,NCR), EXT(N)
     $          XLCR(*),   BDI(*),    DNRTC(*), EMUL(*),     EXT(*)
C
      call HI ('SHIVER')
C     !BEG
C---- Compute XL, GL and VL
      call EXCEL    (KK, XK, GK, XNUK, NCR, XLCR, XL, GL)
      call PUREE    (U, N, XL, NCR, EMUL)
      call SQUEEK   (NCR, EMUL, BDI, N, NL, VL, KOLEV)
C
      call ZERO1    (RNDT, N)
      call ZERO1    (DNRT, N)
      if(KOOL) then
        call ZERO1  (DNRTC, N)
      end if
C
      do 101 L = 1,NCR
        call DIVIDE ((GL(L)*XICR(L)), (XL(L)**3), TC)
        call DIVIDE (TC, XL(L), T)
        call RAGE   (TNUL(1,L), N, EXT)
        do 100 I = 1,N
          DNRT(I) = DNRT(I)+EXT(I)*T
          if(KOOL) then
            DNRTC(I) = DNRTC(I)+EXT(I)*TC
          end if
          RNDT(I) = RNDT(I)+EXT(I)*T*VL(I,L)
  100   continue
  101 continue
C
      call RIGEL    (7,  CON7)
      call RIGEL    (20, CON20)
      T   = TWO*XNUK*FRQUNT
      TC  = (TWO*PI*CPK*(XNUK-XNU(KOLEV)))/XNUK
      CA  = CON7*(XNUK**3)
      call DIVIDE   (ONE, (T*CA), TR)
      call DIVIDE   ((CON20*CPK), T, TD)
      do 102 I = 1,N
        DNRT(I) = TD*DNRT(I)
        if(KOOL) then
          DNRTC(I) = TC*DNRTC(I)
        end if
        call DIVIDE ((TR*RNDT(I)), F1(I), RNDT(I))
  102 continue
C     !END
      call BYE ('SHIVER')
C
      return
      end
