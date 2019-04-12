      subroutine QUINCY
     $(NW,A,WAVE,N,XJAYB,TE,RMC,RDC,BDHM,XNHM,CRH,KOOL)
C
C     Rudolf Loeser, 1979 Oct 19
C---- Computes net radiative cooling rate, for H-.
C     !DASH
      save
C     !DASH
      real*8 A, BDHM, CRH, FRQUNT, RDC, RMC, TE, WAVE, XJAYB, XNHM
      integer KCRH, N, NW
      logical KOOL
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(48),KCRH )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 3),FRQUNT)
C     !DASH
      external CONMUL, ARRDIV, ARRSUB, ARRMUL, AMON, HI, BYE
C
C               CRH(N), XNHM(N), BDHM(N), RDC(N), RMC(N), A(NW), TE(N),
      dimension CRH(*), XNHM(*), BDHM(*), RDC(*), RMC(*), A(*),  TE(*),
C
C               XJAYB(N,NW), WAVE(NW)
     $          XJAYB(*),    WAVE(*)
C
      call HI ('QUINCY')
C     !BEG
      KCRH = 0
      if(KOOL) then
        KCRH = 1
        call AMON   (NW, A, WAVE, N, XJAYB, TE, RMC, RDC, KOOL)
        call CONMUL (FRQUNT, RDC, N)
        call CONMUL (FRQUNT, RMC, N)
        call ARRDIV (RDC, BDHM, CRH, N)
        call ARRSUB (CRH, RMC, CRH, N)
        call ARRMUL (CRH, XNHM, CRH, N)
      end if
C     !END
      call BYE ('QUINCY')
C
      return
      end
