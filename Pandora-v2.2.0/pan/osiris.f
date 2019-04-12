      subroutine OSIRIS
     $(NO,NJ,NW,WAVE,A,IND,BDHM,N,XNE,HN1,TE,RM,RD,GP,DEL,EPS,G,YAYB,
     $ XCBL,XNHM,Z,HND,KOOL,CRH,RMC,RDC,ZL,XNEL,HNDL,XNHML,HN1L)
C
C     Rudolf Loeser, 1971 Jun 15
C---- Calculates the H- Departure coefficient.
C     !DASH
      save
C     !DASH
      real*8 A, BDHM, CRH, DEL, EPS, G, GP, HN1, HN1L, HND, HNDL, RD,
     $       RDC, RM, RMC, TE, WAVE, XCBL, XNE, XNEL, XNHM, XNHML, YAYB,
     $       Z, ZL
      integer IND, N, NJ, NO, NW
      logical KOOL, WOOL
C     !DASH
C     !EJECT
      external LUXOR, JUNK, BINNA, AMON, FELIS, QUINCY, HORUS, MELANIE,
     $         RAH, PTAH, HI, BYE
C
C               WAVE(NW), A(NW), IND(NW), BDHM(N), XNE(N), Z(N), GP(N),
      dimension WAVE(*),  A(*),  IND(*),  BDHM(*), XNE(*), Z(*), GP(*),
C
C               HN1(N), DEL(N), RM(N), RD(N), XCBL(Miklen), YAYB(N,NW),
     $          HN1(*), DEL(*), RM(*), RD(*), XCBL(*),      YAYB(*),
C
C               XNHM(N), HND(N), CRH(N), RMC(N), RDC(N), EPS(N), TE(N),
     $          XNHM(*), HND(*), CRH(*), RMC(*), RDC(*), EPS(*), TE(*),
C
C               G(N), ZL(N), XNEL(N), HNDL(N), XNHML(N), HN1L(N)
     $          G(*), ZL(*), XNEL(*), HNDL(*), XNHML(*), HN1L(*)
C
      data WOOL /.false./
C
      call HI ('OSIRIS')
C     !BEG
C---- Write heading
      call LUXOR   (NO)
C---- Get mean intensities (and print ?)
      call BINNA   (NW, WAVE, N, YAYB, XCBL)
      call RAH     (NJ, (NW-1), WAVE, A, IND, N, YAYB)
C---- Compute RM and RD
      call AMON    (NW, A, WAVE, N, YAYB, TE, RM, RD, WOOL)
C---- Compute BDHM
      call PTAH    (N, HN1, XNE, TE, GP, DEL, EPS, G, RM, RD, BDHM,
     $              XNHM)
C---- Compute cooling rate
      call QUINCY  (NW, A, WAVE, N, YAYB, TE, RMC, RDC, BDHM, XNHM,
     $              CRH, KOOL)
C---- Print things
      call HORUS   (NO, N, TE, XNE, HN1, DEL, EPS, GP, RM, RD, G,
     $              BDHM, XNHM, CRH, RMC, RDC, KOOL)
C---- Checksums
      call FELIS   (N, RM, RD, BDHM)
C---- Plot things (including mean intensities ?)
      call MELANIE (N, Z, XNE, HND, XNHM, HN1, ZL, XNEL, HNDL, XNHML,
     $              HN1L, NO)
      call JUNK    (NJ, YAYB, (NW-1), Z, N, ZL)
C     !END
      call BYE ('OSIRIS')
C
      return
      end
