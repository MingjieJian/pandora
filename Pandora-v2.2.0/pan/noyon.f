      subroutine NOYON
     $(N,TE,XNE,CHI,CPR,HND,RAB,ABDEL,
     $ INDX1,NL1,POP1,POP1K,KODE1,IPOP1,LIMP1,X1,D1,
     $ INDX2,NL2,POP2,POP2K,KODE2,IPOP2,LIMP2,X2,D2)
C
C     Rudolf Loeser, 1978 Sep 27
C---- Sets up default level populations for an atom
C     with two stages of ionization.
C     (This is version 2 of NOYON.)
C     !DASH
      save
C     !DASH
      real*8 ABDEL, CHI, CPR, D1, D2, HND, POP1, POP1K, POP2, POP2K,
     $       RAB, TE, X1, X2, XNE
      integer INDX1, INDX2, IPOP1, IPOP2, KODE1, KODE2, KODI, LIMP1,
     $        LIMP2, N, NL1, NL2
C     !DASH
      external NYANZA, PLUME, GAMA, HI, BYE
C
C               ABDEL(N), XNE(N), CPR(N,NMT), CHI(NMT), HND(N), RAB(N),
      dimension ABDEL(*), XNE(*), CPR(*),     CHI(*),   HND(*), RAB(*),
C
C               POP1(N,LIMP1), POP1K(N), X1(N), D1(N), TE(N),
     $          POP1(*),       POP1K(*), X1(*), D1(*), TE(*),
C
C               POP2(N,LIMP2), POP2K(N), X2(N), D2(N)
     $          POP2(*),       POP2K(*), X2(*), D2(*)
C
      call HI ('NOYON')
C     !BEG
C---- Set up control switches
      call NYANZA (NL1, POP1K, KODE1, NL2, POP2K, KODE2, KODI)
C---- Compute intermediates
      call PLUME  (N, TE, XNE, CHI, CPR, HND, RAB, ABDEL,
     $             INDX1, POP1K, POP1, INDX2, POP2K, POP2,
     $             KODI, X1, X2, D1, D2)
C---- Compute populations for first stage
      call GAMA   (N, NL1, LIMP1, KODE1, D1, X1, TE, IPOP1, POP1K, POP1)
C---- Compute populations for second stage
      call GAMA   (N, NL2, LIMP2, KODE2, D2, X2, TE, IPOP2, POP2K, POP2)
C     !END
      call BYE ('NOYON')
C
      return
      end
