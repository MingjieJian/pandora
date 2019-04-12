      subroutine CALVIN
     $(N,TE,XNE,CHI,CPR,HND,RAB,ABDEL,
     $ INDX1,NL1,POP1,POP1K,KODE1,IPOP1,LIMP1,X1,D1,
     $ INDX2,NL2,POP2,POP2K,KODE2,IPOP2,LIMP2,X2,D2,
     $ INDX3,NL3,POP3,POP3K,KODE3,IPOP3,LIMP3,X3,D3)
C
C     Rudolf Loeser, 2007 Jan 15
C---- Sets up default level populations for an atom
C     with three stages of ionization.
C     !DASH
      save
C     !DASH
      real*8 ABDEL, CHI, CPR, D1, D2, D3, HND, POP1, POP1K, POP2, POP2K,
     $       POP3, POP3K, RAB, TE, X1, X2, X3, XNE
      integer INDX1, INDX2, INDX3, IPOP1, IPOP2, IPOP3, KODE1, KODE2,
     $        KODE3, KODI, LIMP1, LIMP2, LIMP3, N, NL1, NL2, NL3
C     !DASH
      external VCTORIA, PLUMP, GAMA, HI, BYE
C
C               ABDEL(N), XNE(N), CPR(N,NMT), CHI(NMT), HND(N), RAB(N),
      dimension ABDEL(*), XNE(*), CPR(*),     CHI(*),   HND(*), RAB(*),
C
C               POP1(N,LIMP1), POP1K(N), X1(N), D1(N), TE(N),
     $          POP1(*),       POP1K(*), X1(*), D1(*), TE(*),
C
C               POP2(N,LIMP2), POP2K(N), X2(N), D2(N),
     $          POP2(*),       POP2K(*), X2(*), D2(*),
C
C               POP3(N,LIMP3), POP3K(N), X3(N), D3(N)
     $          POP3(*),       POP3K(*), X3(*), D3(*)
C
      call HI ('CALVIN')
C     !BEG
C---- Set up control switches
      call VCTORIA (NL1, POP1K, KODE1, NL2, POP2K, KODE2,
     $              NL3, POP3K, KODE3, KODI)
C---- Compute intermediates
      call PLUMP   (N, TE, XNE, CHI, CPR, HND, RAB, ABDEL,
     $              INDX1, POP1K, POP1, INDX2, POP2K, POP2,
     $              INDX3, POP3K, POP3, KODI, X1, X2, X3, D1, D2, D3)
C---- Compute populations for first stage
      call GAMA    (N, NL1, LIMP1, KODE1, D1, X1, TE, IPOP1, POP1K,
     $              POP1)
C---- Compute populations for second stage
      call GAMA    (N, NL2, LIMP2, KODE2, D2, X2, TE, IPOP2, POP2K,
     $              POP2)
C---- Compute populations for third stage
      call GAMA    (N, NL3, LIMP3, KODE3, D3, X3, TE, IPOP3, POP3K,
     $              POP3)
C     !END
      call BYE ('CALVIN')
C
      return
      end
