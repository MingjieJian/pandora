      subroutine PETRA
     $(XND,POP,XNK,POPK,N,TIT,NT,HND,XNE,TE,ABDEL,IPOP,LIMP,BDI,BD,
     $ IPSW,BATAL)
C
C     Rudolf Loeser, 1972 Nov 24
C---- Puts number density data into population slots, and prints.
C     (See also ULLA.)
C     !DASH
      save
C     !DASH
      real*8 ABDEL, BATAL, BD, BDI, HND, POP, POPK, TE, XND, XNE, XNK,
     $       dummy
      integer IPOP, IPSW, LC, LIM, LIMP, MO, N, NT
      character TIT*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external UPDOWN, GAGGLE, GOGGLE, SERF, HI, BYE
C
C               XND(N,NL), POP(N,LIMP), XNK(N), POPK(N), HND(N), TE(N),
      dimension XND(*),    POP(*),      XNK(*), POPK(*), HND(*), TE(*),
C
C               BATAL(N,max(NL,LIMP)), BDI(N,NL), BD(N,LIMP), ABDEL(N),
     $          BATAL(*),              BDI(*),    BD(*),      ABDEL(*),
C
C               XNE(N)
     $          XNE(*)
C
      call HI ('PETRA')
C     !BEG
C---- Copy data into proper slots: POPK, POP and BD, and compute LIM
      call UPDOWN (XNK, XND, BDI, POPK, POP, BD, LIMP, LIM)
C---- Defaults for "missing" number densities in POP (if needed)
      call GAGGLE (N, LIM, TE, IPOP, POP, LIMP)
C---- Defaults for "missing" departure coefficients in BD (if needed)
      call GOGGLE (N, LIM, LIMP, 'ZZZ', TE, dummy, dummy, dummy,
     $             ABDEL, IPOP, POP, POPK, BD, LC, 1, BATAL)
C---- Print POPK, POP and BD, etc.
      call SERF   (MO, LIM, N, HND, XNE, POP, POPK, 1, TIT, NT, LIMP,
     $             IPOP, IPSW, BD, LC, 1, .true., .true.)
C     !END
      call BYE ('PETRA')
C
      return
      end
