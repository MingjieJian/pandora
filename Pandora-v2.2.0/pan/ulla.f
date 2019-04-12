      subroutine ULLA
     $(XND,XNK,HNI,XNP,N,TIT,NT,HND,XNE,TE,ABDEL,LIMP,BDI,BD,IPSW,
     $ BATAL,W)
C
C     Rudolf Loeser, 1970 Feb 02
C---- Puts number density data into Hydrogen population slots,
C     and prints.
C     (See also PETRA.)
C     !DASH
      save
C     !DASH
      real*8 ABDEL, BATAL, BD, BDI, HND, HNI, TE, W, XND, XNE, XNK, XNP,
     $       dummy
      integer IPSW, LC, LIM, LIMP, MO, N, NT
      character TIT*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external UPDOWN, BISHOP, GOGGLE, SERF, HI, BYE
C
      dimension W(*)
C
C               XND(N,NL), XNK(N), HNI(N,LIMP), XNP(N), HND(N), XNE(N),
      dimension XND(*),    XNK(*), HNI(*),      XNP(*), HND(*), XNE(*),
C
C               ABDEL(N), BDI(N,NL), BD(N,LIMP), BATAL(N,max(NL,LIMP)),
     $          ABDEL(*), BDI(*),    BD(*),      BATAL(*),
C
C               TE(N)
     $          TE(*)
C
      call HI ('ULLA')
C     !BEG
C---- Copy data into proper slots: XNP, HNI and BD, and compute LIM
      call UPDOWN (XNK, XND, BDI, XNP, HNI, BD, LIMP, LIM)
C---- Defaults for "missing" number densities in HNI (if needed)
      call BISHOP (N, LIM, HNI, TE, XNE, XNP, LIMP, W)
C---- Defaults for "missing" departure coefficients in BD (if needed)
      call GOGGLE (N, LIM, LIMP, 'ZZZ', TE, dummy, dummy, dummy,
     $             ABDEL, 1, HNI, XNP, BD, LC, 1, BATAL)
C---- Print XNP, HNI and BD, etc.
      call SERF   (MO, LIM, N, HND, XNE, HNI, XNP, 1, TIT, NT, LIMP,
     $             1, IPSW, BD, LC, 1, .true., .true.)
C     !END
      call BYE ('ULLA')
C
      return
      end
