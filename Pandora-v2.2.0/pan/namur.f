      subroutine NAMUR
     $(N,TE,XNE,CHI,CPR,HND,RAB,ABDEL,INDX,NL,POP,POPK,KODE,IPOP,LIMP,
     $ X,D)
C
C     Rudolf Loeser, 1978 Sep 27
C---- Sets up default level populations for an atom
C     with one stage of ionization.
C     (This is version 2 of NAMUR.)
C     !DASH
      save
C     !DASH
      real*8 ABDEL, CHI, CPR, D, HND, POP, POPK, RAB, TE, X, XNE
      integer INDX, IPOP, KODE, KODI, LIMP, N, NL
C     !DASH
      external NYASA, SUDAN, GAMA, HI, BYE
C
C               RAB(N), XNE(N), CHI(NMT), CPR(N,NMT), HND(N), ABDEL(N),
      dimension RAB(*), XNE(*), CHI(*),   CPR(*),     HND(*), ABDEL(*),
C
C               TE(N), POP(N,LIMP), POPK(N), X(N), D(N)
     $          TE(*), POP(*),      POPK(*), X(*), D(*)
C
      call HI ('NAMUR')
C     !BEG
C---- Set up control switches
      call NYASA (NL,POPK,KODE,KODI)
C---- Compute intermediates
      call SUDAN (INDX,N,TE,XNE,CHI,CPR,HND,RAB,ABDEL,KODI,X,D)
C---- Compute populations
      call GAMA  (N,NL,LIMP,KODE,D,X,TE,IPOP,POPK,POP)
C     !END
      call BYE ('NAMUR')
C
      return
      end
