      subroutine NANCY
     $(J,TE,DENS,CMCI,CACI,XNE,XNU,XNUC,NPQ,LRQ,NLE,TER,NTE,CII,
     $ DUMP,CK)
C
C     Rudolf Loeser, 1974 Jun 74
C---- Computes collisional ionization rate.
C     (This is version 2 of NANCY.)
C     !DASH
      save
C     !DASH
      real*8 CACI, CI, CII, CK, CMCI, DENS, DNUJ, HNUKT, SE, TE, TER,
     $       XNE, XNU, XNUC
      integer J, LRQ, NLE, NPQ, NTE
      logical DUMP
C     !DASH
      external CHINON, PROD, HI, BYE
C
C               CII(NTE,NSL), TER(NTE), CMCI(NSL), CACI(NSL), XNU(NSL),
      dimension CII(*),       TER(*),   CMCI(*),   CACI(*),   XNU(*),
C
C               XNUC(NSL), NPQ(NSL), LRQ(NSL), NLE(NSL)
     $          XNUC(*),   NPQ(*),   LRQ(*),   NLE(*)
C
      call HI ('NANCY')
C     !BEG
      call CHINON (J, NTE, TER, CII, TE, DENS, XNU, XNUC, NPQ, LRQ,
     $             NLE, CMCI, CACI, DUMP, CI)
C
      DNUJ = XNUC(J)-XNU(J)
      call PROD   (TE, DNUJ, 1, HNUKT, SE)
C
      CK = CI*XNE*SE
C     !END
      call BYE ('NANCY')
C
      return
      end
