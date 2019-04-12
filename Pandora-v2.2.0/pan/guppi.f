      subroutine GUPPI
     $(NTITER,T5,CQ,Z,OP5,TAU5,G,H,HND,XNE,HNDT,V,C,F)
C
C     Rudolf Loeser, 1979 Sep 28
C---- Print debug HSE data.
C     !DASH
      save
C     !DASH
      real*8 C, CLNH, CQ, F, G, H, HND, HNDT, HTAU, OP5, T5, TAU5, V,
     $       XNE, Z
      integer I, KZERO, LUEO, N, NTITER
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 70),CLNH )
      equivalence (RZQ( 71),HTAU )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(10),KZERO)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, MESHED, MASHED, HI, BYE
C
C               Z(N), OP5(N), TAU5(N), HNDT(N), HND(N), XNE(N), T5(12),
      dimension Z(*), OP5(*), TAU5(*), HNDT(*), HND(*), XNE(*), T5(*),
C
C               H(N), G(N), F(12)
     $          H(*), G(*),  F(*)
C     !EJECT
C
      call HI ('GUPPI')
C     !BEG
      call MESHED ('GUPPI', 2)
      write (LUEO,100) NTITER,KZERO,T5(NTITER),HTAU,CQ,V,C,CLNH,
     $                 F(NTITER)
  100 format(' ','HSE:  TAU5000-iteration',I3,5X,'KZERO',I3/
     $       ' ','TAU5000(Ref)',1PE15.7,3X,'HTAU',E10.2,3X,'Q',E15.7,
     $           3X,'V',E10.2,3X,'C',E15.7,3X,'CLNH',E10.2/
     $       ' ','F(Ref)',E15.7//
     $       ' ',17X,'Z',5X,'KAPPA 5000',7X,'TAU 5000',14X,'G',14X,
     $           'H',12X,'HND',12X,'XNE',11X,'HNDT')
      call LINER  (1, LUEO)
C
      write (LUEO,101) (I,Z(I),OP5(I),TAU5(I),G(I),H(I),HND(I),XNE(I),
     $                  HNDT(I),I=1,N)
  101 format(5(' ',I3,1P8E15.7/))
      call MASHED ('GUPPI')
C     !END
      call BYE ('GUPPI')
C
      return
      end
