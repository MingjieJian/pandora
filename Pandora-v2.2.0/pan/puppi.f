      subroutine PUPPI
     $(Z,HND,RZM,ZME,G,H,HNK,HNKR,H2N,H2NR,HNI,HNIR,LIMP)
C
C     Rudolf Loeser, 1979 Sep 28
C---- Print debug HSE data.
C     !DASH
      save
C     !DASH
      real*8 CGR, G, H, H2N, H2NR, HND, HNI, HNIR, HNK, HNKR, RZM, YH,
     $       Z, ZME
      integer I, LIMP, LUEO, N
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
      equivalence (RZQ( 15),YH   )
      equivalence (RZQ( 16),CGR  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external LINER, FUPPI, MESHED, MASHED, HI, BYE
C
C               HND(N), RZM(N), ZME(N), HNK(N), HNKR(N), G(N), H2NR(N),
      dimension HND(*), RZM(*), ZME(*), HNK(*), HNKR(*), G(*), H2NR(*),
C
C               HNI(N,LIMP), HNIR(N,LIMP), H(N), H2N(N), Z(N)
     $          HNI(*),      HNIR(*),      H(*), H2N(*), Z(*)
C
      call HI ('PUPPI')
C     !BEG
      call MESHED ('PUPPI', 2)
C
      write (LUEO,100) LIMP,N,YH,CGR
  100 format(' ','HSE: LIMP =',I4,',  N =',I4,',  YH',1PE15.7,',  CGR',
     $           E15.7//
     $       ' ',15X,'HND',12X,'RZM',12X,'ZME',14X,'G',14X,'H',14X,'Z')
      call LINER  (1, LUEO)
      write (LUEO,101) (I,HND(I),RZM(I),ZME(I),G(I),H(I),Z(I),I=1,N)
  101 format(5(' ',I3,1P6E15.7/))
C
      call LINER  (2, LUEO)
      write (LUEO,102)
  102 format(' ',15X,'HNK',11X,'HNKR',12X,'H2N',11X,'H2NR')
      call LINER  (1, LUEO)
      write (LUEO,103) (I,HNK(I),HNKR(I),H2N(I),H2NR(I),I=1,N)
  103 format(5(' ',I3,1P4E15.7/))
C
      call FUPPI  (HNI, HNIR, LIMP, N, LUEO)
C
      call MASHED ('PUPPI')
C     !END
      call BYE ('PUPPI')
C
      return
      end
