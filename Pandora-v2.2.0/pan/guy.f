      subroutine GUY
     $(X,W,BDI,XNKPR,XNKUW,XNK,XNKS,XNDPR,XNDUW,XND,XNDS,ABDEL,
     $ WEIT,IMG)
C
C     Rudolf Loeser, 2002 Feb 13
C---- Computes number densities, for GORSE.
C     (This is version 3 of GUY.)
C     !DASH
      save
C     !DASH
      real*8 ABDEL, BDI, W, WEIT, WPOP, X, XND, XNDPR, XNDS, XNDUW, XNK,
     $       XNKPR, XNKS, XNKUW, dummy
      integer IMG, KLOG, KMSS, MODE, N, NL, NNL
      character qummy*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
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
      equivalence (RZQ( 25),WPOP )
C     !DASH
C     !EJECT
      external NUMB, WEITER, HI, BYE
C
      dimension X(*), W(*)
C
C               XNDPR(N,NL), XNDUW(N,NL), XNDS(N,NL), ABDEL(N), XNK(N),
      dimension XNDPR(*),    XNDUW(*),    XNDS(*),    ABDEL(*), XNK(*),
C
C               XND(N,NL), XNKPR(N), WEIT(N,NL), BDI(N,NL), XNKUW(N),
     $          XND(*),    XNKPR(*), WEIT(*),    BDI(*),    XNKUW(*),
C
C               XNKS(N), IMG(N)
     $          XNKS(*), IMG(*)
C
      data KLOG,MODE,KMSS /1, 0, 0/
C
      call HI ('GUY')
C     !BEG
      call NUMB   (X, W, BDI, 1, ABDEL, XNKUW, XNDUW, IMG)
C
      NNL = N*NL
      call WEITER (XND, XNDUW, XNDPR, dummy, WPOP, NNL, KLOG, MODE,
     $             KMSS, qummy, WEIT)
      call WEITER (XNK, XNKUW, XNKPR, dummy, WPOP, N  , KLOG, MODE,
     $             KMSS, qummy, WEIT)
C
      call NUMB   (X, W, BDI, 0, ABDEL, XNKS,  XNDS,  IMG)
C     !END
      call BYE ('GUY')
C
      return
      end
