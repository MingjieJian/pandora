      subroutine FOOT
     $(X,XNK,XND,HND,ABDEL,FION,FLVS,FIONL,FLVSL)
C
C     Rudolf Loeser, 1980 May 22
C---- Computes ionization fractions.
C     (This is version 3 of FOOT.)
C     !DASH
      save
C     !DASH
      real*8 ABD, ABDEL, DIV, FION, FIONL, FLVS, FLVSL, HND, X, XND,
     $       XNK
      integer I, N, NL
      character QELSM*8
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
      equivalence (RZQ(  6),ABD  )
      equivalence (QZQ(  2),QELSM)
C     !DASH
      external LUNA, ROWSUM, DIVIDE, TUFA, HI, BYE
C
      dimension X(*)
C
C               XNK(N), XND(N,NL), HND(N), FION(N), FIONL(N), ABDEL(N),
      dimension XNK(*), XND(*),    HND(*), FION(*), FIONL(*), ABDEL(*),
C
C               FLVS(N), FLVSL(N)
     $          FLVS(*), FLVSL(*)
C     !EJECT
C
      call HI ('FOOT')
C     !BEG
      call LUNA     (X, QELSM, ABD, ABDEL)
C
      do 100 I = 1,N
        DIV = ABDEL(I)*HND(I)
        call ROWSUM (XND(I), N, N, 1, NL, FLVS(I))
        call DIVIDE (FLVS(I), DIV, FLVS(I))
        call TUFA   (I, FLVS(I), FLVSL(I), 'FLVS')
C
        call DIVIDE (XNK(I),  DIV, FION(I))
        call TUFA   (I, FION(I), FIONL(I), 'FION')
  100 continue
C     !END
      call BYE ('FOOT')
C
      return
      end
