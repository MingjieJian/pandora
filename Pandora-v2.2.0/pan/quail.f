      subroutine QUAIL
     $(TE,XNE,XN1,HN1,TR,TV,TK,TS)
C
C     Rudolf Loeser, 1980 Aug 14
C---- Computes transition-independent intermediates for
C     Damping Paramater calculation.
C     (This is version 2 of QUAIL.)
C     !DASH
      save
C     !DASH
      real*8 C2, C3, C4, C5, HN1, ONE, PW, TE, TK, TR, TS, TV, XN1, XNE
      integer I, N
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
      equivalence (RZQ(  7),PW   )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external HI, BYE
C
C               TE(N), XNE(N), HN1(N), TR(N), TV(N), TK(N), XN1(N),
      dimension TE(*), XNE(*), HN1(*), TR(*), TV(*), TK(*), XN1(*),
C
C               TS(N)
     $          TS(*)
C
      data C2,C3,C4,C5 /5.D3, 3.D-1, 1.D16, 1.D12/
C
      call HI ('QUAIL')
C     !BEG
      do 100 I = 1,N
        TR(I) = ONE
        TV(I) = (HN1(I)/C4)*((TE(I)/C2)**C3)
        TK(I) = (XNE(I)/C5)**PW
        TS(I) = XN1(I)/C4
  100 continue
C     !END
      call BYE ('QUAIL')
C
      return
      end
