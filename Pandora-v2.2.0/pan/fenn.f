      subroutine FENN
     $(TAU,EP1,EPS,N)
C
C     Rudolf Loeser, 1981 Dec 11
C---- New procedure for negative EP1.
C     (This is version 2 of FENN.)
C     !DASH
      save
C     !DASH
      real*8 EP1, EPS, T, TAU, XLMA, XLMB, XLME, XLMF, XLMR, XLMT
      integer I, KLAR, KSMA, N
      logical DUMP
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 26),XLMT )
      equivalence (RZQ( 33),XLMF )
      equivalence (RZQ( 27),XLME )
      equivalence (RZQ( 35),XLMA )
      equivalence (RZQ( 36),XLMB )
      equivalence (RZQ( 37),XLMR )
C     !DASH
      external MOVE1, MOOR, HI, BYE
C
C               TAU(N), EP1(N), EPS(N)
      dimension TAU(*), EP1(*), EPS(*)
C
      data DUMP /.true./
C     !EJECT
C
      call HI ('FENN')
C     !BEG
      KSMA = 0
      KLAR = 0
      call MOVE1  (EP1, N, EPS)
C
      do 100 I = 1,N
        T = TAU(I)
        if((T.ge.XLMT).and.(EP1(I).lt.XLMF)) then
          EP1(I) = XLME
          KSMA = 1
        end if
        if(((T.ge.XLMA).and.(T.le.XLMB)).and.(EP1(I).gt.XLMR)) then
          EP1(I) = XLMR
          KLAR = 1
        end if
  100 continue
C
      if(((KSMA.eq.1).or.(KLAR.eq.1)).and.DUMP) then
        call MOOR (TAU, EP1, EPS, N, KSMA, KLAR, 'FENN')
      end if
C     !END
      call BYE ('FENN')
C
      return
      end
