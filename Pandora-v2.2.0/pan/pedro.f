      subroutine PEDRO
     $(X,W,N,F,R,LABEL,IMG)
C
C     Rudolf Loeser, 2006 Dec 19
C---- Computes R = integral(F), for H.S.E.
C     !DASH
      save
C     !DASH
      real*8 F, R, W, X
      integer IFF, IMG, IN, IRR, IS, IZZ, JJZ, LHHSE, MOX, N
      character LABEL*100
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
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
      equivalence (KZQ(129),LHHSE)
C     !DASH
      external JUANA, TERESA, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               F(N), R(N), IMG(N)
      dimension F(*), R(*), IMG(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IZZ   ),(IN( 2),IFF   ),(IN( 3),IRR   )
C
      call HI ('PEDRO')
C     !BEG
C     (Get W allotment)
      call JUANA  (IN, IS, MOX, 'PEDRO')
C
      call TERESA (X, W, N, LHHSE, X(JJZ), W(IZZ), F, W(IFF),
     $             R, W(IRR), LABEL, IMG)
C
C     (Give back W allotment)
      call WGIVE  (W, 'PEDRO')
C     !END
      call BYE ('PEDRO')
C
      return
      end
