      subroutine FELIPE
     $(X,W,N,F,R,LABEL,IMG)
C
C     Rudolf Loeser, 2006 Dec 19
C---- Computes R = integral(F), for H.S.E.
C     !DASH
      save
C     !DASH
      real*8 F, R, W, X
      integer IMG, JJZ, LHHSE, N
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
      external XAVIER, PEDRO, HI, BYE
C
      dimension X(*), W(*)
C
C               F(N), R(N), IMG(N)
      dimension F(*), R(*), IMG(*)
C
      call HI ('FELIPE')
C     !BEG
      if(LHHSE.le.1) then
C----   Integrate inward all the way
        call XAVIER (X, W, N, X(JJZ), F, R, LABEL, IMG)
      else
C----   Integrate in two parts --- in both directions
        call PEDRO  (X, W, N, F, R, LABEL, IMG)
      end if
C     !END
      call BYE ('FELIPE')
C
      return
      end
