      subroutine PUDDING
     $(N,NL,GVL,LU)
C
C     Rudolf Loeser, 2004 Feb 24
C---- Suppresses GVL for selected levels.
C     !DASH
      save
C     !DASH
      real*8 GVL
      integer J, LIM, LU, N, NGNV, NL
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
      equivalence (KZQ(205),NGNV )
C     !DASH
      external  ZERO1, LINER, HI, BYE
      intrinsic min
C
C               GVL(N,NL)
      dimension GVL(N,*)
C
      call HI ('PUDDING')
C     !BEG
      if(NGNV.gt.0) then
        LIM = min(NGNV,NL)
        if(LIM.le.NL) then
C
          do 100 J = LIM,NL
            call ZERO1 (GVL(1,J), N)
  100     continue
C
          if(LU.gt.0) then
            call LINER (5, LU)
            write (LU,101) LIM,NL
  101       format(' ','GVL for levels',I5,' through',I5,
     $                 ' were suppressed.')
          end if
C
        end if
      end if
C     !END
      call BYE ('PUDDING')
C
      return
      end
