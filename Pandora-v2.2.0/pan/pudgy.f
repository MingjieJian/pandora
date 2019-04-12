      subroutine PUDGY
     $(N,NL,GVL,GVI,GRF,WONE,LU)
C
C     Rudolf Loeser, 1997 Aug 15
C---- Fudges and prints the computed diffusion terms GVL and GVI.
C     !DASH
      save
C     !DASH
      real*8 GRF, GVI, GVL
      integer J, KDFGS, LU, N, NL
      logical WONE
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
      equivalence (KZQ(154),KDFGS)
C     !DASH
      external ARRMUL, STOAT, LINER, VECOUT, HI, BYE
C
C               GVL(N,NL), GVI(N), GRF(N)
      dimension GVL(N,*),  GVI(*), GRF(*)
C
      call HI ('PUDGY')
C     !BEG
      if(KDFGS.gt.0) then
        if(.not.WONE) then
          J = 1
          call ARRMUL     (GVL(1,J), GRF, GVL(1,J), N)
C
          if(KDFGS.eq.2) then
            call ARRMUL   (GVI     , GRF, GVI     , N)
            do 100 J = 2,NL
              call ARRMUL (GVL(1,J), GRF, GVL(1,J), N)
  100       continue
          end if
        end if
C
        if(LU.gt.0) then
          call LINER    (5, LU)
          call VECOUT   (LU, GRF, N, 'GRF table: used for fudging')
          if(.not.WONE) then
            call STOAT  (LU, N, NL, GVL, GVI, 'fudged')
          end if
        end if
      end if
C     !END
      call BYE ('PUDGY')
C
      return
      end
