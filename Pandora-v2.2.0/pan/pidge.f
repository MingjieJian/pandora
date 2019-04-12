      subroutine PIDGE
     $(GRF,N,WONE)
C
C     Rudolf Loeser, 1997 Aug 15
C---- Sets up GRF, the GNV-fudge-factor for the diffusion calculations.
C     Returns WONE = .true. if all GRF = 1.
C     (This is version 2 of PIDGE.)
C     !DASH
      save
C     !DASH
      real*8 GRF
      integer INDXA, KDFGA, KDFGB, KDFGS, N
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
      equivalence (KZQ(155),KDFGA)
      equivalence (KZQ(156),KDFGB)
C     !DASH
      external MOLLUSC, HI, BYE
C
C               GRF(N)
      dimension GRF(*)
C
      call HI ('PIDGE')
C     !BEG
      if(KDFGS.gt.0) then
        INDXA = KDFGA
      else
        INDXA = N
      end if
      call MOLLUSC (WONE, GRF, N, INDXA, KDFGB)
C     !END
      call BYE ('PIDGE')
C
      return
      end
