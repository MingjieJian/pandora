      subroutine NADAL
     $(M,TAU,IZOPT,IBEG,IEND)
C
C     Rudolf Loeser, 1996 Oct 02
C---- Selects abscissa limit indices, for ZED.
C     (This is version 2 of NADAL.)
C     !DASH
      save
C     !DASH
      real*8 TAU, TH, TL
      integer IBEG, IEND, IZOPT, JZOPT, M, N, NGRL, NGRR
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
      equivalence (KZQ( 37),JZOPT)
      equivalence (KZQ( 21),NGRL )
      equivalence (KZQ( 22),NGRR )
C     !DASH
C     !EJECT
      external  NOTMORE, NOTLESS, HI, BYE
      intrinsic min,max
C
C               TAU(N)
      dimension TAU(*)
C
      data TL,TH /1.D-4, 1.D3/
C
      call HI ('NADAL')
C     !BEG
      if((JZOPT.eq.1).and.(M.gt.0)) then
        call NOTMORE (TAU, N, TL, IBEG)
        if(IBEG.le.0) then
          IBEG = 1
        end if
        call NOTLESS (TAU, N, TH, IEND)
        if((IEND.le.0).or.(IEND.gt.N)) then
          IEND = N
        end if
      else
        if(IBEG.le.0) then
          IBEG = min((max(NGRL,1)),N)
        end if
        if(IEND.le.0) then
          IEND = min((max(NGRR,1)),N)
        end if
      end if
      if(IZOPT.eq.4) then
        IBEG = max(IBEG,2)
      end if
      if(IEND.le.IBEG) then
        IBEG = 1
        IEND = N
      end if
C     !END
      call BYE ('NADAL')
C
      return
      end
