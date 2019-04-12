      subroutine LION
     $(TMU,N,M,IS,IL)
C
C     Rudolf Loeser, 1970 Jan 22
C---- Establish TMU-indices, for SIMBA.
C     !DASH
      save
C     !DASH
      real*8 TML, TMU, TSM
      integer I, IL, IQFIN, IQUTM, IS, M, N
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
      equivalence (RZQ( 21),TSM  )
      equivalence (RZQ( 73),TML  )
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ( 49),IQFIN)
      equivalence (IQQ(321),IQUTM)
C     !DASH
      external HI, BYE
C
C               TMU(N)
      dimension TMU(*)
C     !EJECT
C
      call HI ('LION')
C     !BEG
      if(IQUTM.gt.0) then
        do 100 I = 2,N
          IS = I
          if(TMU(I).ge.TSM) then
            goto 101
          end if
  100   continue
      else
        IS = 2
      end if
C
  101 continue
C
      do 102 I = IS,N
        IL = I
        if(TMU(I).gt.TML) then
          goto 103
        end if
  102 continue
C
  103 continue
C
      if(IL.lt.N) then
        IL = IL+1
      end if
C
      M = IL-IS+2
C
      if((IQFIN.gt.0).and.(M.lt.4).and.(TMU(N).le.TML)) then
        IS = N-2
        IL = N
        M  = 4
      end if
C     !END
      call BYE ('LION')
C
      return
      end
