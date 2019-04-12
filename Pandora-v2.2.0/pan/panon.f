      subroutine PANON
     $(FRS,F,P,LABEL,LU)
C
C     Rudolf Loeser, 1982 Mar 31
C---- Produces auxiliary printout for spherical case.
C     P is working storage.
C     !DASH
      save
C     !DASH
      real*8 F, FRS, P
      integer IQSFS, LU, N
      character LABEL*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (IQQ( 31),IQSFS)
C     !DASH
      external OXUS, LINER, PRIVET, HI, BYE
C
C               FRS(N), P(N), F(N)
      dimension FRS(*), P(*), F(*)
C     !EJECT
C
      call HI ('PANON')
C     !BEG
      if((LU.gt.0).and.(IQSFS.gt.0)) then
        call OXUS   (F,FRS,P,N)
C
        call LINER  (2,LU)
        write (LU,100) LABEL
  100   format(' ',A,' times R squared')
        call PRIVET (LU,P,N)
      end if
C     !END
      call BYE ('PANON')
C
      return
      end
