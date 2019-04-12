      subroutine VIPER
     $(YNT,SNU,N,KODE)
C
C     Rudolf Loeser, 1981 Jan 26
C---- Checks to see whether the computed value of YNT is reasonable,
C     that is, whether it falls within the extrema of SNU.
C     If yes, does nothing; if no, sets YNT=0 and KODE=3.
C     !DASH
      save
C     !DASH
      real*8 CRIT, F, G, HALF, ONE, SLOP, SNU, YNT, ZERO
      integer IMAX, IMIN, IQFIN, IQIVK, KODE, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
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
      equivalence (IQQ(144),IQIVK)
C     !DASH
      external MINMAXD, HI, BYE
C
C               SNU(N)
      dimension SNU(*)
C
      data CRIT,SLOP /1.D20, 1.1D0/
C     !EJECT
C
      call HI ('VIPER')
C     !BEG
      if(IQIVK.gt.0) then
C
        call MINMAXD (SNU,1,N,IMIN,IMAX)
C
        if(IQFIN.gt.0) then
          F = CRIT
          G = SLOP
        else
          F = SLOP
          G = SLOP
        end if
C
        if((YNT.lt.(HALF*SNU(IMIN)/F)).or.(YNT.gt.(SNU(IMAX)*G))) then
          YNT  = ZERO
          KODE = 3
        end if
C
      end if
C     !END
      call BYE ('VIPER')
C
      return
      end
