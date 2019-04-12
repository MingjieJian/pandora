      subroutine FARRE
     $(N,IU,IL,T,PTAU,WW,DUMP,KILROY)
C
C     Rudolf Loeser, 1991 Sep 25
C---- Debug printout for DAISY.
C     (This is version 2 of FARRE.)
C     !DASH
      save
C     !DASH
      real*8 PTAU, T, WW
      integer IL, IQRWO, IU, LUEO, N
      logical DUMP, KILROY
C     !COM
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
      equivalence (IQQ(204),IQRWO)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external VECOUT, DASHER, LINER, HI, BYE
C
C               T(N), PTAU(N), WW(N)
      dimension T(*), PTAU(*), WW(*)
C     !EJECT
C
      call HI ('FARRE')
C     !BEG
      if(DUMP.and.KILROY) then
        call DASHER  (LUEO)
        write (LUEO,100) IU,IL
  100   format(' ','Intermediates for RHOW(',I2,'/',I2,')')
C
        call VECOUT  (LUEO, T   , N, 'TAUW')
        call VECOUT  (LUEO, PTAU, N, 'WT'  )
        call VECOUT  (LUEO, WW  , N, 'WW'  )
C
        if(IQRWO.gt.0) then
          call LINER (2, LUEO)
          write (LUEO,101)
  101     format(' ','The above values are used for all transitions ',
     $               '(option RHOWOPT).')
          KILROY = .false.
        end if
      end if
C     !END
      call BYE ('FARRE')
C
      return
      end
