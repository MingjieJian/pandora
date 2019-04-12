      subroutine ALFALFA
     $(LZA,ZAUX,NAUX,LZM,NV,VAUX,QNAME,I1,I2)
C
C     Rudolf Loeser, 1972 Feb 01
C---- Enters data into print file, for CREAM.
C     !DASH
      save
C     !DASH
      real*8 VAUX, ZAUX
      integer I1, I2, IQSTO, LZA, LZM, NAUX, NO, NV
      logical KILROY
      character LABEL*20, QNAME*8, ZLAB*8
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
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
      equivalence (IQQ( 21),IQSTO)
C     !DASH
C     !EJECT
      external ABJECT, VECOUT, GAMON, HI, BYE
C
C               LZA(50), ZAUX(LZM,NZM), VAUX(LZM)
      dimension LZA(*),  ZAUX(LZM,*),   VAUX(*)
C
      data ZLAB   /'ZAUX    '/
      data KILROY /.true./
C
      call HI ('ALFALFA')
C     !BEG
      if(IQSTO.gt.0) then
C
        if(LZA(NAUX).gt.0) then
          if(KILROY) then
            KILROY = .false.
            call ABJECT (NO)
            write (NO,100)
  100       format(' ','Provisional Input tables:',83X,
     $                 '(Option STANPRNT)')
          end if
C
          call GAMON    (ZLAB, NAUX, 0, LABEL)
          call VECOUT   (NO, ZAUX(1,NAUX), NV, LABEL)
C
          LZA(NAUX) = -NV
        end if
C
        call GAMON      (QNAME, I1, I2, LABEL)
        call VECOUT     (NO, VAUX, NV, LABEL)
      end if
C     !END
      call BYE ('ALFALFA')
C
      return
      end
