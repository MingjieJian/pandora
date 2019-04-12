      subroutine LINEN
     $(KODE,ITAU,JNEG,LEGEND,NL,Z,ML,XMS,XR,XINV,F,BDR,KTYP,KILROY)
C
C     Rudolf Loeser, 1981 Feb 12
C---- Dumps error data, for basic b-ratio calculation.
C
C     Input  : KTYP = 1: from RACE; = 2: from CRYSTAL.
C              KODE = 2: for debug; = 3: for error.
C
C     (This is version 3 of LINEN.)
C     !DASH
      save
C     !DASH
      real*8 BDR, F, XINV, XMS, XR, Z
      integer IQBRD, IQLNB, ITAU, JNEG, KODE, KTYP, LUEO, ML, NL
      logical KILROY, LONG
      character FLAB*21, LEGEND*33
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
      equivalence (IQQ(193),IQLNB)
      equivalence (IQQ(311),IQBRD)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external MUSHED, DARROUT, DVECOUT, LINER, HI, BYE
C
C               XMS(ML,ML), XR(ML), XINV(ML,ML), Z(NL,NL)
      dimension XMS(*),     XR(*),  XINV(*),     Z(*)
C
      call HI ('LINEN')
C     !BEG
      call MUSHED     ('LINEN', KODE, KILROY)
C
      call LINER      (2, LUEO)
      if(KODE.eq.2) then
        write (LUEO,100) 'Details of',LEGEND,ITAU
  100   format(' ',A10,' calculation: ',A33,', at depth #',I3,:,A,
     $             ' BD(',I3,'/1) =',1PE18.10)
      else if(KODE.eq.3) then
        if(KTYP.eq.1) then
          write (FLAB,101) F
  101     format(', weight =',F10.7,',')
          write (LUEO,100) '! ERROR in',LEGEND,ITAU,FLAB,JNEG,BDR
        else
          write (LUEO,100) '! ERROR in',LEGEND,ITAU,',',JNEG,BDR
        end if
      end if
C
      LONG = (IQLNB.gt.0).or.(IQBRD.gt.0)
      if((KODE.eq.2).or.((KODE.eq.3).and.LONG)) then
        if(KTYP.eq.1) then
          call DVECOUT (LUEO, XR  , ML    , 'Vector R' )
          call DARROUT (LUEO, Z   , NL, NL, 'Matrix Z' )
          call DARROUT (LUEO, XMS , ML, ML, 'Matrix M' )
          call DARROUT (LUEO, XINV, ML, ML, 'Inverse M')
        else if(KTYP.eq.2) then
          call DVECOUT (LUEO, XR  , ML    , 'Vector c' )
          call DARROUT (LUEO, XMS , ML, ML, 'Matrix a' )
          call DARROUT (LUEO, XINV, ML, ML, 'Inverse a')
        end if
      end if
C     !END
      call BYE ('LINEN')
C
      return
      end
