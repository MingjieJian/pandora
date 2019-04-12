      subroutine RAFTER
     $(NVX,CVX,ISSV)
C
C     Rudolf Loeser, 2005 Jan 06
C---- Checks flow-broadening input.
C     !DASH
      save
C     !DASH
      real*8 CVX, CVXF, CVXM, ONE, WFB, ZERO
      integer IQFLW, ISSV, NFB, NVX
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 9),NFB)
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
      equivalence (IQQ(335),IQFLW)
C     !EJECT
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 65),CVXM )
      equivalence (RZQ( 67),CVXF )
      equivalence (RZQ( 68),WFB  )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
C               CVX(NVX), ISSV(NVX)
      dimension CVX(*),   ISSV(*)
C     !EJECT
C
      call HI ('RAFTER')
C     !BEG
      if(IQFLW.gt.0) then
        if(NVX.ne.(2*NFB+2)) then
          write (MSSLIN(1),100) NVX
  100     format('NVX =',I12,' does not make sense when ',
     $           'FLWBROAD = on.')
          call HALT ('RAFTER', 1)
        end if
        if(CVX(1).ne.ZERO) then
          write (MSSLIN(1),101)
  101     format('Non-zero input values of CVX are not allowed when ',
     $           'FLWBROAD = on.')
          call HALT ('RAFTER', 1)
        end if
        if(CVXM.lt.ZERO) then
          write (MSSLIN(1),102) 'CVXM', CVXM
  102     format(A,' =',1PE12.4,' should not be negative when ',
     $           'FLWBROAD = on.')
          call HALT ('RAFTER', 1)
        end if
        if(CVXF.lt.ZERO) then
          write (MSSLIN(1),102) 'CVXF', CVXF
          call HALT ('RAFTER', 1)
        end if
        if(ISSV(1).ne.0) then
          write (MSSLIN(1),103)
  103     format('Non-zero input vales of ISSV (for shocks) do not ',
     $           'make sense when FLWBROAD = on.')
          call HALT ('RAFTER', 1)
        end if
        if((WFB.lt.ZERO).or.(WFB.gt.ONE)) then
          write (MSSLIN(1),104) WFB
  104     format('WFB =',1PE12.4,' does not make sense when ',
     $           'FLWBROAD = on.')
          call HALT ('RAFTER', 1)
        end if
      end if
C     !END
      call BYE ('RAFTER')
C
      return
      end
