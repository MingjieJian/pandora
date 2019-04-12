      subroutine WEASEL
     $(TMU,N,IS,IL,WS)
C
C     Rudolf Loeser, 1980 Dec 31
C---- Computes a set of intensity integration weights, for SIMBA.
C     These weights correspond to a reduced TMU table, called TRU,
C     which consists of selected TMU values, as follows:
C     TMU(1), TMU(IS), TMU(IS+1), ... TMU(IL-1), TMU(IL).
C     (This is version 2 of WEASEL.)
C     !DASH
      save
C     !DASH
      real*8 DM, EC, EP, ONE, RM, TMU, WS, XM, XQ, ZERO, dummy
      integer I, IL, IQFIN, IQREF, IS, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      equivalence (IQQ( 50),IQREF)
      equivalence (IQQ( 49),IQFIN)
C     !DASH
C     !EJECT
      external QEXP2, QEXP3, DIVIDE, FERRET, HI, BYE
C
C               TMU(N), WS(N)
      dimension TMU(*), WS(*)
C
      call HI ('WEASEL')
C     !BEG
C---- For the normal case.
      EC = ONE
      call QEXP2      (TMU(IS), dummy, 2, XM)
      WS(1) = EC*XM
C
      do 100 I = IS,(IL-1)
        call QEXP3    ((TMU(I)-TMU(I-1)), dummy, 2, XQ)
        call QEXP2    ((TMU(I+1)-TMU(I)), dummy, 2, XM)
        EP = EC
        EC = exp(-TMU(I))
        WS(I) = EC*XM+EP*XQ
  100 continue
C
      EP = EC
      EC = exp(-TMU(IL))
      DM = (TMU(IL)-TMU(IL-1))
      call DIVIDE     (EC, DM, RM)
      WS(IL-1) = WS(IL-1)-RM
C
      call QEXP3      (DM, dummy, 2, XQ)
      WS(IL) = EP*XQ+EC+RM
C
      if(IQFIN.gt.0) then
C----   Modify if FINITE=ON.
        WS(IL-1) = WS(IL-1)   +RM
        WS(IL)   = WS(IL)  -EC-RM
        if(IQREF.gt.0) then
C----     Modify further if also REFLECT=ON.
          call FERRET (WS, IS, IL, TMU, N)
        end if
      end if
C
      if(IS.gt.2) then
C----   Shift the weight for the first source-function-value from
C       there to the last omitted small-Tau point, if possible.
        WS(IS-1) = WS(1)
        WS(1)    = ZERO
      end if
C     !END
      call BYE ('WEASEL')
C
      return
      end
