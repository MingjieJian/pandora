      subroutine DEBORAH
     $(INDX,XLM,N,NOPAC,TDUSTN,XLMDUST,EPDUST,CO,CONT)
C
C     Rudolf Loeser, 1988 Oct 25
C---- Computes a set of Dust Emission values.
C     (This is version 3 of DEBORAH.)
C     !DASH
      save
C     !DASH
      real*8 B, CO, CONT, EPDUST, ONE, R, TDUST, TDUSTN, XKDST, XLM,
     $       XLMDUST, ZERO
      integer INDX, IQDT2, J, LDU, N, NOPAC, jummy
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(26),LDU)
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
      equivalence (RZQ( 28),XKDST)
      equivalence (RZQ( 29),TDUST)
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
      equivalence (IQQ( 99),IQDT2)
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external LININT, PLUNCK, ZEROD, HI, BYE
C
C               XLMDUST(LDU), EPDUST(LDU), CO(Nopac,N), CONT(Nopac,N),
      dimension XLMDUST(*),   EPDUST(*),   CO(NOPAC,*), CONT(NOPAC,*),
C
C               TDUSTN(N)
     $          TDUSTN(*)
C
      call HI ('DEBORAH')
C     !BEG
      if(XKDST.gt.ZERO) then
        if(IQDT2.le.0) then
          call LININT     (XLMDUST,1,EPDUST,1,LDU, XLM,R, 1,1,jummy)
          call PLUNCK     (XLM,TDUST,B)
        else
          R = ONE
        end if
C
        do 100 J = 1,N
          if(CO(INDX,J).ne.ZERO) then
            if(IQDT2.gt.0) then
              call PLUNCK (XLM,TDUSTN(J),B)
            end if
            CONT(INDX,J) = B*R*CO(INDX,J)
          else
            CONT(INDX,J) = ZERO
          end if
  100   continue
C
      else
        call ZEROD        (CONT(INDX,1),NOPAC,N)
      end if
C     !END
      call BYE ('DEBORAH')
C
      return
      end
