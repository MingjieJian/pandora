      subroutine SLING
     $(N,NL,SET,KIJ,MIJ,FCE,FCP,KOUNT)
C
C     Rudolf Loeser, 2004 Mar 26
C---- Examines number densities and adjusts FCE as needed
C     (radiative transitions only).
C     !DASH
      save
C     !DASH
      real*8 FCE, FCP, ONE, SET, XQ, ZERO
      integer IL, IQCEF, ITAU, IU, IUL, KIJ, KOUNT, MIJ, N, NL
      logical OK
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
      equivalence (IQQ(328),IQCEF)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !EJECT
C---- ALTIMA      as of 2004 Mar 15
      real*8      ZZLALT,ZZSALT
      common      /ALTIMA/ ZZLALT,ZZSALT
C     Extreme values of NK, ND and BD.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
      external  CRANKY, INDXNT, HI, BYE
      intrinsic abs
C
C               SET(N,MUL), FCE(N,NT), KIJ(NL,NL), FCP(NT), MIJ(NL,NL)
      dimension SET(*),     FCE(N,*),  KIJ(*),     FCP(*),  MIJ(NL,*)
C
      call HI ('SLING')
C     !BEG
      if(IQCEF.gt.0) then
C
        do 102 ITAU = 1,N
          do 101 IL = (NL-1),1,-1
            do 100 IU = (IL+1),NL
              call CRANKY   (ITAU, IU, IL, KIJ, SET, XQ)
C
              if(XQ.le.ONE) then
                call INDXNT (IU, IL, OK, IUL)
                if((FCP(IUL).ne.ZERO).and.(MIJ(IU,IL).gt.0)) then
                  if(abs(FCE(ITAU,IUL)).lt.ZZLALT) then
C
                    FCE(ITAU,IUL) = FCP(IUL)*FCE(ITAU,IUL)
                    KOUNT = KOUNT+1
C
                  end if
                end if
              end if
C
  100       continue
  101     continue
  102   continue
C
      end if
C     !END
      call BYE ('SLING')
C
      return
      end
