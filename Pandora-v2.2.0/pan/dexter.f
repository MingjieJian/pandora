      subroutine DEXTER
     $(IPRNT,NW,NO,NECLP)
C
C     Rudolf Loeser, 1993 Feb 24
C---- Sets up LUNs, for RAJA.
C     !DASH
      save
C     !DASH
      real*8 DL, DNE, DNP, DNW, DR, XK, XR, ZERO
      integer I, IPRNT, IQAVK, IQESD, J, K, NECLP, NO, NW
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      equivalence (IQQ( 40),IQESD)
      equivalence (IQQ(303),IQAVK)
C     !DASH
      external SETI, ZEROI, HI, BYE
C
C               IPRNT(NW)
      dimension IPRNT(*)
C     !EJECT
C
      call HI ('DEXTER')
C     !BEG
      call SETI    (IPRNT,1,NW, NO)
C
      if((IQESD.le.0).and.(IQAVK.gt.0).and.(NECLP.lt.NW)) then
C
        call ZEROI (IPRNT,1,NW)
C
        if(NECLP.gt.0) then
          K = 1
          DNW = NW-1
          DNE = NECLP-1
          DNP = ZERO
          DR  = ZERO
          do 100 I = 2,NW
            DL = DR
            XR = I-1
            DR = XR/DNW
C
            if((DNP.ge.DL).and.(DNP.le.DR)) then
              if(DNP.eq.DL) then
                J = I-1
              else if(DNP.eq.DR) then
                J = I
              else if((DNP-DL).le.(DR-DNP)) then
                J = I-1
              else
                J = I
              end if
C
              IPRNT(J) = NO
C
              XK = K
              K  = K+1
              if(K.gt.NECLP) go to 101
              DNP = XK/DNE
            end if
C
  100     continue
  101     continue
        end if
C
      end if
C     !END
      call BYE ('DEXTER')
C
      return
      end
