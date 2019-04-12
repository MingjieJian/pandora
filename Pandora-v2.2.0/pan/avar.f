      subroutine AVAR
     $(N,NL,JS,JE,BDU,BDI,XP,MODE)
C
C     Rudolf Loeser, 1982 Feb 19
C---- Edits departure coefficients, for levels JS-JE, inclusive.
C     !DASH
      save
C     !DASH
      real*8 BDI, BDN, BDO, BDU, DELTB, ONE, XP, ZERO
      integer I, IQBED, J, JE, JS, KODE, MODE, N, NL, NO
      logical NEG
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
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
      equivalence (RZQ( 74),DELTB)
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
      equivalence (IQQ(161),IQBED)
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
      external MOVE1, ABJECT, LINER, HI ,BYE
C
C               BDU(N,NL), XP(N,NL), BDI(N,NL)
      dimension BDU(*),    XP(N,*),  BDI(N,*)
C
      call HI ('AVAR')
C     !BEG
      if(MODE.eq.1) then
        call MOVE1          (BDU, (N*NL), BDI)
      end if
C
      if(IQBED.gt.0) then
        NEG  = DELTB.lt.ZERO
        KODE = 0
        do 103 J = JS,JE
          do 102 I = 1,N
            if(BDI(I,J).lt.XP(I,J)) then
C
              if(KODE.eq.0) then
                KODE = 1
                call ABJECT (NO)
                write (NO,100) DELTB
  100           format(' ','Departure Coefficients editing by AVAR',
     $                     15X,'DELTB =',1PE12.4)
                call LINER  (1,NO)
              end if
C
              BDO = BDI(I,J)
              BDN = XP(I,J)*(ONE+DELTB)
              if(NEG) then
                BDN = BDN-DELTB*BDO
              end if
              BDI(I,J) = BDN
C
              write (NO,101) J,I,BDO,XP(I,J),BDN
  101         format(' ','Level ',I2,5X,'Depth ',I3,5X,'BD-old',
     $                   1PE16.8,5X,'XP',E16.8,5X,'BD-new',E16.8)
            end if
  102     continue
  103   continue
      end if
C     !END
      call BYE ('AVAR')
C
      return
      end
