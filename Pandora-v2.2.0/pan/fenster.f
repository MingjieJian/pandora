      subroutine FENSTER
     $(N,NL,KIJ,FCE,NO)
C
C     Rudolf Loeser, 2004 Apr 01
C---- Prints FCE.
C     !DASH
      save
C     !DASH
      real*8 FCE, ONE
      integer I, IQCEF, IUL, J, KIJ, N, NL, NO
      logical ALLONE, OK, PRINTED
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
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external LINER, INDXNT, KONSTD, PRIVET, HI, BYE
C
C               KIJ(NL,NL), FCE(N,NT)
      dimension KIJ(NL,*),  FCE(N,*)
C
      call HI ('FENSTER')
C     !BEG
      if((NO.gt.0).and.(IQCEF.gt.0)) then
        call LINER          (2, NO)
        write (NO,100)
  100   format(' ','FCE: CE-enhancement factors.')
C
        PRINTED = .false.
        do 103 I = 1,NL
          do 102 J = 1,NL
            if(KIJ(I,J).eq.1) then
              call INDXNT   (I, J, OK, IUL)
              call KONSTD   (FCE(1,IUL), 1, N, ONE, ALLONE)
              if(.not.ALLONE) then
                call LINER  (1, NO)
                write (NO,101) I,J
  101           format(' ','FCE(',I2,'/',I2,')')
                call PRIVET (NO, FCE(1,IUL), N)
                PRINTED = .true.
              end if
            end if
  102     continue
  103   continue
C
        if(.not.PRINTED) then
          call LINER        (1, NO)
          write (NO,104)
  104     format(' ','     All = 1.')
        end if
      end if
C     !END
      call BYE ('FENSTER')
C
      return
      end
