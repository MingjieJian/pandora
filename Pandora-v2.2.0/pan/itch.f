      subroutine ITCH
     $(CALLER,IU,IL,L,K,N,XKPC,BC,SIG,XJNU,RXI,XRD,ZRD,YRD)
C
C     Rudolf Loeser, 1987 Jan 13
C---- Dumps DIANA/ORION PRD arrays.
C     (This is version 2 of ITCH.)
C     !DASH
      save
C     !DASH
      real*8 BC, RXI, SIG, XJNU, XKPC, XRD, YRD, ZRD
      integer I, IL, IQPD3, IU, K, L, LSAV, MS, N, NO, NS
      character CALLER*(*)
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(  2),MS   )
      equivalence (KZQ(  3),NS   )
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
      equivalence (IQQ(206),IQPD3)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
C     !EJECT
      external ABJECT, LINER, HI, BYE
C
C               BC(N), XJNU(N), RXI(N), SIG(N), XRD(N), YRD(N), ZRD(N),
      dimension BC(*), XJNU(*), RXI(*), SIG(*), XRD(*), YRD(*), ZRD(*),
C
C               XKPC(N)
     $          XKPC(*)
C
      data LSAV /-1/
C
      call HI ('ITCH')
C     !BEG
      if(IQPD3.gt.0) then
        if((IU.eq.MS).and.(IL.eq.NS)) then
          if(L.ne.LSAV) then
C
            LSAV = L
C
            if(L.eq.1) then
              call ABJECT (NO)
              write (NO,100) CALLER,IU,IL,K,N
  100         format(' ','Dump from ITCH, called by ',A,', for line (',
     $                   I2,'/',I2,'); K =',I3,' and N =',I3/
     $               ' ','(This dump is controlled by option PERDUMP3 ',
     $                   'and transition indices MS/NS.)')
            end if
C
            call LINER    (1, NO)
            write (NO,101) L,K,N
  101       format(' ','Frequency #',I3,20X,'K =',I3,' and N =',I3//
     $             ' ',17X,'KPC',13X,'BC',12X,'SIG',12X,'JNU',12X,'RXI',
     $                 12X,'XRD',12X,'YRD',12X,'ZRD')
            write (NO,102) (I,XKPC(I),BC(I),SIG(I),XJNU(I),RXI(I),
     $                        XRD(I),YRD(I),ZRD(I),I=1,N)
  102       format(5(' ',I5,1P8E15.7/))
C
          else
            call LINER    (1, NO)
            write (NO,103) L,K,N
  103       format(' ','Frequency #',I3,20X,'K =',I3,' and N =',I3,20X,
     $                 'Different ray, same data.')
          end if
C
        end if
      end if
C     !END
      call BYE ('ITCH')
C
      return
      end
