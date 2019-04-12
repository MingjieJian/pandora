      subroutine OTUMBA
     $(I,L,N,NW,GOOD,LFB,WAVES,EMU,EMINT,WS,SNU,WSSAV,SNUSAV,CONINT)
C
C     Rudolf Loeser, 1982 Apr 29
C---- Saves results and intermediates, for the I'th wavelength,
C     for VISHNU.
C     !DASH
      save
C     !DASH
      real*8 EMINT, EMU, SNU, SNUSAV, WAVES, WS, WSSAV
      integer I, IQORI, ITOPE, J, K, L, LFB, LUNC, LUNN, N, NW
      logical CONINT, DOOR, DOSS, GOOD
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
      equivalence (KZQ( 17),ITOPE)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(12),LUNC )
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
      equivalence (IQQ(101),IQORI)
C     !DASH
C     !EJECT
      external ZEUS, MOVE1, HI, BYE
C
C               WSSAV(N,Nmkuse,L), SNUSAV(N,Nmkuse,L), EMINT(Nmkuse,L),
      dimension WSSAV(N,NW,*),     SNUSAV(N,NW,*),     EMINT(NW,*),
C
C               EMU(L), WAVES(Nmkuse), WS(N,L), SNU(N)
     $          EMU(*), WAVES(*),      WS(N,*), SNU(*)
C
      call HI ('OTUMBA')
C     !BEG
      if(I.eq.1) then
        call ZEUS     (LUNC, ITOPE, LUNN)
        DOOR = IQORI.gt.0
        DOSS = GOOD.and.(LUNN.gt.0).and.CONINT
      end if
C
      do 100 J = 1,L
        if(DOOR) then
          call MOVE1  (WS(1,J), N, WSSAV (1,I,J))
          call MOVE1  (SNU    , N, SNUSAV(1,I,J))
        end if
C
        if(DOSS) then
          write (LUNN) LFB,WAVES(I),I,EMU(J),J,EMINT(I,J),N,(WS(K,J),
     $                 SNU(K),K=1,N)
        end if
  100 continue
C     !END
      call BYE ('OTUMBA')
C
      return
      end
