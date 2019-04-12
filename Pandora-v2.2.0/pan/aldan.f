      subroutine ALDAN
     $(CALLER,ICE,N,K,IU,IL,H,RF,H1,H1M,ABC,SNU,DL,Z,TE)
C
C     Rudolf Loeser, 1985 Jan 23
C---- Prints, for TOBOL.
C     (This is version 2 of ALDAN.)
C     !DASH
      save
C     !DASH
      real*8 ABC, DL, H, H1, H1M, RF, SNU, TE, Z
      integer ICE, IL, IQENH, IQLFD, IQSHF, IU, K, MO, N
      character CALLER*(*)
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
      equivalence (IQQ(172),IQSHF)
      equivalence (IQQ( 38),IQENH)
      equivalence (IQQ(188),IQLFD)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
C     !EJECT
      external ANGARA, BIYA, BIRYUSA, HI, BYE
C
C               DL(K), H1(K), ABC(N,K), TE(N), RF(N), H(N,K), SNU(N,K),
      dimension DL(*), H1(*), ABC(*),   TE(*), RF(*), H(*),   SNU(*),
C
C               Z(N), H1M(K)
     $          Z(*), H1M(*)
C
      call HI ('ALDAN')
C     !BEG
      if(MO.gt.0) then
        if(IQLFD.gt.0) then
C----     Extra arrays printout
          call ANGARA (CALLER, N, K, IU, IL, ABC, SNU, H)
        end if
C----   Regular printout
        call BIYA     (MO, N, K, IU, IL, DL, H1, H1M, Z, TE, RF, IQSHF,
     $                 IQENH, ICE)
C----   Graph
        call BIRYUSA  (N, K, IU, IL, DL, H, MO)
      end if
C     !END
      call BYE ('ALDAN')
C
      return
      end
