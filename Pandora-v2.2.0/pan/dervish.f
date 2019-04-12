      subroutine DERVISH
     $(X,W,IW,N,TAU,OPAC,Z,YDAMP,MOVING,WN,WH,ILFLX,TITLE,DUMP,KODE,IMG)
C
C     Rudolf Loeser, 1981 May 08
C---- Sets up WN = "Lambda-minus-one" operator.
C     Also sets up WH = "Phi" operator, but only if ILFLX .gt. 0.
C     Note: WH is not referred to when ILFLX .eq. 0!
C     !DASH
      save
C     !DASH
      real*8 OPAC, TAU, THREE, W, WH, WN, X, YDAMP, Z
      integer ILFLX, IMG, IQSFS, IQWDD, IW, KODE, N
      logical DUMP, MOVING
      character TITLE*100
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 4),THREE )
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
      equivalence (IQQ( 31),IQSFS)
      equivalence (IQQ(146),IQWDD)
C     !DASH
C     !EJECT
      external WHIRL, HOLDA, KLAMATH, IOTA, ZERO1, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               TAU(N), IMG(N), Z(N), WN(N,N), WH(N,N), OPAC(*)
      dimension TAU(*), IMG(*), Z(*), WN(*),   WH(*),   OPAC(*)
C
      call HI ('DERVISH')
C     !BEG
      if(IQSFS.le.0) then
C----   Plane-parallel geometry
        if(YDAMP.eq.-THREE) then
C----     Method with numerical angle integration
          call HOLDA   (X, W, IW, TAU, Z, N, YDAMP, MOVING, WN, WH,
     $                  ILFLX, TITLE)
          KODE = 1
        else
C----     Methods with analytical angle integration
          call WHIRL   (X, W, IW, TAU, Z, N, YDAMP, WN, TITLE, KODE)
          if(ILFLX.gt.0) then
            call ZERO1 (WH, (N**2))
          end if
        end if
      else
C----   Spherical geometry
        call KLAMATH   (X, W, IW, OPAC, YDAMP, MOVING, WN, WH, ILFLX,
     $                  IMG, TITLE, KODE)
      end if
C
      if(DUMP.and.(IQWDD.le.0)) then
        call IOTA      (WN, N, TITLE, 'WN')
        if(ILFLX.gt.0) then
          call IOTA    (WH, N, TITLE, 'WH')
        end if
      end if
C     !END
      call BYE ('DERVISH')
C
      return
      end
