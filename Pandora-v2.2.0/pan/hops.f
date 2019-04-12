      subroutine HOPS
     $(J,K,DL,SNU,EMU,WVL,VXA,FRR,Z,TYPE,IRAY,W,S)
C
C     Rudolf Loeser, 1983 Aug 30
C---- Picks out SNU values, and does a frequency shift, if needed.
C     "TYPE" may take on the values: 'PLANE', 'SHELL', 'DISK'.
C     !DASH
      save
C     !DASH
      real*8 DL, EMU, FRR, S, SNU, VXA, W, WVL, Z
      integer IQSHF, IRAY, J, K, LUEO, N
      logical SHIFTED, VZERO
      character TYPE*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
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
C     !DASH
C     !EJECT
      external BARBARA, GYRTH, VECOUT, SVEIN, ABORT, HALT, KNUT, MOVE1,
     $         LINER, MESHED, NAUGHTD, HI, BYE
C
      dimension W(*)
C
C               SNU(N,KM), VXA(N), DL(KM), Z(N), S(*)
      dimension SNU(N,*),  VXA(*), DL(*),  Z(*), S(*)
C
      call HI ('HOPS')
C     !BEG
      if(IQSHF.gt.0) then
C----   Check on actual VXA
        call NAUGHTD   (VXA,1,N,VZERO)
        if(VZERO) then
          call MESHED  ('HOPS',1)
          call VECOUT  (LUEO,VXA,N,'Actual V')
          call LINER   (2,LUEO)
          write (LUEO,100)
  100     format('SNU-shift requested but V = 0!')
          call ABORT
        else
C
C----     V not equal to zero everywhere, so do it
          SHIFTED = .true.
C----     Do shift, according to type of geometry
          if(TYPE.eq.'PLANE') then
            call GYRTH (N,J,K,DL,SNU,WVL,VXA, EMU,    S,W)
          else if(TYPE.eq.'SHELL') then
            call KNUT  (N,J,K,DL,SNU,WVL,VXA, Z,IRAY, S,W)
          else if(TYPE.eq.'DISK') then
            call SVEIN (N,J,K,DL,SNU,WVL,VXA, Z,FRR,  S,W)
C
          else
            write (MSSLIN(1),101) TYPE
  101       format('TYPE = ',A6,', which is neither PLANE, SHELL, nor ',
     $             'DISK.')
            call HALT  ('HOPS',1)
          end if
        end if
C
      else
        SHIFTED = .false.
      end if
C
      if(.not.SHIFTED) then
        if(TYPE.eq.'SHELL') then
C----     Extend unshifted SNU into S
          call BARBARA (SNU(1,J),IRAY,S)
        else
C----     Move unshifted SNU into S
          call MOVE1   (SNU(1,J),N,S)
        end if
      end if
C     !END
      call BYE ('HOPS')
C
      return
      end
