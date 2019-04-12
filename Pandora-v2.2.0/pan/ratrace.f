      subroutine RATRACE
     $(X,TAU,N,Y,FIN,TAURED,GDIL,TITLE,WN,W,KODE)
C
C     Rudolf Loeser, 1989 Dec 08
C---- Computes WN by the "ray-tracing" method.
C     Returns with KODE=1 if all seems OK, with KODE=0 otherwise.
C     !DASH
      save
C     !DASH
      real*8 TAU, TMS, W, WN, X, Y
      integer IN, IQREF, IS, ITNP, IWK, K, KASE, KIL, KIS, KODE, MOX, N
      logical FIN, GDIL, REF, TAURED
      character TITLE*(*)
C     !COM
C---- MUNUXI      as of 2005 Apr 14
      logical     LAMHED,LAMDMP
      common      /MUNUXI/ LAMHED,LAMDMP
C     Subroutine "LAMBDA" extra printout header control.
C     .
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 44),TMS  )
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
      equivalence (IQQ( 50),IQREF)
C     !DASH
C     !EJECT
      external RATTAN, NATTER, JOCOTE, PINE, IOTA, JILGA, ILETO, SOTUR,
     $         WGIVE, RADISH, JUNO, HI, BYE
C
      dimension X(*), W(*)
C
C               TAU(N), WN(N,N)
      dimension TAU(*), WN(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),ITNP  ),(IN( 2),IWK   )
C
      data KASE /3/
C
      call HI ('RATRACE')
C     !BEG
C     (Get W allotment)
      call RATTAN     (IN, IS, MOX, 'RATRACE', N)
C
      if(LAMDMP) then
        call JUNO
      end if
C---- Set up "reduced-TAU" table, TNP
      call JOCOTE     (TAURED, TAU, N, FIN, W(ITNP), K, KIS, KIL,
     $                 TITLE, KODE, LAMDMP)
      call SOTUR      (1, KASE, KIS, KIL)
      if(KODE.gt.0) then
        REF = IQREF.gt.0
C----   Check options (abort if incompatible)
        call RADISH   (FIN, REF)
C----   Compute "reduced" WN matrix
        call PINE     (W(ITNP), K, W(IWK), REF, FIN, TMS, W)
        if(LAMDMP) then
          call IOTA   (W(IWK), K, TITLE, 'WN as computed')
        end if
C----   "Expand" WN matrix to final size
        call JILGA    (W(IWK), K, WN, N, TAU, KIS, KIL, TITLE, LAMDMP)
C----   Apply geometrical corrections (if needed)
        call ILETO    (X, GDIL, WN, N, TITLE, LAMDMP)
      end if
C---- Error advisory (if needed)
      call NATTER     (KODE, TITLE, Y, TAU, N, 'RATRACE')
C
C     (Give back W allotment)
      call WGIVE      (W, 'RATRACE')
C     !END
      call BYE ('RATRACE')
C
      return
      end
