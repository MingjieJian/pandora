      subroutine VILA
     $(Z,TE,XNE,XPBL,CONDAP,CONDEX,KCOND,VEC,W)
C
C     Rudolf Loeser, 1981 Feb 03
C---- Computes conductive flux gradient:
C     CONDAP - from the approximate equation;
C     CONDEX - from the general equations.
C     !DASH
      save
C     !DASH
      real*8 CONDAP, CONDEX, TE, VEC, W, XNE, XPBL, Z
      integer IQCFD, KCOND, LU, N, NO
      character QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (QZQ(  1),QNAME)
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
      equivalence (IQQ(143),IQCFD)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external GAFF, ZEUS, DAFFY, ZERO1, HI, BYE
C
      dimension W(*)
C
C               XPBL(Lenpbl), VEC(N), XNE(N), CONDEX(N), Z(N), TE(N),
      dimension XPBL(*),      VEC(*), XNE(*), CONDEX(*), Z(*), TE(*),
C
C               CONDAP(N)
     $          CONDAP(*)
C
      call HI ('VILA')
C     !BEG
      if(QNAME.eq.'HYDROGEN') then
        call GAFF  (Z, TE, CONDAP, VEC)
        call ZEUS  (NO, IQCFD, LU)
        call DAFFY (Z, TE, XNE, XPBL, CONDEX, LU, W)
        KCOND = 1
      else
        KCOND = 0
        call ZERO1 (CONDEX, N)
        call ZERO1 (CONDAP, N)
      end if
C     !END
      call BYE ('VILA')
C
      return
      end
