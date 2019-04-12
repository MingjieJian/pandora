      subroutine MANNA
     $(WTAB,K,PROGLI,RCN,KRCN,RCL,KRCL,RCI,KRCI,KODE,NO,LINE,
     $ XL,XU,IMAGE)
C
C     Rudolf Loeser, 1980 Aug 05
C---- Initializes the residuals plot image, for ORANGE.
C     Returns with KODE=1 if everything seems OK, =0 if not.
C     (This is version 2 of MANNA.)
C     !DASH
      save
C     !DASH
      real*8 BOT, PROGLI, RCI, RCL, RCN, THREE, TOP, WTAB, XL, XU, YMAX,
     $       YMIN, YMN1, YMN2, YMN3, YMX1, YMX2, YMX3, ZERO
      integer IPEX, K, KODE, KRCI, KRCL, KRCN, LUEO, NO
      logical DUMP
      character IMAGE*(*), LINE*(*)
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
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 4),THREE )
C     !DASH
C     !EJECT
      external  BRUSH, TITAN, MELIC, LUTALE, MESHED, MASHED, DANIEL,
     $          HI, BYE
      intrinsic min,max
C
C               RCN(KM), RCL(KM), RCI(KM), WTAB(KM)
      dimension RCN(*),  RCL(*),  RCI(*),  WTAB(*)
C
      call HI ('MANNA')
C     !BEG
      KODE = 0
C
      DUMP = (IPEX.lt.0).or.(IPEX.eq.4)
C---- Find extrema of residuals
      call BRUSH     (RCN, K, KRCN, YMX1, YMN1)
      call BRUSH     (RCL, K, KRCL, YMX2, YMN2)
      call BRUSH     (RCI, K, KRCI, YMX3, YMN3)
      YMIN = min(YMN1,YMN2,YMN3)
      YMAX = max(YMX1,YMX2,YMX3)
C---- Set up ordinate limits
      call TITAN     (YMIN, YMAX, BOT, TOP)
      BOT = max(BOT,-THREE)
      TOP = min(TOP, THREE)
C
      if(DUMP) then
        call MESHED  ('MANNA', 2)
        write (LUEO,100) YMN1,YMN2,YMN3,YMIN,YMX1,YMX2,YMX3,YMAX
  100   format(' ','min',1P4E12.4/
     $         ' ','max',  4E12.4)
      end if
C
      if((YMAX.gt.YMIN).and.(YMAX.gt.ZERO).and.(YMIN.gt.ZERO)) then
C----   Set up abscissa limits
        call LUTALE  (WTAB, RCN, K, PROGLI, XL, XU)
C
        if(DUMP) then
          write (LUEO,101) XL,XU,BOT,TOP
  101     format(' ','XL,XU,BOT,TOP=',1P4E12.4)
        end if
C
        if(TOP.gt.BOT) then
C----     Initialize plot image
          call MELIC (IMAGE, XL, XU, BOT, TOP)
          KODE = 1
        end if
      else
        call DANIEL  (NO, LINE, YMAX, YMIN)
      end if
C
      if(DUMP) then
        call MASHED  ('MANNA')
      end if
C     !END
      call BYE ('MANNA')
C
      return
      end
