      subroutine BERGAMO
     $(X,DDL,LDL,WVL,CDW,DNU,GOODX,GOODY,LAB,YUSED,W,IW,XIF,KF,
     $ BXIX,BAX,BDLX,KX)
C
C     Rudolf Loeser, 1989 Feb 02
C---- Makes special XI, A and DL tables for a blended line,
C     leaving them in the appropriate slots of the corresponding
C     Line Intensity Data Block.
C
C     This "special XI table" (i.e. BXIX, length KX) is obtained
C     from LDL (overlapping) copies of XIF.
C     The corresponding summation weights (BAX) and
C     Delta-Lambdas (BDLX) are then computed from BXIX.
C     !DASH
      save
C     !DASH
      real*8 BAX, BDLX, BXIX, CDW, DDL, DNU, W, WVL, X, XIF, Y, YUSED
      integer IN, IOFF, IPNT, IS, IW, IWS, JN, KF, KMMAX, KX, LDL, MOX,
     $        MUX
      logical GOODX, GOODY
      character LAB*(*)
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
      equivalence (RZQ(  8),Y    )
      equivalence (KZQ(132),KMMAX)
C     !DASH
C     !EJECT
      external COHOSH, COWRY, DEBRIS, SIDA, MAGNUS, IGIVE, WGIVE,
     $         HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               DDL(LDL), BXIX(KM), BAX(KM), BDLX(KM), XIF(KM)
      dimension DDL(*),   BXIX(*),  BAX(*),  BDLX(*),  XIF(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IOFF  )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IPNT  )
C
      call HI ('BERGAMO')
C     !BEG
C     (Get W & IW allotments)
      call SIDA   (IN, IS,  MOX, 'BERGAMO')
      call MAGNUS (JN, IWS, MUX, 'BERGAMO')
C
C---- Use the "candidate" full XI-table (XIF[KF]), together with
C     DDL[LDL] and CDW, to make a "special" XI-table (BXIX[KX])
      call COHOSH (X, LDL, DDL, WVL, CDW, DNU, KF, XIF, KMMAX, KX,
     $             BXIX, W(IOFF), IW(IPNT), W, IW, GOODY)
C---- Compute corresponding summation weights (BAX[KX])
      call COWRY  (BXIX, KX, Y, BAX, W, IW, LAB, YUSED, GOODX)
C---- Compute corresponding Delta-Lambda (BDLX[KX])
      call DEBRIS (BXIX, KX, CDW, BDLX)
C
C     (Give back W & IW allotments)
      call WGIVE  (W,  'BERGAMO')
      call IGIVE  (IW, 'BERGAMO')
C     !END
      call BYE ('BERGAMO')
C
      return
      end
