      subroutine BUSTLE
     $(X,W,IW,WVL,CDW,DNU,XI,A,K,GOODX,GOODY,LAB,YUSED,YES)
C
C     Rudolf Loeser, 2003 Mar 11
C---- Makes an augmented full XI table, and computes corresponding As,
C     as necessary.
C
C     NOTE:  K < KM .
C
C     (This is version 2 of BUSTLE.)
C     !DASH
      save
C     !DASH
      real*8 A, CDW, DNU, W, WVL, X, XI, Y, YUSED
      integer IW, K
      logical GOODX, GOODY, YES
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
C     !DASH
      external PALLE, COWRY, HARE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XI(KM), A(KM)
      dimension XI(*),  A(*)
C
      call HI ('BUSTLE')
C     !BEG
C---- Make augmented, sorted XI
      call PALLE   (X, W, IW, WVL, CDW, DNU, XI, K, YES)
C
      if(YES) then
C----   Check K
        call HARE  (K, GOODY)
C----   Compute corresponding As (summation weights)
        call COWRY (XI, K, Y, A, W, IW, LAB, YUSED, GOODX)
      end iF
C     !END
      call BYE ('BUSTLE')
C
      return
      end
