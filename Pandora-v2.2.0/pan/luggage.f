      subroutine LUGGAGE
     $(ITAU,JLEV,XNU,CP,RK,TRMN,TRMX,TR,DUMP1,DUMP2)
C
C     Rudolf Loeser, 1984 Apr 11
C---- Computes the effective radiation temperature at depth ITAU.
C     (This is version 2 of LUGGAGE.)
C     !DASH
      save
C     !DASH
      real*8 CP, CRIT, ONE, RK, RK1, RK2, RKN, TR, TR1, TR2, TRFLI,
     $       TRMN, TRMX, TRP, XNU, ZERO
      integer IFLGRK, IFLGTR, ITAU, ITER, ITMX, JLEV
      logical DUMP1, DUMP2
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
      equivalence (RZQ(117),TRFLI)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  CNIDOS, FULMAR, COMPD, STRAP, GRIP, HI, BYE
      intrinsic abs, max
C
C               XNU(NSL), CP(NSL+1)
      dimension XNU(*),   CP(*)
C
      data CRIT,ITMX /1.D-5, 20/
C     !EJECT
C
      call HI ('LUGGAGE')
C     !BEG
C---- Initialize
      TR1 = TRMN
      TR2 = TRMX
      TR  = ZERO
      call CNIDOS   (JLEV, XNU, CP, TR1, RK1)
      call CNIDOS   (JLEV, XNU, CP, TR2, RK2)
C
C---- Iterate
      do 100 ITER = 1,ITMX
        TRP = TR
        call FULMAR (TR1, RK1, TR2, RK2, TRFLI, TR, RK)
        call CNIDOS (JLEV, XNU, CP, TR, RKN)
        call COMPD  (TR, TRP, CRIT, IFLGTR)
        call COMPD  (RK, RKN, CRIT, IFLGRK)
        if(DUMP1) then
          call GRIP (ITER, JLEV, ITAU, TR1, RK1, TR2, RK2, TRFLI, TR,
     $               RKN, RK)
        end if
C
        if((IFLGTR.eq.0).or.(IFLGRK.eq.0)) goto 101
C
        if(abs(RK-RK1).le.abs(RK-RK2)) then
          TR2 = TR
          RK2 = RKN
        else
          TR1 = TR
          RK1 = RKN
        end if
  100 continue
C
C---- Error
      if(DUMP2) then
        call STRAP  (JLEV, ITAU, TRMN, TRMX, TR1, RK1, TR2, RK2, TR,
     $               RKN, RK)
      end if
      TR = ZERO
C
C---- That's all
  101 continue
C     !END
      call BYE ('LUGGAGE')
C
      return
      end
