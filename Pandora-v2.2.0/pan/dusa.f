      subroutine DUSA
     $(DDR,CDR,K,FRAC,DELTA,DH,CH)
C
C     Rudolf Loeser, 1992 Apr 09
C---- Eliminates "minor components" for TABOR.
C     (This is version 5 of DUSA.)
C     !DASH
      save
C     !DASH
      real*8 C, CDR, CH, CRIT, D, DDR, DELTA, DH, F, FLEFT, FRAC, FRITE,
     $       ONE, TWO, ZERO
      integer FLAG, IMAX, IMIN, K, NH
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external CLAUDE, MINMAXD, COMPD, MELVIN, WESLEY, HI, BYE
C
C               DDR(K), CDR(K), DH(K/2), CH(K/2)
      dimension DDR(*), CDR(*), DH(*),   CH(*)
C     !EJECT
C
      call HI ('DUSA')
C     !BEG
C---- Extract half sets
      call CLAUDE     (DDR,CDR,K,DH,CH,NH)
C
  100 continue
C----   Set up deletion criterion
        call MINMAXD  (CH,1,NH,IMIN,IMAX)
        CRIT = CH(IMAX)*FRAC
        if(DH(IMIN).eq.ZERO) then
          F = TWO
        else
          F = ONE
        end if
        C = CH(IMIN)*F
C
        call COMPD    (C,CRIT,DELTA,FLAG)
        if(FLAG.lt.0) then
C----     Eliminate this one...
          if(IMIN.le.1) then
C----       ...by merging to right
            CH(2) = CH(2)+CH(1)
          else if(IMIN.ge.NH) then
C----       ...by merging to left
            CH(IMIN-1) = CH(IMIN-1)+CH(IMIN)
          else
C----       ...by distributing to both sides
            D = (DH(IMIN+1)-DH(IMIN-1))
            if(D.le.ZERO) then
              go to 101
            end if
            FLEFT = (DH(IMIN+1)-DH(IMIN  ))/D
            FRITE = (DH(IMIN  )-DH(IMIN-1))/D
            CH(IMIN-1) = CH(IMIN-1)+CH(IMIN)*FLEFT
            CH(IMIN+1) = CH(IMIN+1)+CH(IMIN)*FRITE
          end if
          CH(IMIN) = ZERO
          call MELVIN (DH,CH,NH)
          if(NH.gt.1) go to 100
C
          K = 1
          DDR(K) = ZERO
          CDR(K) = ONE
          go to 102
        end if
  101 continue
C---- Restore full sets
      call WESLEY     (DH,CH,NH,DDR,CDR,K)
C
  102 continue
C     !END
      call BYE ('DUSA')
C
      return
      end
