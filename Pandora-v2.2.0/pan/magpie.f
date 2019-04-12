      subroutine MAGPIE
     $(A,B,W,K)
C
C     Rudolf Loeser, 1974 Jan 15
C---- Determines an iteration summary code, for JAY.
C     Returns the code in W, and returns K=1 if W is blank,
C                                        K=0 if W is nonblank.
C     !DASH
      save
C     !DASH
      real*8 A, B, BELT, ONE, TWO, WALL, ZERO
      integer K, LAB, LBC, MBC
      character BLANK*1, MINUS*1, W*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(40),MINUS )
C
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
      external COMPD, HI, BYE
C
      data  BELT, WALL /2.D-2, 2.D-1/
C
      call HI ('MAGPIE')
C     !BEG
      W = BLANK
      K = 1
      if(B.lt.ZERO) then
        W = MINUS
        K = 0
C     !EJECT
      else
        if((A+B).ne.TWO) then
          call COMPD (A, B,   BELT, LAB)
          call COMPD (B, ONE, BELT, LBC)
          call COMPD (B, ONE, WALL, MBC)
          if(LAB.lt.0) then
            if(LBC.lt.0) then
              if(MBC.eq.0) then
                W = ALPHS(13)
              else
                W = ALPHS(12)
              end if
              K = 0
            else if(LBC.gt.0) then
              if(MBC.eq.0) then
                W = ALPHS(15)
              else
                W = ALPHS(16)
              end if
              K = 0
            end if
          else if(LAB.eq.0) then
            if(LBC.ne.0) then
              if(MBC.eq.0) then
                W = ALPHS(13)
              else
                W = ALPHS(12)
              end if
              K = 0
            end if
          else
            if(LBC.lt.0) then
              if(MBC.eq.0) then
                W = ALPHS(15)
              else
                W = ALPHS(16)
              end if
              K = 0
            else if(LBC.gt.0) then
              if(MBC.eq.0) then
                W = ALPHS(13)
              else
                W = ALPHS(12)
              end if
              K = 0
            end if
          end if
        end if
      end if
C     !END
      call BYE ('MAGPIE')
C
      return
      end
