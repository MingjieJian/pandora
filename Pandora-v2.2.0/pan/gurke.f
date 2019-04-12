      subroutine GURKE
     $(LU,N,ABD,IN,HND,CON,CHN,OHN,RABD)
C
C     Rudolf Loeser, 2006 Dec 29
C---- Modifies C or O RABD.
C     Input IN = 1 means Carbon, = 2 means Oxygen.
C     !DASH
      save
C     !DASH
      real*8 ABD, CHN, CON, CRIT, FAC, HND, OHN, ONE, RABD, RAT, RCOMN
      integer I, IN, LU, N
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
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
      equivalence (RZQ(148),RCOMN)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external  DIVIDE, LINER, DASHER, VECOUT, HALT, HI, BYE
      intrinsic max
C
C               RABD(N), ABD(3), HND(N), CON(N), CHN(N), OHN(N)
      dimension RABD(*), ABD(*), HND(*), CON(*), CHN(*), OHN(*)
C
      data CRIT /1.D-12/
C
      call HI ('GURKE')
C     !BEG
      if((IN.lt.1).or.(IN.gt.2)) then
        write (MSSLIN(1),100) IN
  100   format('IN =',I12,'; which is neither 1 nor 2.')
        call HALT ('GURKE', 1)
      end if
C
      do 101 I = 1,N
        if(IN.eq.1) then
          call DIVIDE ((CON(I)+CHN(I)), (ABD(1)*HND(I)), RAT)
        else
          call DIVIDE ((CON(I)+OHN(I)), (ABD(2)*HND(I)), RAT)
        end if
        if(RAT.ge.CRIT) then
          FAC = ONE-RAT
          FAC = max(FAC,RCOMN)
        else
          FAC = ONE
        end if
C
        RABD(I) = FAC*RABD(I)
  101 continue
C
      if(LU.gt.0) then
        call LINER    (2, LU)
        call DASHER   (LU)
        call VECOUT   (LU, RABD, N, 'RABD, updated for molecules')
        call LINER    (1, LU)
        call DASHER   (LU)
      end if
C     !END
      call BYE ('GURKE')
C
      return
      end
