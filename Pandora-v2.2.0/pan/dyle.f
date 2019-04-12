      subroutine DYLE
     $(K)
C
C     Rudolf Loeser, 1990 Nov 29
C---- Computes the matrix DELCHX(n,l) for upper-level charge exchange.
C     For l .ge. 3  and  n .ge. l+1.
C     Since the maximum permitted value of n is NPQLM, the
C     maximum value of l is NPQLM-1.
C     !DASH
      save
C     !DASH
      real*8 AF, CON2, ELD, ELL, ELN, EN2, EN5, FAC, HALF, ONE, RAT,
     $       THREE, THRHLF, TWO
      integer K, L, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 4),THREE )
C
C---- XINGU       as of 1999 Sep 21
      real*8      AXED,BXED,RCHX,DELCHX
      character   NAMXED*3
      integer     NXI,NPQLM,NPQMX
      parameter   (NXI=10)
C     (Remember to change all users when changing NXI)
      parameter   (NPQLM=15)
C     (Maximum permitted value of principal quantum number n)
C     (NPQLM must not exceed LIMDAT(1) [in popdata.inc], the
C     number of levels in the Hydrogen population ion model.)
      dimension   AXED(NXI), BXED(NXI), NAMXED(NXI)
      dimension   RCHX(NPQLM,NPQLM), DELCHX(NPQLM,NPQLM)
      common      /XINGU1/ AXED,BXED,RCHX,DELCHX
      common      /XINGU2/ NAMXED
      common      /XINGU3/ NPQMX
C---- Charge Exchange data tables
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external HALT, ZERO1, RIGEL, HI, BYE
C
      data THRHLF, FAC /1.5D0, 1.09735D5/
C
      call HI ('DYLE')
C     !BEG
      if((K.lt.1).or.(K.gt.NXI)) then
        write (MSSLIN(1),100) K,NXI
  100   format('K =',I12,', which is .lt. 1 or .gt. NXI =',I12)
        call HALT ('DYLE', 1)
      end if
C
      call ZERO1  (DELCHX, (NPQLM*(NPQLM-1)))
C
      call RIGEL (2, CON2)
      AF = AXED(K)*FAC
C
      do 102 L = 3,(NPQMX-1)
        ELL = L
        ELN = ELL*(ELL+ONE)
        ELD = (ELL-HALF)*(ELL)*(ELL+HALF)*(ELL+ONE)*(ELL+THRHLF)
C
        do 101 N = (L+1),NPQMX
          EN2 = N**2
          EN5 = N**5
C
          RAT = AF*(THREE*EN2-ELN)/(TWO*EN5*ELD)
          DELCHX(N,L) = CON2*((BXED(K)/EN2)+RAT)
C
  101   continue
  102 continue
C     !END
      call BYE ('DYLE')
C
      return
      end
