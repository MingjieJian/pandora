      subroutine TRINITY
     $(CODES)
C
C     Rudolf Loeser, 1983 Mar 14
C---- Sets up Shell-Ray WN-matrix calculation codes.
C
C     CODE=0: compute directly;
C     CODE=1: obtain by single-step interpolation; and
C     CODE=2: obtain by double-step interpolation.
C
C     (This is version 2 of TRINITY.)
C     !DASH
      save
C     !DASH
      real*8 CODES, ONE, TWO
      integer I, J, MSKIP, NSHL, NTAN
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
      equivalence (KZQ( 45),NTAN )
      equivalence (KZQ( 61),MSKIP)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 4),NSHL )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !EJECT
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external ZERO1, HALT, HI, BYE
C
C               CODES(NSHL)
      dimension CODES(*)
C
      call HI ('TRINITY')
C     !BEG
      call ZERO1 (CODES, NSHL)
C
      if((NTAN.le.1).and.(MSKIP.ne.0)) then
        if(MSKIP.eq.1) then
          J = NSHL
          I = 0
  100     continue
            I = I+2
            if(I.lt.J) then
              CODES(I) = ONE
              goto 100
            end if
          continue
C
        else if(MSKIP.eq.3) then
          J = NSHL-1
          I = -1
  101     continue
            I = I+4
            if(I.lt.J) then
              CODES(I-1) = ONE
              CODES(I  ) = TWO
              CODES(I+1) = ONE
              goto 101
            end if
          continue
C
        else
          write (MSSLIN(1),102) MSKIP
  102     format('MSKIP =',I12,', which is neither 1 nor 3.')
          call HALT ('TRINITY', 1)
        end if
      end if
C     !END
      call BYE ('TRINITY')
C
      return
      end
