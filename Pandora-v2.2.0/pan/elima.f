      subroutine ELIMA
     $(ABD,KODE,STOP)
C
C     Rudolf Loeser, 1987 Nov 16
C---- Gets abundances of C, O and H.
C     !DASH
      save
C     !DASH
      real*8 ABD, ONE, ZERO, dummy
      integer KODE, KODE1, KODE2
      logical STOP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external FRANK, SAGA, HI, BYE
C
      dimension ABD(2)
C
      data GO /.false./
C
      call HI ('ELIMA')
C     !BEG
C---- Get Carbon abundance
      call FRANK  ('C  ', 0, ABD(1), dummy, dummy, dummy, KODE1)
      if((KODE1.eq.0).or.(ABD(1).eq.ZERO)) then
C----   Error - print message
        call SAGA ('C  ', 'ELIMA', STOP)
      end if
C
C---- Get Oxygen abundance
      call FRANK  ('O  ', 0, ABD(2), dummy, dummy, dummy, KODE2)
      if((KODE2.eq.0).or.(ABD(2).eq.ZERO)) then
C----   Error - print message
        call SAGA ('O  ', 'ELIMA', STOP)
      end if
C
C---- Set up Hydrogen abundance
      ABD(3) = ONE
C
      if((KODE1.eq.0).or.(KODE2.eq.0)) then
        KODE = 0
      else
        KODE = 1
      end if
C     !END
      call BYE ('ELIMA')
C
      return
      end
