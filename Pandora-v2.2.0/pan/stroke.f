      subroutine STROKE
     $(CODSRW,NSHL,MM,INC)
C
C     Rudolf Loeser, 2003 Aug 27
C---- Checks index and code combinations for STORK, and aborts if bad.
C     !DASH
      save
C     !DASH
      real*8 CODSRW, ONE, ZERO
      integer INC, LUEO, MM, NSHL
      logical CDBAD, MMBAD
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
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
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HALT, MESHED, VECOUT, ABORT, HI, BYE
C
C               CODSRW(NSHL)
      dimension CODSRW(*)
C     !EJECT
C
      call HI ('STROKE')
C     !BEG
      if((INC.lt.1).or.(INC.gt.2)) then
        write (MSSLIN(1),100) INC
  100   format('INC =',I12,', which is neither 1 nor 2.')
        call HALT   ('STROKE', 1)
      end if
C
      if(INC.eq.2) then
        MMBAD = ((MM-2).lt.1).or.((MM+2).gt.NSHL)
        CDBAD = (CODSRW(MM-2).ne.ZERO).or.(CODSRW(MM+2).ne.ZERO)
      else
        MMBAD = ((MM-1).lt.1).or.((MM+1).gt.NSHL)
        CDBAD = (CODSRW(MM-1).eq.ONE).or.(CODSRW(MM+1).eq.ONE)
      end if
C
      if(MMBAD.or.CDBAD) then
        call MESHED ('STROKE', 1)
        write (LUEO,101) MM,INC,NSHL
  101   format('MM =',I12,', INC =',I2,', NSHL =',I12)
        call VECOUT (LUEO, CODSRW, NSHL, 'CODSRW')
        call ABORT
      end if
C     !END
      call BYE ('STROKE')
C
      return
      end
