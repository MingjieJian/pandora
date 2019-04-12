      subroutine LISKO
     $(MN1,KINOUT,KBNDS,PALBET,PBETAL,PBETGM,PGMBET,KBINN,KBOUT,ALFB,
     $ BETB)
C
C     Rudolf Loeser, 1998 JUl 08
C---- Deals with boundary conditions for diffusion.
C     !DASH
      save
C     !DASH
      real*8 ALFB, BETB, DIV, ONE, PALBET, PBETAL, PBETGM, PGMBET, RAB,
     $       RGB, ZERO
      integer J, KBINN, KBNDS, KBOUT, KINOUT, MN1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external DIVIDE, HALT, HI, BYE
C
C               PALBET(N), PBETAL(N), PBETGM(N), PGMBET(N)
      dimension PALBET(*), PBETAL(*), PBETGM(*), PGMBET(*)
C     !EJECT
C
      call HI ('LISKO')
C     !BEG
      ALFB  = ZERO
      BETB  = ZERO
      KBINN = 0
      KBOUT = 0
C
      J = 0
      if(KBNDS.gt.0) then
        if(KINOUT.eq.1) then
          KBINN = 1
          J = 1
        else if(KINOUT.eq.2) then
          KBOUT = 1
          J = MN1
        end if
C
        if((KBINN+KBOUT).gt.1) then
          write (MSSLIN(1),100) KBINN,KBOUT
  100     format('KBINN =',I12,', KBOUT =',I12,'; their sum should ',
     $           'not exceed 1.')
          call HALT   ('LISKO',1)
        end if
C
        if(J.gt.0) then
          call DIVIDE (PBETAL(J),PALBET(J),RAB)
          call DIVIDE (PBETGM(J),PGMBET(J),RGB)
          DIV = ONE+RAB+RGB
          call DIVIDE (RAB,DIV,ALFB)
          call DIVIDE (ONE,DIV,BETB)
        end if
      end if
C     !END
      call BYE ('LISKO')
C
      return
      end
