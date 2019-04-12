      subroutine JETLAG
     $(KODE)
C
C     Rudolf Loeser, 2005 May 17
C---- Suspends (KODE=1) or resumes (KODE=2) printing (i.e. writing
C     to the output file), for PRD-iterations.
C     (This is version 2 of JETLAG.)
C
C     The PRD-iterations are a very late addition to PANDORA.
C     They span many deeply-nested routines that use unit MO as a
C     printing switch. Rather than pass another switch down many levels
C     of the hierarchy, I decided to resort to fiddling with MO.
C
C     I am (painfully) aware this isn't clean, but it will have to do
C     for the moment.
C                                               RL, 2005 May 17
C
C     !DASH
      save
C     !DASH
      integer FLAG, KODE, MO, MOSAVE
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external HI, BYE
C
      parameter (FLAG = -317)
      data      MOSAVE /FLAG/
C
      call HI ('JETLAG')
C     !BEG
      if(KODE.eq.1) then
        MOSAVE = MO
        MO     = 0
      else
        if(MOSAVE.ne.FLAG) then
          MO   = MOSAVE
        end if
      end if
C     !END
      call BYE ('JETLAG')
C
      return
      end
