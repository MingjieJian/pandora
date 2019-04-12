      subroutine FOSS
     $(COOL,COOLI,HEAT,HEATI)
C
C     Rudolf Loeser, 1989 Aug 04
C---- Saves debug checksums, for Cooling Rates calculation.
C     !DASH
      save
C     !DASH
      real*8 COOL, COOLI, HEAT, HEATI
      integer N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external CHECKER, HI, BYE
C
C               COOL(N), COOLI(N), HEAT(N), HEATI(N)
      dimension COOL(*), COOLI(*), HEAT(*), HEATI(*)
C
      call HI ('FOSS')
C     !BEG
      call CHECKER (COOL ,1,N,' Cooling rate')
      call CHECKER (COOLI,1,N,' Integrated cooling rate')
      call CHECKER (HEAT ,1,N,' Heating rate')
      call CHECKER (HEATI,1,N,' Integrated heating rate')
C     !END
      call BYE ('FOSS')
C
      return
      end
