      subroutine LID
     $(QNAME,KODE)
C
C     Rudolf Loeser, 1987 Oct 30
C---- Sets up input files for use, for CHIRON.
C     (This is version 6 of LID.)
C     !DASH
      save
C     !DASH
      integer KODE, LUAT, LUIN, LUMO, LURE
      character QNAME*8
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
      equivalence (LUNITS( 2),LUMO )
      equivalence (LUNITS( 3),LUAT )
      equivalence (LUNITS(20),LURE )
      equivalence (LUNITS( 1),LUIN )
C     !DASH
      external GABON, MUNI, HALT, HI, BYE
C
      call HI ('LID')
C     !BEG
      if     (QNAME.eq.'INPUT'  ) then
                                  call GABON (LUIN, KODE)
C
      else if(QNAME.eq.'MODEL'  ) then
                                  call GABON (LUMO, KODE)
C
      else if(QNAME.eq.'ATOM'   ) then
                                  call GABON (LUAT, KODE)
C
      else if(QNAME.eq.'RESTART') then
                                  call GABON (LURE, KODE)
C
      else if(QNAME.eq.'GENERAL') then
                                  call MUNI
C
      else
        write (MSSLIN(1),100) QNAME
  100   format('NAME =',A8,' , which is unrecognizable.')
        call HALT  ('LID', 1)
      end if
C     !END
      call BYE ('LID')
C
      return
      end
