      subroutine ANKER
     $(DUMP,NMX,IND,I,DMPI)
C
C     Rudolf Loeser, 1984 Jul 06
C---- Sets up dump control, for fast-electrons calculations.
C     !DASH
      save
C     !DASH
      integer I, IND, J, NMX
      logical DMPI, DUMP
C     !DASH
      external HI, BYE
C
      call HI ('ANKER')
C     !BEG
      DMPI = .false.
      if(DUMP.and.(IND.ne.0)) then
        DMPI = NMX.eq.1
        if(.not.DMPI) then
          J = IND
          if(J.lt.0) then
            J = NMX/2
          end if
          DMPI = I.eq.J
        end if
      end if
C     !END
      call BYE ('ANKER')
C
      return
      end
