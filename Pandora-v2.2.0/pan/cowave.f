      subroutine COWAVE
     $(J,K,ISO,METH,EN)
C
C     Rudolf Loeser, 1992 Nov 02
C---- Computes the energy of the level with quantum numbers
C     j=J and v=K of the CO molecule.
C     !DASH
      save
C     !DASH
      real*8 EN
      integer ISO, J, K, METH
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external COWAV12, COWVX12, COWAV13, COWVX13, HALT, HI, BYE
C
      call HI ('COWAVE')
C     !BEG
      if(ISO.eq.12) then
        if(METH.eq.1) then
          call COWAV12 (J,K,EN)
        else
          call COWVX12 (J,K,EN)
        end if
      else if(ISO.eq.13) then
        if(METH.eq.1) then
          call COWAV13 (J,K,EN)
        else
          call COWVX13 (J,K,EN)
        end if
      else
        write (MSSLIN(1),100) ISO
  100   format('ISO =',I12,' (C isotope #) does not = 12 or 13.')
        call HALT ('COWAVE',1)
      end if
C     !END
      call BYE ('COWAVE')
C
      return
      end
