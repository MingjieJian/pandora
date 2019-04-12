      subroutine LOTHAR
     $(IU,IL,KODE,LINE,NC)
C
C     Rudolf Loeser, 1994 Aug 30
C---- Encodes a transition heading.
C     !DASH
      save
C     !DASH
      integer IL, IU, KODE, NC
      character LAB*4, LINE*12
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
      dimension LAB(4)
C
      data LAB /'BACK', 'LINE', 'POST', 'PROF'/
C
      call HI ('LOTHAR')
C     !BEG
      if((KODE.lt.1).or.(KODE.gt.4)) then
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is not 1, 2, 3, or 4.')
        call HALT ('LOTHAR', 1)
      end if
C
      if(IU.gt.9) then
        if(IL.gt.9) then
          write (LINE,101) LAB(KODE),IU,IL
  101     format(A4,' (',I2,'/',I2,')')
          NC = 12
        else
          write (LINE,102) LAB(KODE),IU,IL
  102     format(A4,' (',I2,'/',I1,')')
          NC = 11
        end if
      else
        if(IL.gt.9) then
          write (LINE,103) LAB(KODE),IU,IL
  103     format(A4,' (',I1,'/',I2,')')
          NC = 11
        else
          write (LINE,104) LAB(KODE),IU,IL
  104     format(A4,' (',I1,'/',I1,')')
          NC = 10
        end if
      end if
C     !END
      call BYE ('LOTHAR')
C
      return
      end
