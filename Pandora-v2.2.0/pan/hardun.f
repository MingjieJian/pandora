      subroutine HARDUN
     $(OPF,NCR,IQINC,IDNRT,NO)
C
C     Rudolf Loeser, 1981 Jan 02
C---- Prints incident radiation information, for LINTEL.
C     !DASH
      save
C     !DASH
      real*8 OPF
      integer IDNRT, IQINC, NCR, NO
C     !DASH
      external LINER, HI, BYE
C
      call HI ('HARDUN')
C     !BEG
      if(NCR.gt.0) then
        call LINER (1,NO)
        write (NO,100)
  100   format(' ',34X,'Incident Coronal Radiation is specified, ',
     $                 '(see tabulation below)')
      end if
C
      if(IQINC.gt.0) then
        call LINER (1,NO)
        write (NO,101) OPF,IDNRT
  101   format(' ','OPF     ',1PE12.4,14X,
     $             'opacity factor for Lyman Incident Radiation term',
     $             34X,'(IDNRT =',I2,')')
      end if
C     !END
      call BYE ('HARDUN')
C
      return
      end
