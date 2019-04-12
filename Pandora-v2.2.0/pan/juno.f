      subroutine JUNO
C
C     Rudolf Loeser, 2005 Apr 14
C---- Writes a LAMBDA-header, if needed.
C     (This is version 2 of JUNO.)
C     !DASH
      save
C     !COM
C---- MUNUXI      as of 2005 Apr 14
      logical     LAMHED,LAMDMP
      common      /MUNUXI/ LAMHED,LAMDMP
C     Subroutine "LAMBDA" extra printout header control.
C     .
C     !DASH
      external MESHED, HI, BYE
C
      call HI ('JUNO')
C     !BEG
      if(.not.LAMHED) then
        LAMHED = .true.
        call MESHED ('LAMBDA', 4)
      end if
C     !END
      call BYE ('JUNO')
C
      return
      end
