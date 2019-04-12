      subroutine ACARI
C
C     Rudolf Loeser, 1987 Mar 22
C---- Dumps for BRABANT.
C     !DASH
      save
C     !DASH
      integer LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
      call HI ('ACARI')
C     !BEG
      call LINER (4, LUEO)
      write (LUEO,100)
  100 format(' ','>>>>>     Details of integrated flux accumulation.')
      call LINER (2, LUEO)
C     !END
      call BYE ('ACARI')
C
      return
      end
