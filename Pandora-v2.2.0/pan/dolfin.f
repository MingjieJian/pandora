      subroutine DOLFIN
     $(J,JP,A,GII,XJNU,DUMP)
C
C     Rudolf Loeser, 2005 Feb 16
C---- Dumps, for GAFFE.
C     !DASH
      save
C     !DASH
      real*8 A, GII, XJNU
      integer J, JP, LUEO
      logical DUMP
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
      call HI ('DOLFIN')
C     !BEG
      if(DUMP) then
        call LINER (1, LUEO)
        write (LUEO,100) J,JP,A,XJNU,GII
  100   format(' ','J =',I8,', JP =',I8,5X,'A(JP) =',1PE14.6,5X,
     $             'JNU(JP) =',E14.6,5X,'GII(JP,J) =',E14.6)
      end if
C     !END
      call BYE ('DOLFIN')
C
      return
      end
