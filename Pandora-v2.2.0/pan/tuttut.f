      subroutine TUTTUT
     $(J,CHKA,SA,RCHK,CN1S,I,KZAUG,BANG,JZAUG,HANG,KSUM,KILROY,CALLER)
C
C     Rudolf Loeser, 2004 Jan 30
C---- Dumps, for SUMMIT.
C     !DASH
      save
C     !DASH
      real*8 CHKA, CN1S, RCHK, SA
      integer I, J, JZAUG, KSUM, KZAUG, LUEO
      logical BANG, HANG, KILROY
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, LINER, HI, BYE
C
      call HI ('TUTTUT')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call MESHED (CALLER, 2)
        write (LUEO,100) CN1S
  100   format(' ','Updating Z-augmentation indices.',10X,
     $             'CN1S ("N1lim") =',1PE12.4,10X,'(KZAUG aka "ZA")')
      end if
C     !EJECT
      call LINER (1, LUEO)
      write (LUEO,101) J,I
  101 format(' ','Augmented-Z table index J =',I6,' pertains to ',
     $           'regular-Z table index I =',I6)
      write (LUEO,102) CHKA,SA,RCHK
  102 format(' ','check(J) =',1PE16.8,'; s(J) =',E16.8,'; rcheck(J) =',
     $           E16.8)
      if(BANG) then
        if(HANG) then
          write (LUEO,103) KZAUG,JZAUG,KSUM
  103     format(' ','Augmentation counter KZAUG(I)   was increased ',
     $               'to',I5,', and'/
     $           ' ','augmentation counter KZAUG(I-1) was increased ',
     $               'to',I5,5X,'Ksum =',I6)
        else
          write (LUEO,104) 'was increased to',KZAUG,KSUM
  104     format(' ','Augmentation counter KZAUG(I)   ',A,I5,5X,
     $               'Ksum =',I6)
        end if
      else
        if(HANG) then
          write (LUEO,105) KZAUG,JZAUG,KSUM
  105     format(' ','Augmentation counter KZAUG(I)   was left ',
     $               'unchanged at ',I5,', and'/
     $           ' ','augmentation counter KZAUG(I-1) was increased ',
     $               'to',I5,5X,'Ksum =',I6)
        else
          write (LUEO,104) 'was left unchanged at',KZAUG,KSUM
        end if
      end if
C     !END
      call BYE ('TUTTUT')
C
      return
      end
