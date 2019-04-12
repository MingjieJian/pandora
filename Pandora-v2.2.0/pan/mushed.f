      subroutine MUSHED
     $(CALLER,KODE,KILROY)
C
C     Rudolf Loeser, 2002 Oct 18
C---- Controls multiple appearances of MESHED.
C     !DASH
      save
C     !DASH
      integer KODE
      logical KILROY
      character CALLER*(*)
C     !DASH
      external MESHED, HI, BYE
C
      call HI ('MUSHED')
C     !BEG
      if(KILROY) then
        call MESHED (CALLER, KODE)
        KILROY = .false.
      end if
C     !END
      call BYE ('MUSHED')
C
      return
      end
