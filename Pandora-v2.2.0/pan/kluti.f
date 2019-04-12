      subroutine KLUTI
     $(N,HEABL,RHEAB,EDIT)
C
C     Rudolf Loeser, 1991 Jan 08
C---- Edits RHEAB, for THALIA.
C     !DASH
      save
C     !DASH
      real*8 HEABL, ONE, OOHL, RHEAB
      integer I, N
      logical EDIT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
C               RHEAB(N)
      dimension RHEAB(*)
C
      call HI ('KLUTI')
C     !BEG
      EDIT = .false.
      OOHL = ONE/HEABL
      do 100 I = 1,N
        if(RHEAB(I).gt.HEABL) then
          EDIT =. true.
          RHEAB(I) = HEABL
        else if(RHEAB(I).lt.OOHL) then
          EDIT = .true.
          RHEAB(I) = OOHL
        end if
  100 continue
C     !END
      call BYE ('KLUTI')
C
      return
      end
