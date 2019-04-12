      subroutine GRUNT
     $(LURR,LUMR,LUPR)
C
C     Rudolf Loeser, 1984 Jul 27
C---- Initializes (and opens) restart files.
C     !DASH
      save
C     !DASH
      integer JPOP, LUEO, LUMR, LUPR, LURR
      logical KILROY, POP
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(22),JPOP )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external DUCK, RUFF, HI, BYE
C
      data KILROY /.true./
C
      call HI ('GRUNT')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        POP    = JPOP.gt.0
C
C----   Open files
        call DUCK   (LURR, LUEO)
        call DUCK   (LUMR, LUEO)
        if(POP) then
          call DUCK (LUPR, LUEO)
        end if
      end if
C
C---- Initialize files files
      call RUFF     (LURR)
      call RUFF     (LUMR)
      if(POP) then
        call RUFF   (LUPR)
      end if
C     !END
      call BYE ('GRUNT')
C
      return
      end
