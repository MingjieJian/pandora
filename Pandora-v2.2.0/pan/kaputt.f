      subroutine KAPUTT
     $(IQHEA,IHEDF,JHEAS,DUMP,DOIT)
C
C     Rudolf Loeser, 1999 Jul 13
C---- Decides whether to do depth-dependent Helium abundance.
C     (This is version 2 of KAPUTT.)
C     !DASH
      save
C     !DASH
      integer IHEDF, IQHEA, JHEAS, LUEO
      logical DOIT, DUMP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, DASHER, HI, BYE
C
      call HI ('KAPUTT')
C     !BEG
      DOIT = .false.
      if((IQHEA.gt.0).and.(IHEDF.eq.0)) then
        if(JHEAS.eq.0) then
          if(DUMP) then
            call LINER  (5, LUEO)
            call DASHER (LUEO)
            call LINER  (1, LUEO)
            write (LUEO,100) JHEAS
  100       format(' ','Depth-dependent He abundance calculation is ',
     $                 'disabled because it does not work (JHEAS =',
     $                 I2,').   (1999 Jul 13)')
            call LINER  (1, LUEO)
            call DASHER (LUEO)
            call LINER  (5, LUEO)
          end if
        else
          DOIT = .true.
        end if
      end if
C     !END
      call BYE ('KAPUTT')
C
      return
      end
