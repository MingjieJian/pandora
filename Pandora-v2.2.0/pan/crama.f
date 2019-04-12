      subroutine CRAMA
     $(DUMP,ITER,ITN1R,KODE)
C
C     Rudolf Loeser, 1999 Feb 19
C---- Diffusion-printout marker.
C     !DASH
      save
C     !DASH
      integer ITER, ITN1R, KODE, LUEO
      logical DUMP
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
      call HI ('CRAMA')
C     !BEG
      if(DUMP) then
        call LINER    (2, LUEO)
        if(KODE.eq.1) then
          call DASHER (LUEO)
          write (LUEO,100) 'Start',ITER,ITN1R
  100     format(' ','$$$$$ ',A,' N1-iteration',I3,' of',I3,'.')
        else if(KODE.eq.2) then
          write (LUEO,100) 'End',ITER,ITN1R
          call DASHER (LUEO)
        end if
        call LINER    (2, LUEO)
      end if
C     !END
      call BYE ('CRAMA')
C
      return
      end
