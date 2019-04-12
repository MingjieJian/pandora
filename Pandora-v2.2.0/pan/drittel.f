      subroutine DRITTEL
     $(NAME,INDX,NOPAC,N,CO,CB,ML,CABS,CEMI,KEMIT)
C
C     Rudolf Loeser, 2004 Apr 22
C---- Dumps background line calculation results.
C     !DASH
      save
C     !DASH
      real*8 CABS, CB, CEMI, CO
      integer I, INDX, KEMIT, LUEO, ML, N, NOPAC
      character NAME*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, ARROUT, SHIM, MESHED, MASHED, HI, BYE
C
C               CO(Nopac,N), CB(Nopac,N), CABS(N,Nlin), CEMI(N,Nlin)
      dimension CO(NOPAC,*), CB(NOPAC,*), CABS(*),      CEMI(*)
C
      call HI ('DRITTEL')
C     !BEG
      call MESHED   ('DRITTEL', 2)
C
      write (LUEO,100) NAME,INDX
  100 format(' ','Absorption and emission of ',A,' lines, ',
     $           'contributor #',I3)
C
      call ARROUT   (LUEO, CABS, N, ML, 'Individual opacities')
      if(KEMIT.gt.0) then
        call ARROUT (LUEO, CEMI, N, ML, 'Individual source functions')
      end if
C
      call LINER    (2, LUEO)
      write (LUEO, 101)
  101 format(' ',12X,'total opacity',4X,'total num. of BHS')
      call LINER    (1, LUEO)
      do 103 I = 1,N
        write (LUEO,102) I,CO(INDX,I),CB(INDX,I)
  102   format(' ',I5,1P2E20.12)
        call SHIM   (I, 5, LUEO)
  103 continue
C
      call MASHED   ('DRITTEL')
C     !END
      call BYE ('DRITTEL')
C
      return
      end
