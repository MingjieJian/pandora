      subroutine MUNI
C
C     Rudolf Loeser, 1988 Apr 25
C---- Opens the current "general" input file.
C     !DASH
      save
C     !DASH
      integer LUEO, LUGI
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(13),LUGI )
      equivalence (LUNITS( 6),LUEO )
C
C---- SUKU        as of 1988 Apr 22
      integer     NAF
      parameter   (NAF=5)
      integer     KFLOPN,KODEGN
      character   FILNMS*8, FILSPEC*60
      dimension   KFLOPN(NAF),FILNMS(NAF)
      common      /SUKU1/ KFLOPN
      common      /SUKU2/ FILNMS
      common      /SUKU3/ KODEGN,FILSPEC
C     Names and in-use codes for the main and the auxiliary
C     input files.
C     1=INPUT,  2=MODEL,  3=ATOM,  4=RESTART,  5=GENERAL.
C     .
C
C---- DATAFIL     as of 1984 Apr 19
      integer     KIWILFN
      common      /DATAFIL/ KIWILFN
C     Number of unit from which to read input statements.
C     .
C     !DASH
C     !EJECT
      external LACK, MUCK, MESHED, MASHED, HI, BYE
C
      call HI ('MUNI')
C     !BEG
      if(KFLOPN(5).eq.1) then
C----   Close the file that is currently open on this unit
        call LACK   (LUGI, LUEO)
        KFLOPN(5) = 0
      end if
C
      if(KODEGN.eq.1) then
C----   Print message prior to attempt to open
        call MESHED ('MUNI', 3)
        write (LUEO,100) FILSPEC
  100   format(' ','Attempting to open the "general" input file: ',A)
        call MASHED ('MUNI')
      end if
C
C---- Now, open it
C
C     (This is the    O N L Y    place where LUGI is opened!)
C
      call MUCK     (LUGI, FILSPEC, LUEO)
      KFLOPN(5) = 1
      KIWILFN   = LUGI
C     !END
      call BYE ('MUNI')
C
      return
      end
