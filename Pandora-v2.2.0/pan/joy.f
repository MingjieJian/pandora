      subroutine JOY
     $(LU,EDITED,OFCE)
C
C     Rudolf Loeser, 2003 Jun 20
C---- Prints an explanation for EBISSA.
C     (This is version 3 of JOY.)
C     !DASH
      save
C     !DASH
      integer IQEND, JEDIT, LU, MBREC
      logical EDITED, OFCE
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(194),JEDIT)
      equivalence (KZQ(195),MBREC)
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(306),IQEND)
C     !DASH
      external LINER, HI, BYE
C     !EJECT
C
      call HI ('JOY')
C     !BEG
      if(LU.gt.0) then
        call LINER (1, LU)
C
        if(IQEND.gt.0) then
          if(EDITED) then
            write (LU,100) JEDIT
  100       format(' ','Option NEDIT=on (JEDIT =',I6,') and some ',
     $                 'N-values were edited to prevent negative ',
     $                 'values of Line Source Function.')
          else
            write (LU,101) JEDIT
  101       format(' ','Option NEDIT=on (JEDIT =',I6,') but no ',
     $                 'N-values were actually changed.')
          end if
        else
          write (LU,102)
  102     format(' ','Option NEDIT=off; thus N-values were not edited ',
     $               'to prevent negative values of LSF.')
        end if
C
        if(MBREC.eq.1) then
          write (LU,103) MBREC, 'were'
  103     format(' ','MBREC =',I2,': B-values ',A,' edited to remain ',
     $               'consistent with N-values.')
        else
          write (LU,103) MBREC, 'were not'
        end if
C
        write (LU,104)
  104   format(' ','(B-editing mainly is intended for use together ',
     $             'with N-editing; thus MBREC = 1 by default.)')
C
        if(OFCE) then
          write (LU,105)
  105     format(' ','All values of FCE (CE enhancement) = 1.')
        else
          write (LU,106)
  106     format(' ','Some values of FCE (CE enhancement) differ ',
     $               'from 1.')
        end if
      end if
C     !END
      call BYE ('JOY')
C
      return
      end
