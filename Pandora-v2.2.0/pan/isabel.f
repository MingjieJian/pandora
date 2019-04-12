      subroutine ISABEL
C
C     Rudolf Loeser, 1984 Mar 06
C---- Prints random access records index for DIANA/ORION Data Blocks.
C     (This is version 2 of ISABEL.)
C     !DASH
      save
C     !DASH
      integer I, IQIXD, LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
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
      equivalence (IQQ(174),IQIXD)
C
C---- URANUS      as of 2005 Dec 12
      integer     LEMUR
      parameter   (LEMUR=30000)
      integer     LIMPID,IUOP,ILOP,NBOP,MBOP,KEROP,INDOP
      real*8      OPNAM
      dimension   OPNAM(LEMUR),KEROP(LEMUR),INDOP(LEMUR)
      common      /URANUS1/ LIMPID,IUOP,ILOP,NBOP,MBOP
      common      /URANUS2/ KEROP
      common      /URANUS3/ INDOP
      common      /URANUS4/ OPNAM
C     Record index for Diana/Orion Data Blocks.
C
C     KEROP .eq. 0: acceptable data block, use it;
C     KEROP .gt. 0: defective data block, do not use it.
C     (The various values of KEROP signify different error conditions).
C     .
C     !DASH
C     !EJECT
      external MESHED, MASHED, LINER, HI, BYE
C
      call HI ('ISABEL')
C     !BEG
      if(IQIXD.gt.0) then
        call MESHED ('ISABEL', 2)
        write (LUEO,100)
  100   format(' ','Random access file record index of DIANA/ORION ',
     $             'Data Blocks.'//
     $         ' ',30X,'OPNAM',9X,'Address')
        call LINER  (1, LUEO)
C
        write (LUEO,101) (I,OPNAM(I),INDOP(I),I=1,MBOP)
  101   format(5(' ',I5,1PE30.16,I16/))
C
        call LINER  (2, LUEO)
        write (LUEO,102)
  102   format(' ',111X,'(Option INDXDMP)')
        call MASHED ('ISABEL')
      end if
C     !END
      call BYE ('ISABEL')
C
      return
      end
