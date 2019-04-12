      subroutine AARDVRK
     $(LU,IRUNT,LUSD)
C
C     Rudolf Loeser, 1979 May 25
C     RL/SGK revised Jul 14 2014 
C---- Ends PANDORA's printout.
C     (This is version 2.1 of AARDVRK.)
C     !DASH
      save
C     !DASH
      integer I, IRUNT, LU, LUSD
      character qummy*1
C     !COM
C---- VERSION     as of 2006 May 16
      real*8      VERSION
      integer     NVDSCR
      character   VDSCRPT*63
      dimension   VDSCRPT(45)
      common      /VERSION1/ VERSION
      common      /VERSION2/ NVDSCR
      common      /VERSION3/ VDSCRPT
C     Identifier and description of this version of PANDORA.
C     (Values set by subroutine AARDVRK.)
C     .
C---- CLOCK       as of 1998 Apr 02
      character   BEGDAT*11, ENDDAT*11, BEGTIM*8, ENDTIM*8
      common      /CLOCK/ BEGDAT,ENDDAT,BEGTIM,ENDTIM
C     .
C     !DASH
      external LINER, GET_DATE, GET_TIME, HOSTAGE, HI, BYE
C
C
      data NVDSCR /4/
C
C
      data VERSION /79.009/
C     !EJECT
      data      (VDSCRPT(I),I=1,15) /
C     '---------1---------2---------3---------4---------5---------6---'
     $'Sep 2014 -----',
     $'Miscellany (01); Revised NE calculation (3) RL/SGK.',
     $'Input elements check expanded SGK; option ALLCICE(1).',
     $'no call ABORT in ENLIL when beta1 => 0 SGK/EA.',
     $ 11*' '/
      data      (VDSCRPT(I),I=16,30) /
C     '---------1---------2---------3---------4---------5---------6---'
     $ 15*' '/
      data      (VDSCRPT(I),I=31,45) /
C     '---------1---------2---------3---------4---------5---------6---'
     $ 15*' '/
C     !EJECT
C
      call HI ('AARDVRK')
C     !BEG
      call HOSTAGE  (LU, LUSD, qummy)
      call GET_DATE (ENDDAT)
      call GET_TIME (ENDTIM)
C
      call LINER    (3, LU)
      if(IRUNT.gt.0) then
        write (LU,100) BEGDAT,BEGTIM,ENDDAT,ENDTIM,VERSION
  100   format(' ','This  PANDORA  run began on ',A11,', at ',A8,','/
     $         ' ','               and ended on ',A11,', at ',A8,','//
     $         ' ','               using program version ',F7.3,'.')
        call LINER  (2, LU)
        write (LU,101) (VDSCRPT(I),I=1,NVDSCR)
  101   format(' ',15X,A63)
        call LINER  (2, LU)
        write (LU,102)
  102   format(' ','EHA'/' ','RKL')
      else
        write (LU,100) BEGDAT,BEGTIM,ENDDAT,ENDTIM,VERSION
      end if
C     !END
      call BYE ('AARDVRK')
C
      return
      end
