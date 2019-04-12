      subroutine LIVER
     $(NO,I,EMU,LFB,WAVENO,IJECT)
C
C     Rudolf Loeser, 1983 Oct 28
C---- Prints a heading, for HEART.
C     !DASH
      save
C     !DASH
      real*8 EMU
      integer I, IJECT, IQWNM, LFB, NO
      logical WAVENO
      character BLANK*1, FACELAB*10, LAB*32
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
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
      equivalence (IQQ(290),IQWNM)
C     !DASH
      external DEJECT, LINER, DOUBLER, TUMBLE, HI, BYE
C
C               EMU(L)
      dimension EMU(*)
C     !EJECT
C
      call HI ('LIVER')
C     !BEG
      if(NO.gt.0) then
C
        if(I.eq.1) then
          WAVENO = IQWNM.gt.0
          call DEJECT  (NO,IJECT)
          call TUMBLE  (LFB,FACELAB)
C
          if(FACELAB(10:10).eq.BLANK) then
            write (NO,100)
  100       format(' ',35X,'Averages over Composite Lines Opacity ',
     $                 'wavelengths bands.')
          else
            write (NO,101) FACELAB
  101       format(' ',29X,'Averages over Composite Lines Opacity ',
     $                 'wavelengths bands.',2X,A10)
          end if
C
          call LINER   (1,NO)
          call DOUBLER (NO)
        end if
C
        call LINER     (2,NO)
        write (NO,102) EMU(I)
  102   format(' ',7('--------'),'  Mu =',F7.4,'  ',7('--------'))
        call LINER    (1,NO)
        if(WAVENO) then
          LAB = 'Wavenumber interval     (/cm)   '
        else
          LAB = 'Wavelength interval  (Angstroms)'
        end if
C
        write (NO,103) LAB
  103   format(' ',12X,'Band',4X,'Members',4X,A32,7X,'<I/HZ>',8X,
     $            '<I/A>',10X,'<TB>'/
     $         ' ',37X,'from',14X,'to')
        call LINER     (1,NO)
      end if
C     !END
      call BYE ('LIVER')
C
      return
      end
