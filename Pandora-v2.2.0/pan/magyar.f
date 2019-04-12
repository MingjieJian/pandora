      subroutine MAGYAR
     $(WTAB,KOUNT,ISMBD,N,TMU,SNU,WS,YNT,YY,MUX,KODE)
C
C     Rudolf Loeser, 1987 Feb 02
C---- Produces a "SIMBA" dump.
C     (This is version 2 of MAGYAR.)
C     !DASH
      save
C     !DASH
      real*8 SNU, TMU, W, WS, WTAB, YNT, YY
      integer I, ISMBD, KODE, KOUNT, LUEO, MUX, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- MOSTAR      as of 2000 Sep 26
      real*8      COREWL,COREWN
      integer     ICORE
      character   WLAB1*10,WLAB2*2,WLAB3*12,WLAB4*10,WLAB5*2
      logical     WAVENO,SLINE,WHOLE,RED,BLUE
      common      /MOSTAR1/ COREWL,COREWN
      common      /MOSTAR2/ ICORE
      common      /MOSTAR3/ WLAB1,WLAB2,WLAB3,WLAB4,WLAB5
      common      /MOSTAR4/ WAVENO,SLINE,WHOLE,RED,BLUE
C     Wavelength/Wavenumber print/plot controls (subroutine WUMBLE).
C     .
C     !DASH
C     !EJECT
      external MESHED, MASHED, LINER, SHIM, HI, BYE
C
C               TMU(N), SNU(N), WS(N)
      dimension TMU(*), SNU(*), WS(*)
C
      call HI ('MAGYAR')
C     !BEG
      call MESHED ('MAGYAR', 2)
C
      write (LUEO,100) ISMBD,KOUNT,WLAB1(2:),WTAB,YNT,YY,MUX,KODE
  100 format(' ','Intensity Integration (SIMBA) dump, produced (by ',
     $           'MAGYAR) with ISMBD =',I5,5X,'(For no dump, leave ',
     $           'ISMBD = 0.)'//
     $       ' ','This is the',I5,'. integral to be computed.'//
     $       ' ','W',A,' =',1PE16.8,5X,'Intensity =',E16.8,5X,
     $           'F =',0PF6.3,5X,'K =',I4,5X,'Error code =',I3//
     $       ' ',4X,'I',16X,'TAU',19X,'S',18X,'WS',16X,'WS*S')
      call LINER  (1,LUEO)
C
      do 102 I = 1,N
        W = WS(I)*SNU(I)
        write (LUEO,101) I,TMU(I),SNU(I),WS(I),W
  101   format(' ',I5,1P4E20.12)
        call SHIM (I,5,LUEO)
  102 continue
C
      call MASHED ('MAGYAR')
C     !END
      call BYE ('MAGYAR')
C
      return
      end
