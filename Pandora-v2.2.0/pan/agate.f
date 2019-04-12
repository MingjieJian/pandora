      subroutine AGATE
     $(NUMKON,INDX,KODE)
C
C     Rudolf Loeser, 1980 Jun 26
C---- Retrieves maximum contribution depth indices, for SPIDER.
C     (This is version 3 of AGATE.)
C     !DASH
      save
C     !DASH
      real*8 WAVE, dummy
      integer I, INDX, KEOF, KODE, LUEO, M, MODE, NUMKON, jummy
      logical lummy
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- ICON        as of 1999 Mar 30
      integer     ICBNCH,MXICON,MXIADR
      parameter   (ICBNCH=10)
      parameter   (MXICON=50*ICBNCH)
      parameter   (MXIADR=1000)
C     (Remember to recompile all users when changing any parameter!)
      integer     ICADRS,NIADR,NICON
      real*8      SSBUFF
      logical     ICSTRT, ICFULL
      dimension   ICADRS(MXIADR),SSBUFF(MXICON+ICBNCH)
      common      /ICON1/ NICON,NIADR,ICADRS
      common      /ICON2/ SSBUFF
      common      /ICON3/ ICSTRT, ICFULL
C     Buffer, and record addresses, and control parameters,
C     for saving/restoring Spectrum Summary data.
C     .
C     !DASH
C     !EJECT
      external ZORRO, MESHED, MASHED, HI, BYE
C
C               INDX(Numkon1)
      dimension INDX(*)
C
      call HI ('AGATE')
C     !BEG
      ICSTRT = .true.
      KODE   = 1
C
      I = 0
  100 continue
        call ZORRO    (WAVE, dummy, M, dummy, jummy, dummy, dummy,
     $                 jummy, MODE, lummy, KEOF)
C
        if(KEOF.eq.1) then
          call MESHED ('AGATE', 3)
          write (LUEO,101) I,NUMKON
  101     format(' ','Trouble in AGATE - not enough data in file.',
     $               5X,2I10//
     $           ' ','Check Emergent Intensity calculations.'//
     $           ' ','"CONTRIBUTORS" will be skipped.')
          call MASHED ('AGATE')
          KODE = 0
          goto 102
        end if
C
        I = I+1
        if(MODE.eq.2) then
          INDX(I) = M
        else
          INDX(I) = -1
        end if
      if(I.lt.NUMKON) goto 100
C
  102 continue
C     !END
      call BYE ('AGATE')
C
      return
      end
