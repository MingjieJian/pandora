      subroutine KRIGIA
     $(XL,XH,YL,YH,NV,NH)
C
C     Rudolf Loeser, 1988 Jan 04
C---- Prints and aborts for a KINIT error.
C     !DASH
      save
C     !DASH
      real*8 XH, XL, YH, YL
      integer LUEO, NH, NV
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, ABORT, HI, BYE
C
      call HI ('KRIGIA')
C     !BEG
      call MESHED ('KRIGIA', 1)
      write (LUEO,100) XL,XH,NH, YL,YH,NV
  100 format(' ','Printer-Plot error.',1P//
     $       ' ','XL =',E24.16,', XH =',E24.16,', NH =',I12/
     $       ' ','YL =',E24.16,', YH =',E24.16,', NV =',I12)
C
      call ABORT
C     !END
      call BYE ('KRIGIA')
C
      return
      end
