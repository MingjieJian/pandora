      subroutine WHIN
     $(I,FDX,EFDX,PEFDX,XK,TERM,FI)
C
C     Rudolf Loeser, 1983 Oct 19
C---- Dumps, for FURZE.
C     !DASH
      save
C     !DASH
      real*8 EFDX, FDX, FI, PEFDX, TERM, XK
      integer I, LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external SHIM, HI, BYE
C
      call HI ('WHIN')
C     !BEG
      call SHIM (I,5,LUEO)
      write (LUEO,100) I,FDX,EFDX,PEFDX,XK,TERM,FI
  100 format(' ',I4,1P6E16.8)
C     !END
      call BYE ('WHIN')
C
      return
      end
