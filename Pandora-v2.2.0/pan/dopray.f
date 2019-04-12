      subroutine DOPRAY
     $(MODE,N,IZ,CZ,ZOLD,GMASIN,P,Y,GG,ZZ,OP5,TAU5,Z,WAVE,ISWA,XCBL)
C
C     Rudolf Loeser, 2003 Nov 03
C---- Prints for Z-from-mass calculation.
C     MODE = 1 means: HSE
C     MODE = 2 means: initialization
C     !DASH
      save
C     !DASH
      real*8 CZ, GG, GMASIN, OP5, P, TAU5, WAVE, XCBL, Y, Z, ZOLD, ZZ
      integer I, ISWA, IZ, LU, MO, MODE, N, NO
      character NOTE*26
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external LINER, ABJECT, SHIM, PLINTH, HI, BYE
C
C               GMASIN(N), ZOLD(N), OP5(N), TAU5(N), ZZ(N), P(N), Y(N),
      dimension GMASIN(*), ZOLD(*), OP5(*), TAU5(*), ZZ(*), P(*), Y(*),
C
C               GG(N), Z(N), XCBL(Miklen), ISWA(Nopac)
     $          GG(*), Z(*), XCBL(*),      ISWA(*)
C     !EJECT
C
      call HI ('DOPRAY')
C     !BEG
      if(MODE.eq.1) then
        LU   = MO
        NOTE = '.'
      else
        LU   = NO
        NOTE = ' (of the HSE calculation).'
      end if
C
      if(LU.gt.0) then
        call LINER    (5, LU)
        write (LU,100) NOTE
  100   format(' ','Calculation of Z from input mass and the ',
     $             'function G',A//
     $         ' ',6X,'i',6X,'Z-old',9X,'Mass',12X,'P',13X,'y',
     $             11X,'1/G',8X,'(Z-Z1)',6X,'OP-5000',5X,'TAU-5000',
     $             8X,'Z-new')
        call LINER    (1, LU)
C
        do 103 I = 1,N
          if(I.eq.(IZ+1)) then
            write (LU,101) CZ
  101       format(' ',74X,1PE14.6,13X,'  1.0')
          end if
          write (LU,102) I,ZOLD(I),GMASIN(I),P(I),Y(I),GG(I),ZZ(I),
     $                     OP5(I),TAU5(I),Z(I)
  102     format(' ',I7,1P3E13.5,3E14.6,3E13.5)
          call SHIM   (I, 5, LU)
  103   continue
C
        if(MODE.eq.1) then
          call ABJECT (LU)
        else
          call PLINTH (WAVE, XCBL, ISWA, LU)
        end if
      end if
C     !END
      call BYE ('DOPRAY')
C
      return
      end
